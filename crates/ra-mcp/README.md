# ra-mcp

An MCP (Model Context Protocol) server exposing rust-analyzer's semantic code
search, navigation, and refactoring to coding agents. Unlike textual search,
every result is resolved through the compiler's view of the code: usage and
call edges found through traits, macros, method dispatch, and re-exports;
no hits from comments, strings, or same-named symbols in other scopes.

## Tools

| Instead of… | Use |
|---|---|
| `rg 'fn process_batch'` (find a definition) | `find_symbol` |
| manually tracing callers/callees, symbol usages, or implementations | `reachable` |
| grepping for a declaration after seeing a call | `inspect` (definition + type + docs) |
| grep regexes approximating code structure | `search` (structural patterns) |
| `sed`/find-and-replace renames | `rename` |
| `sed` rewrites across call sites | `ssr` (previews by default; `apply: true` writes) |
| opening a file to check a type or signature | `inspect` |
| parsing `cargo metadata` | `workspace_crates` |

The server is stateless: every result is recomputed from the request, so
nothing goes stale and nothing survives a restart to dangle. Truncated results
end with a `[K more; refine the query or rerun with offset=N]` line; `search`
narrows by semantic criteria directly (test/unsafe/async context, enclosing
function or impl, usage kind, path glob), and its `countBy` mode returns
grouped counts — like `grep -c` — to size up a broad pattern before paging.
Write-capable tools (`ssr`, `add_argument`) preview by default and only touch
disk when called with `apply: true`.

## Setup with Claude Code

```bash
claude mcp add ra-mcp -- /path/to/ra-mcp
```

The server speaks stdio and initializes its workspace from the client's
`file://` roots — no arguments or configuration needed. Indexing a large
workspace can take minutes on first launch; queries that arrive before the
index is ready block until it is, then answer. They do not error.

### Transport and timeouts

stdio only. In Claude Code, stdio servers are exempt from the idle timeout and
the tool timeout defaults to roughly 28 hours when `MCP_TOOL_TIMEOUT` is unset,
so blocking through a cold index is safe. Claude Desktop hardcodes a ~60 second
client timeout and silently drops late results; **Claude Desktop is not a
supported client.**

## Recommended: enforce adoption with a PreToolUse hook

Coding agents have a strong prior toward bash `grep`/`rg` for symbol search.
This prior survives system-prompt and CLAUDE.md instructions — it is documented
even against Claude Code's own built-in tools
([anthropics/claude-code#21696](https://github.com/anthropics/claude-code/issues/21696)).
Exhortations in CLAUDE.md are read once at session start and decay as the
context grows; by the time the model reaches for a symbol search, the
instruction is long out of attention. The only mechanism that reliably beats
the prior is a denial **at the moment of the grep attempt**, whose reason lands
in context as an immediate, actionable correction.

Save this as `.claude/hooks/deny-symbol-grep.sh` (and `chmod +x` it):

```bash
#!/usr/bin/env bash
# Deny bash grep/rg Rust symbol searches; redirect to ra-mcp semantic tools.
input=$(cat)
command=$(jq -r '.tool_input.command // empty' <<<"$input")
[ -z "$command" ] && exit 0

grep -qE '(^|[|;& ])(rg|grep|egrep)\b' <<<"$command" || exit 0
grep -qE "(fn|struct|enum|trait|impl|mod|type|const|static|macro_rules!?)[[:space:]]+[A-Za-z_]" \
  <<<"$command" || exit 0

jq -n '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    permissionDecision: "deny",
    permissionDecisionReason: "Use the ra-mcp semantic tools for Rust symbol searches, not grep/rg: definitions -> mcp__ra-mcp__find_symbol (query=NAME, fuzzy by default), call/usage/implementation graph reachability -> mcp__ra-mcp__reachable (file plus line/column or range, direction, edgeKinds, depth, optional target/scope, and filters path/nodeKinds/dispatch/referenceCategories/usageKinds), structural patterns -> mcp__ra-mcp__search. These resolve through traits, macros, and re-exports and skip comments and strings — grep cannot."
  }
}'
```

Then register it in `.claude/settings.json`:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "$CLAUDE_PROJECT_DIR/.claude/hooks/deny-symbol-grep.sh"
          }
        ]
      }
    ]
  }
}
```

The pattern is deliberately conservative: it only denies grep/rg invocations
that search for Rust declaration syntax (`fn name`, `struct Name`, `impl Name`,
…), leaving greps over logs, configs, and non-symbol text untouched. Tighten or
loosen the second regex to taste.

## Output format

Every successful tool result includes `structuredContent` with the typed result
JSON advertised by that tool's `outputSchema`. Treat that structured content as
the authoritative model-facing API.

The plain-text `content` block is transcript-oriented Markdown for humans.
Most navigation and search tools render grep-shaped lines:

```
src/batch.rs:142:8: fn process_batch: pub fn process_batch(items: &[Item]) -> Result<()>
src/server.rs:87:13: read: let total = process_batch(&items)?;
[3 more; refine the query or rerun with offset=2]
```

Paths are workspace-relative and line/column are 1-based, so result lines are
directly usable as arguments to file tools and to other ra-mcp calls (which
accept workspace-relative paths back). Apply-style tools return
`applied N edits in M files` plus a capped sample of edit lines.

`inspect` keeps the location line grep-shaped, then returns labeled Markdown
sections such as `signature:`, `declaration:`, `type:`, `code:`, and `docs:`.
Rust metadata sections contain fenced `rust` blocks; function, method, const,
and static declarations include source bodies when available. Focused fields and
enum variants are marked with Rust comments so the snippet remains regular Rust code.
Macro expansion also renders the generated Rust in a fenced `rust` block. The
structured `LineColRange` values use the same 1-based line/column convention as
the text lines, so they can be fed directly back into ra-mcp or file-editing tools.

`reachable` keeps transcript lines compact, but its structured edge locations
also include the enclosing item and, when useful, a bounded multiline source
excerpt such as the enclosing `match` arm or statement. From a trait or trait
method declaration, use `edgeKinds: ["implementation"]` to reach impl blocks
and implementing method bodies; `usage` edges are references, not impl
navigation. Prefer filtering `reachable` with `path`, `nodeKinds`, `dispatch`,
`referenceCategories`, and `usageKinds` before falling back to `read`.

Rejected tool requests are returned as ordinary successful MCP calls, not
JSON-RPC call failures and not `isError: true` tool failures. Their
`structuredContent` has `code`, `message`, and `guidance` fields so agents can
adjust arguments and retry without treating ra-mcp as unavailable.
