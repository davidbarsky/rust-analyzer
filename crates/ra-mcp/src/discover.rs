//! Support for non-cargo build systems that describe their project by running a
//! command that emits a `rust-project.json` (the `discoverProject` flow).
//!
//! The configuration lives in an `ra-mcp.json` file at the workspace root:
//!
//! ```json
//! { "discover": { "command": ["my-tool", "develop-json", "{arg}"],
//!                 "progressLabel": "my-tool", "filesToWatch": ["BUCK"] } }
//! ```
//!
//! The command receives `{arg}` replaced by a JSON object — `{"path": "…"}` to
//! discover the project owning a file, or `{"buildfile": "…"}` to refresh a
//! known one — and streams JSON-lines back: `progress`, then either `finished`
//! (carrying the project) or `error`.

use std::io::{BufRead as _, BufReader};
use std::process::Stdio;

use anyhow::Context as _;
use ide_db::FxHashMap;
use paths::{AbsPath, AbsPathBuf};
use project_model::ProjectJsonData;
use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoverConfig {
    pub command: Vec<String>,
    #[serde(default)]
    pub progress_label: String,
    #[serde(default)]
    pub files_to_watch: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RaMcpConfig {
    discover: Option<DiscoverConfig>,
}

/// Reads the discover configuration from `ra-mcp.json` at `root`, if present.
pub fn read_discover_config(root: &AbsPath) -> Option<DiscoverConfig> {
    let path = root.join("ra-mcp.json");
    let contents = std::fs::read_to_string(&path).ok()?;
    match serde_json::from_str::<RaMcpConfig>(&contents) {
        Ok(config) => config.discover,
        Err(e) => {
            tracing::warn!(%path, error = %e, "failed to parse ra-mcp.json");
            None
        }
    }
}

/// Which project to (re)discover: `Path` finds the project owning a file,
/// `Buildfile` refreshes the project a known buildfile belongs to.
pub enum DiscoverArgument {
    Path(AbsPathBuf),
    Buildfile(AbsPathBuf),
}

impl DiscoverArgument {
    fn to_json(&self) -> String {
        let (key, path) = match self {
            DiscoverArgument::Path(path) => ("path", path),
            DiscoverArgument::Buildfile(path) => ("buildfile", path),
        };
        serde_json::json!({ key: path.as_str() }).to_string()
    }
}

#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum DiscoverMessage {
    Finished { buildfile: String, project: Box<ProjectJsonData> },
    Error { error: String, source: Option<String> },
    Progress { message: String },
}

/// Runs the configured discover command for `arg`, returning the emitted project
/// and the buildfile it belongs to.
pub fn run_discover(
    config: &DiscoverConfig,
    arg: &DiscoverArgument,
    current_dir: &AbsPath,
) -> anyhow::Result<(ProjectJsonData, AbsPathBuf)> {
    let Some((program, args)) = config.command.split_first() else {
        anyhow::bail!("discover command is empty");
    };
    let arg = arg.to_json();
    let args: Vec<String> = args.iter().map(|it| it.replace("{arg}", &arg)).collect();

    let mut command = toolchain::command(program, current_dir, &FxHashMap::default());
    command.args(&args).stdout(Stdio::piped()).stderr(Stdio::inherit());
    let mut child =
        command.spawn().with_context(|| format!("failed to spawn discover command `{program}`"))?;

    let stdout = child.stdout.take().expect("stdout was piped");
    let mut project = None;
    for line in BufReader::new(stdout).lines() {
        let line = line?;
        if line.is_empty() {
            continue;
        }
        match serde_json::from_str::<DiscoverMessage>(&line) {
            Ok(DiscoverMessage::Finished { buildfile, project: data }) => {
                let buildfile = AbsPathBuf::try_from(buildfile.as_str()).map_err(|_| {
                    anyhow::anyhow!("discover returned a non-absolute buildfile: {buildfile}")
                })?;
                project = Some((*data, buildfile));
            }
            Ok(DiscoverMessage::Progress { message }) => {
                tracing::debug!(label = %config.progress_label, message, "discover progress");
            }
            Ok(DiscoverMessage::Error { error, source }) => {
                anyhow::bail!("discover command reported an error: {error} ({source:?})");
            }
            Err(e) => tracing::warn!(error = %e, line, "unparseable discover output line"),
        }
    }

    let status = child.wait().context("waiting on discover command")?;
    if !status.success() {
        anyhow::bail!("discover command exited unsuccessfully: {status}");
    }
    project.context("discover command finished without producing a project")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cwd() -> AbsPathBuf {
        AbsPathBuf::assert_utf8(std::env::current_dir().unwrap())
    }

    #[cfg(unix)]
    #[test]
    fn argument_serializes_to_json() {
        let path = AbsPathBuf::assert_utf8("/ws/src/foo.rs".into());
        assert_eq!(DiscoverArgument::Path(path).to_json(), r#"{"path":"/ws/src/foo.rs"}"#);
        let buildfile = AbsPathBuf::assert_utf8("/ws/BUCK".into());
        assert_eq!(DiscoverArgument::Buildfile(buildfile).to_json(), r#"{"buildfile":"/ws/BUCK"}"#);
    }

    #[test]
    fn reads_config_from_ra_mcp_json() {
        let dir = temp_dir::TempDir::new().unwrap();
        std::fs::write(
            dir.path().join("ra-mcp.json"),
            r#"{"discover":{"command":["tool","{arg}"],"progressLabel":"tool","filesToWatch":["BUCK"]}}"#,
        )
        .unwrap();
        let root = AbsPathBuf::assert_utf8(dir.path().to_path_buf());
        let config = read_discover_config(&root).expect("config should be present");
        assert_eq!(config.command, ["tool", "{arg}"]);
        assert_eq!(config.progress_label, "tool");
        assert_eq!(config.files_to_watch, ["BUCK"]);
    }

    #[test]
    fn missing_or_malformed_config_is_none() {
        let dir = temp_dir::TempDir::new().unwrap();
        let root = AbsPathBuf::assert_utf8(dir.path().to_path_buf());
        assert!(read_discover_config(&root).is_none());

        std::fs::write(dir.path().join("ra-mcp.json"), "not json at all").unwrap();
        assert!(read_discover_config(&root).is_none());

        std::fs::write(dir.path().join("ra-mcp.json"), "{}").unwrap();
        assert!(read_discover_config(&root).is_none());
    }

    #[test]
    fn empty_command_errors() {
        let config = DiscoverConfig {
            command: vec![],
            progress_label: String::new(),
            files_to_watch: vec![],
        };
        let cwd = cwd();
        let err = run_discover(&config, &DiscoverArgument::Path(cwd.clone()), &cwd).unwrap_err();
        assert!(err.to_string().contains("empty"), "{err}");
    }

    #[cfg(unix)]
    fn shell_discover(script: &str) -> DiscoverConfig {
        DiscoverConfig {
            command: vec!["sh".to_owned(), "-c".to_owned(), script.to_owned()],
            progress_label: String::new(),
            files_to_watch: vec![],
        }
    }

    #[cfg(unix)]
    #[test]
    fn runs_command_and_returns_finished_project() {
        let config = shell_discover(
            r#"printf '%s\n' '{"kind":"progress","message":"working"}' '{"kind":"finished","buildfile":"/tmp/ra-mcp-BUCK","project":{"crates":[]}}'"#,
        );
        let cwd = cwd();
        let (_project, buildfile) =
            run_discover(&config, &DiscoverArgument::Path(cwd.clone()), &cwd).expect("discover ok");
        assert_eq!(buildfile.as_str(), "/tmp/ra-mcp-BUCK");
    }

    #[cfg(unix)]
    #[test]
    fn surfaces_command_error() {
        let config =
            shell_discover(r#"printf '%s\n' '{"kind":"error","error":"boom","source":null}'"#);
        let cwd = cwd();
        let err = run_discover(&config, &DiscoverArgument::Path(cwd.clone()), &cwd).unwrap_err();
        assert!(err.to_string().contains("boom"), "{err}");
    }

    #[cfg(unix)]
    #[test]
    fn errors_when_no_project_emitted() {
        let config =
            shell_discover(r#"printf '%s\n' '{"kind":"progress","message":"only progress"}'"#);
        let cwd = cwd();
        let err = run_discover(&config, &DiscoverArgument::Path(cwd.clone()), &cwd).unwrap_err();
        assert!(err.to_string().contains("without producing a project"), "{err}");
    }
}
