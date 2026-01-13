//! MCP server surface for querying and editing rust-analyzer workspaces.

pub mod analysis;
pub mod discover;
pub mod error;
pub mod params;
pub mod render;
pub mod requests;
pub mod server;
pub mod types;

pub use analysis::{Analysis, Snapshot, Workspace};
pub use error::McpError;
pub use server::RaMcpServer;
