//! Binary entry point for the rust-analyzer MCP server.

use ra_mcp_handlers::RaMcpServer;
use rmcp::ServiceExt;
use tracing_subscriber::EnvFilter;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting ra-mcp server");

    let parallelism = std::thread::available_parallelism().map_or(4, |n| n.get());
    let runtime = tokio::runtime::Builder::new_current_thread()
        .max_blocking_threads(parallelism)
        .enable_all()
        .build()?;

    runtime.block_on(async {
        let server = RaMcpServer::new();
        let service = server.serve(rmcp::transport::stdio()).await?;
        service.waiting().await?;
        Ok(())
    })
}
