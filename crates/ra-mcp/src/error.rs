//! Error types shared by MCP request handling and tool responses.

use std::borrow::Cow;

use ide_db::base_db::salsa::Cancelled;
use rmcp::model::{ErrorCode, ErrorData};

#[derive(Debug, thiserror::Error)]
pub enum McpError {
    #[error(transparent)]
    Request(#[from] RequestError),
    #[error(transparent)]
    Analysis(#[from] AnalysisError),
    #[error(transparent)]
    Environment(#[from] EnvironmentError),
}

#[derive(Debug, thiserror::Error)]
pub enum RequestError {
    #[error(
        "Server workspace not initialized. Client must provide workspace roots during MCP initialization."
    )]
    NotInitialized,
    #[error("Invalid path: {path}")]
    InvalidPath { path: String },
    #[error("File not found in workspace: {path}")]
    FileNotFound { path: String },
    #[error("Invalid position (line {line}, column {column}): {reason}")]
    InvalidPosition { line: u32, column: u32, reason: String },
    #[error("Invalid {kind}: {message}")]
    InvalidSyntax { kind: &'static str, message: String },
    #[error("Invalid glob pattern '{pattern}': {message}")]
    InvalidPattern { pattern: String, message: String },
    #[error("Invalid request: {message}")]
    InvalidRequest { message: String },
    #[error("Rename rejected: {reason}")]
    RenameRejected { reason: String },
}

#[derive(Debug, thiserror::Error)]
pub enum AnalysisError {
    #[error("Analysis failed during {operation}: {details}")]
    Failed { operation: &'static str, details: String },
    #[error("No {what} found")]
    NotFound { what: &'static str },
    #[error("Analysis snapshot became stale")]
    StaleSnapshot,
    #[error("Analysis cancelled during {operation}: {source}")]
    Cancelled {
        operation: &'static str,
        #[source]
        source: Cancelled,
    },
}

/// Returns a closure that converts a salsa `Cancelled` into an
/// `AnalysisError::Cancelled` tagged with the given operation name.
pub fn cancelled_in(operation: &'static str) -> impl FnOnce(Cancelled) -> AnalysisError {
    move |source| AnalysisError::Cancelled { operation, source }
}

#[derive(Debug, thiserror::Error)]
pub enum EnvironmentError {
    #[error("IO error for {path}: {message}")]
    Io { path: String, message: String },
    #[error("Task failed: {message}")]
    TaskFailed { message: String },
    #[error("Failed to load workspace: {message}")]
    WorkspaceLoadFailed { message: String },
}

impl RequestError {
    fn error_code(&self) -> ErrorCode {
        match self {
            RequestError::NotInitialized => ErrorCode::INVALID_REQUEST,
            RequestError::InvalidPath { .. }
            | RequestError::FileNotFound { .. }
            | RequestError::InvalidPosition { .. }
            | RequestError::InvalidSyntax { .. }
            | RequestError::InvalidPattern { .. }
            | RequestError::InvalidRequest { .. }
            | RequestError::RenameRejected { .. } => ErrorCode::INVALID_PARAMS,
        }
    }
}

impl AnalysisError {
    fn error_code(&self) -> ErrorCode {
        match self {
            AnalysisError::Failed { .. }
            | AnalysisError::NotFound { .. }
            | AnalysisError::StaleSnapshot
            | AnalysisError::Cancelled { .. } => ErrorCode::INTERNAL_ERROR,
        }
    }
}

impl EnvironmentError {
    fn error_code(&self) -> ErrorCode {
        match self {
            EnvironmentError::Io { .. }
            | EnvironmentError::TaskFailed { .. }
            | EnvironmentError::WorkspaceLoadFailed { .. } => ErrorCode::INTERNAL_ERROR,
        }
    }
}

impl McpError {
    fn error_code(&self) -> ErrorCode {
        match self {
            McpError::Request(e) => e.error_code(),
            McpError::Analysis(e) => e.error_code(),
            McpError::Environment(e) => e.error_code(),
        }
    }

    pub fn into_error_data(self) -> ErrorData {
        ErrorData { code: self.error_code(), message: Cow::Owned(self.to_string()), data: None }
    }
}

impl From<McpError> for ErrorData {
    fn from(err: McpError) -> Self {
        err.into_error_data()
    }
}

impl From<RequestError> for ErrorData {
    fn from(err: RequestError) -> Self {
        McpError::from(err).into_error_data()
    }
}

impl From<AnalysisError> for ErrorData {
    fn from(err: AnalysisError) -> Self {
        McpError::from(err).into_error_data()
    }
}

impl From<EnvironmentError> for ErrorData {
    fn from(err: EnvironmentError) -> Self {
        McpError::from(err).into_error_data()
    }
}
