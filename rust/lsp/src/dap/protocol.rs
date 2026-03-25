//! DAP protocol types.
//!
//! Minimal serde structs for the Debug Adapter Protocol messages we handle.
//! No external DAP library — the protocol is ~15 message types over JSON.

use serde::{Deserialize, Serialize};

// ── Envelope ─────────────────────────────────────────────────────────────────

#[derive(Deserialize, Debug)]
pub struct DapRequest {
    pub seq: i64,
    pub command: String,
    #[serde(default)]
    pub arguments: serde_json::Value,
}

#[derive(Serialize, Debug)]
pub struct DapResponse {
    pub seq: i64,
    #[serde(rename = "type")]
    pub msg_type: &'static str,
    pub request_seq: i64,
    pub success: bool,
    pub command: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
}

#[derive(Serialize, Debug)]
pub struct DapEvent {
    pub seq: i64,
    #[serde(rename = "type")]
    pub msg_type: &'static str,
    pub event: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
}

// ── Request arguments ────────────────────────────────────────────────────────

#[derive(Deserialize, Debug, Default)]
#[serde(rename_all = "camelCase")]
pub struct LaunchArgs {
    pub grammar: String,
    #[serde(default)]
    pub input: Option<String>,
    #[serde(default)]
    pub input_text: Option<String>,
    #[serde(default)]
    pub entry_rule: Option<String>,
    #[serde(default)]
    pub stop_on_entry: Option<bool>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SetBreakpointsArgs {
    pub source: Source,
    #[serde(default)]
    pub breakpoints: Vec<SourceBreakpoint>,
}

#[derive(Deserialize, Debug)]
pub struct Source {
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub name: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct SourceBreakpoint {
    pub line: u32,
    #[serde(default)]
    pub column: Option<u32>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SetFunctionBreakpointsArgs {
    pub breakpoints: Vec<FunctionBreakpoint>,
}

#[derive(Deserialize, Debug)]
pub struct FunctionBreakpoint {
    pub name: String,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct StackTraceArgs {
    pub thread_id: i64,
    #[serde(default)]
    pub start_frame: Option<i64>,
    #[serde(default)]
    pub levels: Option<i64>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ScopesArgs {
    pub frame_id: i64,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct VariablesArgs {
    pub variables_reference: i64,
}

// ── Response bodies ──────────────────────────────────────────────────────────

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Capabilities {
    pub supports_configuration_done_request: bool,
    pub supports_function_breakpoints: bool,
    pub supports_step_back: bool,
    pub supports_stepping_granularity: bool,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Thread {
    pub id: i64,
    pub name: String,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct StackFrame {
    pub id: i64,
    pub name: String,
    pub source: Option<SourceRef>,
    pub line: u32,
    pub column: u32,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SourceRef {
    pub name: Option<String>,
    pub path: Option<String>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Scope {
    pub name: String,
    pub variables_reference: i64,
    pub expensive: bool,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Variable {
    pub name: String,
    pub value: String,
    #[serde(rename = "type")]
    pub ty: Option<String>,
    pub variables_reference: i64,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Breakpoint {
    pub id: Option<i64>,
    pub verified: bool,
    pub line: Option<u32>,
    pub message: Option<String>,
}
