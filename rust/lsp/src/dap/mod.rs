//! Debug Adapter Protocol server for BBNF grammars.
//!
//! Activated via `bbnf-lsp --dap`. Uses the same stdin/stdout JSON transport
//! as LSP (Content-Length framed), but speaks DAP instead.
//!
//! Sub-modules:
//! - `protocol`: DAP message type definitions (serde)
//! - `adapter`: Grammar compilation, interpreter lifecycle, request dispatch
//! - `mapping`: Line↔offset conversion, breakpoint resolution

pub mod adapter;
pub mod mapping;
pub mod protocol;

use std::io::{self, BufRead, Write};

use adapter::DapAdapter;
use bbnf_ir::interpreter::StepMode;
use protocol::*;

/// Run the DAP server on stdin/stdout.
///
/// Reads Content-Length framed JSON requests, dispatches to the adapter,
/// and writes JSON responses/events.
pub fn serve_dap() {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut reader = stdin.lock();
    let mut writer = stdout.lock();

    let mut seq_counter: i64 = 1;
    let mut adapter: Option<DapAdapter> = None;
    let mut grammar_path = String::new();
    let mut last_snapshot: Option<bbnf_ir::interpreter::DebugSnapshot> = None;

    while let Ok(msg) = read_message(&mut reader) {
        let request: DapRequest = match serde_json::from_str(&msg) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("DAP: invalid request: {}", e);
                continue;
            }
        };

        match request.command.as_str() {
            "initialize" => {
                let body = Capabilities {
                    supports_configuration_done_request: true,
                    supports_function_breakpoints: true,
                    supports_step_back: true,
                    supports_stepping_granularity: true,
                };
                send_response(
                    &mut writer,
                    &mut seq_counter,
                    &request,
                    true,
                    Some(serde_json::to_value(body).unwrap()),
                );
                // Send initialized event.
                send_event(&mut writer, &mut seq_counter, "initialized", None);
            }

            "launch" => {
                let args: LaunchArgs =
                    serde_json::from_value(request.arguments.clone()).unwrap_or_default();
                grammar_path = args.grammar.clone();

                match DapAdapter::launch(&args) {
                    Ok(a) => {
                        adapter = Some(a);
                        send_response(&mut writer, &mut seq_counter, &request, true, None);
                    }
                    Err(e) => {
                        send_response_error(
                            &mut writer,
                            &mut seq_counter,
                            &request,
                            &format!("Launch failed: {}", e),
                        );
                    }
                }
            }

            "setBreakpoints" => {
                if let Some(ref mut a) = adapter {
                    let args: SetBreakpointsArgs =
                        serde_json::from_value(request.arguments.clone()).unwrap_or_else(|_| {
                            SetBreakpointsArgs {
                                source: Source { path: None, name: None },
                                breakpoints: Vec::new(),
                            }
                        });
                    let verified = a.set_breakpoints(&args);
                    let body = serde_json::json!({ "breakpoints": verified });
                    send_response(
                        &mut writer,
                        &mut seq_counter,
                        &request,
                        true,
                        Some(body),
                    );
                } else {
                    send_response(&mut writer, &mut seq_counter, &request, true, None);
                }
            }

            "setFunctionBreakpoints" => {
                if let Some(ref mut a) = adapter {
                    let args: SetFunctionBreakpointsArgs =
                        serde_json::from_value(request.arguments.clone()).unwrap_or_else(|_| {
                            SetFunctionBreakpointsArgs { breakpoints: Vec::new() }
                        });
                    let verified = a.set_function_breakpoints(&args);
                    let body = serde_json::json!({ "breakpoints": verified });
                    send_response(
                        &mut writer,
                        &mut seq_counter,
                        &request,
                        true,
                        Some(body),
                    );
                } else {
                    send_response(&mut writer, &mut seq_counter, &request, true, None);
                }
            }

            "configurationDone" => {
                send_response(&mut writer, &mut seq_counter, &request, true, None);

                // If stop_on_entry, send a stopped event immediately.
                if let Some(ref a) = adapter {
                    if a.stop_on_entry {
                        let body = serde_json::json!({
                            "reason": "entry",
                            "threadId": 1,
                        });
                        send_event(&mut writer, &mut seq_counter, "stopped", Some(body));
                    } else {
                        // Run until breakpoint or completion.
                        run_and_report(
                            &mut writer,
                            &mut seq_counter,
                            adapter.as_ref().unwrap(),
                            &grammar_path,
                            StepMode::Continue,
                            &mut last_snapshot,
                        );
                    }
                }
            }

            "threads" => {
                let body = serde_json::json!({
                    "threads": [Thread { id: 1, name: "parse".into() }]
                });
                send_response(
                    &mut writer,
                    &mut seq_counter,
                    &request,
                    true,
                    Some(body),
                );
            }

            "stackTrace" => {
                let frames = if let (Some(a), Some(snap)) = (&adapter, &last_snapshot) {
                    a.build_stack_frames(snap, &grammar_path)
                } else if let Some(ref a) = adapter {
                    // No snapshot yet (stop_on_entry before first run) — show entry rule.
                    let entry = &a.ir.rules[a.ir.entry as usize];
                    let name = a.ir.get_string(entry.name);
                    let line = entry
                        .source_span
                        .as_ref()
                        .map(|s| a.line_index.offset_to_line(s.start))
                        .unwrap_or(1);
                    vec![StackFrame {
                        id: 0,
                        name: name.to_string(),
                        source: Some(SourceRef {
                            name: Some(grammar_path.clone()),
                            path: Some(grammar_path.clone()),
                        }),
                        line,
                        column: 1,
                    }]
                } else {
                    Vec::new()
                };

                let body = serde_json::json!({
                    "stackFrames": frames,
                    "totalFrames": frames.len(),
                });
                send_response(
                    &mut writer,
                    &mut seq_counter,
                    &request,
                    true,
                    Some(body),
                );
            }

            "scopes" => {
                let scopes = vec![Scope {
                    name: "Parse State".into(),
                    variables_reference: 1,
                    expensive: false,
                }];
                let body = serde_json::json!({ "scopes": scopes });
                send_response(
                    &mut writer,
                    &mut seq_counter,
                    &request,
                    true,
                    Some(body),
                );
            }

            "variables" => {
                let variables =
                    if let (Some(a), Some(snap)) = (&adapter, &last_snapshot) {
                        a.build_state_variables(snap)
                    } else {
                        Vec::new()
                    };
                let body = serde_json::json!({ "variables": variables });
                send_response(
                    &mut writer,
                    &mut seq_counter,
                    &request,
                    true,
                    Some(body),
                );
            }

            "continue" => {
                send_response(&mut writer, &mut seq_counter, &request, true, None);
                if let Some(ref a) = adapter {
                    run_and_report(
                        &mut writer,
                        &mut seq_counter,
                        a,
                        &grammar_path,
                        StepMode::Continue,
                        &mut last_snapshot,
                    );
                }
            }

            "next" => {
                send_response(&mut writer, &mut seq_counter, &request, true, None);
                if let Some(ref a) = adapter {
                    run_and_report(
                        &mut writer,
                        &mut seq_counter,
                        a,
                        &grammar_path,
                        StepMode::StepRule,
                        &mut last_snapshot,
                    );
                }
            }

            "stepIn" => {
                send_response(&mut writer, &mut seq_counter, &request, true, None);
                if let Some(ref a) = adapter {
                    run_and_report(
                        &mut writer,
                        &mut seq_counter,
                        a,
                        &grammar_path,
                        StepMode::StepNode,
                        &mut last_snapshot,
                    );
                }
            }

            "stepOut" => {
                // Step out: run until current rule exits.
                send_response(&mut writer, &mut seq_counter, &request, true, None);
                if let Some(ref a) = adapter {
                    run_and_report(
                        &mut writer,
                        &mut seq_counter,
                        a,
                        &grammar_path,
                        StepMode::StepRule,
                        &mut last_snapshot,
                    );
                }
            }

            "disconnect" => {
                send_response(&mut writer, &mut seq_counter, &request, true, None);
                break;
            }

            _other => {
                // Unknown command — respond with success (DAP spec: adapters should
                // not fail on unknown requests).
                send_response(&mut writer, &mut seq_counter, &request, true, None);
            }
        }
    }
}

/// Run the interpreter and send stopped/terminated events.
fn run_and_report(
    writer: &mut impl Write,
    seq: &mut i64,
    adapter: &DapAdapter,
    _grammar_path: &str,
    step_mode: StepMode,
    last_snapshot: &mut Option<bbnf_ir::interpreter::DebugSnapshot>,
) {
    let (_result, snapshots) = adapter.run(step_mode);

    if let Some(snap) = snapshots.into_iter().next() {
        // Stopped at a breakpoint/step.
        let reason = if adapter.breakpoints.contains(&snap.rule_id) {
            "breakpoint"
        } else {
            "step"
        };
        *last_snapshot = Some(snap);
        let body = serde_json::json!({
            "reason": reason,
            "threadId": 1,
        });
        send_event(writer, seq, "stopped", Some(body));
    } else {
        // Completed — send terminated.
        *last_snapshot = None;
        send_event(writer, seq, "terminated", None);
    }
}

// ── Transport ────────────────────────────────────────────────────────────────

/// Read a Content-Length framed JSON message from stdin.
fn read_message(reader: &mut impl BufRead) -> io::Result<String> {
    let mut content_length: usize = 0;

    // Read headers.
    loop {
        let mut header = String::new();
        reader.read_line(&mut header)?;
        let header = header.trim();
        if header.is_empty() {
            break;
        }
        if let Some(len_str) = header.strip_prefix("Content-Length:") {
            content_length = len_str
                .trim()
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        }
    }

    if content_length == 0 {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "no content"));
    }

    // Read body.
    let mut body = vec![0u8; content_length];
    reader.read_exact(&mut body)?;
    String::from_utf8(body).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Write a Content-Length framed JSON message to stdout.
fn write_message(writer: &mut impl Write, json: &str) {
    let _ = write!(writer, "Content-Length: {}\r\n\r\n{}", json.len(), json);
    let _ = writer.flush();
}

fn send_response(
    writer: &mut impl Write,
    seq: &mut i64,
    request: &DapRequest,
    success: bool,
    body: Option<serde_json::Value>,
) {
    *seq += 1;
    let resp = DapResponse {
        seq: *seq,
        msg_type: "response",
        request_seq: request.seq,
        success,
        command: request.command.clone(),
        message: None,
        body,
    };
    let json = serde_json::to_string(&resp).unwrap();
    write_message(writer, &json);
}

fn send_response_error(
    writer: &mut impl Write,
    seq: &mut i64,
    request: &DapRequest,
    message: &str,
) {
    *seq += 1;
    let resp = DapResponse {
        seq: *seq,
        msg_type: "response",
        request_seq: request.seq,
        success: false,
        command: request.command.clone(),
        message: Some(message.to_string()),
        body: None,
    };
    let json = serde_json::to_string(&resp).unwrap();
    write_message(writer, &json);
}

fn send_event(
    writer: &mut impl Write,
    seq: &mut i64,
    event: &str,
    body: Option<serde_json::Value>,
) {
    *seq += 1;
    let evt = DapEvent {
        seq: *seq,
        msg_type: "event",
        event: event.to_string(),
        body,
    };
    let json = serde_json::to_string(&evt).unwrap();
    write_message(writer, &json);
}
