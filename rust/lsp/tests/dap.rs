//! Tests for the DAP module: protocol serialization, mapping, and adapter.

// ── Protocol serialization ───────────────────────────────────────────────────

mod protocol {
    use bbnf_lsp::dap::protocol::*;

    #[test]
    fn capabilities_serialize() {
        let caps = Capabilities {
            supports_configuration_done_request: true,
            supports_function_breakpoints: true,
            supports_step_back: true,
            supports_stepping_granularity: true,
        };
        let json = serde_json::to_string(&caps).unwrap();
        assert!(json.contains("supportsConfigurationDoneRequest"));
        assert!(json.contains("supportsFunctionBreakpoints"));
        assert!(json.contains("supportsStepBack"));
    }

    #[test]
    fn response_serializes_correctly() {
        let resp = DapResponse {
            seq: 1,
            msg_type: "response",
            request_seq: 0,
            success: true,
            command: "initialize".into(),
            message: None,
            body: Some(serde_json::json!({"test": true})),
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains(r#""type":"response""#));
        assert!(json.contains(r#""success":true"#));
        assert!(!json.contains("message")); // None → skipped.
    }

    #[test]
    fn event_serializes_correctly() {
        let evt = DapEvent {
            seq: 2,
            msg_type: "event",
            event: "stopped".into(),
            body: Some(serde_json::json!({"reason": "breakpoint", "threadId": 1})),
        };
        let json = serde_json::to_string(&evt).unwrap();
        assert!(json.contains(r#""event":"stopped""#));
        assert!(json.contains(r#""reason":"breakpoint""#));
    }

    #[test]
    fn launch_args_deserialize_with_defaults() {
        let json = r#"{"grammar": "test.bbnf"}"#;
        let args: LaunchArgs = serde_json::from_str(json).unwrap();
        assert_eq!(args.grammar, "test.bbnf");
        assert!(args.input.is_none());
        assert!(args.stop_on_entry.is_none());
    }

    #[test]
    fn set_breakpoints_deserialize() {
        let json = r#"{
            "source": {"path": "test.bbnf"},
            "breakpoints": [{"line": 5}, {"line": 10}]
        }"#;
        let args: SetBreakpointsArgs = serde_json::from_str(json).unwrap();
        assert_eq!(args.source.path.as_deref(), Some("test.bbnf"));
        assert_eq!(args.breakpoints.len(), 2);
        assert_eq!(args.breakpoints[0].line, 5);
    }
}

// ── Mapping ──────────────────────────────────────────────────────────────────

mod mapping {
    use bbnf_lsp::dap::mapping::*;

    #[test]
    fn line_index_single_line() {
        let idx = LineIndex::new("hello");
        assert_eq!(idx.line_to_offset(1), 0);
        assert_eq!(idx.offset_to_line(0), 1);
        assert_eq!(idx.offset_to_line(3), 1);
    }

    #[test]
    fn line_index_multi_line() {
        let text = "abc\ndef\nghi";
        let idx = LineIndex::new(text);
        assert_eq!(idx.line_to_offset(1), 0);
        assert_eq!(idx.line_to_offset(2), 4);
        assert_eq!(idx.line_to_offset(3), 8);
        assert_eq!(idx.offset_to_line(0), 1);
        assert_eq!(idx.offset_to_line(5), 2);
        assert_eq!(idx.offset_to_line(8), 3);
    }

    #[test]
    fn line_index_out_of_bounds() {
        let idx = LineIndex::new("a\nb");
        // Line beyond file → clamps to 0.
        assert_eq!(idx.line_to_offset(100), 0);
    }

    #[test]
    fn resolve_breakpoint_finds_rule() {
        use bbnf::pipeline::{compile_grammar, PipelineOptions};

        let source = "value = /[0-9]+/ ;\nentry = value ;";
        let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();
        let idx = LineIndex::new(source);

        // Line 1 should resolve to whatever rule starts there.
        let result = resolve_breakpoint(&ir, &idx, 1);
        assert!(result.is_some(), "should find a rule at line 1");
    }

    #[test]
    fn rule_at_offset_basic() {
        use bbnf::pipeline::{compile_grammar, PipelineOptions};

        let source = "value = /[0-9]+/ ;";
        let ir = compile_grammar(source, &PipelineOptions::default()).unwrap();

        // Offset 0 should be within the "value" rule's source_span.
        let result = rule_at_offset(&ir, 0);
        assert!(result.is_some());
    }
}

// ── Adapter ──────────────────────────────────────────────────────────────────

mod adapter {
    use bbnf_lsp::dap::adapter::DapAdapter;
    use bbnf_lsp::dap::protocol::*;
    use std::io::Write;

    fn create_test_grammar() -> (tempfile::NamedTempFile, String) {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        let grammar = "@debug * ;\nvalue = /[0-9]+/ ;";
        f.write_all(grammar.as_bytes()).unwrap();
        f.flush().unwrap();
        let path = f.path().to_string_lossy().to_string();
        (f, path)
    }

    #[test]
    fn launch_compiles_grammar() {
        let (_tmp, path) = create_test_grammar();
        let args = LaunchArgs {
            grammar: path,
            input_text: Some("42".into()),
            ..Default::default()
        };
        let adapter = DapAdapter::launch(&args);
        assert!(adapter.is_ok(), "launch should succeed: {:?}", adapter.err());
    }

    #[test]
    fn launch_invalid_path_errors() {
        let args = LaunchArgs {
            grammar: "/nonexistent/path.bbnf".into(),
            ..Default::default()
        };
        let result = DapAdapter::launch(&args);
        assert!(result.is_err());
    }

    #[test]
    fn set_function_breakpoint() {
        let (_tmp, path) = create_test_grammar();
        let args = LaunchArgs {
            grammar: path,
            input_text: Some("42".into()),
            ..Default::default()
        };
        let mut adapter = DapAdapter::launch(&args).unwrap();
        let bps = adapter.set_function_breakpoints(&SetFunctionBreakpointsArgs {
            breakpoints: vec![FunctionBreakpoint { name: "value".into() }],
        });
        assert_eq!(bps.len(), 1);
        assert!(bps[0].verified, "breakpoint on 'value' should be verified");
    }

    #[test]
    fn set_function_breakpoint_unknown_rule() {
        let (_tmp, path) = create_test_grammar();
        let args = LaunchArgs {
            grammar: path,
            input_text: Some("42".into()),
            ..Default::default()
        };
        let mut adapter = DapAdapter::launch(&args).unwrap();
        let bps = adapter.set_function_breakpoints(&SetFunctionBreakpointsArgs {
            breakpoints: vec![FunctionBreakpoint { name: "nonexistent".into() }],
        });
        assert_eq!(bps.len(), 1);
        assert!(!bps[0].verified, "breakpoint on nonexistent rule should not verify");
    }

    #[test]
    fn run_produces_result() {
        let (_tmp, path) = create_test_grammar();
        let args = LaunchArgs {
            grammar: path,
            input_text: Some("42".into()),
            ..Default::default()
        };
        let adapter = DapAdapter::launch(&args).unwrap();
        let (result, _) = adapter.run(bbnf_ir::interpreter::StepMode::Continue);
        assert!(result.success, "parse of '42' with /[0-9]+/ should succeed");
    }

    #[test]
    fn build_state_variables() {
        let (_tmp, path) = create_test_grammar();
        let args = LaunchArgs {
            grammar: path,
            input_text: Some("42".into()),
            ..Default::default()
        };
        let adapter = DapAdapter::launch(&args).unwrap();
        let snapshot = bbnf_ir::interpreter::DebugSnapshot {
            pc: 0,
            offset: 0,
            rule_stack: Vec::new(),
            rule_id: 0,
            is_entry: true,
            is_error: false,
            values_depth: 0,
        };
        let vars = adapter.build_state_variables(&snapshot);
        assert!(vars.len() >= 4, "should have offset, isError, currentRule, isEntry vars");
        assert_eq!(vars[0].name, "offset");
    }
}
