use bbnf::compiler::{ReplayComponent, ReplayError, replay_compilation};
use bbnf::{CompileOptions, CompiledModule, ModuleInput, StageResult, compile_module};

const PATH: &str = "contracts/replay-edit.bbnf";
const BASE: &[u8] = b"root = prefix, item*;\nprefix = \"a\";\nitem = /[0-9]+/;\n";

fn compile(bytes: &[u8]) -> CompiledModule {
    match compile_module(ModuleInput::new(PATH, bytes), CompileOptions::default()) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(diagnostics.is_empty());
            value
        }
        StageResult::Rejected { diagnostics } => {
            let rendered = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("expected accepted replay fixture:\n{rendered}");
        }
    }
}

fn stage_packets(compiled: &CompiledModule) -> Vec<Vec<u8>> {
    vec![
        compiled
            .source_map
            .replay_json()
            .expect("serialize source map"),
        compiled.program.replay_json().expect("serialize program"),
        compiled.remap.replay_json().expect("serialize node remap"),
        compiled
            .dependency
            .replay_json()
            .expect("serialize dependency solution"),
        compiled.types.replay_json().expect("serialize types"),
        compiled.facts.replay_json().expect("serialize analysis"),
        compiled.lowering.replay_json().expect("serialize lowering"),
        compiled.events.replay_json().expect("serialize events"),
        compiled
            .generated
            .replay_json()
            .expect("serialize generated module"),
        compiled.replay_json().expect("serialize complete carrier"),
    ]
}

#[test]
fn every_stage_has_a_deterministic_revision_bound_replay() {
    let first = compile(BASE);
    let second = compile(BASE);
    let first_packets = stage_packets(&first);
    let second_packets = stage_packets(&second);
    let revision = first.source_map.revision().hex();

    assert_eq!(first_packets, second_packets);
    assert_eq!(first_packets.len(), 10);
    for packet in first_packets {
        let value: serde_json::Value =
            serde_json::from_slice(&packet).expect("replay packet is JSON");
        assert!(
            value["schema"]
                .as_str()
                .is_some_and(|schema| schema.starts_with("bbnf."))
        );
        assert_eq!(value["revision"].as_str(), Some(revision.as_str()));
        assert!(!value["value"].is_null());
    }
}

#[test]
fn complete_replay_recompiles_and_reconstructs_every_exact_carrier() {
    let original = compile(BASE);
    let packet = original.replay_json().expect("serialize complete replay");

    let rebuilt =
        replay_compilation(&packet, CompileOptions::default()).expect("validated replay succeeds");

    assert_eq!(rebuilt, original);
    assert_eq!(
        rebuilt.replay_json().expect("serialize rebuilt replay"),
        packet
    );
    assert_eq!(
        CompiledModule::from_replay_json(&packet, CompileOptions::default())
            .expect("associated replay succeeds"),
        original
    );
}

#[test]
fn edited_sources_rebuild_fresh_and_restoring_bytes_restores_replay_equivalence() {
    let original = compile(BASE);
    let original_packets = stage_packets(&original);
    let mut working = BASE.to_vec();
    let literal = working
        .iter()
        .position(|&byte| byte == b'a')
        .expect("fixture contains literal a");

    for replacement in [b'b', b'c', b'd', b'a'] {
        working[literal] = replacement;
        let edit_rebuild = compile(&working);
        let fresh_bytes = working.clone();
        let fresh_rebuild = compile(&fresh_bytes);
        assert_eq!(stage_packets(&edit_rebuild), stage_packets(&fresh_rebuild));
    }

    assert_eq!(working, BASE);
    assert_eq!(stage_packets(&compile(&working)), original_packets);
}

fn tampered_packet(packet: &[u8], mutate: impl FnOnce(&mut serde_json::Value)) -> Vec<u8> {
    let mut value: serde_json::Value =
        serde_json::from_slice(packet).expect("canonical replay is JSON");
    mutate(&mut value);
    serde_json::to_vec(&value).expect("tampered replay remains JSON")
}

#[test]
fn complete_replay_rejects_schema_revision_source_stage_and_generated_tampering() {
    let original = compile(BASE);
    let packet = original.replay_json().expect("serialize complete replay");
    let options = CompileOptions::default();

    let schema = tampered_packet(&packet, |value| {
        value["schema"] = serde_json::json!("bbnf.compiled-module.v0");
    });
    assert!(matches!(
        replay_compilation(&schema, options),
        Err(ReplayError::SchemaMismatch { .. })
    ));

    let revision = tampered_packet(&packet, |value| {
        value["revision"] =
            serde_json::json!("0000000000000000000000000000000000000000000000000000000000000000");
    });
    assert!(matches!(
        replay_compilation(&revision, options),
        Err(ReplayError::RevisionMismatch { .. })
    ));

    let source = tampered_packet(&packet, |value| {
        let bytes = value["value"]["source_map"]["modules"][0]["bytes"]
            .as_array_mut()
            .expect("source bytes array");
        let literal = bytes
            .iter()
            .position(|byte| byte.as_u64() == Some(u64::from(b'a')))
            .expect("fixture contains literal a");
        bytes[literal] = serde_json::json!(b'b');
    });
    assert!(matches!(
        replay_compilation(&source, options),
        Err(ReplayError::RevisionMismatch { .. })
    ));

    let program = tampered_packet(&packet, |value| {
        value["value"]["program"]["entry"] = serde_json::json!(99);
    });
    assert_eq!(
        replay_compilation(&program, options),
        Err(ReplayError::ValueMismatch {
            component: ReplayComponent::Program
        })
    );

    let remap = tampered_packet(&packet, |value| {
        value["value"]["remap"]["nodes"][0] = serde_json::Value::Null;
    });
    assert_eq!(
        replay_compilation(&remap, options),
        Err(ReplayError::ValueMismatch {
            component: ReplayComponent::NodeRemap
        })
    );

    let generated_source = tampered_packet(&packet, |value| {
        value["value"]["generated_source"] = serde_json::json!("forged Rust");
    });
    assert_eq!(
        replay_compilation(&generated_source, options),
        Err(ReplayError::ValueMismatch {
            component: ReplayComponent::GeneratedSource
        })
    );

    let generated_manifest = tampered_packet(&packet, |value| {
        value["value"]["generated_manifest"] = serde_json::json!("{}");
    });
    assert_eq!(
        replay_compilation(&generated_manifest, options),
        Err(ReplayError::ValueMismatch {
            component: ReplayComponent::GeneratedManifest
        })
    );
}

#[test]
fn complete_replay_rejects_noncanonical_byte_encodings() {
    let compiled = compile(BASE);
    let mut packet = compiled.replay_json().expect("serialize complete replay");
    packet.push(b'\n');

    assert_eq!(
        replay_compilation(&packet, CompileOptions::default()),
        Err(ReplayError::NonCanonical)
    );
}
