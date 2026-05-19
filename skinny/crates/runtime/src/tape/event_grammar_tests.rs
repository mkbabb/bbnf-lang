#[cfg(feature = "proof")]
use std::fs;
#[cfg(feature = "proof")]
use std::path::PathBuf;
#[cfg(feature = "proof")]
use std::process::Command;
#[cfg(feature = "proof")]
use std::time::{SystemTime, UNIX_EPOCH};

use super::{AnyGrammar, AnyKind, EventGrammar, Tape, ValueRef};

#[cfg(feature = "proof")]
use crate::json_event_grammar_witness::JsonEventGrammar;
#[cfg(feature = "proof")]
use crate::sheets_witness::event_grammar_witness::SheetsEventGrammar;

#[cfg(feature = "proof")]
const _: fn() = _proof_compiles::<JsonEventGrammar>;
#[cfg(feature = "proof")]
const _: fn() = _proof_compiles::<SheetsEventGrammar>;
const _: fn() = _proof_compiles::<AnyGrammar>;

fn _proof_compiles<G: EventGrammar>() {
    fn accepts_ref<'doc, 'input: 'doc, G: EventGrammar>(
        value: ValueRef<'doc, 'input, AnyKind, G>,
    ) -> ValueRef<'doc, 'input, AnyKind, G> {
        value
    }

    let _ = accepts_ref::<G>;
}

#[test]
fn event_grammar_any_default_is_empty() {
    _proof_compiles::<AnyGrammar>();
    assert_eq!(AnyGrammar::STRUCTURAL_CLASS_COUNT, 0);
    assert!(!AnyGrammar::admits_class(1));
}

#[cfg(feature = "proof")]
#[test]
fn event_grammar_witnesses_compile() {
    _proof_compiles::<JsonEventGrammar>();
    _proof_compiles::<SheetsEventGrammar>();
    assert!(JsonEventGrammar::admits_class(7));
    assert!(!JsonEventGrammar::admits_class(8));
    assert!(SheetsEventGrammar::admits_class(5));
    assert!(!SheetsEventGrammar::admits_class(6));
}

#[test]
fn event_grammar_marker_preserves_value_ref_layout() {
    assert_eq!(
        std::mem::size_of::<ValueRef<'_, '_, AnyKind, AnyGrammar>>(),
        std::mem::size_of::<ValueRef<'_, '_, AnyKind>>()
    );
    assert_eq!(
        std::mem::size_of::<ValueRef<'_, '_, AnyKind, AnyGrammar>>(),
        std::mem::size_of::<(&Tape<'_>, u32)>()
    );
}

#[cfg(feature = "proof")]
#[test]
fn event_grammar_value_ref_static_escape_is_rejected() {
    let temp = temp_compile_fail_dir();
    let src = temp.join("src");
    fs::create_dir_all(&src).expect("create compile-fail fixture src dir");
    fs::write(
        temp.join("Cargo.toml"),
        format!(
            r#"[package]
name = "runtime_value_ref_compile_fail"
version = "0.0.0"
edition = "2021"

[dependencies]
runtime = {{ path = "{}", features = ["proof"] }}
"#,
            env!("CARGO_MANIFEST_DIR").replace('\\', "\\\\")
        ),
    )
    .expect("write compile-fail fixture Cargo.toml");
    fs::write(
        src.join("lib.rs"),
        r#"use runtime::json_event_grammar_witness::JsonEventGrammar;
use runtime::tape::{AnyKind, TapeBuilder, ValueRef};

pub fn leak(input: &str) -> ValueRef<'static, 'static, AnyKind, JsonEventGrammar> {
    let mut builder = TapeBuilder::new(input.as_bytes(), 1);
    builder.push_plain_offset(0);
    let tape = builder.finish();
    ValueRef::new(&tape, 0)
}
"#,
    )
    .expect("write compile-fail fixture lib.rs");

    let output = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
        .arg("check")
        .arg("--quiet")
        .arg("--manifest-path")
        .arg(temp.join("Cargo.toml"))
        .output()
        .expect("run compile-fail fixture cargo check");

    let stderr = String::from_utf8_lossy(&output.stderr);
    let _ = fs::remove_dir_all(&temp);

    assert!(
        !output.status.success(),
        "compile-fail fixture unexpectedly passed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        stderr
    );
    assert!(
        stderr.contains("cannot return value referencing local variable")
            || stderr.contains("returns a value referencing data owned by the current function")
            || stderr.contains("lifetime may not live long enough"),
        "compile-fail fixture failed for the wrong reason:\n{stderr}"
    );
}

#[cfg(feature = "proof")]
fn temp_compile_fail_dir() -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time after epoch")
        .as_nanos();
    std::env::temp_dir().join(format!(
        "runtime-value-ref-compile-fail-{}-{unique}",
        std::process::id()
    ))
}
