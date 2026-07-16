// PROTO-1 / LOCK-6 pass-3 probe 1 (family 6B SPEC-GENERATED-SKELETON).
//
// ONE data-driven machine `emit_classify(spec, backend)` -> two backends
// (Rust, Ts) from ONE backend-agnostic `LayoutSpec`. NOT a second include_str!
// TS template. Covers both discriminator strategies:
//   FromSource  (JSON:   kind := classify(source[offset]))
//   FromFlagTag (Sheets: kind := KIND_TABLE[flags_at(cursor) - 1])
//
// The DATA is the spec (kinds, discriminator arms/ranges/default, flag, and the
// per-backend surface symbols/comments a real naming layer carries). The MACHINE
// is the shared rendering structure below; only the leaf token differs per
// backend. Run: rustc emit_classify.rs -o emit && ./emit <out_dir>

use std::fmt::Write as _;

// ---------------------------------------------------------------------------
// DATA MODEL — backend-agnostic LayoutSpec
// ---------------------------------------------------------------------------

/// One enum member: a rule/structural kind, its stable u16 discriminant, and the
/// optional TS const symbol a byte-classified kind projects to (`None` => this
/// kind receives no tape offset, so it never appears on the TS decode face).
struct Kind {
    name: &'static str,     // Rust enum variant / KIND_TABLE entry
    disc: u16,              // stable discriminant (shared across backends)
    ts_sym: Option<&'static str>, // TS const symbol (K_*), None if not recorded
    explicit_disc: bool,    // does the Rust enum spell out `= disc`?
}

/// A single classify arm for the FromSource strategy: an ordered (pattern, kind)
/// row. `rust_pat` is the Rust match pattern; `ts` says how the arm renders on
/// the TS switch (a case with a representative byte, the default arm, or dropped
/// because the kind receives no tape offset).
struct Arm {
    kind: &'static str,     // target kind name
    rust_pat: &'static str, // e.g. "b'{'" or "b'-' | b'0'..=b'9'"
    ts: TsArm,
}

enum TsArm {
    Case(u8),          // `case 0xNN: return <ts_sym>;`
    Default(&'static str), // `default: return <ts_sym>; <comment>`
    Drop,              // separator: no tape offset, absent on TS face
}

enum Discriminator {
    /// JSON: read source byte at the cursor's offset, switch to a kind.
    FromSource {
        arms: Vec<Arm>,
        rust_default: &'static str, // `unreachable!("...")`
    },
    /// Sheets: read the sparse flag tag at the cursor, index a 1-based table.
    FromFlagTag {
        table: Vec<&'static str>, // KIND_TABLE entries in tag order (tag-1 indexes)
        zero_is_none: bool,
    },
}

/// The flag plane the decoder reads alongside the discriminator.
enum FlagSpec {
    /// JSON: a separate escape-bit const (STRING_NEEDS_DECODE) read on strings.
    EscapeBits { sym: &'static str, value: u8, comment: &'static str },
    /// Sheets: the tag IS the discriminator; no separate escape flag const.
    DiscriminatorTag,
}

struct LayoutSpec {
    // enum surface
    enum_name: &'static str,
    enum_doc: Option<&'static str>,   // Rust doc comment block above the enum
    enum_derives: &'static str,       // "#[derive(...)]"
    enum_repr: Option<&'static str>,  // "#[repr(u16)]"
    kinds: Vec<Kind>,
    discriminator: Discriminator,
    flag: FlagSpec,
    // TS surface tokens (naming-layer data)
    ts_consts_header: &'static str,   // "// --- node kinds (first source byte) ---"
    ts_classify_comment: &'static str,// method-doc line above classify
}

#[derive(Clone, Copy)]
enum Backend {
    Rust,
    Ts,
}

// ---------------------------------------------------------------------------
// THE MACHINE — emit_classify(spec, backend). One entry, two backends.
// ---------------------------------------------------------------------------

fn emit_classify(spec: &LayoutSpec, backend: Backend) -> String {
    match backend {
        Backend::Rust => emit_rust(spec),
        Backend::Ts => emit_ts(spec),
    }
}

fn emit_rust(spec: &LayoutSpec) -> String {
    let mut out = String::new();
    // enum def
    if let Some(doc) = spec.enum_doc {
        out.push_str(doc);
        out.push('\n');
    }
    out.push_str(spec.enum_derives);
    out.push('\n');
    if let Some(repr) = spec.enum_repr {
        out.push_str(repr);
        out.push('\n');
    }
    let _ = writeln!(out, "pub enum {} {{", spec.enum_name);
    for k in &spec.kinds {
        if k.explicit_disc {
            let _ = writeln!(out, "    {} = {},", k.name, k.disc);
        } else {
            let _ = writeln!(out, "    {},", k.name);
        }
    }
    out.push_str("}\n");

    match &spec.discriminator {
        Discriminator::FromSource { arms, rust_default } => {
            out.push('\n');
            let _ = writeln!(out, "impl {} {{", spec.enum_name);
            out.push_str(
                "    pub fn at_cursor(tape: &crate::tape::Tape<'_>, cursor: u32) -> Self {\n",
            );
            out.push_str("        let offset = tape\n");
            out.push_str("            .offset_at(cursor)\n");
            out.push_str(
                "            .expect(\"JSON cursor must point into sealed offset tape\");\n",
            );
            out.push_str("        match tape.source()[offset] {\n");
            for a in arms {
                let _ = writeln!(out, "            {} => Self::{},", a.rust_pat, a.kind);
            }
            let _ = writeln!(out, "            _ => {rust_default},");
            out.push_str("        }\n");
            out.push_str("    }\n");
            out.push_str("}\n");
        }
        Discriminator::FromFlagTag { table, zero_is_none } => {
            // KIND_TABLE
            out.push('\n');
            out.push_str(
                "/// The 1-based tag table: a recorded tape flag value decodes to its rule\n",
            );
            out.push_str("/// kind by this table (`tag - 1` indexes the rule order).\n");
            let _ = writeln!(out, "const KIND_TABLE: &[{}] = &[", spec.enum_name);
            for name in table {
                let _ = writeln!(out, "    {}::{},", spec.enum_name, name);
            }
            out.push_str("];\n");
            // at_cursor impl
            out.push('\n');
            let _ = writeln!(out, "impl {} {{", spec.enum_name);
            out.push_str(
                "    /// Decode the kind at a tape cursor from the sparse flag plane. A cursor with\n",
            );
            out.push_str(
                "    /// no flag (tag 0) \u{2014} a bare structural offset \u{2014} has no rule kind.\n",
            );
            out.push_str("    #[inline]\n");
            let _ = writeln!(
                out,
                "    pub fn at_cursor(tape: &Tape<'_>, cursor: u32) -> Option<{}> {{",
                spec.enum_name
            );
            out.push_str("        let tag = tape.flags_at(cursor)?.bits();\n");
            if *zero_is_none {
                out.push_str("        if tag == 0 {\n");
                out.push_str("            return None;\n");
                out.push_str("        }\n");
            }
            out.push_str("        KIND_TABLE.get((tag - 1) as usize).copied()\n");
            out.push_str("    }\n");
            out.push_str("}\n");
        }
    }
    out
}

fn emit_ts(spec: &LayoutSpec) -> String {
    let mut out = String::new();
    // const block
    out.push_str(spec.ts_consts_header);
    out.push('\n');
    match &spec.discriminator {
        Discriminator::FromSource { arms, .. } => {
            // consts in discriminant order for kinds carrying a ts symbol
            for k in &spec.kinds {
                if let Some(sym) = k.ts_sym {
                    let _ = writeln!(out, "const {} = {};", sym, k.disc);
                }
            }
            // flag const
            if let FlagSpec::EscapeBits { sym, value, comment } = &spec.flag {
                out.push('\n');
                let _ = writeln!(out, "const {sym} = 0x{value:02x}; {comment}");
            }
            // classify method — emitted at 2-space class-method indent (it lives
            // inside `class TapeDoc`), while the consts above are module-level.
            out.push('\n');
            let _ = writeln!(out, "  {}", spec.ts_classify_comment);
            out.push_str("  classify(cursor) {\n");
            out.push_str("    const b = this.source[this.offsets[cursor]];\n");
            out.push_str("    switch (b) {\n");
            // Cases render in arm order; the single default arm is deferred to the
            // END of the switch (JS `default:` is conventionally last, regardless
            // of its position in the shared arm list).
            let mut default_arm: Option<(&str, &str)> = None;
            for a in arms {
                match &a.ts {
                    TsArm::Case(byte) => {
                        let sym = ts_sym_for(spec, a.kind);
                        let _ = writeln!(out, "      case 0x{byte:02x}: return {sym};");
                    }
                    TsArm::Default(comment) => {
                        default_arm = Some((ts_sym_for(spec, a.kind), comment));
                    }
                    TsArm::Drop => {}
                }
            }
            if let Some((sym, comment)) = default_arm {
                let _ = writeln!(out, "      default: return {sym}; {comment}");
            }
            out.push_str("    }\n");
            out.push_str("  }\n");
        }
        Discriminator::FromFlagTag { table, zero_is_none } => {
            // TS FromFlagTag face (no hand reference exists; demonstrates the
            // AoS<->SoA expressibility of the SAME machine on the tag strategy).
            for (i, name) in table.iter().enumerate() {
                let _ = writeln!(out, "const K_{} = {};", to_screaming(name), i + 1);
            }
            out.push('\n');
            out.push_str("// kind := KIND_TABLE[flags_at(cursor) - 1]\n");
            out.push_str("classify(cursor) {\n");
            out.push_str("  const tag = this.flagAt(cursor);\n");
            if *zero_is_none {
                out.push_str("  if (tag === 0) return 0;\n");
            }
            out.push_str("  return KIND_TABLE[tag - 1];\n");
            out.push_str("}\n");
        }
    }
    out
}

fn ts_sym_for(spec: &LayoutSpec, kind: &str) -> &'static str {
    spec.kinds
        .iter()
        .find(|k| k.name == kind)
        .and_then(|k| k.ts_sym)
        .unwrap_or("/*NO_TS_SYM*/")
}

fn to_screaming(name: &str) -> String {
    // PascalCase -> SCREAMING_SNAKE (for the FromFlagTag TS demo consts)
    let mut out = String::new();
    for (i, ch) in name.chars().enumerate() {
        if ch.is_uppercase() && i != 0 {
            out.push('_');
        }
        out.push(ch.to_ascii_uppercase());
    }
    out
}

// ---------------------------------------------------------------------------
// SPECS — JSON (FromSource) and Sheets (FromFlagTag), as DATA
// ---------------------------------------------------------------------------

fn json_spec() -> LayoutSpec {
    let kinds = vec![
        Kind { name: "Root", disc: 0, ts_sym: None, explicit_disc: true },
        Kind { name: "ObjectOpen", disc: 1, ts_sym: Some("K_OBJ_OPEN"), explicit_disc: true },
        Kind { name: "ObjectClose", disc: 2, ts_sym: Some("K_OBJ_CLOSE"), explicit_disc: true },
        Kind { name: "ArrayOpen", disc: 3, ts_sym: Some("K_ARR_OPEN"), explicit_disc: true },
        Kind { name: "ArrayClose", disc: 4, ts_sym: Some("K_ARR_CLOSE"), explicit_disc: true },
        Kind { name: "Pair", disc: 5, ts_sym: None, explicit_disc: true },
        Kind { name: "String", disc: 6, ts_sym: Some("K_STRING"), explicit_disc: true },
        Kind { name: "Number", disc: 7, ts_sym: Some("K_NUMBER"), explicit_disc: true },
        Kind { name: "True", disc: 8, ts_sym: Some("K_TRUE"), explicit_disc: true },
        Kind { name: "False", disc: 9, ts_sym: Some("K_FALSE"), explicit_disc: true },
        Kind { name: "Null", disc: 10, ts_sym: Some("K_NULL"), explicit_disc: true },
        Kind { name: "ElementSeparator", disc: 11, ts_sym: None, explicit_disc: true },
        Kind { name: "MemberSeparator", disc: 12, ts_sym: None, explicit_disc: true },
    ];
    let arms = vec![
        Arm { kind: "ObjectOpen", rust_pat: "b'{'", ts: TsArm::Case(b'{') },
        Arm { kind: "ObjectClose", rust_pat: "b'}'", ts: TsArm::Case(b'}') },
        Arm { kind: "ArrayOpen", rust_pat: "b'['", ts: TsArm::Case(b'[') },
        Arm { kind: "ArrayClose", rust_pat: "b']'", ts: TsArm::Case(b']') },
        Arm { kind: "ElementSeparator", rust_pat: "b','", ts: TsArm::Drop },
        Arm { kind: "MemberSeparator", rust_pat: "b':'", ts: TsArm::Drop },
        Arm { kind: "String", rust_pat: "b'\"'", ts: TsArm::Case(b'"') },
        Arm {
            kind: "Number",
            rust_pat: "b'-' | b'0'..=b'9'",
            ts: TsArm::Default("// '-' | '0'..'9'"),
        },
        Arm { kind: "True", rust_pat: "b't'", ts: TsArm::Case(b't') },
        Arm { kind: "False", rust_pat: "b'f'", ts: TsArm::Case(b'f') },
        Arm { kind: "Null", rust_pat: "b'n'", ts: TsArm::Case(b'n') },
    ];
    LayoutSpec {
        enum_name: "JsonNodeKind",
        enum_doc: None,
        enum_derives: "#[derive(Copy, Clone, Eq, PartialEq, Debug)]",
        enum_repr: Some("#[repr(u16)]"),
        kinds,
        discriminator: Discriminator::FromSource {
            arms,
            rust_default: "unreachable!(\"JSON verifier emits only JSON tape offsets\")",
        },
        flag: FlagSpec::EscapeBits {
            sym: "STRING_NEEDS_DECODE",
            value: 0x01,
            comment: "// OffsetFlags::GRAMMAR_BIT0",
        },
        ts_consts_header: "// --- node kinds (first source byte) ---",
        ts_classify_comment:
            "// kind := classify(source[offsets[cursor]])  (JsonNodeKind::at_cursor)",
    }
}

fn sheets_kind_names() -> Vec<&'static str> {
    vec![
        "Number", "String", "Boolean", "ErrorLiteral", "SheetPrefix", "CellRef", "RangeEnd",
        "Cell", "RangeRef", "CellOrRange", "Identifier", "CompareOp", "ComparisonExpr",
        "ConcatExpr", "AddOp", "AddExpr", "MulOp", "MulExpr", "ExpExpr", "UnaryPrefix",
        "UnaryExpr", "PostfixExpr", "Primary", "ParenExpr", "FuncOpen", "Arg", "Comma",
        "FuncArgs", "FuncCall", "LetBinding", "LetArgs", "LetCall", "LambdaParams",
        "LambdaCall", "ArrayRow", "ArrayRows", "ArrayLiteral", "Expression", "Formula",
        "NamedRange",
    ]
}

fn sheets_spec() -> LayoutSpec {
    let names = sheets_kind_names();
    let kinds = names
        .iter()
        .map(|n| Kind { name: n, disc: 0, ts_sym: None, explicit_disc: false })
        .collect();
    LayoutSpec {
        enum_name: "GoogleSheetsKind",
        enum_doc: Some(
            "/// The structural kind a tape cursor projects to, recovered lazily from the\n\
             /// sparse flag plane at the cursor's offset. One variant per grammar rule \u{2014}\n\
             /// derived from the program's rule names, no shared structural vocabulary.",
        ),
        enum_derives: "#[derive(Copy, Clone, Eq, PartialEq, Debug)]",
        enum_repr: None,
        kinds,
        discriminator: Discriminator::FromFlagTag { table: names, zero_is_none: true },
        flag: FlagSpec::DiscriminatorTag,
        ts_consts_header: "// --- rule kinds (flag tag table) ---",
        ts_classify_comment: "// kind := KIND_TABLE[flags_at(cursor) - 1]",
    }
}

fn main() {
    let out_dir = std::env::args().nth(1).expect("usage: emit <out_dir>");
    let json = json_spec();
    let sheets = sheets_spec();
    let write = |name: &str, content: String| {
        std::fs::write(format!("{out_dir}/{name}"), content).expect("write");
    };
    write("gen_json.rs", emit_classify(&json, Backend::Rust));
    write("gen_json.ts", emit_classify(&json, Backend::Ts));
    write("gen_sheets.rs", emit_classify(&sheets, Backend::Rust));
    write("gen_sheets.ts", emit_classify(&sheets, Backend::Ts));
    eprintln!("emitted 4 faces to {out_dir}");
}
