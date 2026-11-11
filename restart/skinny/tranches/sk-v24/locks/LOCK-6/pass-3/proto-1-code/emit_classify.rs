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

// ---------------------------------------------------------------------------
// PROBE 2 — the FULL decoder emitter. Extends the classify FACE (above) to the
// whole load-bearing TapeDoc decoder (classify / stringLeaf / decodeNumber /
// nextSibling / getKey / indexArray / walk) + the shared hash primitives +
// pointer navigation + the JSON.parse oracle lane, all from ONE `LayoutSpec`.
//
// DATA (spec-driven, the discriminator face): the K_* constant block and the
// `classify` switch are rendered from `spec.kinds` + `spec.discriminator` arms —
// the exact seam family 6B claims to generate. MACHINE (the walk/nav/pointer
// hash arithmetic): a shared template rendered here in the GENERATOR's own
// formatting, deliberately NOT transcribed byte-for-byte from the hand PROVE-14
// source, so the emitted file is behaviourally equivalent yet NOT byte-identical
// — the one condition that makes a V8-monomorphism gate on it informative.
//
// The emitted module is a self-contained ESM: `class TapeDoc`, the primitives,
// `emptyDigest/finalizeDigest/digestsEqual`, `walkJs` (oracle), `tapeGet/jsonGet`
// (stop-early pointer), and `unescapeJson`.

/// Render the spec-driven `classify` method body: one `case` per FromSource arm
/// carrying a representative byte, a single deferred `default`, separators
/// dropped (they never receive a tape offset, so classify never sees them).
fn gen_classify(spec: &LayoutSpec) -> String {
    let mut out = String::new();
    out.push_str("  // kind := classify(source[offsets[cursor]]) — generated from LayoutSpec arms\n");
    out.push_str("  classify(cur) {\n");
    out.push_str("    const b = this.source[this.offsets[cur]];\n");
    out.push_str("    switch (b) {\n");
    if let Discriminator::FromSource { arms, .. } = &spec.discriminator {
        let mut deferred: Option<&str> = None;
        for a in arms {
            match &a.ts {
                TsArm::Case(byte) => {
                    let sym = ts_sym_for(spec, a.kind);
                    let _ = writeln!(out, "      case 0x{byte:02x}: return {sym};");
                }
                TsArm::Default(_) => deferred = Some(ts_sym_for(spec, a.kind)),
                TsArm::Drop => {}
            }
        }
        if let Some(sym) = deferred {
            let _ = writeln!(out, "      default: return {sym};");
        }
    }
    out.push_str("    }\n");
    out.push_str("  }\n");
    out
}

fn emit_full_decoder(spec: &LayoutSpec) -> String {
    let mut out = String::new();
    // ---- generated banner (distinct from the hand source header) ----
    let _ = writeln!(
        out,
        "// GENERATED by emit_classify.rs (family 6B SPEC-GENERATED-SKELETON, probe 2).\n\
         // Full TapeDoc decoder for the {} offset-tape SoA layout, emitted from a\n\
         // LayoutSpec. The K_* block + `classify` are spec-driven; the walk/nav/pointer\n\
         // machine is the shared template. NOT a byte-transcription of the hand source.\n\
         //\n\
         // V8 discipline: ONE monomorphic hidden class `TapeDoc`, four typed fields set\n\
         // in a fixed constructor order and never mutated; the walk recurses over integer\n\
         // cursors and returns u32 hashes (no per-node object, no post-ctor field add).",
        spec.enum_name
    );
    out.push('\n');
    // ---- DATA: K_* constants from spec.kinds (discriminant order) ----
    out.push_str("// --- node kinds (first source byte), generated from spec.kinds ---\n");
    for k in &spec.kinds {
        if let Some(sym) = k.ts_sym {
            let _ = writeln!(out, "const {} = {};", sym, k.disc);
        }
    }
    if let FlagSpec::EscapeBits { sym, value, comment } = &spec.flag {
        let _ = writeln!(out, "const {sym} = 0x{value:02x}; {comment}");
    }
    // ---- MACHINE: shared hash primitives (generator's own layout) ----
    out.push_str(PRIMITIVES);
    // ---- class TapeDoc: ctor + spec-generated classify + machine methods ----
    out.push_str("export class TapeDoc {\n");
    out.push_str(CTOR);
    out.push('\n');
    out.push_str(&gen_classify(spec));
    out.push_str(MACHINE_METHODS);
    out.push_str("}\n\n");
    // ---- tail: unescape, oracle walkJs, finalize, pointer lane ----
    out.push_str(TAIL);
    out
}

// The shared machine, rendered in the generator's formatting (NOT the hand
// source's). Constants referenced by name (K_OBJ_OPEN…) are emitted above.
const PRIMITIVES: &str = r#"
// --- shared node-hash primitives (u32, deterministic) ---
export function strHash(s) {
  let h = 0x811c9dc5;
  for (let i = 0; i < s.length; i++) h = Math.imul(h ^ s.charCodeAt(i), 0x01000193);
  return h >>> 0;
}
const _numDV = new DataView(new ArrayBuffer(8));
export function numHash(n) {
  _numDV.setFloat64(0, n, true);
  const lo = _numDV.getUint32(0, true);
  const hi = _numDV.getUint32(4, true);
  return (Math.imul(lo, 0x85ebca77) ^ Math.imul(hi, 0xc2b2ae3d)) >>> 0;
}
const rotl = (x, r) => ((x << r) | (x >>> (32 - r))) >>> 0;
export const nodeStringFromHash = (h) => (h ^ 0x51ed2701) >>> 0;
export const nodeString = (s) => nodeStringFromHash(strHash(s));
export const nodeNumber = (n) => (numHash(n) ^ 0x2f6e1a3b) >>> 0;
export const nodeBool = (v) => (v ? 0x9a1c3f77 : 0x4b7d2e05) >>> 0;
export const nodeNull = () => 0x0d15ea5e >>> 0;
export const memberMix = (kh, ch) => (Math.imul(kh, 0x9e3779b1) ^ rotl(ch, 15)) >>> 0;
export const finishObject = (acc, count) => ((acc ^ Math.imul(count + 1, 0x27d4eb2f)) ^ 0x100000) >>> 0;
export const arrayStart = () => 0x9e3779b1 >>> 0;
export const arrayStep = (h, ch) => Math.imul(h ^ ch, 0x01000193) >>> 0;
export const finishArray = (h, len) => ((Math.imul(h, 0x85ebca77) ^ len) ^ 0x200000) >>> 0;
export function emptyDigest() {
  return { objects: 0, arrays: 0, strings: 0, numbers: 0, bools: 0, nulls: 0, codeUnits: 0, fp: 0 };
}
export function digestsEqual(a, b) {
  return a.objects === b.objects && a.arrays === b.arrays && a.strings === b.strings &&
    a.numbers === b.numbers && a.bools === b.bools && a.nulls === b.nulls &&
    a.codeUnits === b.codeUnits && a.fp === b.fp;
}

"#;

// The constructor: the SINGLE hidden class. Four typed fields, fixed order, no
// post-construction assignment anywhere in the class.
const CTOR: &str = r#"  constructor(source, offsets, flagCursors, flagValues) {
    this.source = source;
    this.offsets = offsets;
    this.flagCursors = flagCursors;
    this.flagValues = flagValues;
  }"#;

// nav + leaf + walk machine (nextSibling emitted BEFORE walk — a generator
// ordering choice that differs from the hand source, byte-non-identical).
const MACHINE_METHODS: &str = r#"

  // sparse flag lookup (binary search over sorted flagCursors)
  needsDecode(cur) {
    let lo = 0, hi = this.flagCursors.length - 1;
    while (lo <= hi) {
      const mid = (lo + hi) >>> 1;
      const c = this.flagCursors[mid];
      if (c === cur) return (this.flagValues[mid] & STRING_NEEDS_DECODE) !== 0;
      if (c < cur) lo = mid + 1; else hi = mid - 1;
    }
    return false;
  }

  // next_sibling_cursor: containers depth-scan to their matching close; leaves are one token
  nextSibling(cur) {
    const k = this.classify(cur);
    if (k === K_OBJ_OPEN || k === K_ARR_OPEN) {
      let depth = 1, scan = cur + 1;
      while (depth > 0) {
        const kk = this.classify(scan);
        if (kk === K_OBJ_OPEN || kk === K_ARR_OPEN) depth++;
        else if (kk === K_OBJ_CLOSE || kk === K_ARR_CLOSE) depth--;
        scan++;
      }
      return scan;
    }
    return cur + 1;
  }

  // lazy string leaf -> {h,len}; ASCII/escape-free bodies fold byte-by-byte, no alloc
  stringLeaf(cur) {
    const src = this.source;
    const open = this.offsets[cur];
    let i = open + 1, h = 0x811c9dc5, clean = true;
    for (;;) {
      const c = src[i];
      if (c === 0x22) break;
      if (c === 0x5c) { clean = false; i += 2; continue; }
      if (c >= 0x80) clean = false;
      h = Math.imul(h ^ c, 0x01000193);
      i += 1;
    }
    if (clean) return { h: h >>> 0, len: i - (open + 1) };
    const raw = src.toString("utf8", open + 1, i);
    const s = this.needsDecode(cur) ? unescapeJson(raw) : raw;
    return { h: strHash(s), len: s.length };
  }

  // full string materialization (pointer key comparison)
  decodeString(cur) {
    const src = this.source;
    const open = this.offsets[cur];
    let i = open + 1;
    for (;;) { const c = src[i]; if (c === 0x22) break; i += c === 0x5c ? 2 : 1; }
    const raw = src.toString("utf8", open + 1, i);
    return this.needsDecode(cur) ? unescapeJson(raw) : raw;
  }

  // number leaf: scan to a delimiter, parse the decimal to a double
  decodeNumber(cur) {
    const src = this.source, n = src.length;
    let end = this.offsets[cur];
    while (end < n) {
      const c = src[end];
      if (c === 0x2c || c === 0x5d || c === 0x7d || c === 0x20 || c === 0x09 || c === 0x0a || c === 0x0d) break;
      end += 1;
    }
    return Number(src.toString("latin1", this.offsets[cur], end));
  }

  // pointer: linear member scan; unaccessed values skipped by nextSibling
  getKey(objCur, key) {
    let c = objCur + 1;
    while (this.classify(c) !== K_OBJ_CLOSE) {
      const valCur = c + 1;
      if (this.decodeString(c) === key) return valCur;
      c = this.nextSibling(valCur);
    }
    return -1;
  }

  indexArray(arrCur, idx) {
    let c = arrCur + 1, i = 0;
    while (this.classify(c) !== K_ARR_CLOSE) {
      if (i === idx) return c;
      c = this.nextSibling(c);
      i++;
    }
    return -1;
  }

  // lazy structural walk: recurse over integer cursors, return this node's u32 hash
  walk(cur, d) {
    switch (this.classify(cur)) {
      case K_OBJ_OPEN: {
        d.objects++;
        let acc = 0, count = 0, c = cur + 1;
        while (this.classify(c) !== K_OBJ_CLOSE) {
          const key = this.stringLeaf(c);
          d.strings++;
          d.codeUnits += key.len;
          const kh = nodeStringFromHash(key.h);
          c += 1;
          const vh = this.walk(c, d);
          c = this.nextSibling(c);
          acc = (acc ^ memberMix(kh, vh)) >>> 0;
          count++;
        }
        return finishObject(acc, count);
      }
      case K_ARR_OPEN: {
        d.arrays++;
        let h = arrayStart(), len = 0, c = cur + 1;
        while (this.classify(c) !== K_ARR_CLOSE) {
          h = arrayStep(h, this.walk(c, d));
          c = this.nextSibling(c);
          len++;
        }
        return finishArray(h, len);
      }
      case K_STRING: {
        const leaf = this.stringLeaf(cur);
        d.strings++;
        d.codeUnits += leaf.len;
        return nodeStringFromHash(leaf.h);
      }
      case K_NUMBER: {
        d.numbers++;
        return nodeNumber(this.decodeNumber(cur));
      }
      case K_TRUE:
        d.bools++;
        return nodeBool(true);
      case K_FALSE:
        d.bools++;
        return nodeBool(false);
      default:
        d.nulls++;
        return nodeNull();
    }
  }"#;

const TAIL: &str = r#"// JSON \-escape unescape at the JS-string level (mirrors as_str -> unescape_string)
export function unescapeJson(s) {
  let out = "", n = s.length, i = 0, run = 0;
  while (i < n) {
    if (s.charCodeAt(i) === 0x5c) {
      if (run > 0) { out += s.substring(i - run, i); run = 0; }
      const e = s[i + 1];
      switch (e) {
        case '"': out += '"'; i += 2; break;
        case "\\": out += "\\"; i += 2; break;
        case "/": out += "/"; i += 2; break;
        case "b": out += "\b"; i += 2; break;
        case "f": out += "\f"; i += 2; break;
        case "n": out += "\n"; i += 2; break;
        case "r": out += "\r"; i += 2; break;
        case "t": out += "\t"; i += 2; break;
        case "u": out += String.fromCharCode(parseInt(s.substr(i + 2, 4), 16)); i += 6; break;
        default: out += e; i += 2;
      }
    } else { run++; i++; }
  }
  if (run > 0) out += s.substring(n - run, n);
  return out;
}

// oracle lane: JSON.parse tree -> SAME digest via the SAME primitives
export function walkJs(v, d) {
  if (Array.isArray(v)) {
    d.arrays++;
    let h = arrayStart();
    for (let i = 0; i < v.length; i++) h = arrayStep(h, walkJs(v[i], d));
    return finishArray(h, v.length);
  }
  const t = typeof v;
  if (t === "object") {
    if (v === null) { d.nulls++; return nodeNull(); }
    d.objects++;
    let acc = 0, count = 0;
    for (const key in v) {
      if (!Object.prototype.hasOwnProperty.call(v, key)) continue;
      d.strings++;
      d.codeUnits += key.length;
      acc = (acc ^ memberMix(nodeString(key), walkJs(v[key], d))) >>> 0;
      count++;
    }
    return finishObject(acc, count);
  }
  if (t === "string") { d.strings++; d.codeUnits += v.length; return nodeString(v); }
  if (t === "number") { d.numbers++; return nodeNumber(v); }
  d.bools++;
  return nodeBool(v);
}

export function finalizeDigest(d, rootHash) { d.fp = rootHash >>> 0; return d; }

// stop-early pointer lane: extract statuses[0].{id_str,text,user.screen_name}, fold, STOP
const _mix = (h, x) => Math.imul(h ^ x, 0x01000193) >>> 0;
export function tapeGet(doc) {
  const s0 = doc.indexArray(doc.getKey(0, "statuses"), 0);
  const idStr = doc.stringLeaf(doc.getKey(s0, "id_str"));
  const text = doc.stringLeaf(doc.getKey(s0, "text"));
  const screen = doc.stringLeaf(doc.getKey(doc.getKey(s0, "user"), "screen_name"));
  let h = 0x811c9dc5;
  h = _mix(h, nodeStringFromHash(idStr.h));
  h = _mix(h, nodeStringFromHash(text.h));
  h = _mix(h, nodeStringFromHash(screen.h));
  return { fp: h >>> 0, codeUnits: idStr.len + text.len + screen.len };
}
export function jsonGet(v) {
  const s0 = v.statuses[0];
  let h = 0x811c9dc5;
  h = _mix(h, nodeString(s0.id_str));
  h = _mix(h, nodeString(s0.text));
  h = _mix(h, nodeString(s0.user.screen_name));
  return { fp: h >>> 0, codeUnits: s0.id_str.length + s0.text.length + s0.user.screen_name.length };
}
"#;

fn main() {
    // `emit`               -> full JSON decoder to stdout (probe-2, the brief's command)
    // `emit full`          -> same, explicit
    // `emit <out_dir>`     -> the 4 classify FACE files (probe-1, back-compat)
    let arg = std::env::args().nth(1);
    match arg.as_deref() {
        None | Some("full") | Some("full-json") => {
            print!("{}", emit_full_decoder(&json_spec()));
        }
        Some(out_dir) => {
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
    }
}
