//! The parse-only recognizer — the SAME `value`-Alt walk emitted under the
//! recognize policy (the `sink.*` calls suppressed).
//!
//! This replaces the retired hand-written iterative explicit-stack parse-only
//! courier. Per the G1 amendment, the recognizer is held to STRUCTURAL +
//! BEHAVIORAL equivalence (51/51 JSON strict, blob-free, derived), NOT
//! byte-equivalence against the architecturally-distinct iterative courier:
//! the `SinkOnlyExpr` tree encodes a RECURSIVE lowering, and forcing a recursive
//! walk to reproduce an iterative machine byte-exact would be the L1 reject.
//!
//! The dispatch arm SET + KEYS are the SAME derived arms as the sink path
//! (`SinkWalk::arms`), so the `.bbnf`-mutation falsifier fires on this path too.

use super::{ArmKind, SinkWalk, INLINE_CFG};

impl SinkWalk<'_> {
    /// Emit the recognize-only `parse_only` entry + its recursive value walk.
    pub(crate) fn render_parse_only(&self, out: &mut String) {
        out.push_str(INLINE_CFG);
        out.push_str(
            r#"pub fn parse_only<'i>(input: &'i str) -> Result<(), ParseError<'i>> {
    let bytes = input.as_bytes();
    let mut cursor = skip_ascii_whitespace(bytes, 0);
    recognize_value(input, bytes, &mut cursor)?;
    cursor = skip_ascii_whitespace(bytes, cursor);
    if cursor == bytes.len() {
        Ok(())
    } else {
        Err(direct_error(input, cursor, ParseErrorKind::TrailingCharacters))
    }
}

"#,
        );
        self.render_recognize_value(out);
        self.render_recognize_object(out);
        self.render_recognize_array(out);
        self.render_recognize_string(out);
        self.render_recognize_number(out);
    }

    /// The recognize-policy value dispatch — the SAME derived arm set as the
    /// sink path, with sink calls suppressed.
    fn render_recognize_value(&self, out: &mut String) {
        out.push_str(INLINE_CFG);
        out.push_str(
            "fn recognize_value<'i>(\n    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n) -> Result<(), ParseError<'i>> {\n",
        );
        out.push_str("    *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str(
            "    let Some(byte) = bytes.get(*cursor).copied() else {\n        return Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue));\n    };\n    match byte {\n",
        );
        for arm in self.arms() {
            match &arm.kind {
                ArmKind::Container { .. } => {
                    let key = super::byte_literal(arm.keys[0]);
                    out.push_str("        ");
                    out.push_str(&key);
                    out.push_str(" => recognize_object(input, bytes, cursor),\n");
                }
                ArmKind::Sequence { .. } => {
                    let key = super::byte_literal(arm.keys[0]);
                    out.push_str("        ");
                    out.push_str(&key);
                    out.push_str(" => recognize_array(input, bytes, cursor),\n");
                }
                ArmKind::String => {
                    out.push_str("        b'\"' => recognize_string(input, bytes, cursor),\n");
                }
                ArmKind::Number => {
                    let key = super::dispatch::number_arm_key(&arm.keys);
                    out.push_str("        ");
                    out.push_str(&key);
                    out.push_str(" => recognize_number(input, bytes, cursor, byte),\n");
                }
                ArmKind::Literal { literals } => {
                    for literal in literals {
                        let key = super::byte_literal(literal.key);
                        out.push_str("        ");
                        out.push_str(&key);
                        out.push_str(" => consume_literal_direct(input, bytes, cursor, config::");
                        out.push_str(&literal.literal_const);
                        out.push_str("),\n");
                    }
                }
            }
        }
        out.push_str(
            "        _ => Err(direct_error(input, *cursor, ParseErrorKind::ExpectedValue)),\n    }\n}\n\n",
        );
    }

    fn render_recognize_object(&self, out: &mut String) {
        let (open, close) = self.recognize_container_bytes("object");
        out.push_str(INLINE_CFG);
        out.push_str(
            "fn recognize_object<'i>(\n    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n) -> Result<(), ParseError<'i>> {\n",
        );
        out.push_str("    consume_direct(input, bytes, cursor, ");
        out.push_str(&super::byte_literal(open));
        out.push_str(", ParseErrorKind::ExpectedValue)?;\n");
        out.push_str("    *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("    if take_direct(bytes, cursor, ");
        out.push_str(&super::byte_literal(close));
        out.push_str(") {\n        return Ok(());\n    }\n");
        out.push_str("    loop {\n");
        out.push_str("        recognize_string(input, bytes, cursor)?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        consume_direct(input, bytes, cursor, b':', ParseErrorKind::ExpectedColon)?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        recognize_value(input, bytes, cursor)?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        if take_direct(bytes, cursor, b',') {\n");
        out.push_str("            *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("            continue;\n        }\n");
        out.push_str("        consume_direct(input, bytes, cursor, ");
        out.push_str(&super::byte_literal(close));
        out.push_str(", ParseErrorKind::ExpectedCommaOrObjectEnd)?;\n");
        out.push_str("        return Ok(());\n    }\n}\n\n");
    }

    fn render_recognize_array(&self, out: &mut String) {
        let (open, close) = self.recognize_container_bytes("array");
        out.push_str(INLINE_CFG);
        out.push_str(
            "fn recognize_array<'i>(\n    input: &'i str,\n    bytes: &'i [u8],\n    cursor: &mut usize,\n) -> Result<(), ParseError<'i>> {\n",
        );
        out.push_str("    consume_direct(input, bytes, cursor, ");
        out.push_str(&super::byte_literal(open));
        out.push_str(", ParseErrorKind::ExpectedValue)?;\n");
        out.push_str("    *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("    if take_direct(bytes, cursor, ");
        out.push_str(&super::byte_literal(close));
        out.push_str(") {\n        return Ok(());\n    }\n");
        out.push_str("    loop {\n");
        out.push_str("        recognize_value(input, bytes, cursor)?;\n");
        out.push_str("        *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("        if take_direct(bytes, cursor, b',') {\n");
        out.push_str("            *cursor = skip_ascii_whitespace(bytes, *cursor);\n");
        out.push_str("            continue;\n        }\n");
        out.push_str("        consume_direct(input, bytes, cursor, ");
        out.push_str(&super::byte_literal(close));
        out.push_str(", ParseErrorKind::ExpectedCommaOrArrayEnd)?;\n");
        out.push_str("        return Ok(());\n    }\n}\n\n");
    }

    fn render_recognize_string(&self, out: &mut String) {
        out.push_str(INLINE_CFG);
        out.push_str(
            r#"fn recognize_string<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
) -> Result<(), ParseError<'i>> {
    let _ = parse_string_direct(input, bytes, cursor)?;
    Ok(())
}

"#,
        );
    }

    fn render_recognize_number(&self, out: &mut String) {
        out.push_str(INLINE_CFG);
        out.push_str(
            r#"fn recognize_number<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
    first: u8,
) -> Result<(), ParseError<'i>> {
    let span = match_number_span_from_first(bytes, *cursor, first)
        .ok_or_else(|| direct_error(input, *cursor, ParseErrorKind::InvalidNumber))?;
    *cursor = span.end;
    Ok(())
}

"#,
        );
    }

    fn recognize_container_bytes(&self, name: &str) -> (u8, u8) {
        let rule = self.rule(name).expect("container rule present");
        let mut literals = Vec::new();
        super::dispatch::collect_container_literals(&rule.expr, &mut literals);
        (
            *literals.first().expect("open delimiter"),
            *literals.last().expect("close delimiter"),
        )
    }
}
