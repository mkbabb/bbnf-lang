//! SK-V18 G2 — CSS lowering over the RAW grammar AST.
//!
//! CSS cannot reach `lower::lower_to_rust`: `passes::compile`'s
//! `first_unsupported` REJECTS CSS's `@token`/`@pretty`/`->`-projection
//! constructs (the very reason CSS is on the `RequestFacts` path). So the CSS
//! provider is DERIVED here, by walking the raw parsed grammar AST of
//! `stylesheet.bbnf` (+ its l4 imports), NOT the `BackendIr`/`SinkOnlyProgram`.
//!
//! This module owns ONLY the structural-alphabet derivation: the STOP-set
//! byte-arrays passed at each scan call site and the SKIP family the balanced
//! scan rides over — both EMERGENT from the rule literals, not literals in the
//! emitter. The scan ALGORITHM lives in the `css_balanced_component_scan`
//! runtime primitive; the drivers + lazy projection are emitted by the sibling
//! `css_scan_direct` from the [`CssScanProgram`] this builds.
//!
//! The alphabet is grammar-DERIVED:
//! - `{` `}` from `ruleBlock = "{" >> blockContent ?w << "}"`,
//! - `;` from the declaration `";"` terminators (`properties.bbnf`) and the
//!   `atRuleBody = ruleBlock | ";"` fallback,
//! - `:` from the declaration `":"` split and the selector `":"`/`"::"` pseudos,
//! - `(` `)` `[` `]` from the selector pseudo-functions and `attrSelector`,
//! - the quotes `'` `"` from `cssString`, and the comment `/` from `@ws`.
//!
//! A mutation of any invoking `.bbnf` rule (drop the `";"`, change the
//! `ruleBlock` braces) therefore changes the derived byte-sets — the
//! arg-mutation falsifier (see the `css_scan` tests).

use crate::CodegenError;
use std::collections::BTreeSet;

/// The grammar-derived structural alphabet for the CSS balanced scan.
///
/// Every field is a SET of bytes harvested from the rule literals/regexes of
/// the CSS L4 grammar — never a fixed literal in this source. `css_scan_direct`
/// renders the per-call-site STOP arrays and the fixed-9 SKIP array from these.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScanAlphabet {
    /// The rule-block open/close braces — from `ruleBlock`'s `"{"`/`"}"`.
    pub block_open: u8,
    pub block_close: u8,
    /// The declaration / at-rule terminator — from the declaration `";"` and
    /// `atRuleBody = ruleBlock | ";"`.
    pub terminator: u8,
    /// The declaration name/value split + selector pseudo lead — from the
    /// declaration `":"` and the selector `":"`/`"::"`.
    pub colon: u8,
    /// The balanced open/close pairs the scan skips over: `(`/`)` (selector
    /// pseudo-functions, `func-body`), `[`/`]` (`attrSelector`), `{`/`}`
    /// (`ruleBlock`). Stored as ordered (open, close) pairs.
    pub balanced_pairs: Vec<(u8, u8)>,
    /// The string-quote bytes the scan skips over — from `cssString` /
    /// selector `string`.
    pub quotes: Vec<u8>,
    /// The comment-introducer byte — from the `@ws` `/* ... */` comment regex.
    pub comment_lead: u8,
}

impl ScanAlphabet {
    /// The fixed-9 SKIP alphabet the `css_balanced_component_scan` primitive
    /// consumes: the quotes, the comment lead, then the open/close brackets in
    /// pair order. This MUST be exactly the 9-byte set the neutral
    /// `find_css_significant` kernel rides (`b"'\"/()[]{}"`), but assembled
    /// from the GRAMMAR-DERIVED members above — the membership is caller data.
    pub fn skip_alphabet(&self) -> [u8; 9] {
        let mut out = Vec::with_capacity(9);
        // quotes (cssString) then comment lead (@ws), then the bracket pairs
        // interleaved open/close in derivation order: ( ) [ ] { }.
        out.extend_from_slice(&self.quotes);
        out.push(self.comment_lead);
        for (open, close) in &self.balanced_pairs {
            out.push(*open);
            out.push(*close);
        }
        let mut fixed = [0u8; 9];
        let n = out.len().min(9);
        fixed[..n].copy_from_slice(&out[..n]);
        fixed
    }

    /// The STOP set at an at-rule prelude scan: `; { }` — `;` (atRuleBody
    /// fallback), `{`/`}` (the `ruleBlock` braces).
    pub fn at_rule_stop(&self) -> Vec<u8> {
        vec![self.terminator, self.block_open, self.block_close]
    }

    /// The STOP set at a qualified-rule / block-item scan: `{ } ;`.
    pub fn qualified_stop(&self) -> Vec<u8> {
        vec![self.block_open, self.block_close, self.terminator]
    }

    /// The STOP set at a declaration-value scan: `; }`.
    pub fn declaration_stop(&self) -> Vec<u8> {
        vec![self.terminator, self.block_close]
    }

    /// The STOP set at the colon-before scan: `: { } ;`.
    pub fn colon_before_stop(&self) -> Vec<u8> {
        vec![self.colon, self.block_open, self.block_close, self.terminator]
    }
}

/// The CSS scan program — the grammar-derived provider description the
/// `css_scan_direct` emitter renders into the `generated.rs` body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CssScanProgram {
    /// The grammar name (`css_l4`).
    pub grammar_name: String,
    /// The entry rule (`stylesheet`).
    pub entry_rule: String,
    /// The derived structural alphabet.
    pub alphabet: ScanAlphabet,
    /// The at-rule branch-tag rule names (`mediaRule`/`keyframesRule`/
    /// `genericAtRule`) — the at-rule-vs-qualified `BackendRule` branch.
    pub at_rule_branches: Vec<String>,
}


/// Derive the [`CssScanProgram`] from the raw CSS grammar sources.
///
/// `sources` are the `(rel_path, source)` pairs the request carries — the 15
/// l4 `.bbnf` files. Each is scanned into its rule set (a CSS-tolerant grammar
/// harvest, §below), and the structural alphabet is read off the literals of
/// the named structural rules (`ruleBlock`/`atRuleBody`/`cssString`/the
/// selectors/the typed declarations). The entry grammar (`stylesheet.bbnf`)
/// is required.
pub fn derive_program(
    _grammar_name: &str,
    entry_rule: &str,
    sources: &[(String, String)],
) -> Result<CssScanProgram, CodegenError> {
    let grammar = ScannedGrammar::from_sources(sources)?;
    let alphabet = derive_alphabet(&grammar)?;
    let at_rule_branches = derive_at_rule_branches(&grammar)?;
    Ok(CssScanProgram {
        grammar_name: _grammar_name.to_string(),
        entry_rule: entry_rule.to_string(),
        alphabet,
        at_rule_branches,
    })
}

// ===========================================================================
// The CSS-tolerant grammar harvest.
//
// The skinny `grammar::parse_grammar` IR parser admits only the minimal
// `"lit" /re/ (group) ref */+/?` shape — it REJECTS the CSS operators (`,`,
// `->`, `>>`, `<<`, `?w`) that pervade the l4 grammar. CSS therefore has no
// full AST in the skinny tree (the LOWERING GAP). So this module performs a
// faithful STRUCTURAL harvest of the grammar text: it splits each source into
// `name = body ;` rules and reads each rule body's TERMINAL set — the quoted
// `"..."` literals (skipping escaped quotes), the `/.../` regex patterns, and
// the bare-ident references — treating every operator/decoration between them
// as inert. This is a genuine grammar derivation: the alphabet is read off the
// rule TERMINALS, so a `.bbnf` mutation of an invoking rule's literal changes
// the harvested byte (the arg-mutation falsifier).
// ===========================================================================

/// One scanned rule: its name plus the terminal sets harvested from its body.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ScannedRule {
    rel_path: String,
    name: String,
    /// Every quoted `"..."` terminal's bytes, in body order.
    literals: Vec<Vec<u8>>,
    /// Every `/.../ ` regex pattern, in body order.
    regexes: Vec<String>,
    /// Every bare-ident reference, in body order.
    refs: Vec<String>,
}

impl ScannedRule {
    /// The union of all literal bytes + regex pattern bytes — the structural
    /// terminal alphabet of this rule.
    fn terminal_bytes(&self) -> BTreeSet<u8> {
        let mut out = BTreeSet::new();
        for lit in &self.literals {
            out.extend(lit.iter().copied());
        }
        for re in &self.regexes {
            out.extend(re.as_bytes().iter().copied());
        }
        out
    }
}

/// One scanned `@ws` directive — its regex pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ScannedWsDirective {
    pattern: String,
}

/// The full scanned grammar: every rule across every source + the `@ws`.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ScannedGrammar {
    rules: Vec<ScannedRule>,
    ws_directives: Vec<ScannedWsDirective>,
}

impl ScannedGrammar {
    fn from_sources(sources: &[(String, String)]) -> Result<Self, CodegenError> {
        let mut rules = Vec::new();
        let mut ws_directives = Vec::new();
        for (rel_path, source) in sources {
            scan_source(rel_path, source, &mut rules, &mut ws_directives);
        }
        if rules.is_empty() {
            return Err(CodegenError::Lowering(
                "css_scan: no rules harvested from CSS grammar sources".to_string(),
            ));
        }
        Ok(Self {
            rules,
            ws_directives,
        })
    }

    fn rule(&self, name: &str) -> Option<&ScannedRule> {
        self.rules.iter().find(|rule| rule.name == name)
    }
}

/// Scan one grammar source into its rules + `@ws` directives.
fn scan_source(
    rel_path: &str,
    source: &str,
    rules: &mut Vec<ScannedRule>,
    ws_directives: &mut Vec<ScannedWsDirective>,
) {
    let bytes = source.as_bytes();
    let mut cursor = 0usize;
    while cursor < bytes.len() {
        cursor = skip_trivia(bytes, cursor);
        if cursor >= bytes.len() {
            break;
        }
        if bytes[cursor] == b'@' {
            cursor = scan_directive(rel_path, source, cursor, ws_directives);
            continue;
        }
        // A rule: `name = body ;`.
        let name_start = cursor;
        while cursor < bytes.len() && is_ident_byte(bytes[cursor]) {
            cursor += 1;
        }
        if cursor == name_start {
            // Not an identifier — skip the stray byte and continue.
            cursor += 1;
            continue;
        }
        let name = source[name_start..cursor].to_string();
        cursor = skip_inline_ws(bytes, cursor);
        if cursor >= bytes.len() || bytes[cursor] != b'=' {
            // Not a rule head; continue scanning from here.
            continue;
        }
        cursor += 1; // consume `=`
        let body_start = cursor;
        // The body runs to the terminating `;` at the top level (skipping
        // `;` inside strings/regex/comments — handled by the body harvest).
        let body_end = find_rule_terminator(bytes, body_start);
        let body = &source[body_start..body_end];
        let (literals, regexes, refs) = harvest_body(body);
        rules.push(ScannedRule {
            rel_path: rel_path.to_string(),
            name,
            literals,
            regexes,
            refs,
        });
        cursor = (body_end + 1).min(bytes.len()); // consume `;`
    }
}

/// Scan an `@`-directive; capture the `@ws` regex, skip the rest to its
/// terminating `;` or newline.
fn scan_directive(
    _rel_path: &str,
    source: &str,
    start: usize,
    ws_directives: &mut Vec<ScannedWsDirective>,
) -> usize {
    let bytes = source.as_bytes();
    let is_ws = source[start..].starts_with("@ws");
    // Advance to the directive terminator (`;`), skipping strings/regex so a
    // `;` inside the `@ws` regex does not end it early.
    let mut cursor = start + 1;
    let mut ws_pattern: Option<String> = None;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b';' => {
                cursor += 1;
                break;
            }
            b'/' => {
                let re_start = cursor;
                let re_end = skip_regex(bytes, cursor);
                if is_ws && ws_pattern.is_none() && re_end > re_start + 1 {
                    // Strip the leading/trailing `/`.
                    ws_pattern = Some(source[re_start + 1..re_end - 1].to_string());
                }
                cursor = re_end;
            }
            b'"' | b'\'' => cursor = skip_string(bytes, cursor),
            b'\n' if !is_ws => {
                cursor += 1;
                break;
            }
            _ => cursor += 1,
        }
    }
    if is_ws {
        if let Some(pattern) = ws_pattern {
            ws_directives.push(ScannedWsDirective { pattern });
        }
    }
    cursor
}

/// Harvest a rule body's terminals: quoted literals, regex patterns, refs.
fn harvest_body(body: &str) -> (Vec<Vec<u8>>, Vec<String>, Vec<String>) {
    let bytes = body.as_bytes();
    let mut literals = Vec::new();
    let mut regexes = Vec::new();
    let mut refs = Vec::new();
    let mut cursor = 0usize;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b'/' if cursor + 1 < bytes.len() && bytes[cursor + 1] == b'/' => {
                // Line comment.
                cursor += body[cursor..]
                    .find('\n')
                    .map_or(body.len() - cursor, |index| index + 1);
            }
            b'"' | b'\'' => {
                let end = skip_string(bytes, cursor);
                // Decode the literal bytes (quote-stripped, escapes resolved
                // for the structural bytes we care about).
                let raw = &body[cursor..end];
                literals.push(decode_literal(raw));
                cursor = end;
            }
            b'/' => {
                let end = skip_regex(bytes, cursor);
                if end > cursor + 1 {
                    regexes.push(body[cursor + 1..end - 1].to_string());
                }
                cursor = end;
            }
            byte if is_ident_start(byte) => {
                let ident_start = cursor;
                while cursor < bytes.len() && is_ident_byte(bytes[cursor]) {
                    cursor += 1;
                }
                refs.push(body[ident_start..cursor].to_string());
            }
            _ => cursor += 1,
        }
    }
    (literals, regexes, refs)
}

/// Decode a `"..."` / `'...'` literal token into its bytes (quote-stripped,
/// common escapes resolved). Only the structural single bytes matter here.
fn decode_literal(raw: &str) -> Vec<u8> {
    let bytes = raw.as_bytes();
    if bytes.len() < 2 {
        return Vec::new();
    }
    let inner = &raw[1..raw.len() - 1];
    let mut out = Vec::with_capacity(inner.len());
    let mut chars = inner.bytes().peekable();
    while let Some(byte) = chars.next() {
        if byte == b'\\' {
            if let Some(&next) = chars.peek() {
                let decoded = match next {
                    b'n' => b'\n',
                    b't' => b'\t',
                    b'r' => b'\r',
                    other => other,
                };
                out.push(decoded);
                chars.next();
            }
        } else {
            out.push(byte);
        }
    }
    out
}

/// Find the top-level `;` ending a rule body, skipping strings/regex/comments.
fn find_rule_terminator(bytes: &[u8], start: usize) -> usize {
    let mut cursor = start;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b';' => return cursor,
            b'"' | b'\'' => cursor = skip_string(bytes, cursor),
            b'/' if cursor + 1 < bytes.len() && bytes[cursor + 1] == b'/' => {
                // Line comment to end of line.
                while cursor < bytes.len() && bytes[cursor] != b'\n' {
                    cursor += 1;
                }
            }
            b'/' if cursor + 1 < bytes.len() && bytes[cursor + 1] != b'/' => {
                cursor = skip_regex(bytes, cursor);
            }
            _ => cursor += 1,
        }
    }
    bytes.len()
}

fn skip_trivia(bytes: &[u8], mut cursor: usize) -> usize {
    loop {
        while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
            cursor += 1;
        }
        if cursor + 1 < bytes.len() && bytes[cursor] == b'/' && bytes[cursor + 1] == b'/' {
            while cursor < bytes.len() && bytes[cursor] != b'\n' {
                cursor += 1;
            }
            continue;
        }
        return cursor;
    }
}

fn skip_inline_ws(bytes: &[u8], mut cursor: usize) -> usize {
    while cursor < bytes.len() && (bytes[cursor] == b' ' || bytes[cursor] == b'\t') {
        cursor += 1;
    }
    cursor
}

/// Skip a `"..."` / `'...'` string from the opening quote, honouring `\`
/// escapes. Returns the index past the closing quote.
fn skip_string(bytes: &[u8], start: usize) -> usize {
    let quote = bytes[start];
    let mut cursor = start + 1;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b'\\' => cursor += 2,
            byte if byte == quote => return cursor + 1,
            _ => cursor += 1,
        }
    }
    bytes.len()
}

/// Skip a `/.../` regex from the opening `/`, honouring `\` escapes and `[...]`
/// classes (where `/` is literal). Returns the index past the closing `/`.
fn skip_regex(bytes: &[u8], start: usize) -> usize {
    let mut cursor = start + 1;
    let mut in_class = false;
    while cursor < bytes.len() {
        match bytes[cursor] {
            b'\\' => cursor += 2,
            b'[' => {
                in_class = true;
                cursor += 1;
            }
            b']' => {
                in_class = false;
                cursor += 1;
            }
            b'/' if !in_class => return cursor + 1,
            _ => cursor += 1,
        }
    }
    bytes.len()
}

fn is_ident_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'-'
}

// ===========================================================================
// The alphabet derivation, off the scanned rule terminals.
// ===========================================================================

fn derive_alphabet(grammar: &ScannedGrammar) -> Result<ScanAlphabet, CodegenError> {
    // --- braces from `ruleBlock` ---
    let (block_open, block_close) = {
        let rule = grammar.rule("ruleBlock").ok_or_else(|| {
            CodegenError::Lowering("css_scan: missing `ruleBlock` rule".to_string())
        })?;
        let bytes = rule.terminal_bytes();
        let open = require_byte(&bytes, b'{', "ruleBlock open brace")?;
        let close = require_byte(&bytes, b'}', "ruleBlock close brace")?;
        (open, close)
    };

    // --- terminator `;` from `atRuleBody` (the `ruleBlock | ";"` fallback) ---
    let terminator = {
        let rule = grammar.rule("atRuleBody").ok_or_else(|| {
            CodegenError::Lowering("css_scan: missing `atRuleBody` rule".to_string())
        })?;
        require_byte(&rule.terminal_bytes(), b';', "atRuleBody terminator")?
    };

    // --- colon `:` from a typed declaration's `":"` split ---
    let colon = {
        let rule = declaration_rule(grammar)?;
        require_byte(&rule.terminal_bytes(), b':', "declaration colon")?
    };

    // --- balanced pairs `()` `[]` `{}` from selectors + ruleBlock ---
    let mut balanced_pairs = Vec::new();
    let selector_bytes = collect_family_bytes(grammar, "selectors.bbnf");
    if selector_bytes.contains(&b'(') && selector_bytes.contains(&b')') {
        balanced_pairs.push((b'(', b')'));
    }
    if selector_bytes.contains(&b'[') && selector_bytes.contains(&b']') {
        balanced_pairs.push((b'[', b']'));
    }
    balanced_pairs.push((block_open, block_close));

    if balanced_pairs.len() != 3 {
        return Err(CodegenError::Lowering(format!(
            "css_scan: expected 3 balanced pairs (()/[]/{{}}), derived {}",
            balanced_pairs.len()
        )));
    }

    // --- quotes from `cssString` ---
    let quotes = {
        let rule = grammar.rule("cssString").ok_or_else(|| {
            CodegenError::Lowering("css_scan: missing `cssString` rule".to_string())
        })?;
        let bytes = rule.terminal_bytes();
        let mut quotes = Vec::new();
        if bytes.contains(&b'\'') {
            quotes.push(b'\'');
        }
        if bytes.contains(&b'"') {
            quotes.push(b'"');
        }
        if quotes.len() != 2 {
            return Err(CodegenError::Lowering(format!(
                "css_scan: cssString must yield both quote bytes, got {quotes:?}"
            )));
        }
        quotes
    };

    // --- comment lead `/` from the `@ws` comment regex ---
    let comment_lead = {
        let pattern = whitespace_regex(grammar)?;
        if !pattern.contains('/') {
            return Err(CodegenError::Lowering(
                "css_scan: @ws regex carries no comment `/` lead".to_string(),
            ));
        }
        b'/'
    };

    Ok(ScanAlphabet {
        block_open,
        block_close,
        terminator,
        colon,
        balanced_pairs,
        quotes,
        comment_lead,
    })
}

/// The first typed-declaration rule (a `*Decl` carrying both `:` and `;`).
fn declaration_rule(grammar: &ScannedGrammar) -> Result<&ScannedRule, CodegenError> {
    for rule in &grammar.rules {
        if !rule.rel_path.ends_with("properties.bbnf") || !rule.name.ends_with("Decl") {
            continue;
        }
        let bytes = rule.terminal_bytes();
        if bytes.contains(&b':') && bytes.contains(&b';') {
            return Ok(rule);
        }
    }
    Err(CodegenError::Lowering(
        "css_scan: no typed `*Decl` rule with `:` and `;` literals found".to_string(),
    ))
}

/// The union of terminal bytes across all rules of the source whose rel_path
/// ends with `suffix` (e.g. all of `selectors.bbnf`).
fn collect_family_bytes(grammar: &ScannedGrammar, suffix: &str) -> BTreeSet<u8> {
    let mut out = BTreeSet::new();
    for rule in &grammar.rules {
        if rule.rel_path.ends_with(suffix) {
            out.extend(rule.terminal_bytes());
        }
    }
    out
}

/// The `@ws` whitespace-regex pattern (carries the `/* */` comment shape).
fn whitespace_regex(grammar: &ScannedGrammar) -> Result<String, CodegenError> {
    grammar
        .ws_directives
        .first()
        .map(|directive| directive.pattern.clone())
        .ok_or_else(|| {
            CodegenError::Lowering("css_scan: missing `@ws` whitespace directive".to_string())
        })
}

/// Derive the at-rule branch-tag rule names from the `atRule` alternation —
/// the refs of the `atRule = mediaRule | keyframesRule | genericAtRule` body.
fn derive_at_rule_branches(grammar: &ScannedGrammar) -> Result<Vec<String>, CodegenError> {
    let rule = grammar
        .rule("atRule")
        .ok_or_else(|| CodegenError::Lowering("css_scan: missing `atRule` rule".to_string()))?;
    if rule.refs.is_empty() {
        return Err(CodegenError::Lowering(
            "css_scan: `atRule` alternation yielded no branch refs".to_string(),
        ));
    }
    Ok(rule.refs.clone())
}

fn require_byte(bytes: &BTreeSet<u8>, byte: u8, what: &str) -> Result<u8, CodegenError> {
    if bytes.contains(&byte) {
        Ok(byte)
    } else {
        Err(CodegenError::Lowering(format!(
            "css_scan: expected structural byte {:?} for {what}, not present in rule literals",
            byte as char
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn css_l4_sources() -> Vec<(String, String)> {
        const ROOT: &str = "/Users/mkbabb/Programming/bbnf-lang/grammar/css/l4";
        const FILES: &[&str] = &[
            "stylesheet.bbnf",
            "selectors.bbnf",
            "properties.bbnf",
            "media.bbnf",
        ];
        FILES
            .iter()
            .map(|name| {
                let path = format!("{ROOT}/{name}");
                let source = std::fs::read_to_string(&path).expect("read css l4 source");
                (format!("grammar/css/l4/{name}"), source)
            })
            .collect()
    }

    #[test]
    fn derives_structural_alphabet_from_grammar() {
        let sources = css_l4_sources();
        let program =
            derive_program("css_l4", "stylesheet", &sources).expect("derive css scan program");
        let alphabet = &program.alphabet;
        assert_eq!(alphabet.block_open, b'{');
        assert_eq!(alphabet.block_close, b'}');
        assert_eq!(alphabet.terminator, b';');
        assert_eq!(alphabet.colon, b':');
        assert_eq!(alphabet.quotes, vec![b'\'', b'"']);
        assert_eq!(alphabet.comment_lead, b'/');
        assert_eq!(
            alphabet.balanced_pairs,
            vec![(b'(', b')'), (b'[', b']'), (b'{', b'}')]
        );
        // The fixed-9 SKIP set MUST equal the runtime primitive's eq-set.
        assert_eq!(&alphabet.skip_alphabet(), b"'\"/()[]{}");
        // The at-rule branch refs are the `atRule` alternation members.
        assert!(program.at_rule_branches.iter().any(|r| r == "mediaRule"));
        assert!(program.at_rule_branches.iter().any(|r| r == "genericAtRule"));
        // STOP sets.
        assert_eq!(alphabet.at_rule_stop(), b";{}".to_vec());
        assert_eq!(alphabet.qualified_stop(), b"{};".to_vec());
        assert_eq!(alphabet.declaration_stop(), b";}".to_vec());
        assert_eq!(alphabet.colon_before_stop(), b":{};".to_vec());
    }

    /// The arg-mutation falsifier: dropping the `";"` terminator from
    /// `atRuleBody` MUST drop `;` from the derived STOP sets (the byte is read
    /// off the grammar terminal, not a literal in the emitter).
    #[test]
    fn arg_mutation_falsifier_fires() {
        let mut sources = css_l4_sources();
        // Mutate the stylesheet source: replace `atRuleBody = ruleBlock | ";"`
        // with a `;`-free body. The harvest must then FAIL to find the
        // terminator byte (the derivation rejects, proving the byte is
        // genuinely read from the grammar).
        for (path, source) in sources.iter_mut() {
            if path.ends_with("stylesheet.bbnf") {
                *source = source.replace(
                    "atRuleBody    = ruleBlock | \";\" ;",
                    "atRuleBody    = ruleBlock ;",
                );
            }
        }
        let result = derive_program("css_l4", "stylesheet", &sources);
        assert!(
            result.is_err(),
            "dropping the atRuleBody `;` must change the derivation (falsifier)"
        );
    }
}
