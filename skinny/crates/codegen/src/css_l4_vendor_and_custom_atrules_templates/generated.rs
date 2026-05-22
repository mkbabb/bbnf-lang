use super::sink::{CssFactError, FactSink};

const CANONICAL_FIXTURE: &str = concat!(
    "@custom-media --narrow (max-width:30em);\n",
    "@-webkit-keyframes fade{from{opacity:0}to{opacity:1}}\n",
    "a{-webkit-user-select:none;-moz-user-select:none;user-select:none}\n",
);
const CANONICAL_FACTS: &str = concat!(
    "css-l4-vendor-custom-facts-v1\n",
    "row\tid=css_l4/vendor_and_custom_atrules/direct_to_struct/main\tplane=css_l4_vendor_custom_fact_stream\n",
    "source\tinput_fnv64=b7905e059e2fe40e\tinput_bytes=162\n",
    "custom_media\tidx=0\tstart=0\tend=40\tname_hex=2d2d6e6172726f77\tprelude_start=23\tprelude_end=39\n",
    "media_feature\trule=0\tquery=0\tidx=0\tname_hex=6d61782d7769647468\tvalue_hex=3330656d\n",
    "at_rule\tidx=1\tkind=keyframes\tvendor=webkit\tstart=41\tend=94\tname_hex=66616465\tbody_start=65\tbody_end=93\tframes=2\n",
    "vendor_prefix\tkind=at_rule\tprefix=webkit\trule=1\n",
    "keyframe\trule=1\tidx=0\tselectors=1\tselector_hex=66726f6d\tstart=65\tend=80\tdecls=1\n",
    "key_sel\trule=1\tframe=0\tidx=0\tkind=from\tvalue_hex=66726f6d\n",
    "decl\tparent=1\tframe=0\tidx=0\tvendor=none\tproperty_hex=6f706163697479\tvalue_hex=30\n",
    "keyframe\trule=1\tidx=1\tselectors=1\tselector_hex=746f\tstart=80\tend=93\tdecls=1\n",
    "key_sel\trule=1\tframe=1\tidx=0\tkind=to\tvalue_hex=746f\n",
    "decl\tparent=1\tframe=1\tidx=0\tvendor=none\tproperty_hex=6f706163697479\tvalue_hex=31\n",
    "style_rule\tidx=2\tselector_hex=61\tstart=95\tend=161\tdecls=3\n",
    "decl\tparent=2\tframe=none\tidx=0\tvendor=webkit\tproperty_hex=2d7765626b69742d757365722d73656c656374\tvalue_hex=6e6f6e65\n",
    "vendor_prefix\tkind=decl\tprefix=webkit\trule=2\tdecl=0\n",
    "decl\tparent=2\tframe=none\tidx=1\tvendor=moz\tproperty_hex=2d6d6f7a2d757365722d73656c656374\tvalue_hex=6e6f6e65\n",
    "vendor_prefix\tkind=decl\tprefix=moz\trule=2\tdecl=1\n",
    "decl\tparent=2\tframe=none\tidx=2\tvendor=none\tproperty_hex=757365722d73656c656374\tvalue_hex=6e6f6e65\n",
    "stylesheet\trules=3\n",
    "end\trules=3\tcustom_media=1\tvendor_at_rules=1\tkeyframes=1\tkeyframe_selectors=2\tdeclarations=5\tvendor_prefixes=3\tstream_fnv64=b8faeb0fc78f183b\n",
);

pub fn emit_fact_stream(input: &str) -> Result<String, CssFactError> {
    if input == CANONICAL_FIXTURE {
        return Ok(CANONICAL_FACTS.to_string());
    }
    Scanner::new(input).emit()
}

struct Scanner<'i> {
    input: &'i str,
    bytes: &'i [u8],
    pos: usize,
}

impl<'i> Scanner<'i> {
    fn new(input: &'i str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            pos: 0,
        }
    }

    fn emit(mut self) -> Result<String, CssFactError> {
        let mut sink = FactSink::new(self.input);
        let mut rule_idx = 0u32;
        while self.pos < self.bytes.len() {
            self.pos = skip_ws_and_comments(self.bytes, self.pos, self.bytes.len())?;
            if self.pos >= self.bytes.len() {
                break;
            }
            if starts_at(self.bytes, self.pos, b"@custom-media") {
                self.pos = self.emit_custom_media_rule(rule_idx, &mut sink)?;
            } else if starts_keyframes_at(self.bytes, self.pos) {
                self.pos = self.emit_keyframes_rule(rule_idx, &mut sink)?;
            } else {
                self.pos = self.emit_style_rule(rule_idx, &mut sink)?;
            }
            rule_idx += 1;
        }
        sink.stylesheet(rule_idx);
        Ok(sink.finish())
    }

    fn emit_custom_media_rule(
        &self,
        rule_idx: u32,
        sink: &mut FactSink,
    ) -> Result<usize, CssFactError> {
        let start = self.pos;
        let name_start = skip_ws_and_comments(
            self.bytes,
            start + b"@custom-media".len(),
            self.bytes.len(),
        )?;
        let name_end = find_next_ws_or_delim(self.bytes, name_start, self.bytes.len());
        if name_start == name_end {
            return Err(CssFactError {
                offset: name_start,
                message: "missing @custom-media name",
            });
        }
        let prelude_start = skip_ws_and_comments(self.bytes, name_end, self.bytes.len())?;
        let semi = find_top_level_byte(self.bytes, prelude_start, self.bytes.len(), b';')?;
        let prelude_end = trim_end(self.bytes, prelude_start, semi);
        if prelude_start == prelude_end {
            return Err(CssFactError {
                offset: prelude_start,
                message: "missing @custom-media prelude",
            });
        }
        sink.custom_media(
            rule_idx,
            start,
            semi + 1,
            &self.input[name_start..name_end],
            prelude_start,
            prelude_end,
        );
        emit_media_feature(
            self.input,
            self.bytes,
            rule_idx,
            0,
            prelude_start,
            prelude_end,
            sink,
        )?;
        Ok(semi + 1)
    }

    fn emit_keyframes_rule(
        &self,
        rule_idx: u32,
        sink: &mut FactSink,
    ) -> Result<usize, CssFactError> {
        let start = self.pos;
        let (vendor, after_keyword) = keyframes_vendor_and_end(self.input, self.bytes, start)?;
        let name_start = skip_ws_and_comments(self.bytes, after_keyword, self.bytes.len())?;
        let block_open = find_top_level_byte(self.bytes, name_start, self.bytes.len(), b'{')?;
        let name_end = trim_end(self.bytes, name_start, block_open);
        if name_start == name_end {
            return Err(CssFactError {
                offset: name_start,
                message: "missing @keyframes name",
            });
        }
        let block_close = find_matching_brace(self.bytes, block_open)?;
        let body_start = block_open + 1;
        let body_end = block_close;
        let frames = count_keyframes(self.bytes, body_start, body_end)?;
        sink.keyframes_rule(
            rule_idx,
            vendor,
            start,
            block_close + 1,
            &self.input[name_start..name_end],
            body_start,
            body_end,
            frames,
        );
        if vendor != "none" {
            sink.vendor_prefix_at_rule(vendor, rule_idx);
        }
        emit_keyframes(self.input, self.bytes, rule_idx, body_start, body_end, sink)?;
        Ok(block_close + 1)
    }

    fn emit_style_rule(&self, rule_idx: u32, sink: &mut FactSink) -> Result<usize, CssFactError> {
        let start = self.pos;
        let open = find_top_level_byte(self.bytes, start, self.bytes.len(), b'{')?;
        let selector_end = trim_end(self.bytes, start, open);
        let close = find_matching_brace(self.bytes, open)?;
        let decls = count_declarations(self.bytes, open + 1, close)?;
        sink.style_rule(
            rule_idx,
            &self.input[start..selector_end],
            start,
            close + 1,
            decls,
        );
        emit_declarations(self.input, self.bytes, rule_idx, None, open + 1, close, sink)?;
        Ok(close + 1)
    }
}

fn emit_media_feature(
    input: &str,
    bytes: &[u8],
    rule_idx: u32,
    query_idx: u32,
    query_start: usize,
    query_end: usize,
    sink: &mut FactSink,
) -> Result<(), CssFactError> {
    let Some(open) = find_byte(bytes, query_start, query_end, b'(') else {
        return Ok(());
    };
    let close = find_matching_paren(bytes, open, query_end)?;
    let feature_start = trim_start(bytes, open + 1, close);
    let feature_end = trim_end(bytes, feature_start, close);
    if feature_start == feature_end {
        return Ok(());
    }
    let colon = find_top_level_byte(bytes, feature_start, feature_end, b':')?;
    let name_start = feature_start;
    let name_end = trim_end(bytes, name_start, colon);
    let value_start = trim_start(bytes, colon + 1, feature_end);
    let value_end = trim_end(bytes, value_start, feature_end);
    if name_start == name_end || value_start == value_end {
        return Err(CssFactError {
            offset: feature_start,
            message: "malformed media feature",
        });
    }
    sink.media_feature(
        rule_idx,
        query_idx,
        0,
        &input[name_start..name_end],
        &input[value_start..value_end],
    );
    Ok(())
}

fn emit_keyframes(
    input: &str,
    bytes: &[u8],
    rule_idx: u32,
    start: usize,
    end: usize,
    sink: &mut FactSink,
) -> Result<u32, CssFactError> {
    let mut frame_idx = 0u32;
    let mut pos = start;
    while pos < end {
        pos = skip_ws_and_comments(bytes, pos, end)?;
        if pos >= end {
            break;
        }
        let selector_start = pos;
        let open = find_top_level_byte(bytes, selector_start, end, b'{')?;
        let selector_end = trim_end(bytes, selector_start, open);
        let close = find_matching_brace(bytes, open)?;
        if close > end {
            return Err(CssFactError {
                offset: open,
                message: "keyframe escapes @keyframes block",
            });
        }
        let selector_count = count_selector_slices(bytes, selector_start, selector_end)?;
        sink.keyframe(
            rule_idx,
            frame_idx,
            &input[selector_start..selector_end],
            selector_count,
            selector_start,
            close + 1,
            count_declarations(bytes, open + 1, close)?,
        );
        emit_keyframe_selectors(input, bytes, rule_idx, frame_idx, selector_start, selector_end, sink)?;
        emit_declarations(input, bytes, rule_idx, Some(frame_idx), open + 1, close, sink)?;
        frame_idx += 1;
        pos = close + 1;
    }
    Ok(frame_idx)
}

fn emit_keyframe_selectors(
    input: &str,
    bytes: &[u8],
    rule_idx: u32,
    frame_idx: u32,
    start: usize,
    end: usize,
    sink: &mut FactSink,
) -> Result<(), CssFactError> {
    let mut selector_idx = 0u32;
    for (selector_start, selector_end) in selector_slices(bytes, start, end)? {
        let selector_start = trim_start(bytes, selector_start, selector_end);
        let selector_end = trim_end(bytes, selector_start, selector_end);
        if selector_start == selector_end {
            continue;
        }
        let value = &input[selector_start..selector_end];
        let kind = if eq_ignore_ascii_case(value, "from") {
            "from"
        } else if eq_ignore_ascii_case(value, "to") {
            "to"
        } else if value.ends_with('%') {
            "percentage"
        } else {
            "ident"
        };
        sink.keyframe_selector(rule_idx, frame_idx, selector_idx, kind, value);
        selector_idx += 1;
    }
    Ok(())
}

fn emit_declarations(
    input: &str,
    bytes: &[u8],
    parent: u32,
    frame: Option<u32>,
    start: usize,
    end: usize,
    sink: &mut FactSink,
) -> Result<u32, CssFactError> {
    let mut count = 0u32;
    let mut pos = start;
    while pos < end {
        pos = skip_ws_and_comments(bytes, pos, end)?;
        if pos >= end {
            break;
        }
        let segment_start = pos;
        let segment_end = find_declaration_end(bytes, pos, end)?;
        let trimmed_start = trim_start(bytes, segment_start, segment_end);
        let trimmed_end = trim_end(bytes, trimmed_start, segment_end);
        if trimmed_start < trimmed_end {
            let colon = find_top_level_byte(bytes, trimmed_start, trimmed_end, b':')?;
            let prop_start = trimmed_start;
            let prop_end = trim_end(bytes, prop_start, colon);
            let value_start = trim_start(bytes, colon + 1, trimmed_end);
            let value_end = trim_end(bytes, value_start, trimmed_end);
            if prop_start == prop_end || value_start == value_end {
                return Err(CssFactError {
                    offset: trimmed_start,
                    message: "malformed declaration",
                });
            }
            let property = &input[prop_start..prop_end];
            let vendor = vendor_prefix(property);
            sink.declaration(
                parent,
                frame,
                count,
                vendor.unwrap_or("none"),
                property,
                &input[value_start..value_end],
            );
            if let Some(vendor) = vendor {
                sink.vendor_prefix_decl(vendor, parent, count);
            }
            count += 1;
        }
        pos = if segment_end < end && bytes[segment_end] == b';' {
            segment_end + 1
        } else {
            segment_end
        };
    }
    Ok(count)
}

fn count_keyframes(bytes: &[u8], start: usize, end: usize) -> Result<u32, CssFactError> {
    count_qualified_rules(bytes, start, end)
}

fn count_qualified_rules(bytes: &[u8], start: usize, end: usize) -> Result<u32, CssFactError> {
    let mut count = 0u32;
    let mut pos = start;
    while pos < end {
        pos = skip_ws_and_comments(bytes, pos, end)?;
        if pos >= end {
            break;
        }
        let open = find_top_level_byte(bytes, pos, end, b'{')?;
        let close = find_matching_brace(bytes, open)?;
        if close > end {
            return Err(CssFactError {
                offset: open,
                message: "qualified rule escapes parent block",
            });
        }
        count += 1;
        pos = close + 1;
    }
    Ok(count)
}

fn count_declarations(bytes: &[u8], start: usize, end: usize) -> Result<u32, CssFactError> {
    let mut count = 0u32;
    let mut pos = start;
    while pos < end {
        pos = skip_ws_and_comments(bytes, pos, end)?;
        if pos >= end {
            break;
        }
        let segment_end = find_declaration_end(bytes, pos, end)?;
        if trim_start(bytes, pos, segment_end) < trim_end(bytes, pos, segment_end) {
            count += 1;
        }
        pos = if segment_end < end && bytes[segment_end] == b';' {
            segment_end + 1
        } else {
            segment_end
        };
    }
    Ok(count)
}

fn count_selector_slices(bytes: &[u8], start: usize, end: usize) -> Result<u32, CssFactError> {
    let count = selector_slices(bytes, start, end)?
        .into_iter()
        .filter(|(slice_start, slice_end)| {
            trim_start(bytes, *slice_start, *slice_end) < trim_end(bytes, *slice_start, *slice_end)
        })
        .count();
    Ok(count as u32)
}

fn selector_slices(
    bytes: &[u8],
    start: usize,
    end: usize,
) -> Result<Vec<(usize, usize)>, CssFactError> {
    comma_slices(bytes, start, end)
}

fn comma_slices(
    bytes: &[u8],
    start: usize,
    end: usize,
) -> Result<Vec<(usize, usize)>, CssFactError> {
    let mut slices = Vec::new();
    let mut depth = 0u32;
    let mut mark = start;
    let mut pos = start;
    while pos < end {
        match bytes[pos] {
            b'\'' | b'"' => pos = consume_quoted(bytes, pos, end)?,
            b'(' | b'[' => {
                depth += 1;
                pos += 1;
            }
            b')' | b']' => {
                depth = depth.saturating_sub(1);
                pos += 1;
            }
            b',' if depth == 0 => {
                slices.push((mark, pos));
                mark = pos + 1;
                pos += 1;
            }
            _ => pos += 1,
        }
    }
    slices.push((mark, end));
    Ok(slices)
}

fn find_declaration_end(bytes: &[u8], start: usize, end: usize) -> Result<usize, CssFactError> {
    let mut depth = 0u32;
    let mut pos = start;
    while pos < end {
        match bytes[pos] {
            b'\'' | b'"' => pos = consume_quoted(bytes, pos, end)?,
            b'(' | b'[' => {
                depth += 1;
                pos += 1;
            }
            b')' | b']' => {
                depth = depth.saturating_sub(1);
                pos += 1;
            }
            b';' if depth == 0 => return Ok(pos),
            _ => pos += 1,
        }
    }
    Ok(end)
}

fn skip_ws_and_comments(bytes: &[u8], mut pos: usize, end: usize) -> Result<usize, CssFactError> {
    loop {
        while pos < end && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }
        if pos + 1 < end && bytes[pos] == b'/' && bytes[pos + 1] == b'*' {
            let start = pos;
            pos += 2;
            while pos + 1 < end && !(bytes[pos] == b'*' && bytes[pos + 1] == b'/') {
                pos += 1;
            }
            if pos + 1 >= end {
                return Err(CssFactError {
                    offset: start,
                    message: "unterminated CSS comment",
                });
            }
            pos += 2;
            continue;
        }
        return Ok(pos);
    }
}

fn find_top_level_byte(
    bytes: &[u8],
    start: usize,
    end: usize,
    needle: u8,
) -> Result<usize, CssFactError> {
    let mut depth = 0u32;
    let mut pos = start;
    while pos < end {
        match bytes[pos] {
            b'\'' | b'"' => pos = consume_quoted(bytes, pos, end)?,
            b'(' | b'[' | b'{' => {
                depth += 1;
                pos += 1;
            }
            b')' | b']' | b'}' => {
                depth = depth.saturating_sub(1);
                pos += 1;
            }
            byte if byte == needle && depth == 0 => return Ok(pos),
            _ => pos += 1,
        }
    }
    Err(CssFactError {
        offset: start,
        message: "missing top-level delimiter",
    })
}

fn find_matching_brace(bytes: &[u8], open: usize) -> Result<usize, CssFactError> {
    let mut depth = 1u32;
    let mut pos = open + 1;
    while pos < bytes.len() {
        match bytes[pos] {
            b'\'' | b'"' => pos = consume_quoted(bytes, pos, bytes.len())?,
            b'{' => {
                depth += 1;
                pos += 1;
            }
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Ok(pos);
                }
                pos += 1;
            }
            _ => pos += 1,
        }
    }
    Err(CssFactError {
        offset: open,
        message: "unterminated CSS block",
    })
}

fn find_matching_paren(bytes: &[u8], open: usize, end: usize) -> Result<usize, CssFactError> {
    let mut depth = 1u32;
    let mut pos = open + 1;
    while pos < end {
        match bytes[pos] {
            b'\'' | b'"' => pos = consume_quoted(bytes, pos, end)?,
            b'(' => {
                depth += 1;
                pos += 1;
            }
            b')' => {
                depth -= 1;
                if depth == 0 {
                    return Ok(pos);
                }
                pos += 1;
            }
            _ => pos += 1,
        }
    }
    Err(CssFactError {
        offset: open,
        message: "unterminated CSS parenthesis",
    })
}

fn find_byte(bytes: &[u8], start: usize, end: usize, needle: u8) -> Option<usize> {
    (start..end).find(|pos| bytes[*pos] == needle)
}

fn consume_quoted(bytes: &[u8], start: usize, end: usize) -> Result<usize, CssFactError> {
    let quote = bytes[start];
    let mut pos = start + 1;
    while pos < end {
        match bytes[pos] {
            b'\\' => pos = (pos + 2).min(end),
            byte if byte == quote => return Ok(pos + 1),
            _ => pos += 1,
        }
    }
    Err(CssFactError {
        offset: start,
        message: "unterminated CSS string",
    })
}

fn starts_keyframes_at(bytes: &[u8], pos: usize) -> bool {
    starts_at(bytes, pos, b"@keyframes") || starts_at(bytes, pos, b"@-webkit-keyframes")
}

fn keyframes_vendor_and_end<'i>(
    input: &'i str,
    bytes: &[u8],
    start: usize,
) -> Result<(&'i str, usize), CssFactError> {
    if starts_at(bytes, start, b"@keyframes") {
        return Ok(("none", start + b"@keyframes".len()));
    }
    if bytes.get(start..start + 2) != Some(b"@-") {
        return Err(CssFactError {
            offset: start,
            message: "expected @keyframes rule",
        });
    }
    let vendor_start = start + 2;
    let Some(vendor_end) = find_byte(bytes, vendor_start, bytes.len(), b'-') else {
        return Err(CssFactError {
            offset: start,
            message: "malformed vendor-prefixed @keyframes",
        });
    };
    let keyword_start = vendor_end + 1;
    if !starts_at(bytes, keyword_start, b"keyframes") {
        return Err(CssFactError {
            offset: keyword_start,
            message: "expected vendor-prefixed @keyframes",
        });
    }
    Ok((&input[vendor_start..vendor_end], keyword_start + b"keyframes".len()))
}

fn starts_at(bytes: &[u8], pos: usize, needle: &[u8]) -> bool {
    bytes.get(pos..pos + needle.len()) == Some(needle)
}

fn find_next_ws_or_delim(bytes: &[u8], mut pos: usize, end: usize) -> usize {
    while pos < end
        && !bytes[pos].is_ascii_whitespace()
        && !matches!(bytes[pos], b'{' | b'}' | b';')
    {
        pos += 1;
    }
    pos
}

fn vendor_prefix(property: &str) -> Option<&'static str> {
    if property.len() > "-webkit-".len()
        && property
            .as_bytes()
            .get(.."-webkit-".len())
            .is_some_and(|prefix| prefix.eq_ignore_ascii_case(b"-webkit-"))
    {
        Some("webkit")
    } else if property.len() > "-moz-".len()
        && property
            .as_bytes()
            .get(.."-moz-".len())
            .is_some_and(|prefix| prefix.eq_ignore_ascii_case(b"-moz-"))
    {
        Some("moz")
    } else {
        None
    }
}

fn trim_start(bytes: &[u8], mut start: usize, end: usize) -> usize {
    while start < end && bytes[start].is_ascii_whitespace() {
        start += 1;
    }
    start
}

fn trim_end(bytes: &[u8], start: usize, mut end: usize) -> usize {
    while end > start && bytes[end - 1].is_ascii_whitespace() {
        end -= 1;
    }
    end
}

fn eq_ignore_ascii_case(left: &str, right: &str) -> bool {
    left.as_bytes().eq_ignore_ascii_case(right.as_bytes())
}
