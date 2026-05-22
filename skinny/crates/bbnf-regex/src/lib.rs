#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegexFacts {
    pub nullable: bool,
    pub first: FirstSet,
    pub byte_classes: Vec<ByteClass>,
    pub hir: RegexHir,
    pub string: Option<StringFacts>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegexHir {
    pub kind: RegexKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegexKind {
    Whitespace,
    QuotedString,
    Numeric,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FirstSet {
    Exact(ByteSet256),
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ByteClass {
    pub bytes: ByteSet256,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ByteSet256 {
    words: [u64; 4],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringFacts {
    pub delimiter: u8,
    pub escape: u8,
}

pub fn analyze(pattern: &str) -> RegexFacts {
    let kind = classify(pattern);
    let nullable = regex_is_nullable(pattern, kind);
    let first = first_set(pattern, kind);
    let byte_classes = match &first {
        FirstSet::Exact(bytes) if !bytes.is_empty() => vec![ByteClass { bytes: *bytes }],
        _ => Vec::new(),
    };
    let string = (kind == RegexKind::QuotedString).then_some(StringFacts {
        delimiter: b'"',
        escape: b'\\',
    });
    RegexFacts {
        nullable,
        first,
        byte_classes,
        hir: RegexHir { kind },
        string,
    }
}

impl ByteSet256 {
    pub fn insert(&mut self, byte: u8) {
        self.words[usize::from(byte / 64)] |= 1u64 << (byte % 64);
    }

    pub fn insert_range(&mut self, start: u8, end: u8) {
        for byte in start..=end {
            self.insert(byte);
        }
    }

    pub fn contains(&self, byte: u8) -> bool {
        (self.words[usize::from(byte / 64)] & (1u64 << (byte % 64))) != 0
    }

    pub fn is_empty(&self) -> bool {
        self.words.iter().all(|word| *word == 0)
    }

    pub fn to_vec(self) -> Vec<u8> {
        (0u8..=u8::MAX)
            .filter(|byte| self.contains(*byte))
            .collect()
    }
}

fn classify(pattern: &str) -> RegexKind {
    if pattern == r"[ \t\n\r]*" {
        RegexKind::Whitespace
    } else if pattern.starts_with('"') {
        RegexKind::QuotedString
    } else if pattern.starts_with("-?(0|[1-9]")
        && (pattern.contains(r"\d") || pattern.contains("[0-9]"))
        && pattern.contains("[eE]")
    {
        RegexKind::Numeric
    } else {
        RegexKind::Unknown
    }
}

fn regex_is_nullable(pattern: &str, kind: RegexKind) -> bool {
    if kind == RegexKind::Whitespace || pattern.is_empty() {
        return true;
    }
    let Some(atom) = pattern
        .strip_suffix('*')
        .or_else(|| pattern.strip_suffix('?'))
    else {
        return false;
    };
    is_single_atom(atom)
}

fn first_set(pattern: &str, kind: RegexKind) -> FirstSet {
    let mut bytes = ByteSet256::default();
    match kind {
        RegexKind::Whitespace => {
            for byte in [b' ', b'\t', b'\n', b'\r'] {
                bytes.insert(byte);
            }
            FirstSet::Exact(bytes)
        }
        RegexKind::QuotedString => {
            bytes.insert(b'"');
            FirstSet::Exact(bytes)
        }
        RegexKind::Numeric => {
            bytes.insert(b'-');
            bytes.insert_range(b'0', b'9');
            FirstSet::Exact(bytes)
        }
        RegexKind::Unknown => first_from_prefix(pattern),
    }
}

fn first_from_prefix(pattern: &str) -> FirstSet {
    if let Some(bytes) = parse_leading_class(pattern) {
        return FirstSet::Exact(bytes);
    }
    let mut bytes = ByteSet256::default();
    let raw = pattern.as_bytes();
    match raw {
        [b'\\', b'd', ..] => bytes.insert_range(b'0', b'9'),
        [b'\\', b't', ..] => bytes.insert(b'\t'),
        [b'\\', b'n', ..] => bytes.insert(b'\n'),
        [b'\\', b'r', ..] => bytes.insert(b'\r'),
        [b'\\', escaped, ..] => bytes.insert(*escaped),
        [byte, ..] if byte.is_ascii() && !matches!(byte, b'(' | b'|' | b'*' | b'+' | b'?') => {
            bytes.insert(*byte)
        }
        _ => return FirstSet::Unknown,
    }
    FirstSet::Exact(bytes)
}

fn parse_leading_class(pattern: &str) -> Option<ByteSet256> {
    let bytes = pattern.as_bytes();
    if bytes.first().copied() != Some(b'[') || bytes.get(1).copied() == Some(b'^') {
        return None;
    }
    let end = closing_class_index(bytes)?;
    let mut set = ByteSet256::default();
    let mut index = 1;
    while index < end {
        let current = class_byte(bytes, &mut index, end)?;
        if index + 1 < end && bytes[index] == b'-' {
            index += 1;
            let range_end = class_byte(bytes, &mut index, end)?;
            if current <= range_end {
                set.insert_range(current, range_end);
                continue;
            }
            return None;
        }
        set.insert(current);
    }
    Some(set)
}

fn closing_class_index(bytes: &[u8]) -> Option<usize> {
    let mut escaped = false;
    for (index, byte) in bytes.iter().enumerate().skip(1) {
        if escaped {
            escaped = false;
        } else if *byte == b'\\' {
            escaped = true;
        } else if *byte == b']' {
            return Some(index);
        }
    }
    None
}

fn class_byte(bytes: &[u8], index: &mut usize, end: usize) -> Option<u8> {
    let byte = *bytes.get(*index)?;
    *index += 1;
    if byte != b'\\' {
        return Some(byte);
    }
    let escaped = *bytes.get(*index)?;
    if *index >= end {
        return None;
    }
    *index += 1;
    Some(match escaped {
        b'd' => return None,
        b't' => b'\t',
        b'n' => b'\n',
        b'r' => b'\r',
        other => other,
    })
}

fn is_single_atom(pattern: &str) -> bool {
    if pattern.is_empty() {
        return false;
    }
    if pattern.starts_with('[') {
        return closing_class_index(pattern.as_bytes()) == Some(pattern.len() - 1);
    }
    if pattern.starts_with("(?:") || pattern.starts_with('(') {
        return pattern.ends_with(')') && balanced_parenthesized(pattern);
    }
    if pattern.starts_with('\\') {
        return pattern.as_bytes().len() == 2;
    }
    pattern.chars().count() == 1
}

fn balanced_parenthesized(pattern: &str) -> bool {
    let mut depth = 0i32;
    let mut escaped = false;
    for byte in pattern.bytes() {
        if escaped {
            escaped = false;
            continue;
        }
        match byte {
            b'\\' => escaped = true,
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth < 0 {
                    return false;
                }
            }
            _ => {}
        }
    }
    depth == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analyzes_whitespace() {
        let facts = analyze(r"[ \t\n\r]*");
        assert!(facts.nullable);
        assert_eq!(facts.hir.kind, RegexKind::Whitespace);
        let FirstSet::Exact(first) = facts.first else {
            panic!("whitespace first set should be exact");
        };
        for byte in [b' ', b'\t', b'\n', b'\r'] {
            assert!(first.contains(byte));
        }
    }

    #[test]
    fn analyzes_number_spellings() {
        for pattern in [
            r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?",
            r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+\-]?[0-9]+)?",
        ] {
            let facts = analyze(pattern);
            assert!(!facts.nullable);
            assert_eq!(facts.hir.kind, RegexKind::Numeric);
            let FirstSet::Exact(first) = facts.first else {
                panic!("numeric first set should be exact");
            };
            assert!(first.contains(b'-'));
            assert!(first.contains(b'0'));
            assert!(first.contains(b'9'));
        }
    }

    #[test]
    fn analyzes_quoted_string_and_unknown() {
        let quoted = analyze(r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#);
        assert_eq!(quoted.hir.kind, RegexKind::QuotedString);
        assert_eq!(
            quoted.string,
            Some(StringFacts {
                delimiter: b'"',
                escape: b'\\'
            })
        );

        let unknown = analyze(r"(?:ab|cd)+");
        assert_eq!(unknown.hir.kind, RegexKind::Unknown);
        assert_eq!(unknown.first, FirstSet::Unknown);
    }

    #[test]
    fn analyzes_byte_classes_and_nullable_atoms() {
        let facts = analyze(r"[a-c]");
        let FirstSet::Exact(first) = facts.first else {
            panic!("class first set should be exact");
        };
        assert_eq!(first.to_vec(), vec![b'a', b'b', b'c']);
        assert!(analyze(r"[abc]*").nullable);
        assert!(analyze(r"\t?").nullable);
        assert!(analyze(r"(?:ab)?").nullable);
    }
}
