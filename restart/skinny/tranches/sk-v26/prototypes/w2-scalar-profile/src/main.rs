use std::alloc::{GlobalAlloc, Layout, System};
use std::cmp::Ordering;
use std::fmt::Write as _;
use std::hint::black_box;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
use std::time::Instant;

const WARMUP_PAIRS: usize = 8;
const SAMPLE_PAIRS: usize = 40;
const BOOTSTRAP_RESAMPLES: usize = 50_000;
const FUZZ_DOCUMENTS: usize = 10_000;
const FULL_BYTES_PER_SAMPLE: usize = 1024 * 1024;
const RAW_BYTES_PER_SAMPLE: usize = 4 * 1024 * 1024;

struct CountingAllocator;

static ALLOC_CALLS: AtomicU64 = AtomicU64::new(0);
static REALLOC_CALLS: AtomicU64 = AtomicU64::new(0);
static ALLOCATED_BYTES: AtomicU64 = AtomicU64::new(0);

// This is an isolated measurement instrument, not production BBNF code.
unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOC_CALLS.fetch_add(1, AtomicOrdering::Relaxed);
        ALLOCATED_BYTES.fetch_add(layout.size() as u64, AtomicOrdering::Relaxed);
        // SAFETY: forwarding the allocator contract unchanged to System.
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        // SAFETY: forwarding the allocator contract unchanged to System.
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        REALLOC_CALLS.fetch_add(1, AtomicOrdering::Relaxed);
        ALLOCATED_BYTES.fetch_add(new_size as u64, AtomicOrdering::Relaxed);
        // SAFETY: forwarding the allocator contract unchanged to System.
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static GLOBAL_ALLOCATOR: CountingAllocator = CountingAllocator;

#[derive(Clone, Copy, Debug)]
struct AllocationCount {
    calls: u64,
    reallocs: u64,
    requested_bytes: u64,
    retained_capacity_bytes: u64,
}

fn reset_allocations() {
    ALLOC_CALLS.store(0, AtomicOrdering::SeqCst);
    REALLOC_CALLS.store(0, AtomicOrdering::SeqCst);
    ALLOCATED_BYTES.store(0, AtomicOrdering::SeqCst);
}

fn allocation_count() -> AllocationCount {
    AllocationCount {
        calls: ALLOC_CALLS.load(AtomicOrdering::SeqCst),
        reallocs: REALLOC_CALLS.load(AtomicOrdering::SeqCst),
        requested_bytes: ALLOCATED_BYTES.load(AtomicOrdering::SeqCst),
        retained_capacity_bytes: 0,
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Scanner {
    Oracle,
    Classifier,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct ClassifierStats {
    eligible_regions: u64,
    eligible_bytes: u64,
    block_bytes: u64,
    fallback_tail_bytes: u64,
    fallback_utf8_bytes: u64,
    fallback_escape_bytes: u64,
    rejecting_blocks: u64,
}

impl ClassifierStats {
    fn add(&mut self, other: Self) {
        self.eligible_regions += other.eligible_regions;
        self.eligible_bytes += other.eligible_bytes;
        self.block_bytes += other.block_bytes;
        self.fallback_tail_bytes += other.fallback_tail_bytes;
        self.fallback_utf8_bytes += other.fallback_utf8_bytes;
        self.fallback_escape_bytes += other.fallback_escape_bytes;
        self.rejecting_blocks += other.rejecting_blocks;
    }
}

#[derive(Clone)]
struct ByteTable {
    accepted: [bool; 256],
}

impl ByteTable {
    fn new(mut predicate: impl FnMut(u8) -> bool) -> Self {
        let mut accepted = [false; 256];
        for byte in 0_u16..=255 {
            accepted[usize::from(byte)] = predicate(byte as u8);
        }
        Self { accepted }
    }

    #[inline]
    fn contains(&self, byte: u8) -> bool {
        self.accepted[usize::from(byte)]
    }

    fn first_bytes(&self) -> impl Iterator<Item = u8> + '_ {
        self.accepted
            .iter()
            .enumerate()
            .filter(|(_, accepted)| **accepted)
            .map(|(byte, _)| byte as u8)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
enum Tag {
    Name,
    Number,
    Text,
    Flag,
    Equals,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    End,
}

#[derive(Clone, Copy)]
enum ClassValidator {
    Name,
    Decimal,
}

#[derive(Clone)]
enum TerminalShape {
    Exact {
        bytes: &'static [u8],
        word_boundary: bool,
    },
    Class {
        first: ByteTable,
        continuation: Box<ByteTable>,
        validator: ClassValidator,
    },
    Quoted {
        open: u8,
        close: u8,
        escape: u8,
        safe: ByteTable,
    },
}

#[derive(Clone)]
struct Terminal {
    tag: Tag,
    shape: TerminalShape,
}

struct LexicalGrammar {
    terminals: Vec<Terminal>,
    candidates: [Vec<usize>; 256],
    trivia: ByteTable,
    name_continuation: ByteTable,
}

impl LexicalGrammar {
    fn compile() -> Self {
        let name_first =
            ByteTable::new(|byte| byte.is_ascii_alphabetic() || byte == b'_' || byte >= 0x80);
        let name_continuation = ByteTable::new(|byte| {
            byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-') || byte >= 0x80
        });
        let number_first =
            ByteTable::new(|byte| byte.is_ascii_digit() || matches!(byte, b'+' | b'-'));
        let number_continuation = ByteTable::new(|byte| {
            byte.is_ascii_digit() || matches!(byte, b'+' | b'-' | b'.' | b'e' | b'E')
        });
        let quoted_safe = |close, escape| {
            ByteTable::new(move |byte| byte >= 0x20 && byte != close && byte != escape)
        };

        // Order is the lexical-choice priority. Word literals precede the
        // overlapping generic name class and require the same compiled boundary.
        let terminals = vec![
            Terminal {
                tag: Tag::Flag,
                shape: TerminalShape::Exact {
                    bytes: b"on",
                    word_boundary: true,
                },
            },
            Terminal {
                tag: Tag::Flag,
                shape: TerminalShape::Exact {
                    bytes: b"off",
                    word_boundary: true,
                },
            },
            exact(Tag::Equals, b"="),
            exact(Tag::Semicolon, b";"),
            exact(Tag::LeftParen, b"("),
            exact(Tag::RightParen, b")"),
            exact(Tag::LeftBracket, b"["),
            exact(Tag::RightBracket, b"]"),
            exact(Tag::Comma, b","),
            Terminal {
                tag: Tag::Text,
                shape: TerminalShape::Quoted {
                    open: b'"',
                    close: b'"',
                    escape: b'\\',
                    safe: quoted_safe(b'"', b'\\'),
                },
            },
            Terminal {
                tag: Tag::Text,
                shape: TerminalShape::Quoted {
                    open: b'\'',
                    close: b'\'',
                    escape: b'\\',
                    safe: quoted_safe(b'\'', b'\\'),
                },
            },
            Terminal {
                tag: Tag::Number,
                shape: TerminalShape::Class {
                    first: number_first,
                    continuation: Box::new(number_continuation),
                    validator: ClassValidator::Decimal,
                },
            },
            Terminal {
                tag: Tag::Name,
                shape: TerminalShape::Class {
                    first: name_first,
                    continuation: Box::new(name_continuation.clone()),
                    validator: ClassValidator::Name,
                },
            },
        ];

        let mut candidates: [Vec<usize>; 256] = std::array::from_fn(|_| Vec::new());
        for (terminal_id, terminal) in terminals.iter().enumerate() {
            match &terminal.shape {
                TerminalShape::Exact { bytes, .. } => {
                    candidates[usize::from(bytes[0])].push(terminal_id);
                }
                TerminalShape::Class { first, .. } => {
                    for byte in first.first_bytes() {
                        candidates[usize::from(byte)].push(terminal_id);
                    }
                }
                TerminalShape::Quoted { open, .. } => {
                    candidates[usize::from(*open)].push(terminal_id);
                }
            }
        }

        Self {
            terminals,
            candidates,
            trivia: ByteTable::new(|byte| byte.is_ascii_whitespace()),
            name_continuation,
        }
    }
}

fn exact(tag: Tag, bytes: &'static [u8]) -> Terminal {
    Terminal {
        tag,
        shape: TerminalShape::Exact {
            bytes,
            word_boundary: false,
        },
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Token {
    tag: Tag,
    start: usize,
    end: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Event {
    tag: Tag,
    start: usize,
    end: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ErrorKind {
    Utf8,
    Lexical,
    Syntax,
    Resource,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Outcome {
    accepted: bool,
    error: Option<(ErrorKind, usize)>,
    checksum: u64,
    events: usize,
}

#[derive(Default)]
struct Buffers {
    tokens: Vec<Token>,
    events: Vec<Event>,
}

fn utf8_scalar_len(input: &[u8], at: usize) -> Option<usize> {
    let first = *input.get(at)?;
    let width = if first < 0x80 {
        1
    } else if first & 0xe0 == 0xc0 {
        2
    } else if first & 0xf0 == 0xe0 {
        3
    } else if first & 0xf8 == 0xf0 {
        4
    } else {
        return None;
    };
    (at + width <= input.len()).then_some(width)
}

#[inline]
fn accepted_prefix_8(table: &ByteTable, bytes: &[u8]) -> usize {
    debug_assert!(bytes.len() >= 8);
    let mask = u8::from(table.contains(bytes[0]))
        | (u8::from(table.contains(bytes[1])) << 1)
        | (u8::from(table.contains(bytes[2])) << 2)
        | (u8::from(table.contains(bytes[3])) << 3)
        | (u8::from(table.contains(bytes[4])) << 4)
        | (u8::from(table.contains(bytes[5])) << 5)
        | (u8::from(table.contains(bytes[6])) << 6)
        | (u8::from(table.contains(bytes[7])) << 7);
    mask.trailing_ones() as usize
}

fn scan_class(
    scanner: Scanner,
    input: &[u8],
    start: usize,
    continuation: &ByteTable,
    stats: &mut ClassifierStats,
) -> usize {
    let mut at = start;
    if scanner == Scanner::Classifier {
        stats.eligible_regions += 1;
    }

    while at < input.len() {
        if input[at] >= 0x80 {
            if !continuation.contains(input[at]) {
                break;
            }
            let width = utf8_scalar_len(input, at).unwrap_or(1);
            if scanner == Scanner::Classifier {
                stats.fallback_utf8_bytes += width as u64;
            }
            at += width;
            continue;
        }

        if scanner == Scanner::Classifier && input.len() - at >= 8 {
            if input[at..at + 8].iter().any(|byte| *byte >= 0x80) {
                if continuation.contains(input[at]) {
                    stats.fallback_tail_bytes += 1;
                    at += 1;
                    continue;
                }
                break;
            }
            let prefix = accepted_prefix_8(continuation, &input[at..]);
            stats.eligible_bytes += prefix as u64;
            if prefix == 8 {
                stats.block_bytes += 8;
                at += 8;
                continue;
            }
            stats.rejecting_blocks += 1;
            at += prefix;
            break;
        }

        if !continuation.contains(input[at]) {
            break;
        }
        if scanner == Scanner::Classifier {
            stats.eligible_bytes += 1;
            stats.fallback_tail_bytes += 1;
        }
        at += 1;
    }
    at
}

fn scan_quoted(
    scanner: Scanner,
    input: &[u8],
    start: usize,
    close: u8,
    escape: u8,
    safe: &ByteTable,
    stats: &mut ClassifierStats,
) -> Result<usize, usize> {
    let mut at = start + 1;
    if scanner == Scanner::Classifier {
        stats.eligible_regions += 1;
    }
    loop {
        if at >= input.len() {
            return Err(at);
        }
        let byte = input[at];
        if byte == close {
            return Ok(at + 1);
        }
        if byte == escape {
            if at + 1 >= input.len() {
                return Err(at);
            }
            let width = utf8_scalar_len(input, at + 1).ok_or(at + 1)?;
            if scanner == Scanner::Classifier {
                stats.fallback_escape_bytes += (1 + width) as u64;
            }
            at += 1 + width;
            continue;
        }
        if byte < 0x20 {
            return Err(at);
        }
        if byte >= 0x80 {
            let width = utf8_scalar_len(input, at).ok_or(at)?;
            if scanner == Scanner::Classifier {
                stats.fallback_utf8_bytes += width as u64;
            }
            at += width;
            continue;
        }
        if scanner == Scanner::Classifier && input.len() - at >= 8 {
            if input[at..at + 8].iter().any(|candidate| *candidate >= 0x80) {
                stats.eligible_bytes += 1;
                stats.fallback_tail_bytes += 1;
                at += 1;
                continue;
            }
            let prefix = accepted_prefix_8(safe, &input[at..]);
            stats.eligible_bytes += prefix as u64;
            if prefix == 8 {
                stats.block_bytes += 8;
                at += 8;
                continue;
            }
            stats.rejecting_blocks += 1;
            at += prefix;
            continue;
        }
        if !safe.contains(byte) {
            continue;
        }
        if scanner == Scanner::Classifier {
            stats.eligible_bytes += 1;
            stats.fallback_tail_bytes += 1;
        }
        at += 1;
    }
}

fn valid_decimal(bytes: &[u8]) -> bool {
    let mut at = 0;
    if bytes
        .get(at)
        .is_some_and(|byte| matches!(byte, b'+' | b'-'))
    {
        at += 1;
    }
    let integer_start = at;
    while bytes.get(at).is_some_and(u8::is_ascii_digit) {
        at += 1;
    }
    if at == integer_start {
        return false;
    }
    if bytes.get(at) == Some(&b'.') {
        at += 1;
        let fraction_start = at;
        while bytes.get(at).is_some_and(u8::is_ascii_digit) {
            at += 1;
        }
        if at == fraction_start {
            return false;
        }
    }
    if bytes
        .get(at)
        .is_some_and(|byte| matches!(byte, b'e' | b'E'))
    {
        at += 1;
        if bytes
            .get(at)
            .is_some_and(|byte| matches!(byte, b'+' | b'-'))
        {
            at += 1;
        }
        let exponent_start = at;
        while bytes.get(at).is_some_and(u8::is_ascii_digit) {
            at += 1;
        }
        if at == exponent_start {
            return false;
        }
    }
    at == bytes.len()
}

fn lex(
    grammar: &LexicalGrammar,
    scanner: Scanner,
    input: &[u8],
    output: &mut Vec<Token>,
    stats: &mut ClassifierStats,
) -> Result<(), (ErrorKind, usize)> {
    output.clear();
    if std::str::from_utf8(input).is_err() {
        return Err((ErrorKind::Utf8, 0));
    }
    let mut at = 0;
    while at < input.len() {
        if grammar.trivia.contains(input[at]) {
            at = scan_class(scanner, input, at, &grammar.trivia, stats);
            continue;
        }

        let mut matched = None;
        for terminal_id in &grammar.candidates[usize::from(input[at])] {
            let terminal = &grammar.terminals[*terminal_id];
            let end = match &terminal.shape {
                TerminalShape::Exact {
                    bytes,
                    word_boundary,
                } => {
                    if !input[at..].starts_with(bytes) {
                        continue;
                    }
                    let end = at + bytes.len();
                    if *word_boundary
                        && input
                            .get(end)
                            .is_some_and(|byte| grammar.name_continuation.contains(*byte))
                    {
                        continue;
                    }
                    end
                }
                TerminalShape::Class {
                    continuation,
                    validator,
                    ..
                } => {
                    let end = scan_class(scanner, input, at, continuation, stats);
                    let valid = match validator {
                        ClassValidator::Name => end > at,
                        ClassValidator::Decimal => valid_decimal(&input[at..end]),
                    };
                    if !valid {
                        continue;
                    }
                    end
                }
                TerminalShape::Quoted {
                    close,
                    escape,
                    safe,
                    ..
                } => scan_quoted(scanner, input, at, *close, *escape, safe, stats)
                    .map_err(|error_at| (ErrorKind::Lexical, error_at))?,
            };
            matched = Some(Token {
                tag: terminal.tag,
                start: at,
                end,
            });
            break;
        }
        let token = matched.ok_or((ErrorKind::Lexical, at))?;
        at = token.end;
        output.push(token);
    }
    output.push(Token {
        tag: Tag::End,
        start: input.len(),
        end: input.len(),
    });
    Ok(())
}

#[derive(Clone)]
enum Expr {
    Token(Tag),
    Sequence(Vec<Expr>),
    Choice(Vec<Expr>),
    Repeat(Box<Expr>),
    Optional(Box<Expr>),
    Rule(usize),
    End,
}

struct ParserGrammar {
    rules: Vec<Expr>,
}

impl ParserGrammar {
    fn compile() -> Self {
        use Expr::{Choice, End, Optional, Repeat, Rule, Sequence, Token};
        let value = Rule(5);
        let args = Rule(8);
        let invoke = Sequence(vec![
            Token(Tag::Name),
            Token(Tag::LeftParen),
            Optional(Box::new(args.clone())),
            Token(Tag::RightParen),
        ]);
        let series = Sequence(vec![
            Token(Tag::LeftBracket),
            Optional(Box::new(args.clone())),
            Token(Tag::RightBracket),
        ]);
        let rules = vec![
            Sequence(vec![Repeat(Box::new(Rule(1))), End]),
            Choice(vec![Rule(2), Rule(3), Rule(4)]),
            Sequence(vec![
                Token(Tag::Name),
                Token(Tag::Equals),
                value.clone(),
                Token(Tag::Semicolon),
            ]),
            Sequence(vec![Rule(6), Token(Tag::Semicolon)]),
            Sequence(vec![Rule(7), Token(Tag::Semicolon)]),
            Choice(vec![
                Rule(6),
                Rule(7),
                Token(Tag::Text),
                Token(Tag::Number),
                Token(Tag::Flag),
                Token(Tag::Name),
            ]),
            invoke,
            series,
            Sequence(vec![
                value.clone(),
                Repeat(Box::new(Sequence(vec![Token(Tag::Comma), value]))),
            ]),
        ];
        Self { rules }
    }
}

struct ParseCursor<'a> {
    tokens: &'a [Token],
    token_at: usize,
    events: &'a mut Vec<Event>,
    work: usize,
}

fn parse_expr(
    grammar: &ParserGrammar,
    expr: &Expr,
    cursor: &mut ParseCursor<'_>,
    depth: usize,
) -> Result<bool, (ErrorKind, usize)> {
    if depth > 256 || cursor.work > 16_000_000 {
        let at = cursor
            .tokens
            .get(cursor.token_at)
            .map_or(0, |token| token.start);
        return Err((ErrorKind::Resource, at));
    }
    cursor.work += 1;
    let token_start = cursor.token_at;
    let event_start = cursor.events.len();
    let matched = match expr {
        Expr::Token(tag) => {
            let Some(token) = cursor.tokens.get(cursor.token_at) else {
                return Ok(false);
            };
            if token.tag != *tag {
                false
            } else {
                cursor.token_at += 1;
                cursor.events.push(Event {
                    tag: token.tag,
                    start: token.start,
                    end: token.end,
                });
                true
            }
        }
        Expr::Sequence(expressions) => {
            let mut matched = true;
            for expression in expressions {
                if !parse_expr(grammar, expression, cursor, depth + 1)? {
                    matched = false;
                    break;
                }
            }
            matched
        }
        Expr::Choice(expressions) => {
            let mut matched = false;
            for expression in expressions {
                cursor.token_at = token_start;
                cursor.events.truncate(event_start);
                if parse_expr(grammar, expression, cursor, depth + 1)? {
                    matched = true;
                    break;
                }
            }
            matched
        }
        Expr::Repeat(expression) => {
            loop {
                let before_token = cursor.token_at;
                let before_event = cursor.events.len();
                if !parse_expr(grammar, expression, cursor, depth + 1)? {
                    cursor.token_at = before_token;
                    cursor.events.truncate(before_event);
                    break;
                }
                if cursor.token_at == before_token {
                    return Err((
                        ErrorKind::Resource,
                        cursor
                            .tokens
                            .get(before_token)
                            .map_or(0, |token| token.start),
                    ));
                }
            }
            true
        }
        Expr::Optional(expression) => {
            if !parse_expr(grammar, expression, cursor, depth + 1)? {
                cursor.token_at = token_start;
                cursor.events.truncate(event_start);
            }
            true
        }
        Expr::Rule(rule) => parse_expr(grammar, &grammar.rules[*rule], cursor, depth + 1)?,
        Expr::End => {
            let Some(token) = cursor.tokens.get(cursor.token_at) else {
                return Ok(false);
            };
            if token.tag == Tag::End {
                cursor.token_at += 1;
                cursor.events.push(Event {
                    tag: Tag::End,
                    start: token.start,
                    end: token.end,
                });
                true
            } else {
                false
            }
        }
    };
    if !matched {
        cursor.token_at = token_start;
        cursor.events.truncate(event_start);
    }
    Ok(matched)
}

fn checksum_events(input: &[u8], events: &[Event]) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for event in events {
        hash ^= u64::from(event.tag as u8);
        hash = hash.wrapping_mul(0x100_0000_01b3);
        hash ^= event.start as u64;
        hash = hash.wrapping_mul(0x100_0000_01b3);
        hash ^= event.end as u64;
        hash = hash.wrapping_mul(0x100_0000_01b3);
        for byte in &input[event.start..event.end] {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(0x100_0000_01b3);
        }
    }
    hash
}

fn parse_document(
    lexical: &LexicalGrammar,
    parser: &ParserGrammar,
    scanner: Scanner,
    input: &[u8],
    buffers: &mut Buffers,
    stats: &mut ClassifierStats,
) -> Outcome {
    buffers.events.clear();
    if let Err(error) = lex(lexical, scanner, input, &mut buffers.tokens, stats) {
        return Outcome {
            accepted: false,
            error: Some(error),
            checksum: 0,
            events: 0,
        };
    }
    let mut cursor = ParseCursor {
        tokens: &buffers.tokens,
        token_at: 0,
        events: &mut buffers.events,
        work: 0,
    };
    let result = parse_expr(parser, &parser.rules[0], &mut cursor, 0);
    let (accepted, error) = match result {
        Ok(true) => (true, None),
        Ok(false) => {
            let at = buffers
                .tokens
                .get(cursor.token_at)
                .map_or(input.len(), |token| token.start);
            (false, Some((ErrorKind::Syntax, at)))
        }
        Err(error) => (false, Some(error)),
    };
    let checksum = checksum_events(input, &buffers.events);
    Outcome {
        accepted,
        error,
        checksum,
        events: buffers.events.len(),
    }
}

#[derive(Clone)]
struct Fixture {
    family: &'static str,
    target_size: usize,
    bytes: Vec<u8>,
    sha256: String,
}

fn fit_repeating(target: usize, fragments: &[&str]) -> Vec<u8> {
    let mut output = Vec::with_capacity(target);
    let mut at = 0;
    while output.len() + fragments[at % fragments.len()].len() <= target {
        output.extend_from_slice(fragments[at % fragments.len()].as_bytes());
        at += 1;
    }
    output.resize(target, b' ');
    output
}

fn make_fixture(family: &'static str, target: usize) -> Fixture {
    let bytes = match family {
        "compact" => fit_repeating(
            target,
            &[
                "alpha0=beta0;",
                "toggle0=on;",
                "apply0(12,\"seed\",off);",
                "items0=[one,2,'three'];",
            ],
        ),
        "whitespace_punctuation" => fit_repeating(
            target,
            &[
                "\n  alpha_0 = beta-0 ;  ",
                "\n invoke_0 ( 12.5e+2 , [ on , off , name_0 ] ) ; ",
            ],
        ),
        "long_scalar" => {
            let prefix = b"payload=\"";
            let suffix = b"\";";
            let mut output = Vec::with_capacity(target);
            output.extend_from_slice(prefix);
            output.resize(target.saturating_sub(suffix.len()), b'a');
            output.extend_from_slice(suffix);
            output.truncate(target);
            output
        }
        "utf8_escape" => {
            let prefix = b"message=\"";
            let suffix = b"\";";
            let unit = "café-λ-\\n-\\\"-";
            let mut output = Vec::with_capacity(target);
            output.extend_from_slice(prefix);
            while output.len() + unit.len() + suffix.len() <= target {
                output.extend_from_slice(unit.as_bytes());
            }
            while output.len() + suffix.len() < target {
                output.push(b'x');
            }
            output.extend_from_slice(suffix);
            output
        }
        _ => unreachable!(),
    };
    assert_eq!(bytes.len(), target);
    let sha256 = sha256_hex(&bytes);
    Fixture {
        family,
        target_size: target,
        bytes,
        sha256,
    }
}

fn fixtures() -> Vec<Fixture> {
    let mut fixtures = Vec::new();
    for family in [
        "compact",
        "whitespace_punctuation",
        "long_scalar",
        "utf8_escape",
    ] {
        for size in [64, 1024, 65_536, 1_048_576] {
            fixtures.push(make_fixture(family, size));
        }
    }
    fixtures
}

fn equality(lexical: &LexicalGrammar, parser: &ParserGrammar, input: &[u8]) -> Result<(), String> {
    let mut oracle = Buffers::default();
    let mut classifier = Buffers::default();
    let mut oracle_stats = ClassifierStats::default();
    let mut classifier_stats = ClassifierStats::default();
    let oracle_outcome = parse_document(
        lexical,
        parser,
        Scanner::Oracle,
        input,
        &mut oracle,
        &mut oracle_stats,
    );
    let classifier_outcome = parse_document(
        lexical,
        parser,
        Scanner::Classifier,
        input,
        &mut classifier,
        &mut classifier_stats,
    );
    if oracle_outcome != classifier_outcome
        || oracle.tokens != classifier.tokens
        || oracle.events != classifier.events
    {
        return Err(format!(
            "oracle/classifier divergence: oracle={oracle_outcome:?}, classifier={classifier_outcome:?}"
        ));
    }
    Ok(())
}

fn correctness_gates(
    lexical: &LexicalGrammar,
    parser: &ParserGrammar,
    fixtures: &[Fixture],
) -> Result<CorrectnessReport, String> {
    let mut checks = 0_u64;
    for fixture in fixtures {
        equality(lexical, parser, &fixture.bytes)?;
        checks += 1;
    }
    let deep = format!("root({});", "node(".repeat(80) + "on" + &")".repeat(80));
    equality(lexical, parser, deep.as_bytes())?;
    checks += 1;

    for location in [1_usize, 32, 62] {
        let mut invalid = make_fixture("compact", 64).bytes;
        invalid[location] = b'@';
        equality(lexical, parser, &invalid)?;
        checks += 1;
    }
    for offset in 0..16 {
        for tail in 0..32 {
            let mut input = vec![b' '; offset];
            input.extend(std::iter::repeat_n(b'a', 32 + tail));
            input.push(b';');
            let start = offset;
            let table = ByteTable::new(|byte| byte == b'a');
            let mut oracle_stats = ClassifierStats::default();
            let mut candidate_stats = ClassifierStats::default();
            let oracle = scan_class(Scanner::Oracle, &input, start, &table, &mut oracle_stats);
            let candidate = scan_class(
                Scanner::Classifier,
                &input,
                start,
                &table,
                &mut candidate_stats,
            );
            if oracle != candidate {
                return Err(format!(
                    "raw alignment divergence at offset={offset}, tail={tail}"
                ));
            }
            checks += 1;
        }
    }

    let mut seed = 0x6a09_e667_f3bc_c909_u64;
    for index in 0..FUZZ_DOCUMENTS {
        seed = xorshift(seed);
        let mut document = format!(
            "n{}={};f{}({}.{}e+{},\"v{}\",{});",
            seed & 1023,
            if seed & 1 == 0 { "on" } else { "name" },
            (seed >> 10) & 1023,
            (seed >> 20) & 9999,
            (seed >> 32) & 999,
            (seed >> 42) & 9,
            seed.rotate_left(17),
            if seed & 2 == 0 { "off" } else { "other" },
        )
        .into_bytes();
        if index % 7 == 0 {
            let position = 1 + (seed as usize % (document.len() - 1));
            document[position] = b'@';
        } else if index % 11 == 0 {
            document.extend_from_slice("u=\"café-λ-\\n\";".as_bytes());
        }
        equality(lexical, parser, &document)?;
        checks += 1;
    }
    Ok(CorrectnessReport {
        passed: true,
        checks,
        fuzz_documents: FUZZ_DOCUMENTS,
    })
}

#[derive(Clone, Copy)]
struct CorrectnessReport {
    passed: bool,
    checks: u64,
    fuzz_documents: usize,
}

#[derive(Clone)]
struct Samples {
    left_ns: Vec<u128>,
    right_ns: Vec<u128>,
    iterations: usize,
    left_checksum: u64,
    right_checksum: u64,
}

fn timed_full(
    lexical: &LexicalGrammar,
    parser: &ParserGrammar,
    scanner: Scanner,
    input: &[u8],
    iterations: usize,
    buffers: &mut Buffers,
) -> u64 {
    let mut checksum = 0_u64;
    for _ in 0..iterations {
        let mut stats = ClassifierStats::default();
        let outcome = parse_document(
            lexical,
            parser,
            scanner,
            black_box(input),
            buffers,
            &mut stats,
        );
        assert!(outcome.accepted);
        checksum = checksum.wrapping_add(black_box(outcome.checksum));
    }
    checksum
}

fn measure_full_pair(
    lexical: &LexicalGrammar,
    parser: &ParserGrammar,
    input: &[u8],
    left: Scanner,
    right: Scanner,
) -> Samples {
    let iterations = (FULL_BYTES_PER_SAMPLE / input.len()).clamp(1, 2048);
    let mut left_buffers = Buffers::default();
    let mut right_buffers = Buffers::default();
    let mut stats = ClassifierStats::default();
    assert!(parse_document(lexical, parser, left, input, &mut left_buffers, &mut stats).accepted);
    assert!(
        parse_document(
            lexical,
            parser,
            right,
            input,
            &mut right_buffers,
            &mut stats
        )
        .accepted
    );

    for pair in 0..WARMUP_PAIRS {
        if pair % 2 == 0 {
            black_box(timed_full(
                lexical,
                parser,
                left,
                input,
                iterations,
                &mut left_buffers,
            ));
            black_box(timed_full(
                lexical,
                parser,
                right,
                input,
                iterations,
                &mut right_buffers,
            ));
        } else {
            black_box(timed_full(
                lexical,
                parser,
                right,
                input,
                iterations,
                &mut right_buffers,
            ));
            black_box(timed_full(
                lexical,
                parser,
                left,
                input,
                iterations,
                &mut left_buffers,
            ));
        }
    }

    let mut left_ns = Vec::with_capacity(SAMPLE_PAIRS);
    let mut right_ns = Vec::with_capacity(SAMPLE_PAIRS);
    let mut left_checksum = 0_u64;
    let mut right_checksum = 0_u64;
    for pair in 0..SAMPLE_PAIRS {
        if pair % 2 == 0 {
            let started = Instant::now();
            let checksum = timed_full(lexical, parser, left, input, iterations, &mut left_buffers);
            left_checksum = left_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            left_ns.push(started.elapsed().as_nanos());
            let started = Instant::now();
            let checksum = timed_full(
                lexical,
                parser,
                right,
                input,
                iterations,
                &mut right_buffers,
            );
            right_checksum = right_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            right_ns.push(started.elapsed().as_nanos());
        } else {
            let started = Instant::now();
            let checksum = timed_full(
                lexical,
                parser,
                right,
                input,
                iterations,
                &mut right_buffers,
            );
            right_checksum = right_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            right_ns.push(started.elapsed().as_nanos());
            let started = Instant::now();
            let checksum = timed_full(lexical, parser, left, input, iterations, &mut left_buffers);
            left_checksum = left_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            left_ns.push(started.elapsed().as_nanos());
        }
    }
    Samples {
        left_ns,
        right_ns,
        iterations,
        left_checksum,
        right_checksum,
    }
}

fn raw_input(kind: &str, size: usize) -> Vec<u8> {
    match kind {
        "name_class" => {
            let unit = b"alpha_0123456789-";
            (0..size).map(|at| unit[at % unit.len()]).collect()
        }
        "number_class" => {
            let unit = b"1234567890";
            (0..size).map(|at| unit[at % unit.len()]).collect()
        }
        "quoted_safe" => {
            let mut input = Vec::with_capacity(size + 2);
            input.push(b'"');
            input.resize(size + 1, b'a');
            input.push(b'"');
            input
        }
        "quoted_fallback" => {
            let unit = "café-λ-\\n-";
            let mut input = Vec::with_capacity(size + 2);
            input.push(b'"');
            while input.len() + unit.len() < size + 2 {
                input.extend_from_slice(unit.as_bytes());
            }
            input.resize(size + 1, b'x');
            input.push(b'"');
            input
        }
        _ => unreachable!(),
    }
}

fn raw_once(
    kind: &str,
    scanner: Scanner,
    input: &[u8],
    table: &ByteTable,
    stats: &mut ClassifierStats,
) -> usize {
    match kind {
        "name_class" | "number_class" => scan_class(scanner, input, 0, table, stats),
        "quoted_safe" | "quoted_fallback" => {
            scan_quoted(scanner, input, 0, b'"', b'\\', table, stats)
                .expect("raw quote probe must be valid")
        }
        _ => unreachable!(),
    }
}

fn timed_raw(
    kind: &str,
    scanner: Scanner,
    input: &[u8],
    table: &ByteTable,
    iterations: usize,
    aggregate: &mut ClassifierStats,
) -> u64 {
    let mut checksum = 0_u64;
    for _ in 0..iterations {
        let mut stats = ClassifierStats::default();
        checksum = checksum.wrapping_add(black_box(raw_once(
            kind,
            scanner,
            black_box(input),
            black_box(table),
            &mut stats,
        )) as u64);
        aggregate.add(stats);
    }
    checksum
}

fn measure_raw_pair(kind: &str, input: &[u8], table: &ByteTable) -> (Samples, ClassifierStats) {
    let iterations = (RAW_BYTES_PER_SAMPLE / input.len()).clamp(1, 16_384);
    for _ in 0..WARMUP_PAIRS {
        let mut stats = ClassifierStats::default();
        black_box(timed_raw(
            kind,
            Scanner::Oracle,
            input,
            table,
            iterations,
            &mut stats,
        ));
        black_box(timed_raw(
            kind,
            Scanner::Classifier,
            input,
            table,
            iterations,
            &mut stats,
        ));
    }
    let mut left_ns = Vec::with_capacity(SAMPLE_PAIRS);
    let mut right_ns = Vec::with_capacity(SAMPLE_PAIRS);
    let mut left_checksum = 0_u64;
    let mut right_checksum = 0_u64;
    let mut aggregate = ClassifierStats::default();
    for pair in 0..SAMPLE_PAIRS {
        let mut ignored = ClassifierStats::default();
        if pair % 2 == 0 {
            let started = Instant::now();
            let checksum = timed_raw(
                kind,
                Scanner::Oracle,
                input,
                table,
                iterations,
                &mut ignored,
            );
            left_checksum = left_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            left_ns.push(started.elapsed().as_nanos());
            let started = Instant::now();
            let checksum = timed_raw(
                kind,
                Scanner::Classifier,
                input,
                table,
                iterations,
                &mut aggregate,
            );
            right_checksum = right_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            right_ns.push(started.elapsed().as_nanos());
        } else {
            let started = Instant::now();
            let checksum = timed_raw(
                kind,
                Scanner::Classifier,
                input,
                table,
                iterations,
                &mut aggregate,
            );
            right_checksum = right_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            right_ns.push(started.elapsed().as_nanos());
            let started = Instant::now();
            let checksum = timed_raw(
                kind,
                Scanner::Oracle,
                input,
                table,
                iterations,
                &mut ignored,
            );
            left_checksum = left_checksum.wrapping_add(checksum.rotate_left((pair % 63) as u32));
            left_ns.push(started.elapsed().as_nanos());
        }
    }
    (
        Samples {
            left_ns,
            right_ns,
            iterations,
            left_checksum,
            right_checksum,
        },
        aggregate,
    )
}

#[derive(Clone, Copy, Debug)]
struct Interval {
    estimate_pct: f64,
    low_pct: f64,
    high_pct: f64,
}

fn median(values: &mut [f64]) -> f64 {
    values.sort_by(|left, right| left.partial_cmp(right).unwrap_or(Ordering::Equal));
    let middle = values.len() / 2;
    if values.len().is_multiple_of(2) {
        (values[middle - 1] + values[middle]) / 2.0
    } else {
        values[middle]
    }
}

fn log_ratios(samples: &Samples) -> Vec<f64> {
    samples
        .left_ns
        .iter()
        .zip(&samples.right_ns)
        .map(|(control, candidate)| (*control as f64 / *candidate as f64).ln())
        .collect()
}

fn pct_from_log(log_ratio: f64) -> f64 {
    (1.0 - (-log_ratio).exp()) * 100.0
}

fn bootstrap_interval(samples: &Samples, family_rows: usize, mut seed: u64) -> Interval {
    let ratios = log_ratios(samples);
    let mut point_values = ratios.clone();
    let point = median(&mut point_values);
    let mut bootstrapped = Vec::with_capacity(BOOTSTRAP_RESAMPLES);
    let mut resample = vec![0.0; ratios.len()];
    for _ in 0..BOOTSTRAP_RESAMPLES {
        for item in &mut resample {
            seed = xorshift(seed);
            *item = ratios[seed as usize % ratios.len()];
        }
        bootstrapped.push(median(&mut resample));
    }
    bootstrapped.sort_by(|left, right| left.partial_cmp(right).unwrap_or(Ordering::Equal));
    let tail = 0.05 / family_rows as f64 / 2.0;
    let low_index =
        ((BOOTSTRAP_RESAMPLES as f64 * tail).floor() as usize).min(BOOTSTRAP_RESAMPLES - 1);
    let high_index = ((BOOTSTRAP_RESAMPLES as f64 * (1.0 - tail)).ceil() as usize)
        .saturating_sub(1)
        .min(BOOTSTRAP_RESAMPLES - 1);
    Interval {
        estimate_pct: pct_from_log(point),
        low_pct: pct_from_log(bootstrapped[low_index]),
        high_pct: pct_from_log(bootstrapped[high_index]),
    }
}

fn xorshift(mut value: u64) -> u64 {
    value ^= value << 13;
    value ^= value >> 7;
    value ^= value << 17;
    value
}

#[derive(Clone)]
struct FullRow {
    fixture: Fixture,
    aa: Samples,
    comparison: Samples,
    aa_interval: Interval,
    comparison_interval: Interval,
    fresh_oracle: AllocationCount,
    fresh_classifier: AllocationCount,
    reused_oracle: AllocationCount,
    reused_classifier: AllocationCount,
    stats: ClassifierStats,
}

#[derive(Clone)]
struct RawRow {
    kind: &'static str,
    input_bytes: usize,
    input_sha256: String,
    samples: Samples,
    interval: Interval,
    stats: ClassifierStats,
}

fn allocation_probe(
    lexical: &LexicalGrammar,
    parser: &ParserGrammar,
    scanner: Scanner,
    input: &[u8],
    reuse: bool,
) -> AllocationCount {
    let mut buffers = Buffers::default();
    let mut stats = ClassifierStats::default();
    if reuse {
        assert!(parse_document(lexical, parser, scanner, input, &mut buffers, &mut stats).accepted);
    }
    reset_allocations();
    let outcome = parse_document(lexical, parser, scanner, input, &mut buffers, &mut stats);
    black_box(outcome);
    let mut count = allocation_count();
    count.retained_capacity_bytes = (buffers.tokens.capacity() * size_of::<Token>()
        + buffers.events.capacity() * size_of::<Event>())
        as u64;
    count
}

fn command_output(program: &str, args: &[&str]) -> String {
    Command::new(program)
        .args(args)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .unwrap_or_else(|| "unavailable".to_owned())
}

fn environment(name: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| "unset".to_owned())
}

fn json_string(output: &mut String, value: &str) {
    output.push('"');
    for character in value.chars() {
        match character {
            '"' => output.push_str("\\\""),
            '\\' => output.push_str("\\\\"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            character if character.is_control() => {
                write!(output, "\\u{:04x}", character as u32).unwrap();
            }
            character => output.push(character),
        }
    }
    output.push('"');
}

fn samples_json(output: &mut String, samples: &Samples) {
    write!(
        output,
        "{{\"iterations_per_sample\":{},\"left_checksum\":{},\"right_checksum\":{},\"left_ns\":[",
        samples.iterations, samples.left_checksum, samples.right_checksum
    )
    .unwrap();
    for (index, sample) in samples.left_ns.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        write!(output, "{sample}").unwrap();
    }
    output.push_str("],\"right_ns\":[");
    for (index, sample) in samples.right_ns.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        write!(output, "{sample}").unwrap();
    }
    output.push_str("]}");
}

fn interval_json(output: &mut String, interval: Interval) {
    write!(
        output,
        "{{\"estimate_pct\":{:.6},\"simultaneous_95_low_pct\":{:.6},\"simultaneous_95_high_pct\":{:.6}}}",
        interval.estimate_pct, interval.low_pct, interval.high_pct
    )
    .unwrap();
}

fn allocations_json(output: &mut String, count: AllocationCount) {
    write!(
        output,
        "{{\"allocation_calls\":{},\"reallocation_calls\":{},\"requested_bytes\":{},\
         \"retained_capacity_bytes\":{}}}",
        count.calls, count.reallocs, count.requested_bytes, count.retained_capacity_bytes
    )
    .unwrap();
}

fn sample_summary_json(output: &mut String, samples: &Samples, input_bytes: usize) {
    let mut left: Vec<f64> = samples.left_ns.iter().map(|value| *value as f64).collect();
    let mut right: Vec<f64> = samples.right_ns.iter().map(|value| *value as f64).collect();
    let left_ns = median(&mut left);
    let right_ns = median(&mut right);
    let iterations = samples.iterations as f64;
    let bytes = input_bytes as f64 * iterations;
    write!(
        output,
        "{{\"left_median_ns_per_document\":{:.3},\"right_median_ns_per_document\":{:.3},\
         \"left_median_gib_per_second\":{:.6},\"right_median_gib_per_second\":{:.6}}}",
        left_ns / iterations,
        right_ns / iterations,
        bytes / left_ns / (1024.0 * 1024.0 * 1024.0) * 1_000_000_000.0,
        bytes / right_ns / (1024.0 * 1024.0 * 1024.0) * 1_000_000_000.0,
    )
    .unwrap();
}

fn stats_json(output: &mut String, stats: ClassifierStats) {
    write!(
        output,
        "{{\"eligible_regions\":{},\"eligible_bytes\":{},\"block_bytes\":{},\
         \"fallback_tail_bytes\":{},\"fallback_utf8_bytes\":{},\
         \"fallback_escape_bytes\":{},\"rejecting_blocks\":{}}}",
        stats.eligible_regions,
        stats.eligible_bytes,
        stats.block_bytes,
        stats.fallback_tail_bytes,
        stats.fallback_utf8_bytes,
        stats.fallback_escape_bytes,
        stats.rejecting_blocks
    )
    .unwrap();
}

fn sha256_hex(input: &[u8]) -> String {
    const INITIAL: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];
    let bit_len = (input.len() as u64).wrapping_mul(8);
    let mut padded = input.to_vec();
    padded.push(0x80);
    while padded.len() % 64 != 56 {
        padded.push(0);
    }
    padded.extend_from_slice(&bit_len.to_be_bytes());
    let mut state = INITIAL;
    for block in padded.chunks_exact(64) {
        let mut words = [0_u32; 64];
        for (index, bytes) in block.chunks_exact(4).enumerate() {
            words[index] = u32::from_be_bytes(bytes.try_into().unwrap());
        }
        for index in 16..64 {
            let s0 = words[index - 15].rotate_right(7)
                ^ words[index - 15].rotate_right(18)
                ^ (words[index - 15] >> 3);
            let s1 = words[index - 2].rotate_right(17)
                ^ words[index - 2].rotate_right(19)
                ^ (words[index - 2] >> 10);
            words[index] = words[index - 16]
                .wrapping_add(s0)
                .wrapping_add(words[index - 7])
                .wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h] = state;
        for index in 0..64 {
            let sum1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let choice = (e & f) ^ ((!e) & g);
            let temp1 = h
                .wrapping_add(sum1)
                .wrapping_add(choice)
                .wrapping_add(K[index])
                .wrapping_add(words[index]);
            let sum0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let majority = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = sum0.wrapping_add(majority);
            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }
        for (slot, value) in state.iter_mut().zip([a, b, c, d, e, f, g, h]) {
            *slot = slot.wrapping_add(value);
        }
    }
    let mut output = String::with_capacity(64);
    for word in state {
        write!(output, "{word:08x}").unwrap();
    }
    output
}

fn build_json(
    correctness: CorrectnessReport,
    full_rows: &[FullRow],
    raw_rows: &[RawRow],
    disposition: &str,
    reasons: &[String],
) -> String {
    let executable = std::env::current_exe().expect("current executable path");
    let executable_bytes = std::fs::read(&executable).expect("read current executable");
    let executable_sha256 = sha256_hex(&executable_bytes);
    let clean_build_ns = environment("SKV26_CLEAN_BUILD_NS")
        .parse::<u128>()
        .unwrap_or_default();
    let mut output = String::new();
    output.push_str("{\"schema\":\"sk-v26.w2.scalar-profile.v1\",\"plan_status\":\"sealed_before_measurement\",");
    output.push_str("\"subject\":{");
    for (index, (key, value)) in [
        ("commit", environment("SKV26_SUBJECT_COMMIT")),
        ("dirty_digest", environment("SKV26_DIRTY_DIGEST")),
        ("plan_sha256", environment("SKV26_PLAN_SHA256")),
        ("source_sha256", environment("SKV26_SOURCE_SHA256")),
    ]
    .into_iter()
    .enumerate()
    {
        if index != 0 {
            output.push(',');
        }
        write!(output, "\"{key}\":").unwrap();
        json_string(&mut output, &value);
    }
    write!(
        output,
        "}},\"control\":{{\"identity\":\"same_source_byte_or_utf8_scalar_oracle\",\
         \"commit\":\"{}\",\"semantic_difference\":\"scanner only\"}},\
         \"footprint\":{{\"executable_bytes\":{},\"executable_sha256\":\"{}\",\
         \"main_source_bytes\":{},\"main_source_lines\":{},\"generated_code_bytes\":0,\
         \"clean_release_build_ns\":{}}},\"environment\":{{",
        environment("SKV26_SUBJECT_COMMIT"),
        executable_bytes.len(),
        executable_sha256,
        include_bytes!("main.rs").len(),
        include_str!("main.rs").lines().count(),
        clean_build_ns
    )
    .unwrap();
    let environment_rows = [
        ("host_model", command_output("sysctl", &["-n", "hw.model"])),
        (
            "host_cpu",
            command_output("sysctl", &["-n", "machdep.cpu.brand_string"]),
        ),
        ("os", command_output("sw_vers", &[])),
        ("kernel", command_output("uname", &["-a"])),
        ("rustc", command_output("rustc", &["-Vv"])),
        ("cargo", command_output("cargo", &["-V"])),
        ("rustflags", environment("RUSTFLAGS")),
        ("power_mode", environment("SKV26_POWER_MODE")),
        ("load", command_output("sysctl", &["-n", "vm.loadavg"])),
        (
            "hardware_counters",
            "unavailable_without_privileged_tooling".to_owned(),
        ),
    ];
    for (index, (key, value)) in environment_rows.into_iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        write!(output, "\"{key}\":").unwrap();
        json_string(&mut output, &value);
    }
    write!(
        output,
        "}},\"measurement\":{{\"warmup_pairs\":{WARMUP_PAIRS},\"sample_pairs\":{SAMPLE_PAIRS},\
         \"bootstrap_resamples\":{BOOTSTRAP_RESAMPLES},\"paired_estimator\":\"median_ln_control_over_candidate\",\
         \"multiplicity\":\"bonferroni_per_comparison_family\",\
         \"semantic_plane\":\"P2 token/event\",\"cache\":\"hot\",\"input_mutation\":false,\
         \"allocator_reuse\":\"timed rows reuse token/event buffers\",\
         \"input_copying\":\"fixtures generated once before timing; timed parse does not copy input\",\
         \"input_padding\":\"exact target sizes use grammar trivia; no out-of-bounds padding\",\
         \"retained_output\":\"ordered token kind, exact source span, source-byte checksum contribution, EOF\",\
         \"utf8_policy\":\"whole input valid UTF-8; non-ASCII preserved raw and classifier falls back by scalar\",\
         \"string_policy\":\"raw source bytes and escapes retained; not decoded\",\
         \"number_policy\":\"validated decimal lexical spelling retained exactly\",\
         \"error_policy\":\"first typed lexical or syntax error; no recovery\"}},\
         \"correctness\":{{\"passed\":{},\"checks\":{},\"fuzz_documents\":{},\
         \"oracle\":\"byte_or_utf8_scalar\",\"candidate\":\"grammar_derived_eight_byte_scalar_classifier\"}},",
        correctness.passed, correctness.checks, correctness.fuzz_documents
    )
    .unwrap();
    output.push_str("\"full_parser_rows\":[");
    for (index, row) in full_rows.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        output.push('{');
        output.push_str("\"family\":");
        json_string(&mut output, row.fixture.family);
        write!(
            output,
            ",\"target_bytes\":{},\"actual_bytes\":{},\"input_sha256\":\"{}\",\"aa_samples\":",
            row.fixture.target_size,
            row.fixture.bytes.len(),
            row.fixture.sha256
        )
        .unwrap();
        samples_json(&mut output, &row.aa);
        output.push_str(",\"aa_sample_summary\":");
        sample_summary_json(&mut output, &row.aa, row.fixture.bytes.len());
        output.push_str(",\"aa_interval\":");
        interval_json(&mut output, row.aa_interval);
        output.push_str(",\"oracle_vs_classifier_samples\":");
        samples_json(&mut output, &row.comparison);
        output.push_str(",\"oracle_vs_classifier_sample_summary\":");
        sample_summary_json(&mut output, &row.comparison, row.fixture.bytes.len());
        output.push_str(",\"oracle_vs_classifier_interval\":");
        interval_json(&mut output, row.comparison_interval);
        let noise = row
            .aa_interval
            .low_pct
            .abs()
            .max(row.aa_interval.high_pct.abs());
        write!(
            output,
            ",\"calibrated_noise_floor_pct\":{noise:.6},\"practical_floor_pct\":{:.6},\
             \"regression_guard_pct\":{:.6},\"allocations\":{{\"fresh_oracle\":",
            10_f64.max(3.0 * noise),
            3_f64.max(noise)
        )
        .unwrap();
        allocations_json(&mut output, row.fresh_oracle);
        output.push_str(",\"fresh_classifier\":");
        allocations_json(&mut output, row.fresh_classifier);
        output.push_str(",\"reused_oracle\":");
        allocations_json(&mut output, row.reused_oracle);
        output.push_str(",\"reused_classifier\":");
        allocations_json(&mut output, row.reused_classifier);
        output.push_str("},\"classifier_eligibility\":");
        stats_json(&mut output, row.stats);
        output.push('}');
    }
    output.push_str("],\"raw_classifier_rows\":[");
    for (index, row) in raw_rows.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        output.push_str("{\"kind\":");
        json_string(&mut output, row.kind);
        write!(
            output,
            ",\"input_bytes\":{},\"input_sha256\":\"{}\",\"samples\":",
            row.input_bytes, row.input_sha256
        )
        .unwrap();
        samples_json(&mut output, &row.samples);
        output.push_str(",\"sample_summary\":");
        sample_summary_json(&mut output, &row.samples, row.input_bytes);
        output.push_str(",\"interval\":");
        interval_json(&mut output, row.interval);
        output.push_str(",\"classifier_eligibility\":");
        stats_json(&mut output, row.stats);
        output.push('}');
    }
    output.push_str("],\"disposition\":{\"decision\":");
    json_string(&mut output, disposition);
    output.push_str(",\"reasons\":[");
    for (index, reason) in reasons.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        json_string(&mut output, reason);
    }
    output.push_str("],\"promotion_claim\":false,\"limitation\":\"isolated scalar prototype; no NEON/ASM, cold-cache, hardware-counter, production-emitter, or external-competitor claim\"}}");
    output
}

fn disposition(full_rows: &[FullRow], raw_rows: &[RawRow]) -> (&'static str, Vec<String>) {
    let mut reasons = Vec::new();
    let mut accepted_families = std::collections::BTreeSet::new();
    let mut regression = false;
    for row in full_rows {
        let noise = row
            .aa_interval
            .low_pct
            .abs()
            .max(row.aa_interval.high_pct.abs());
        let practical_floor = 10_f64.max(3.0 * noise);
        let regression_guard = 3_f64.max(noise);
        if row.comparison_interval.high_pct < -regression_guard {
            regression = true;
            reasons.push(format!(
                "{} {} B full-parser upper bound {:.2}% violates -{:.2}% guard",
                row.fixture.family,
                row.fixture.bytes.len(),
                row.comparison_interval.high_pct,
                regression_guard
            ));
        }
        if row.fixture.bytes.len() >= 65_536 && row.comparison_interval.low_pct > practical_floor {
            accepted_families.insert(row.fixture.family);
        }
    }
    let raw_useful = raw_rows
        .iter()
        .any(|row| row.interval.low_pct > 10.0 && row.input_bytes >= 65_536);
    let reuse_allocations_are_zero = full_rows
        .iter()
        .all(|row| row.reused_classifier.calls == 0 && row.reused_classifier.reallocs == 0);
    if regression {
        return ("REJECT", reasons);
    }
    if accepted_families.len() >= 2 && reuse_allocations_are_zero {
        reasons.push(format!(
            "full-parser simultaneous lower bound clears its practical floor in {} representative families",
            accepted_families.len()
        ));
        reasons.push("no full-parser row violates the calibrated regression guard".to_owned());
        reasons.push("reserved classifier parsing performs zero allocations".to_owned());
        return ("ACCEPT", reasons);
    }
    if raw_useful {
        reasons.push(
            "at least one raw lexical region is useful, but the full-parser ACCEPT gate is unmet"
                .to_owned(),
        );
    } else {
        reasons.push(
            "correctness passed, but no raw result alone can authorize production promotion"
                .to_owned(),
        );
    }
    if !reuse_allocations_are_zero {
        reasons.push("reserved classifier parsing still allocates".to_owned());
    }
    ("BANK", reasons)
}

fn main() {
    let lexical = LexicalGrammar::compile();
    let parser = ParserGrammar::compile();
    let fixtures = fixtures();
    eprintln!("running correctness gates");
    let correctness = correctness_gates(&lexical, &parser, &fixtures)
        .unwrap_or_else(|error| panic!("correctness gate failed: {error}"));

    let mut full_rows = Vec::new();
    for (index, fixture) in fixtures.into_iter().enumerate() {
        eprintln!(
            "full parser {}/16: {} {} B",
            index + 1,
            fixture.family,
            fixture.bytes.len()
        );
        let aa = measure_full_pair(
            &lexical,
            &parser,
            &fixture.bytes,
            Scanner::Oracle,
            Scanner::Oracle,
        );
        let comparison = measure_full_pair(
            &lexical,
            &parser,
            &fixture.bytes,
            Scanner::Oracle,
            Scanner::Classifier,
        );
        let mut stats = ClassifierStats::default();
        let mut buffers = Buffers::default();
        assert!(
            parse_document(
                &lexical,
                &parser,
                Scanner::Classifier,
                &fixture.bytes,
                &mut buffers,
                &mut stats
            )
            .accepted
        );
        full_rows.push(FullRow {
            aa_interval: bootstrap_interval(&aa, 16, 0x243f_6a88_85a3_08d3 ^ index as u64),
            comparison_interval: bootstrap_interval(
                &comparison,
                16,
                0x1319_8a2e_0370_7344 ^ index as u64,
            ),
            fresh_oracle: allocation_probe(
                &lexical,
                &parser,
                Scanner::Oracle,
                &fixture.bytes,
                false,
            ),
            fresh_classifier: allocation_probe(
                &lexical,
                &parser,
                Scanner::Classifier,
                &fixture.bytes,
                false,
            ),
            reused_oracle: allocation_probe(
                &lexical,
                &parser,
                Scanner::Oracle,
                &fixture.bytes,
                true,
            ),
            reused_classifier: allocation_probe(
                &lexical,
                &parser,
                Scanner::Classifier,
                &fixture.bytes,
                true,
            ),
            fixture,
            aa,
            comparison,
            stats,
        });
    }

    let mut raw_rows = Vec::new();
    let mut raw_index = 0_u64;
    for kind in [
        "name_class",
        "number_class",
        "quoted_safe",
        "quoted_fallback",
    ] {
        for size in [64, 1024, 65_536, 1_048_576] {
            eprintln!("raw classifier: {kind} {size} B");
            let input = raw_input(kind, size);
            let table = match kind {
                "name_class" => ByteTable::new(|byte| {
                    byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-') || byte >= 0x80
                }),
                "number_class" => ByteTable::new(|byte| byte.is_ascii_digit()),
                "quoted_safe" | "quoted_fallback" => {
                    ByteTable::new(|byte| byte >= 0x20 && !matches!(byte, b'"' | b'\\'))
                }
                _ => unreachable!(),
            };
            let input_sha256 = sha256_hex(&input);
            let (samples, stats) = measure_raw_pair(kind, &input, &table);
            let interval = bootstrap_interval(&samples, 16, 0xa409_3822_299f_31d0 ^ raw_index);
            raw_rows.push(RawRow {
                kind,
                input_bytes: input.len(),
                input_sha256,
                samples,
                interval,
                stats,
            });
            raw_index += 1;
        }
    }

    let (decision, reasons) = disposition(&full_rows, &raw_rows);
    let json = build_json(correctness, &full_rows, &raw_rows, decision, &reasons);
    let output_path = environment("SKV26_OUTPUT").replace("unset", "profile.raw.json");
    std::fs::write(&output_path, json).expect("write profile result");
    eprintln!("decision={decision}; output={output_path}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sha256_matches_published_abc_vector() {
        assert_eq!(
            sha256_hex(b"abc"),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
    }

    #[test]
    fn classifier_matches_oracle_on_fixture_matrix() {
        let lexical = LexicalGrammar::compile();
        let parser = ParserGrammar::compile();
        for fixture in fixtures() {
            equality(&lexical, &parser, &fixture.bytes).unwrap();
        }
    }

    #[test]
    fn invalid_utf8_is_the_same_typed_rejection() {
        let lexical = LexicalGrammar::compile();
        let parser = ParserGrammar::compile();
        equality(&lexical, &parser, &[b'a', b'=', b'"', 0xff, b'"', b';']).unwrap();
        let mut buffers = Buffers::default();
        let outcome = parse_document(
            &lexical,
            &parser,
            Scanner::Classifier,
            &[0xff],
            &mut buffers,
            &mut ClassifierStats::default(),
        );
        assert_eq!(outcome.error, Some((ErrorKind::Utf8, 0)));
    }
}
