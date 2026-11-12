//! sk-v27 LW3.PP-2, design gate G3 for the six gorgeous grammars (json,
//! css, bnf, ebnf, bbnf, google-sheets): on every file of the
//! comment-bearing corpus `tests/corpus/g3/<grammar>/`, formatting
//!
//! 1. succeeds,
//! 2. keeps the token-and-comment stream: the sequence of tokens and
//!    comments of the output equals that of the input (formatting changes
//!    whitespace only, and never drops, moves past a token, or rewrites a
//!    comment),
//! 3. is idempotent: `fmt ∘ fmt = fmt`.
//!
//! The stream is read by one lexical tokenizer (below), shared by the six
//! grammars and parametrized only by each language's comment and string
//! delimiters. It is a test oracle over characters, independent of the
//! generated parsers: it parses no language, it only splits a file into
//! comments, string literals, words and single punctuation characters, so a
//! formatter that drops a comment, splits or joins a token, or edits the
//! inside of a string or comment changes the stream. Where a grammar of
//! record has no comment syntax (json, bnf, ebnf, google-sheets), its
//! corpus has no comments and the stream is the token stream.

use gorgeous::PrinterConfig;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Comment(String),
    Str(String),
    Word(String),
    Punct(char),
}

struct Lex {
    line_comment: Option<&'static str>,
    block_comment: Option<(&'static str, &'static str)>,
    quotes: &'static [char],
    /// BBNF's `/regex/` literal is one token (its spaces are significant).
    slash_regex: bool,
}

fn is_word(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || !c.is_ascii()
}

fn tokens(lex: &Lex, s: &str) -> Vec<Tok> {
    let b: Vec<char> = s.chars().collect();
    let starts = |i: usize, p: &str| {
        let p: Vec<char> = p.chars().collect();
        b.len() >= i + p.len() && b[i..i + p.len()] == p[..]
    };
    let mut out = Vec::new();
    let mut i = 0;
    while i < b.len() {
        let c = b[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if let Some(lc) = lex.line_comment
            && starts(i, lc)
        {
            let j = (i..b.len()).find(|&j| b[j] == '\n').unwrap_or(b.len());
            out.push(Tok::Comment(b[i..j].iter().collect::<String>().trim_end().to_string()));
            i = j;
            continue;
        }
        if let Some((open, close)) = lex.block_comment
            && starts(i, open)
        {
            let mut j = i + open.chars().count();
            while j < b.len() && !starts(j, close) {
                j += 1;
            }
            let end = (j + close.chars().count()).min(b.len());
            out.push(Tok::Comment(b[i..end].iter().collect()));
            i = end;
            continue;
        }
        if lex.quotes.contains(&c) {
            let mut j = i + 1;
            while j < b.len() && b[j] != c {
                if b[j] == '\\' {
                    j += 1;
                }
                j += 1;
            }
            let end = (j + 1).min(b.len());
            out.push(Tok::Str(b[i..end].iter().collect()));
            i = end;
            continue;
        }
        if lex.slash_regex && c == '/' {
            let mut j = i + 1;
            let mut class = false;
            while j < b.len() && (class || b[j] != '/') {
                match b[j] {
                    '\\' => j += 1,
                    '[' => class = true,
                    ']' => class = false,
                    _ => {}
                }
                j += 1;
            }
            let end = (j + 1).min(b.len());
            out.push(Tok::Str(b[i..end].iter().collect()));
            i = end;
            continue;
        }
        if is_word(c) {
            let j = (i..b.len()).find(|&j| !is_word(b[j])).unwrap_or(b.len());
            out.push(Tok::Word(b[i..j].iter().collect()));
            i = j;
            continue;
        }
        out.push(Tok::Punct(c));
        i += 1;
    }
    out
}

type Fmt = fn(&str, &PrinterConfig) -> Option<String>;

fn grammars() -> Vec<(&'static str, Fmt, Lex)> {
    vec![
        (
            "json",
            gorgeous::json::prettify_json as Fmt,
            Lex {
                line_comment: None,
                block_comment: None,
                quotes: &['"'],
                slash_regex: false,
            },
        ),
        (
            "css",
            gorgeous::css::prettify_css,
            Lex {
                line_comment: None,
                block_comment: Some(("/*", "*/")),
                quotes: &['"', '\''],
                slash_regex: false,
            },
        ),
        (
            "bnf",
            gorgeous::bnf::prettify_bnf,
            Lex {
                line_comment: None,
                block_comment: None,
                quotes: &['"'],
                slash_regex: false,
            },
        ),
        (
            "ebnf",
            gorgeous::ebnf::prettify_ebnf,
            Lex {
                line_comment: None,
                block_comment: None,
                quotes: &['"', '\''],
                slash_regex: false,
            },
        ),
        (
            "bbnf",
            gorgeous::bbnf::prettify_bbnf,
            Lex {
                line_comment: Some("//"),
                block_comment: Some(("/*", "*/")),
                quotes: &['"', '\'', '`'],
                slash_regex: true,
            },
        ),
        (
            "google-sheets",
            gorgeous::google_sheets::prettify_formula,
            Lex {
                line_comment: None,
                block_comment: None,
                quotes: &['"', '\''],
                slash_regex: false,
            },
        ),
    ]
}

fn corpus(grammar: &str) -> Vec<PathBuf> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/corpus/g3").join(grammar);
    let mut v: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap_or_else(|_| panic!("{} is missing", dir.display()))
        .map(|e| e.unwrap().path())
        .collect();
    v.sort();
    assert!(!v.is_empty(), "{grammar}: empty corpus");
    v
}

/// G3 on every corpus file of every grammar; every failure is listed.
#[test]
fn g3_token_and_comment_stream_is_kept_and_formatting_is_idempotent() {
    let config = PrinterConfig::default();
    let mut failures = Vec::new();
    let (mut files, mut comments) = (0usize, 0usize);
    for (name, fmt, lex) in grammars() {
        for path in corpus(name) {
            let file = path.file_name().unwrap().to_string_lossy().to_string();
            let input = std::fs::read_to_string(&path).unwrap();
            files += 1;
            let want = tokens(&lex, &input);
            comments += want.iter().filter(|t| matches!(t, Tok::Comment(_))).count();
            let Some(out) = fmt(&input, &config) else {
                failures.push(format!("{name}/{file}: does not format"));
                continue;
            };
            let got = tokens(&lex, &out);
            if got != want {
                let lost: Vec<&Tok> = want.iter().filter(|t| !got.contains(t)).collect();
                failures.push(format!(
                    "{name}/{file}: the token-and-comment stream differs (lost: {lost:?})\n--- output\n{out}"
                ));
                continue;
            }
            match fmt(&out, &config) {
                Some(again) if again == out => {}
                Some(again) => failures.push(format!(
                    "{name}/{file}: fmt . fmt != fmt\n--- once\n{out}\n--- twice\n{again}"
                )),
                None => failures.push(format!("{name}/{file}: its own output does not format")),
            }
        }
    }
    eprintln!("G3: {files} files, {comments} comments in the corpus, {} failures", failures.len());
    assert!(failures.is_empty(), "G3:\n{}", failures.join("\n\n"));
}

/// The corpus is comment-bearing wherever the grammar of record has
/// comments: css and bbnf each hold comments in several positions.
#[test]
fn g3_corpus_bears_comments_where_the_grammar_has_them() {
    for (name, _, lex) in grammars() {
        let n: usize = corpus(name)
            .iter()
            .map(|p| {
                tokens(&lex, &std::fs::read_to_string(p).unwrap())
                    .iter()
                    .filter(|t| matches!(t, Tok::Comment(_)))
                    .count()
            })
            .sum();
        let has_syntax = lex.line_comment.is_some() || lex.block_comment.is_some();
        if has_syntax {
            assert!(n >= 5, "{name}: {n} comments in the corpus");
        }
    }
}
