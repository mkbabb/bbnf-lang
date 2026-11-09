//! `impl Serializer for pprint::FmtBuilder` (feature `pprint`): a traversal
//! written against the `Serializer` trait renders through pprint, flat when
//! it fits and broken at its line breaks when it does not.
#![cfg(feature = "pprint")]

use bbnf_ser::Serializer;
use pprint::{FmtBuilder, Printer, render};

/// A grammar-shaped traversal that knows only the trait.
fn list<'a, S: Serializer<'a>>(s: &mut S, items: &[&'a str]) {
    s.group_open();
    s.char(b'[');
    s.indent_open();
    s.break_line();
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            s.comma_sep();
        }
        s.text(item);
    }
    s.indent_close();
    s.break_line();
    s.char(b']');
    s.group_close();
}

fn pretty(items: &[&str], width: usize) -> String {
    let mut b = FmtBuilder::new();
    list(&mut b, items);
    render(&b.finish(), Printer::new(width, 2, false))
}

#[test]
fn fits_flat() {
    assert_eq!(pretty(&["1", "2", "3"], 80), "[1, 2, 3]");
}

#[test]
fn breaks_when_it_does_not_fit() {
    let out = pretty(&["alpha", "beta", "gamma"], 8);
    assert!(out.contains('\n'), "a group wider than the page breaks: {out:?}");
    assert!(out.starts_with('[') && out.ends_with(']'), "{out:?}");
}
