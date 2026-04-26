# B2.W3 — Post-process inventory + absorption decisions

W3 retires `scripts/bootstrap-bbnf.sh` (350 lines: 47 lines bash + 303 lines
inlined Python regex post-processor) and `scripts/check-bootstrap-clean.sh`
(43 lines). The pipeline that bash + Python implemented — `cargo expand`
on `bbnf-bootstrap` followed by ten regex/text transformations on the
expanded output — exists only because the proc-macro contract forced
`generated.rs` to be reconstructed from rustc's expand-phase output. With
the proc-macro retired (W2) and `cargo xtask regen` running the IR
pipeline natively (W0) and emitting through `prettyplease` (`xtask/src/
regen.rs:268`), the expand-phase shape never appears; every transformation
on it is moot.

This audit catalogues each transformation in the deleted pipeline, names
its motivating defect in the pre-B2 substrate, and records the absorption
verdict.

## Pipeline shape — pre-B2 vs post-B2

Pre-B2:

```
cargo expand -p bbnf-bootstrap --lib   # rustc expand of #[derive(Parser)]
  → /tmp/raw-expand.rs                 # proc-macro emission + rustc preamble
  → python3 (regex transforms)         # strip rustc preamble + post-process
  → crates/core/src/grammar/generated.rs
```

Post-B2 (W0+W2):

```
cargo xtask regen                       # bbnf::pipeline::compile_paths_request
  → bbnf::generate::generate_all        # produces TokenStream directly
  → syn::parse2 + prettyplease::unparse # formats clean Rust
  → crates/core/src/grammar/generated/<ident>.rs
```

The xtask never touches rustc expand. `generate_all` returns a clean
`TokenStream` carrying the proc-macro's *intent* without any of expand's
incidental output (`#![feature(...)]`, `#[automatically_derived]`,
`::core::panicking::panic_fmt`, the auto-derived `impl Clone/Copy/Debug`
boilerplate). `prettyplease::unparse` formats the result.

## Transformation-by-transformation verdict

| # | Transformation | Pre-B2 motivation | Post-B2 fate | Verdict |
|---|---|---|---|---|
| 1 | `re.sub(r'#!\[[^]]*?\]\n', '', text, ...)` — strip crate-level inner attributes (`#![feature(...)]`, `#![allow(...)]`) | `cargo expand` of `bbnf-bootstrap` emits whatever inner attributes the crate's `lib.rs` carries plus rustc-injected `#![feature(...)]` lines under nightly. Checked-in code can't use unstable features. | xtask emits `#![allow(...)]` ONCE in `file_header()` (`xtask/src/regen.rs:300`); the IR pipeline's inner `TokenStream` carries no `#![...]` items. No nightly features in the emission contract. | **Discard** — defect originates in expand-phase noise that no longer exists. |
| 2 | `re.sub(r'#\[prelude_import\]\nuse std::prelude::rust_2024::\*;\n', '', text)` — strip rustc auto-injected prelude | Cargo expand re-emits the rustc-injected prelude import that resolves implicit `std` paths. Spurious in checked-in code. | IR pipeline emits explicit `use ::bbnf::runtime::*` etc. via `file_header`; no rustc prelude reconstruction. | **Discard**. |
| 3 | `re.sub(r'extern crate std;\n', '', text)` — strip implicit extern-crate | Same class as (2): rustc auto-emits this in expand. | IR pipeline emits no `extern crate` directives. | **Discard**. |
| 4 | `re.sub(r'use bbnf_derive::Parser;\n', '', text)` — strip the derive import | `bbnf-bootstrap`'s `lib.rs` had `use bbnf_derive::Parser;` to bring the proc-macro into scope; once expanded, the import is dead but expand re-emits it. | `bbnf_derive` deleted at W2; no consumer imports the proc-macro. | **Discard** — root cause deleted. |
| 5 | `re.sub(r'#\[parser\(.*?\)\]\n', '', text)` — strip the proc-macro attribute | `#[parser(path="...")]` is a proc-macro input; expand re-emits it as a no-op attribute on the marker struct. | xtask emits `pub struct BbnfBootstrap;` directly (`xtask/src/regen.rs:243`); no `#[parser(...)]` attribute appears. | **Discard**. |
| 6 | `re.sub(r'pub struct BbnfBootstrap;\n', '', text)` followed by header re-emission | Hand-typed marker struct in `lib.rs` survived expand; the script stripped it so the header could re-emit it under the canonical doc-comment + allow-block. | xtask emits the marker struct inside the per-grammar body (`xtask/src/regen.rs:243`); the file header is emitted separately (`file_header()`). The two surfaces are already cleanly separated. | **Discard**. |
| 7 | `strip_auto_derived_impls(text)` (39-line helper, lines 75-141) — find every `#[automatically_derived] impl ... { ... }` block and delete it via brace-matching, walking back over preceding attribute runs | rustc expand turns `#[derive(Clone, Copy, Debug)]` into bracketed `impl Clone for X { ... }` blocks marked `#[automatically_derived]`. These use the unstable `derive_clone_copy`, `coerce_pointee`, and `fmt_helpers_for_derive` internals — uncompilable on stable. Stripping them and re-emitting the stable `#[derive(...)]` attribute (transforms 8, 9, 10) fixed the compile. | xtask never expands derives — `bbnf::generate::generate_all` emits `#[derive(Clone, Copy, Debug)]` attributes directly above each struct; rustc handles the derive at the consumer's compile, never inside the xtask. The auto-derived impl shape never appears in the emission. | **Discard** — defect class structurally absent. |
| 8 | `readd_view_derives(text)` — re-add `#[derive(Clone, Copy, Debug)]` above `pub struct <Name>(View\|NodeView)<'tape\|'p>` | Companion to (7): once auto-derived impls were stripped, the struct definitions had no derive attribute, so `readd_view_derives` re-emitted the attribute by regex matching the struct header and splicing the attribute above. | The IR codegen already emits `#[derive(Clone, Copy, Debug)]` on every view struct (per `bbnf::generate::view::*`); no re-add step needed. | **Discard** — companion to (7), same root cause. |
| 9 | `readd_rule_kind_derives(text)` — re-add `#[derive(Clone, Copy, Debug, PartialEq, Eq)]` above each `pub enum <Name>RuleKind { ... }` | Same class as (8) for the `RuleKind` enum. | IR codegen emits the derive directly. | **Discard**. |
| 10 | `readd_cst_directive_derives(body)` — walk into the `cst_directives` sub-module and re-add `#[derive(Clone, Copy)]` above each `pub struct Name { ... }` | Same class as (8) for `cst_directives` structs. | IR codegen emits the derive directly. | **Discard**. |
| 11 | `re.sub(r'\{\s*::core::panicking::panic_fmt\(\s*format_args!\((.*?)\),?\s*\);?\s*\}', r'{ panic!(\1); }', text, flags=re.DOTALL)` — collapse expanded panic-call shape back to `panic!(...)` macro | rustc expand desugars `panic!("msg")` into `{ ::core::panicking::panic_fmt(format_args!("msg")); }`, calling an unstable `core` internal. The collapse re-formed the macro invocation so the checked-in file used only stable surfaces. | xtask never expands panics — the IR codegen emits `panic!(...)` macro invocations directly (e.g., `bbnf::generate::lower::*`); rustc desugars at the consumer's compile. | **Discard** — defect class absent. |
| 12 | Strip `//!` outer doc comments (`lines = text.split('\n'); filtered = [line for line in lines if not line.strip().startswith('//!')]`) | `bbnf-bootstrap/src/lib.rs` carried a `//!` doc-comment about its proc-macro role. Expand emitted the doc comment; the script stripped it so the canonical header replaced it. | xtask emits its own `//!` header (`file_header()`) with the canonical "AUTO-GENERATED" + regenerate-command line. No doc-comment from any consumer crate appears in the emission. | **Discard**. |
| 13 | `re.sub(r'\n{3,}', '\n\n', text)` — collapse blank-line runs | Belt-and-braces formatting cleanup on the post-regex text. | `prettyplease::unparse` produces canonical formatting; no blank-line drift. | **Discard**. |
| 14 | `use ::parse_that::*;` / `use ::bbnf::runtime::...` — strip duplicates introduced by re-runs | Idempotency hedge: if the script ran on an already-headered file, the imports doubled up. The regex stripped the body's copy so the header emitted the canonical set. | xtask writes one fresh file per regen; no idempotency concern (the file is overwritten wholesale; `regen --check` diffs against checked-in content). | **Discard**. |
| 15 | `header = '''//! AUTO-GENERATED ... use ::bbnf::runtime::tape::*; ...'''` — emit the canonical file header above the post-processed body | The fixed file header carrying doc comment + crate-level `#![allow(...)]` + canonical imports + the marker struct declaration. | xtask emits an equivalent header via `file_header()` (`xtask/src/regen.rs:300-322`); the marker struct emits inside the body (`xtask/src/regen.rs:243`). | **Already present in xtask** — no absorption needed. The xtask's header formulation is byte-equivalent in shape: same `//! AUTO-GENERATED` doc, same `#![allow(...)]` block, same three `use` imports. The `Regenerate:` line points at `cargo xtask regen --grammar <ident>` instead of `scripts/bootstrap-bbnf.sh`. |
| 16 | `cargo expand -p bbnf-bootstrap --lib > $TEMP` — drive rustc expand on the bootstrap crate | Pre-B2 entrypoint: rustc was the only path to run the proc-macro IR pipeline. | xtask runs `bbnf::pipeline::compile_paths_request` + `generate_all` natively; no rustc invocation. | **Discard** — substrate replaced. |
| 17 | `BBNF_BOOTSTRAP_CLEAN_CACHE=1 → rm -rf target/.bbnf-cache/` — gated proc-macro cache clear | The B1.W2.b content-hash guard left the cache populated; CI occasionally needed a forced wipe to recover from poisoned entries. | `target/.bbnf-cache/` populates only when the proc-macro runs; the proc-macro deleted at W2; the directory never repopulates. | **Discard**. |
| 18 | `check-bootstrap-clean.sh` — diff committed vs fresh-bootstrap output, with 8000-line floor for stale-cache truncation | CI gate ensuring `generated.rs` matches a fresh bootstrap. The 8000-line floor caught the V0 stale-cache regression class (truncated 23-line outputs when the cache served partial expansion). | `cargo xtask regen --check` regenerates every grammar to a tempdir and `bytes != bytes` diffs against the checked-in tree (`xtask/src/regen.rs:344-392`). Exits non-zero with per-grammar drift messages. The 8000-line floor's stale-cache failure mode no longer applies (no proc-macro cache). | **Already present in xtask** — `regen --check` supersedes the script. Delete the script. |

## Absorption summary

Eighteen transformations catalogued. Verdicts:

- **Discard, 16/18**: defect classes either originate in `cargo expand`'s
  output noise (transforms 1-6, 12, 14, 16-17) or in expand's derive /
  panic / prelude desugaring (transforms 7-11, 13). The xtask sits
  upstream of expand entirely — it constructs the `TokenStream` directly
  from the IR pipeline and formats via `prettyplease`. None of the
  defects manifest in the xtask emission.

- **Already present in xtask, 2/18**: the file header (transform 15) is
  emitted by `xtask/src/regen.rs::file_header()` with the same doc
  comment, allow-block, and imports as the script's header constant. The
  CI-clean gate (transform 18) is `cargo xtask regen --check`, fully
  implemented at W0.

**Net: zero transformations need new code in `xtask/src/regen.rs`.**

The retiring scripts implement a Python-regex-on-text contract that
exists solely to clean up `cargo expand`'s output. With the IR pipeline
moved out of expand-phase entirely (W0 substrate), the expand step never
runs in regen, so the post-process never has anything to clean. The
script's role retires by *substrate reformulation*, not by reproduction
in Rust.

## Mechanism for verifying no-drift

The W3 close gate measures regen idempotence by re-running `cargo xtask
regen --grammar bbnf` and `git diff` on the per-grammar source: a
zero-line diff confirms the xtask's existing emission is byte-stable
against the checked-in W2-close tree. No transformation absorption means
no new code paths to drift across; the W2-close generated tree is the
fixed point.

## Out-of-scope KEEPs

The W3 spec's file-bounds list these as KEEP / KEEP-MODERNIZE per
`meta-audit/08-abrogation-catalog.md`:

- `scripts/prebuild-benches.sh` — `find . -name '.bbnf-cache' ...` line
  is a defensive cache wipe that no-ops post-W2 (no cache populates) but
  costs zero. Out of W3 scope.
- `scripts/prepare-profile-wave.sh` — same defensive wipe. Out of W3
  scope.
- `scripts/profile-bench-headless.sh` — same defensive wipe. Out of W3
  scope.
- `.cargo/config.toml` `expand-bootstrap` / `expand-derive` aliases —
  general-purpose cargo-expand entrypoints; the targets they expand
  (`bbnf-bootstrap`, `bbnf` lib) still exist. Aliases retain their
  value as debug investigation surfaces. Out of W3 scope.

These items can route to a successor wave if the residual `.bbnf-cache`
references become noise; W3 leaves them untouched.
