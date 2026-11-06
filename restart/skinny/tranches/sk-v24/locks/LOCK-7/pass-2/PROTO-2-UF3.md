# LOCK-7 · pass-2 · PROTO-2-UF3 — the fixpoint gate on the self-emitting regex parser

**Plane:** THE GRAMMAR TOTALITY & BOOTSTRAP · family **7A STAGED-BOOTSTRAP** (fills 7D
META-CIRCULAR-ORACLE ledger) · probe **UF-3** — the one fixpoint signal claimed runnable
WITHOUT LOCK-5's engine.

**Verdict line:** **RAN_MEASURED — the regen path is SEVERED; the fixpoint gate CANNOT run
on the regex artifact.** The documented regenerator does not reproduce `generated.rs`, it
**truncates it to 0 bytes**; the alternate (`cargo xtask regen`) has no regex manifest row
and emits none of the parser's symbols. **7D's regex-row `fixpoint` column stays EMPTY /
N-A (not fillable by the current tree). 7A's SPEC_ONLY reduction (OPEN-2/XP-2) is NOT
discharged** — FAMILY-SPECS §0/§4's premise "the regex parser self-emits TODAY" (lines 166,
172, 329) is **empirically false against HEAD**.

---

## Question

The fixpoint gate (`regen∘regen = regen`, byte-identical — the rustc/GHC/pegen `stage2==stage3`
law) has NEVER run on any artifact. 7D's regex promotion-ledger row is complete save its empty
`fixpoint` column. FAMILY-SPECS asserts the regex parser **self-emits today** — the one
self-host edge needing NO LOCK-5 engine — so UF-3 should:

1. Regen `parse_that/src/regex/generated.rs` from `bootstrap/regex.bbnf` (72 LOC, L4) → **G0**.
2. Regen again → **G1**; assert `G1 == G0 == committed` (the fixpoint law).
3. **Non-vacuity:** plant a source mutation in `regex.bbnf`, regen, assert the *derived* region
   of `generated.rs` bytes CHANGES (gate is sensitive to the derivation, not a constant stamp);
   revert.

**PASS** iff regen∘regen is byte-identical AND the mutation moves derived bytes.
**FAIL** iff regen is non-idempotent OR the mutation leaves bytes unchanged.

## Method (scratch, no promotion, no perf/parity claim)

parse-that is **READ-ONLY**. Isolated scratch copy per R3 §8 at
`scratchpad/pt-scratch-uf3/` (rsync of `/Users/mkbabb/Programming/parse-that`, exclude
`target/.git/node_modules/data`; the 7 external sibling patch prefixes in
`rust/.cargo/config.toml` repointed to absolute originals — read-only consumption;
`CARGO_TARGET_DIR=$DST/target`, never touching the original). Base pin **ef10d5b**
(`parse-that` HEAD). Committed reference preserved as `COMMITTED.generated.rs` before any run.

**Regenerator under test** = the *only* command the artifact names. `generated.rs`'s own
header (`//! Regenerate: scripts/bootstrap-regex.sh`) and the pre-B2 derive doc both point at
`scripts/bootstrap-regex.sh`, which runs `cargo expand -p regex-bootstrap` and post-processes
the expansion into `parse_that/src/regex/generated.rs`.

## What ran

1. **Documented regen (stage 1):** `bash scripts/bootstrap-regex.sh` in the scratch copy
   (`CARGO_TARGET_DIR` isolated). Log + resulting file sha'd against the committed reference.
2. **Root-cause capture:** `cargo expand -p regex-bootstrap` re-run with stdout+stderr
   captured (the script discards stderr) to characterize *what* the expansion emits.
3. **Static census of the alternate path:** the outer bbnf-lang `cargo xtask regen` — its
   workspace manifest, `Cmd` enum, `output_path`, and a symbol grep for the regex parser's
   emitted identifiers across `crates/`.
4. Git confirmation of the `bbnf_derive` retirement.

Steps 2 and 3 of the *probe method* (regen-twice byte-diff; planted-mutation non-vacuity)
were **unreachable** — they have no meaning once stage 1 shows regen cannot produce a valid G0.

## The numbers

| measurement | result |
|---|---|
| committed `generated.rs` | **2293 lines / 184 201 bytes** · sha256 `82167cec…0612fcce` |
| **G0 = documented regen output** (`bootstrap-regex.sh`) | **0 lines / 0 bytes** · sha256 `e3b0c442…7852b855` (the empty-file hash) |
| `bootstrap-regex.sh` exit | **1** — python post-processor `StopIteration` at line 4: `next(i … if "use ::parse_that" in l)` finds no such line |
| effect on committed file | **TRUNCATED to empty** — regen is not merely non-reproducing, it is **destructive** |
| `cargo expand -p regex-bootstrap` | exit **0**, **25 lines**, `Finished dev profile in 0.28s` |
| — grammar body in expansion? | `use ::parse_that` matches = **0**; the expansion is the crate's doc-comment + one line: `pub use parse_that::regex::generated::{RegexParser, parse_generated};` |
| — bbnf IR pipeline invoked? | **NO** — cargo reports `patch bbnf / bbnf-ir / bbnf-ser / csp-solver … was not used in the crate graph`. The re-export never touches bbnf-lang. |
| outer `cargo xtask regen` manifest (`[workspace.metadata.bbnf].grammars`) | **9 rows** — bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math. **No regex.** |
| outer `main.rs` `Cmd` regen variants | RegenCss/Math/Csv/Bbnf/Bnf/Ebnf/CssPretty/GoogleSheets/Json — **no RegenRegex** |
| `output_path(ident)` target | `crates/core/src/grammar/generated/<ident>.rs` — **inside bbnf-lang, never parse-that** |
| regex parser symbols (`RegexParserEnum`, `GRAMMAR_RegexParser`, `parse_generated`, `__RegexParserEnumCtx`) in bbnf-lang `crates/` | **0 occurrences** — the outer emitter does not produce this format |
| `bbnf_derive` proc-macro | **retired** — bbnf-lang commit `6142387f1 feat(b2): retire crates/derive proc-macro crate; purge bbnf_derive deps (B2.W2)`; `crates/derive` does not exist |

## Interpretation — the self-host edge is gone, not idle

The committed `parse_that::regex::generated` is a **frozen artifact of a decommissioned
pipeline.** Pre-B2, `regex-bootstrap` carried `#[derive(bbnf_derive::Parser)]` on a
`RegexParser` marker; `cargo expand` surfaced the macro expansion and
`bootstrap-regex.sh`'s python post-processed it into `generated.rs`. **B2 (`6142387f1`)
deleted the proc-macro.** `bootstrap/src/lib.rs` is now a pure re-export
(`pub use parse_that::regex::generated::{RegexParser, parse_generated};`), so the expansion
carries no grammar and the post-processor's anchor line never appears → truncation.

The Cargo.toml / lib.rs comments claim the succession path is "the IR pipeline runs through
bbnf-lang's `cargo xtask regen`." **It does not, for regex.** That xtask enumerates a 9-grammar
manifest that omits regex, writes only into `bbnf-lang/crates/core/src/grammar/generated/`, and
emits an entirely different code shape (marker-struct + `include!` + `registry.json` sidecar,
`use ::parse_that::*` runtime header) — none of the `RegexParserEnum` / `GRAMMAR_RegexParser` /
`parse_generated` identifiers that constitute the committed regex parser. There is **no path,
live or dormant, from `regex.bbnf` to `generated.rs` in the current tree.**

**Consequence for the gate.** `regen∘regen = regen` presupposes a regen. Here **G0 itself is
unproducible** — the sole documented command yields the empty-file hash, not the parser.
Idempotency is vacuously "held" only in the degenerate sense that a second run also truncates
to empty (`e3b0c442… == e3b0c442…`), which certifies nothing about the derivation. The
non-vacuity planted-mutation step is therefore unreachable: with no derivation, no mutation can
be shown to move derived bytes.

## Verdict

**RAN_MEASURED — FAIL (gate precondition absent).** The failure category is *deeper* than the
brief's two anticipated FAIL modes (non-idempotent / constant-stamp): **regen does not run at
all.** UF-3, billed as "the one fixpoint signal runnable WITHOUT LOCK-5's engine, runnable
TODAY on the regex oracle artifact," is **not runnable** — the self-emit edge was severed in B2.

- **7D ledger, regex row, `fixpoint` column:** stays **EMPTY / N-A** — *not* filled TRUE, and
  *not* fillable by the current tree. The row remains "one column short," and that column is
  **structurally unfillable** until a regen edge is rebuilt.
- **7A's SPEC_ONLY reduction (OPEN-2/XP-2):** **NOT discharged.** 7A's one claimed
  non-redundant asset — a byte-fixpoint runnable without LOCK-5 — evaporates. On the survival
  test (FAMILY-SPECS line 171–172: "name ONE thing 7A owns post-graft that 7C+7D do not"), the
  fixpoint-on-regex signal is **not ownable**, because it cannot be produced. This **strengthens
  the case that 7A RETIRES**: the fixpoint law's only engine-free artifact is inert.

## What WOULD discharge the general fixpoint law (flagged, not run — out of scope + cap)

The fixpoint machinery is **live for the bbnf-lang emitter**, just not wired to regex:
`cargo xtask regen --check` regenerates every manifest grammar to a tempdir and diffs against
the checked-in tree (drift ⇒ non-zero) — a committed==regen fixpoint over the 9 live grammars,
reusing the `regen.rs check_target` byte-diff shape the brief cites. A future probe can run
`--check` (idempotency) + a planted mutation on one small grammar (e.g. `csv.bbnf`) to prove
non-vacuity **for the live pipeline** — but that certifies the bbnf-lang xtask emitter, a
*different artifact* from the frozen parse-that regex parser, and cannot reproduce the committed
regex bytes. It does not fill 7D's **regex** row.

## Reproduction

```
SRC=/Users/mkbabb/Programming/parse-that
DST=…/scratchpad/pt-scratch-uf3
rsync -a --exclude target/ --exclude .git/ --exclude node_modules/ --exclude data/ "$SRC/" "$DST/"
# repoint the 7 external patch prefixes in $DST/rust/.cargo/config.toml to absolute originals
cp "$DST/rust/parse_that/src/regex/generated.rs" "$DST/COMMITTED.generated.rs"   # 82167cec…
cd "$DST/rust"; export CARGO_TARGET_DIR="$DST/target"
bash scripts/bootstrap-regex.sh            # → StopIteration, exit 1, generated.rs truncated to 0 bytes (e3b0c442…)
cargo expand -p regex-bootstrap            # → exit 0, 25 lines, pure re-export; "patch bbnf … was not used in the crate graph"
```

Evidence retained in `pt-scratch-uf3/`: `regen-stage1.log`, `expand.out`, `expand.err`.
Original parse-that tree **untouched**; no repo code mutated (see PROTO-2-UF3.patch).
