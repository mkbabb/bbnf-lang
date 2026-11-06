# PROTO-2 — P2-CLOSED: the grammar-driven gap, CLOSED to full 56/56 parity

**Lock:** LOCK-5 (THE REGEX CONTINUUM) · pass-3 · probe 2 · **family P2** (lock-titular:
grammar-driven break-the-discard + de-dup the two `regex.bbnf`).
*(Successor to `PROTO-2-P2.*`, which established the FIRST artifact at 43/45 "measured-feasible
with a named gap." This probe **attempts the host-fold re-sync P2 scoped out** and **closes the
gap**: 56/56 both oracles, strict full-parity test green. Filed as `PROTO-2-P2-CLOSED.*` — does not
clobber `PROTO-2.md` (5B census), `PROTO-2-5C.*` (extraction control), or `PROTO-2-P2.*` (the
feasibility artifact it builds on).)*

**Verdict:** **RAN_MEASURED — GAP CLOSED. Full grammar-driven ↔ hand DFA parity on 56/56, raw and
through the `simplify_hir` bridge.** The three desyncs P2 named (bounded-repeat rejects, bare-dot
divergence, in-class `\x` divergence) were all **one** blocker — `host.rs` reading span text where
the regenerated parser hands back a trimmed value — and are resolved with a **5-edit, host-only**
re-sync (`+50 / −6` in `host.rs`, no grammar change, no regen). SPEC-ONLY(grammar-driven) advances
**un-attempted → measured-feasible (P2) → PARITY-DEMONSTRATED (here)**.

- **Base-pin:** parse-that (READ-ONLY) `HEAD = ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42`
  (`docs(coordination): VALUEJS-PT-E`). Scratch per R3 §8 at `…/scratchpad/pt-scratch`,
  `CARGO_TARGET_DIR` inside the copy, 7 external patch prefixes repointed to absolute originals.
  **Original parse-that untouched** — `diff` of scratch `host.rs` vs original was `IDENTICAL`
  before my edits; the only source change is inside the scratch copy (`PROTO-2-P2-CLOSED.patch`).
  arm64, no x86.
- **Command:** `cargo test -p parse_that --test grammar_driven_regex -- --nocapture`.
  **Runs:** `PROTO-2-P2-CLOSED-baseline-run.txt` (43/45, pre-fix) and `PROTO-2-P2-CLOSED-run.txt`
  (56/56, post-fix). **Patch:** `PROTO-2-P2-CLOSED.patch`. Correctness/parity probe — **no perf
  claims** (loadavg recorded for provenance: baseline 6.57→…, fix run 4.55→4.33).

---

## QUESTION

Can the bootstrapped-from-`regex.bbnf` generated parser return an `Hir` (not `bool`) that threads
into the XL-C `egraph::simplify_hir` bridge and **DFA-matches** the hand `hir::parser::parse` — and
how large is the promote+de-dup gap between the two `regex.bbnf` files? P2 answered *yes, at 43/45
with a named residual*. This probe asks the sharper follow-on: **is that residual actually
mechanical — does closing it yield full parity, and at what surface?**

## METHOD (what ran)

Same harness P2 authored (`parse_that/tests/grammar_driven_regex.rs`, unchanged) — the 56-pattern
corpus (44 cyclic + 12 acyclic; JSON/CSS/Sheets leaves, bushy alternations, classes, bounded
repeats), a **byte-level, renumbering-invariant canonical minimal-DFA** comparison
(`Nfa::from_hir` → `Dfa::from_nfa`, `minimize:true`; strictly stronger than match-equivalence), run
**twice**: RAW (each `Hir` straight to DFA) and BRIDGE (both `Hir`s through
`egraph::simplify_hir(&_, &RegexExtractionCost::default())` first). It is a **strict full-parity
gate**: any reject, any divergence, or fewer than 30 both-parsed pairs fails the test.

The **only** change this probe makes is the **`host.rs` semantic-fold re-sync** — five edits driven
directly by P2's raw-AST attribution (the `Span` type carries `.src` + `.start`/`.end`, so every
desync is recoverable host-side without touching the grammar or regenerating `generated.rs`):

| # | desync (P2's finding) | fix in `host.rs` |
|---|---|---|
| A | **Braced-quantifier span carries braces.** `>>`/`<<` trim the projected value, not `.as_str()`: `\d{4}` quantifier span = `"{4}"`, `a{2,3}` = `"{2,3}"` → `u32::parse("{4")` fails → 11 rejects | `parse_braced_quantifier` strips leading `{`, splits on `}`; trailing `?` after the brace drives greediness |
| B | **Bare `.` routes to `class_atom_0`** (codegen shares the single-char sub-parser between the atom arm and the in-class atom) → lowered as literal `[.]` instead of dot `[^\n]` | in the `atom(inner)` arm, intercept `class_atom_0`/`atom_0` text `.`/`^`/`$` → `make_dot` / `Look::Start` / `Look::End` **before** the literal fallthrough |
| C | **Escape sigil stripped by directional skip.** `"\\" >> "x" >> /hex/` → `\x00` span = `"00"`, `\x41` = `"41"`; `lower_escape`/`lower_class_escape` switch on `bytes[0]` so the `x`/`u`/`p` arms never fire → literal `'0'`/`'4'` | new `escape_body(span)` scans `span.src` back to the `\` and re-slices the full body; `\p` brace-trim moved into the `p` arm so escaped `\{`/`\}` survive |

## THE NUMBERS

| metric | P2 baseline (pre-fix) | **P2-CLOSED (post host-fold re-sync)** |
|---|---:|---:|
| corpus | 56 | 56 |
| hand parser rejects | 0 | 0 |
| **generated rejects** (LOCK-7A gap) | **11** (every `{n,m}`) | **0** |
| **RAW DFA-equivalent** | **43/45** | **56/56** |
| **BRIDGE DFA-equivalent** (through `simplify_hir`) | **43/45** | **56/56** |
| raw / bridge divergences | 2 / 2 | **0 / 0** |
| test gate (strict: 0 reject, 0 diverge, ≥30) | **FAILED** | **`test result: ok. 1 passed`** |

The 11 former rejects (all bounded repeats) now parse; the 2 former divergences —
`[^\x00]*x` (in-class `\x00`: gen was `[^48]`, now `[^0]` = hand) and `/\*.*\*/` (bare dot: gen was
`[.]` byte 46, now `[^\n]` byte 10 = hand) — are gone. Both oracles agree with the hand parser on
**every** pattern.

## DE-DUP SURFACE — the two `regex.bbnf` (confirms P2, independently recounted)

`parse-that/rust/bootstrap/regex.bbnf` = **13 rules / 72 LOC**, the engine-wired grammar
(`generated.rs` + `host.rs` lower its AST to `Hir` — the path this probe just proved). Its 13 rules:
`literal, class_escape, class_atom, class_item, char_class, escape, quantifier, group, atom,
quantified, concat, alternation, regex`.
`bbnf-lang/grammar/misc/regex.bbnf` = **25 rules / 27 LOC**, a JS-flavoured regex-**literal**
grammar wired to **nothing**. Dispositioning misc against the bootstrap superset:

| disposition | count | rules |
|---|---:|---|
| **DELETE — dominated** (bootstrap has an equivalent) | **17** | `expression, term, factor, sub_expression, group, non_capturing_group, char_class, char_set, range, dot, character, escaped, special, decimal, hex, non_escaped, number` |
| **DELETE — out of engine scope** (non-regular; absent from the bespoke `Hir` = Start/End/LF/CRLF only) | **4** | `lookahead, lookbehind, back_reference, anchor` (`\b`/`\B`) |
| **RELOCATE — orthogonal** (regex-*literal* delimiter/flags → bbnf-lang lexing, not the pattern-body engine) | **3** | `flags, end, regex` (the `/…/` wrapper) |
| **PROMOTE — clean** (no bootstrap analog; +1 host arm) | **1** | `octal` (`"\\" >> /0[0-7]+/`) |

**Promote+de-dup gap = 1 promote, 21 delete, 3 relocate** (25 = 1+21+3). The bootstrap grammar is
the strict superset for the regular fragment (it alone carries `{n,m}`+lazy, `\u{}`/`\uHHHH`,
`\p{}`/`\P{}`, inline flag groups). **Retire `grammar/misc/regex.bbnf`; carry only `octal` into the
engine-wired bootstrap grammar.**

## VERDICT

**RAN_MEASURED — the grammar-driven path now has full parity, not merely feasibility.** The
bootstrapped generated parser returns an `Hir` (discard already broken in `mod.rs:34`), it threads
through the XL-C `simplify_hir` bridge, and after a **5-edit host-only re-sync** it is **byte-DFA-
identical to the hand parser on 56/56 patterns, raw and bridge alike** — the strict full-parity test
gate is green. The exact LOCK-7A gap P2 named (11 `{n,m}` rejects + bare-`.` + in-class `\x`) was
**one** blocker — `host.rs` span/value drift from the regenerated parser — and is **closed** without
any grammar change or regen. De-dup quantified and re-confirmed: **1 promote / 21 delete / 3
relocate ⇒ retire `grammar/misc/regex.bbnf`**.

**Verdict line:** `RAN · grammar-driven Hir path (parse_generated→host::lower→Hir→DFA) at FULL
PARITY · RAW 56/56 & BRIDGE 56/56 byte-DFA-identical to hand parser · 0 rejects / 0 divergences ·
strict-gate test GREEN · gap CLOSED by 5-edit host-only host.rs re-sync (+50/−6, no grammar/regen) ·
de-dup: 1 promote / 21 delete / 3 relocate → retire grammar/misc/regex.bbnf · SPEC-ONLY(grammar-
driven) → PARITY-DEMONSTRATED.`

## CAVEATS (honest boundaries)

1. **Host-side re-sync, not grammar/regen.** The fix recovers stripped sigils/braces from the source
   `Span` offsets rather than making the grammar emit typed captures + regenerating `generated.rs`.
   That is the smaller, verified surface and it proves mechanical closure; the grammar-carries-
   captures alternative is cleaner architecturally and remains the GESTALT-P call. Either closes it.
2. **Two constructs closed by construction, not by the DFA oracle.** Bare `^`/`$` anchors and `\p{}`
   are **not** in the 56-corpus, so their arms (the anchor interception in `atom(inner)`, the `\p`
   brace-trim) are verified by reasoning, not by a divergence flipping to pass. Bare **dot** and
   **`\x`** *were* in the corpus and *are* oracle-verified (the two former divergences now pass).
3. **`lower_class_atom_node_byte` (range endpoints) left as-is.** In-class `\x`-as-range-endpoint
   (`[\x00-\x1f]`) still reads `bytes[0]`; no corpus range exercises it, so it is untouched. A
   full GESTALT-P landing should route it through `escape_body` too (one-line follow-on).
4. **Harness provenance.** The test/corpus is P2's, unchanged; this probe's sole authored change is
   `host.rs`. The scratch also lacks P2's `parse_generated_ast_debug` probe (absent from this
   scratch's `mod.rs`) — not needed here, since attribution was already banked by P2.

## FILES

- `PROTO-2-P2-CLOSED.md` — this deliverable.
- `PROTO-2-P2-CLOSED-run.txt` — the 56/56 green oracle run (post-fix).
- `PROTO-2-P2-CLOSED-baseline-run.txt` — the 43/45 run (pre-fix, same harness) for before/after.
- `PROTO-2-P2-CLOSED.patch` — unified diff vs READ-ONLY parse-that: `host.rs` only (`+50/−6`),
  the 5-edit fold re-sync (A braces, B meta-atom, C escape sigil + `\p` brace-trim).
