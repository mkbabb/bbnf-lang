# PROTO-2 — P2 GRAMMAR-DRIVEN: the titular deliverable's first artifact

**Lock:** LOCK-5 (THE REGEX CONTINUUM) · pass-3 · probe 2 · **family P2** (lock-titular:
grammar-driven break-the-discard + de-dup the two regex.bbnf).
*(Filed as `PROTO-2-P2.*` — the pass-3 `PROTO-2.md` slot is held by the 5B fire-census probe and
`PROTO-2-5C.*` by the automata-as-extraction control; this is the distinct titular deliverable, not
a clobber.)*

**Verdict:** **RAN_MEASURED — FIRST ARTIFACT (measured-feasible, with a named, mechanical gap).**
The grammar-driven path is **already wired end-to-end** and **DFA-matches the hand parser on 43/45
both-parsed patterns** through **both** the raw and the `simplify_hir` bridge (≥30 gate met). It is
**not** at full parity: **11/56** patterns (every `{n,m}` bounded repeat) are rejected outright and
**2/56** diverge (bare `.`, in-class `\x`). All 13 misses trace to **one** concrete blocker —
`host.rs` (the hand-written fold) has drifted out of sync with the regenerated parser's
span/variant contract — not to any infeasibility. SPEC-ONLY(grammar-driven) advances from
**un-attempted → measured-feasible**; the residual is a host-fold re-sync, quantified below.

- **Base-pin:** parse-that (READ-ONLY) `HEAD = ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42`
  (`docs(coordination): VALUEJS-PT-E`). Scratch built per R3 §8 into `…/scratchpad/pt-scratch`
  with `CARGO_TARGET_DIR` inside the copy; the 7 external patch prefixes repointed to absolute
  originals; parse-that itself untouched. arm64, no x86.
- **Command:** `cargo test -p parse_that --test grammar_driven_regex -- --nocapture` (+ a
  `raw_ast_dump` probe for root-cause attribution). **Run:** `PROTO-2-P2-run.txt`. **Patch:**
  `PROTO-2-P2.patch`. This is a correctness/parity probe — **no perf claims** (loadavg recorded in
  the run header for provenance only: 4.31→7.44).

---

## QUESTION

Can the bootstrapped-from-`regex.bbnf` generated parser return an `Hir`/AST (not `bool`) that
threads into the XL-C `egraph::simplify_hir` bridge and **DFA-matches** the hand `hir::parser::parse`
— and how large is the promote+de-dup gap between the two `regex.bbnf` files? Titular deliverable,
zero artifacts after three passes.

## FINDING 0 — the discard is already broken at the outer layer

There are **two** `parse_generated`:

| symbol | signature | role |
|---|---|---|
| `regex::generated::parse_generated` (`generated.rs:2288`) | `(&str) -> bool` | diagnostic shim — **builds** the full AST via `parse_return_state_with_context`, then **discards** it (`result.is_some() && state.offset >= len`) |
| `regex::parse_generated` (`mod.rs:34`) | `(&str) -> Result<Hir, ParseError>` | **the real grammar-driven path** — generated parser → typed `RegexParserEnum` AST → `host::lower` fold → bespoke `Hir` |

"Break-the-discard" is therefore **not** a code change to author — the Hir-returning path exists
today (`mod.rs:34-58` + the 525-line `host.rs` fold). The probe's real work is the **parity oracle**
on that existing path, and the **de-dup surface** on the two grammars.

## METHOD (what actually ran)

New integration test `parse_that/tests/grammar_driven_regex.rs`, 56-pattern corpus (verbatim from
`regex/benches/regex_dfa_compile.rs`: 44 cyclic + 12 acyclic — JSON/CSS/Sheets leaves, acyclic
chains, bushy alternations, classes, bounded repeats). For each pattern:

1. `hand = regex::parse(p)` (hand `hir::parser::parse`) and `genh = regex::parse_generated(p)`
   (grammar-driven). A `genh` reject the hand accepts is logged as the **LOCK-7A bootstrap-parity
   gap**.
2. **RAW oracle:** build a minimized DFA from each Hir via `Nfa::from_hir` → `Dfa::from_nfa`
   (default `minimize:true` — identical to the tail of `Dfa::compile`), reduce each to a
   **renumbering- and byte-class-partition-invariant BYTE-level canonical form** (BFS from start
   state 0 in byte order; each state → `(is_accept, [target-canon-id | −1 for DEAD ; 256])`), and
   assert byte-identical. This is strictly stronger than match-equivalence.
3. **BRIDGE oracle:** thread **both** Hirs through XL-C `egraph::simplify_hir(&_, &RegexExtractionCost::default())`
   first, then the same canonical-DFA comparison — proving the generated Hir is a valid input to the
   e-graph simplifier and stays equivalent across it.
4. `raw_ast_dump.rs` prints the raw `RegexParserEnum` (pre-fold) for `.`, `\d{4}`, `[^\x00]`,
   `\x41`, `a{2,3}` to attribute each divergence to an exact AST variant/span.

`Nfa::from_hir` treats `Hir::Group` transparently (`nfa.rs:143-146`), so the host fold's group
wrappers cannot themselves cause DFA divergence — a genuine divergence names a genuine construct.

## THE NUMBERS (from `PROTO-2-P2-run.txt`)

```
corpus                : 56 patterns
hand parser rejects   : 0
generated rejects     : 11   (LOCK-7A bootstrap-parity gap) — every {n,m} bounded repeat
RAW    DFA-equivalent : 43/45  (both-parsed pairs)
BRIDGE DFA-equivalent : 43/45  (both threaded through simplify_hir)
raw / bridge divergences : 2 / 2
```

**11 GEN-REJECTs — all bounded repeats** (`\d{4}-\d{2}-\d{2}`, `#[0-9a-fA-F]{3,6}`,
`[0-9a-fA-F]{3,8}`, `\d{1,3}\.…`, `\d{2}:\d{2}:\d{2}`, `[A-Z]{3}-\d{4}`, `[01]{8}`,
`\d{5}(?:-\d{4})?`, `[a-fA-F0-9]{2}(?::…)+`, `\d+(,\d{3})*`, `\+?\d[\d -]{6,}\d`) fail with
`invalid repetition bound`.

**2 divergences (equal state counts, different accepted language):**

| pattern | hand Hir | generated Hir | construct |
|---|---|---|---|
| `[^\x00]*x` | `Class [^\x00]` (byte 0 negated) | `Class [^0]` (byte **48** negated) | in-class `\x` hex |
| `/\*.*\*/` | dot = `[^\n]` (byte 10 negated) | `[.]` (byte **46**, **not** negated) | bare `.` dot |

## ROOT CAUSE — one blocker, three surfaces (raw-AST evidence)

The raw `RegexParserEnum` dump is dispositive; every miss is `host.rs` reading a **span/variant the
current generated parser does not emit as host assumes**:

1. **Meta-atom desync (dot / anchors).** `.` parses to **`atom(class_atom_0(span="."))`** — *not*
   `atom_0(".")`. host.rs's dot/`^`/`$` handling lives in the `atom_0` arm (`host.rs:99-107`),
   which the generated parser **never emits** — it is **dead code**. `class_atom_0` lowers as a
   literal byte-class (`host.rs:150-156`), so `.` → `[.]`. By the same mechanism bare `^`/`$` would
   lower to literal `^`/`$` (untested — the 56-corpus has no bare-anchor pattern; a `/foo$/`-style
   pattern would surface it).
2. **Escape-sigil-span desync (`\x`, `\u`).** The grammar's directional skip
   `"\\" >> "x" >> /[0-9a-fA-F]{2}/` strips the `x` from the node's `.as_str()` span
   (`[^\x00]` → class_escape span = **`"00"`**; `\x41` → escape span = **`"41"`**). host
   `lower_escape`/`lower_class_escape` switch on `bytes[0]` expecting the sigil present, so the
   `b'x'`/`b'u'` arms never fire → `\x41` → literal `'4'`, `\x00` → literal `'0'`. `\u{…}`/`\uHHHH`
   share the defect.
3. **Quantifier-brace-span desync (`{n,m}`).** The `{n,m}` node's span is the full consumed extent
   **including braces** (`\d{4}` quantifier span = `"{4}"`; `a{2,3}` = `"{2,3}"`) — `>>`/`<<` trim
   the projected *value*, not the `.as_str()` *span*. host `parse_braced_quantifier` assumes the
   braces are already stripped, so `u32::parse("{4")` fails → `invalid repetition bound` on **all
   11** bounded repeats.

All three are **`host.rs` ↔ regenerated-parser drift** (a clean-regen desync: the fold was authored
against an earlier `generated.rs` variant/span shape). The fix is host-fold re-alignment — or,
more robustly, making the grammar carry explicit typed captures instead of relying on span slicing
across `>>`/`<<`. **Not** a fundamental infeasibility of the grammar-driven engine.

## DE-DUP SURFACE — the two `regex.bbnf` quantified

`parse-that/rust/bootstrap/regex.bbnf` (**72 LOC / 13 rules**, engine-wired: `generated.rs` +
`host.rs` lower its AST to `Hir`) vs `bbnf-lang/grammar/misc/regex.bbnf` (**27 LOC / 25 rules**, a
JS-flavoured regex-**literal** grammar, wired to **nothing**). They are ~85% redundant. Unifying on
the bootstrap grammar (the only one the DFA engine consumes):

| disposition | count | misc rules |
|---|---:|---|
| **DELETE — dominated** (bootstrap has an equivalent rule) | **17** | `expression`≈alternation, `term`/`factor`≈quantified, `sub_expression`≈atom, `group`≈group, `non_capturing_group`≈group(arm1), `char_class`≈char_class, `char_set`≈class_item, `range`≈class_item_0, `dot`≈atom(`.`), `character`≈atom/literal, `escaped`≈escape, `special`≈escape(catch-all), `decimal`≈escape(`\d`), `hex`≈escape(`\x`), `non_escaped`≈literal, `number`≈quantifier(`\d+`) |
| **DELETE — out of engine scope** (non-regular / absent from the bespoke `Hir`) | **4** | `lookahead`, `lookbehind`, `back_reference`, `anchor`(`\b`/`\B`) — `Hir::Look` has only Start/End/LF/CRLF; no lookaround, backref, or word-boundary. Cannot promote without abandoning the DFA (regular-language) engine |
| **RELOCATE — orthogonal concern** (regex-*literal* delimiter/flags; belongs to bbnf-lang lexing, not the pattern-body engine grammar) | **3** | `flags`, `end`, `regex`(with `/…/`) |
| **PROMOTE — clean** (no bootstrap analog; host would add one arm) | **1** | `octal` (`"\\" >> /0[0-7]+/`) |

**Promote+de-dup gap = 1 promote, 21 delete, 3 relocate.** The bootstrap grammar is the strict
superset for the regular fragment (it additionally carries `{n,m}`+lazy, `\u{}`/`\uHHHH`,
`\p{}`/`\P{}`, and inline flag groups `(?flags:)`/`(?flags)`, none of which misc has). **Verdict:
retire `grammar/misc/regex.bbnf` as the canonical regex grammar; carry only `octal` into the
engine-wired bootstrap grammar.**

## VERDICT

**RAN_MEASURED — FIRST ARTIFACT for the grammar-driven path.** The bootstrapped generated parser
returns an `Hir` (the discard is already broken in `mod.rs`), that Hir threads into the XL-C
`simplify_hir` bridge, and it **DFA-matches the hand parser on 43/45 both-parsed patterns** (raw and
bridge alike) — clearing the ≥30 gate and moving SPEC-ONLY(grammar-driven) from **un-attempted to
measured-feasible**. It is **not** full parity: the exact bootstrap-parity gap (LOCK-7A) is **11
bounded-repeat rejects + 2 divergences (bare `.`, in-class `\x`)**, all one concrete blocker —
`host.rs` drift from the regenerated parser's span/variant contract on directional-skip escapes,
bare-string meta-atoms, and braced quantifiers. The de-dup surface is quantified: **1 promote, 21
delete, 3 relocate** ⇒ retire `grammar/misc/regex.bbnf`.

**Verdict line:** `RAN · grammar-driven path already wired (parse_generated→host::lower→Hir→DFA) ·
RAW 43/45 & BRIDGE 43/45 DFA-equivalent (≥30 gate MET) · 11 {n,m} rejects + 2 diverge (dot→[.],
in-class \x→literal) = ONE host.rs↔regen span/variant desync · de-dup: 1 promote / 21 delete / 3
relocate → retire grammar/misc/regex.bbnf · FIRST ARTIFACT: SPEC-ONLY(grammar-driven) →
MEASURED-FEASIBLE-with-named-gap.`

## CAVEATS (honest boundaries)

1. **Not clean 45/45.** The ≥30 gate is met (43), but 13/56 patterns exercise a broken construct;
   the "measured-feasible" claim is *feasibility with a mechanical residual*, not *shipped parity*.
2. **Bare anchors untested.** The 56-corpus has no bare `^`/`$` pattern, so anchor mis-lowering is
   *inferred* from the shared `atom_0`-dead-code mechanism, not directly measured. A `/foo$/`-class
   pattern would confirm it (the brief's "anchored" tag is not represented in the actual set).
3. **Scratch reuse.** Built on the pass-3 5C `pt-scratch` (same session, config already repointed);
   `bbnf_regex` debug rlib was cached, only `parse_that` + the two test binaries compiled. Original
   parse-that never mutated; the two scratch edits (a `parse_generated_ast_debug` probe fn in
   `mod.rs`, two `tests/*.rs`) are in `PROTO-2-P2.patch`, additive and READ-ONLY-safe.
4. **host.rs fix scoped, not attempted.** Re-syncing the fold (recognize `class_atom_0`-carried
   meta-atoms; read the sigil from the source not the trimmed span; strip braces in
   `parse_braced_quantifier`) is a GESTALT-P implementation task, out of this probe's parsimony
   budget.

## FILES

- `PROTO-2-P2.md` — this deliverable.
- `PROTO-2-P2-run.txt` — the oracle run (45-pair table, 11 rejects, 2 divergent HIR dumps) + the
  raw-AST attribution dump.
- `PROTO-2-P2.patch` — unified diff vs READ-ONLY parse-that: `regex/mod.rs` (+`parse_generated_ast_debug`
  probe), `tests/grammar_driven_regex.rs` (parity oracle), `tests/raw_ast_dump.rs` (attribution).
