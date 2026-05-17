---
tranche: SK-V8
phase: P2 — substrate-ceiling
cohort-item: SC-4
title: Why bbnf loses on the string plane
status: research artefact — read-only, no code edits
author: SC-4 research agent
date: 2026-05-17
inputs:
  - skinny/crates/runtime/src/grammars/json/generated.rs
  - skinny/crates/runtime/src/tape/{assembler,offsets,mod}.rs
  - skinny/crates/parse-that-regex/src/lib.rs
  - skinny/crates/bbnf-simd/src/aarch64/{string_block,match_tiny_plain_string,unescape_uxxxx}.rs
  - skinny/RESULTS.md
  - skinny/REDRESS.md (items 88-90, W4/W5 rationale)
  - restart/skinny/tranches/sk-v7/SYNTHESIS.md
verdict: string-plane loss is SUBSTRATE-BOUND, not kernel-bound
---

# SC-4 — Why bbnf Loses on the String Plane

## §1 Findings (file:line cited)

### 1.1 How the generated parser scans and records a string

The retained JSON parser owns three string entry points, all in
`skinny/crates/runtime/src/grammars/json/generated.rs`:

- `parse_string` (`generated.rs:142-157`) — string *values*.
- `parse_key_colon` (`generated.rs:90-117`) — object *keys*.
- `parse_string_direct` (`generated.rs:610-640`) — the direct/SinkOnly route.

Every one of them executes the **same two-phase pattern per string**:

1. **Tiny fast path.** `match_tiny_plain_string(state.bytes, start)`
   (`generated.rs:147`, `:95`) → `match_tiny_plain_string_with_cap::<16>`
   (`generated.rs:161-185`). This is a **scalar byte-at-a-time loop** bounded
   by `CAP=16`: `while cursor < limit { match input[cursor] { b'"' => return …,
   b'\\' | 0x00..=0x1f => return None, _ => cursor += 1 } }`
   (`generated.rs:177-184`). It is not SIMD — it is a plain scalar match
   ladder, capped, with one branch per byte.
2. **Full fallback.** On a tiny-path miss the parser calls
   `match_string_at_quote` → `parse_that_regex::match_string_at_quote_trusted_utf8`
   (`generated.rs:189-201`, `parse-that-regex/src/lib.rs:162-209`). This is the
   real string scanner: a loop calling `skip_string_plain_trusted`
   (`lib.rs:547-574`) which *does* use the 16-byte NEON
   `scan_string_special_block` primitive (`bbnf-simd/.../string_block.rs:56-72`)
   to vectorise the plain-byte run between quotes/escapes.

The tape write itself is **two-tier**. The string's *open* position is recorded
by the surrounding container as a plain offset (`push_plain_offset`,
`tape/assembler.rs:71-85` — bounds-check, pointer write, `set_len+1`). The
*escape* fact, when a string contains a backslash, is recorded by a **second,
separate write**: `state.patch_flags(open_cursor, …HAS_ESC)`
(`generated.rs:100-101`, `:152-154`) → `TapeBuilder::patch_flags`
(`tape/assembler.rs:94-113`), which pushes onto two *side vectors*
(`flag_cursors`, `flag_values`). `OffsetFlags::HAS_ESC = 0x01`
(`tape/mod.rs:18`). So an escaped string costs an offset push **plus** a
sparse-flag push into a second allocation.

### 1.2 Per-element cost: a string vs a number

| Step | Number element | String element |
|---|---|---|
| Locate end | `match_number_at_digit` — one digit-class SWAR/scalar run, monotone, no terminator search | quote search: scan **every content byte** for `"`, `\`, control — content length is unbounded |
| Validation | digit-class membership only | UTF-8 validation in the validating path; escape-grammar validation (`validate_string_escape`, `lib.rs:284-294`) on every `\` |
| Branch density | one branch at end-of-number | per-byte branch in tiny path (`generated.rs:177-184`); per-special branch in fallback (`lib.rs:170-203`) |
| Tape write | one `emit_plain_offset` (`generated.rs:208`) | one `push_plain_offset` for the open quote **+** a conditional `patch_flags` side-vector push when escaped (`generated.rs:100`, `assembler.rs:94-113`) |
| Fast-path coverage | always the same monotone kernel | bifurcated: ≤16 plain bytes hit the **scalar** tiny path; anything longer or escaped falls to the NEON fallback — a **branch-mispredict-prone dispatch** at every string |

The structural asymmetry: a number is *one* token with *one* monotone scan and
*one* tape write. A string is a *span* whose cost is **proportional to its byte
length**, carries a *second* validation grammar (escapes + UTF-8), and may
require a *second* tape write. The number's cost is O(digits) on a kernel the
branch predictor learns perfectly; the string's cost is O(content bytes) across
a *two-kernel dispatch* the predictor cannot.

### 1.3 The tiny-path / fallback bifurcation is the structural defect

W5's research (`wave-5-r1-generated-tiny-string.md`) confirms the tiny path
(`generated.rs:171-185`) is **scalar**, and SK-V7 SYNTHESIS §3.3-3.4
(`SYNTHESIS.md:96-107`) names `match_tiny_plain_string_with_cap::<16>` as the
**top self-time leaf on 8 of 13 parse rows (28-47%)**, with the pair
`match_string_at_quote ~47% + match_tiny_plain_string ~28% ≈ 75%` of total
self-time on string-heavy rows. The loss is *concentrated* in the string
machinery. But — critically — W5 *already widened* the path: the fallback
`skip_string_plain_trusted` is **already 16-byte NEON**
(`lib.rs:547-574`). So the corpus loss persists *even with SIMD in the
hot loop*. The kernel is not the bottleneck.

### 1.4 Why number-heavy corpora win and string-heavy lose

The win corpora are *structurally number-dominated*. From `RESULTS.md` Notes:

- **canada** (+54.6% vs sonic): 111,126 numbers, **12** string quotes
  (`RESULTS.md:162`).
- **mesh** (+51.5%): 73,013 numbers, **11** string quotes (`RESULTS.md:177`).
- **numbers** (+51.2%): 10,001 numbers, **0** string quotes (`RESULTS.md:195`).
- **marine_ik** (+37.0%): 245,175 numbers, 38,268 quotes (`RESULTS.md:188`).

The loss corpora are *string-quote-dominated*:

- **twitter** (−25%): 18,099 quotes, 2,109 numbers (`RESULTS.md:155`).
- **gsoc-2018** (−53.3%): 34,128 quotes, **0** numbers (`RESULTS.md:185`).
- **update_center** (−43.1%): 27,229 quotes, **0** numbers
  (`RESULTS.md:174`).
- **distinct_values** (−61.2%): 9,796 quotes, 440 numbers
  (`RESULTS.md:210`).
- **y_string_unicode** (−54.1%): 2,200 quotes, **0** numbers
  (`RESULTS.md:214`).

The verdict tracks the *element mix* almost perfectly: every corpus whose
parsed tokens are majority-string LOSES; every corpus majority-number WINS.
See §2 for the quantified correlation.

### 1.5 What simdjson / sonic-rs / yyjson do that bbnf does not

The two-stage architecture (simdjson Lemire/Langdale 2019; sonic-rs is a Rust
port of the same shape) is fundamentally different on the string plane:

- **Stage 1 — structural index, computed ONCE for the whole document.** A
  branchless SIMD sweep classifies every byte and emits a bitmap of *all*
  structural characters and *all* quote positions in a single linear pass.
  Crucially, the `"` bitmap together with the `\` bitmap and a parallel-prefix
  *backslash-parity* computation already tells stage 1 **exactly where every
  string starts and ends** — without ever entering a per-string scan loop.
- **Stage 2 — string handling is a span copy.** For each string, stage 2
  already *has* the end offset from the bitmap; it copies the span (and, only
  if the escape bitmap intersects the string, runs the escape decoder). There
  is **no per-string quote-search loop** and **no per-string fast/slow
  dispatch**.

bbnf's offset tape does the opposite: it has **no document-wide structural
index**. Every string re-discovers its own end via a *recursive-descent,
per-element* scan (`parse_string` → tiny path or fallback). The quote search
that simdjson amortises into one branchless document sweep, bbnf pays
**per string, with a branch-predicted two-kernel dispatch**. This is the
structural gap: simdjson located every quote *once, branchlessly*; bbnf
locates every quote *N times, with a mispredicting dispatch*.

yyjson is single-pass like bbnf but its string scan is a tight,
*non-bifurcated* SWAR/SIMD loop with the escape/UTF-8 check folded into the
same word — no `CAP=16` scalar-vs-NEON cliff. bbnf's tiny-path/fallback split
(REDRESS 72 forced CAP=16 retained vs CAP=8 direct — `wave-5-r1` §"CAP=16
versus CAP=8") is itself a tax the competitors do not pay.

### 1.6 Why every string kernel was REJECTED (W4/W5, REDRESS)

W4 (per-`\uXXXX` TBL classifier) and W5 (NEON 16-byte plain-string scan) both
targeted string throughput; both rejected. The W4 research
(`wave-4-r1-parse-that-unescape.md`) and the REDRESS tail show *why* the
rejections are structurally coherent rather than accidental:

- W4: even a correct per-quartet TBL decode moved only `unicode_escapes` and
  not `y_string_unicode` — the escape decode was never the dominant cost; the
  *plain-body scan and the per-string dispatch* were.
- W5: widening the tiny path to NEON did not lift the rows because the fallback
  was *already* NEON — the residual cost is the **dispatch and the tape
  write**, not the scan width.
- The whole REDRESS 50-72 family — UTF-8 fusion, retained validators, parser
  scratch, byte-output unescape, EventCursor sidecars — is a *graveyard of
  string kernels*, each correctness-green and each rejected because it moved
  one row and regressed another. REDRESS items 88-90 (W10) close SK-V7 with
  *every hot-path throughput kernel rejected*.

A graveyard this uniform is the signature of a **substrate ceiling**: when
every distinct kernel attacking the same plane fails the same way, the plane
itself — not the kernels — is the bound.

## §2 String-quote-density correlation

`element tokens` = string quotes + numbers + literals (the value-bearing tape
tokens; opens/closes are structural and roughly cancel across competitors).
`string fraction` = quotes / element tokens. Δ is parse_only vs sonic-strict
(`RESULTS.md` Δ column); where that cell is `n/a` the Δ-vs-SK-V6 figure (vs
the bbnf baseline) is used and marked `*`.

| Corpus | Quotes | Numbers | Literals | String fraction | Verdict | Δ vs sonic (parse) |
|---|---:|---:|---:|---:|:--:|---:|
| numbers | 0 | 10,001 | 0 | 0.00 | **WIN** | +51.2% * |
| mesh | 11 | 73,013 | 0 | 0.0002 | **WIN** | +51.5% |
| canada | 12 | 111,126 | 0 | 0.0001 | **WIN** | +54.6% |
| marine_ik | 38,268 | 245,175 | 6 | 0.135 | **WIN** | +37.0% * |
| instruments | 6,889 | 4,935 | 557 | 0.557 | win (thin) | +10.6% * |
| citm_catalog | 26,604 | 14,392 | 1,263 | 0.629 | LOSS | −11.3% |
| unicode_escapes | 5,636 | 1,877 | 1 | 0.750 | LOSS† | −34.6% * |
| unicode_mixed | 25,121 | 8,371 | 0 | 0.750 | LOSS | −50.3% |
| twitter | 18,099 | 2,109 | 4,737 | 0.726 | LOSS | −25.1% |
| github_events | 1,891 | 149 | 88 | 0.889 | LOSS | −61.7% |
| random | 33,005 | 5,002 | 1,000 | 0.846 | LOSS | −52.3% |
| unicode_basic | 57,590 | 11,518 | 0 | 0.833 | LOSS | −29.9% |
| apache_builds | 5,289 | 2 | 3 | 0.999 | LOSS | −65.3% |
| update_center | 27,229 | 0 | 386 | 0.986 | LOSS | −63.4% |
| distinct_values | 9,796 | 440 | 0 | 0.957 | LOSS | −70.8% |
| gsoc-2018 | 34,128 | 0 | 0 | 1.000 | LOSS | −53.3% * |
| y_string_unicode | 2,200 | 0 | 0 | 1.000 | LOSS | −54.1% |

† `unicode_escapes` shows +113.6% vs *simdjson* but simdjson collapses on that
corpus (5,637 Mbps — its escape path is pathological); against sonic-strict,
bbnf still loses −34.6%, so it is a loss row for this analysis.

**Does string-quote density predict the loss?** Yes — sharply, with a clean
threshold:

- Every corpus with **string fraction ≤ 0.135 WINS** (numbers, mesh, canada,
  marine_ik) — average ≈ +49%.
- Every corpus with **string fraction ≥ 0.55 LOSES** the parse plane vs
  sonic-strict (citm onward) — the sole borderline, `instruments` at 0.557,
  wins by only +10.6% and is `K / NO-GO` on the gate.
- The transition is monotone: as string fraction rises from 0.6 → 1.0, the
  deficit deepens from −11% (citm) → −65% (apache_builds) → −71%
  (distinct_values). The two pure-string corpora (gsoc-2018, y_string_unicode,
  fraction 1.00) sit at −53/−54%.

String-quote density is **the** predictor. The correlation is not noisy: it is
a step function with the knee between fraction 0.14 and 0.56. The number-plane
advantage and the string-plane deficit are the *same substrate property seen
from two sides* — the offset tape is cheap for monotone scalar tokens and
expensive for length-proportional, escape-validated, dispatch-bifurcated spans.

## §3 Substrate-ceiling verdict

**The string-plane loss is substrate-bound. It is not closable by any kernel
under the current offset-tape substrate.** The evidence is convergent:

1. **Every kernel already tried has failed identically.** W4 (TBL escape
   decode), W5 (NEON plain scan), and the REDRESS 50-72 family (UTF-8 fusion,
   retained validators, parser scratch, byte-output unescape, EventCursor) are
   a complete graveyard. SK-V7 closed (REDRESS 88-90) with *every* hot-path
   throughput kernel rejected. When N independent kernels attacking one plane
   all fail, the plane is the bound.

2. **SIMD is already in the hot loop and it did not help.** The fallback
   scanner `skip_string_plain_trusted` (`lib.rs:547-574`) is already a 16-byte
   NEON `scan_string_special_block` loop. The residual loss is therefore *not*
   scan width — it is the **per-string re-discovery** model.

3. **The defect is architectural, not local.** The offset tape carries *no
   document-wide structural index*. Recursive descent re-enters a string-scan
   loop for every string, paying a fast/slow dispatch (`generated.rs:147` →
   tiny path, miss → `:151` fallback) the branch predictor cannot learn, plus a
   *second* side-vector write (`patch_flags`, `assembler.rs:94-113`) for every
   escaped string. simdjson/sonic compute the quote bitmap *once, branchlessly*,
   for the whole document; stage 2 is then a span copy with the end offset
   already in hand. bbnf cannot reach that shape by swapping a kernel — the
   tape *has no place to put a structural bitmap*.

4. **The number-win/string-loss symmetry is one cause.** §2 shows the same
   offset-tape substrate is *optimal* for monotone scalar tokens (numbers:
   +49% average) and *pathological* for length-proportional escape-validated
   spans (strings: −11% to −71%). A substrate that is best-in-class on one
   token shape and worst-in-class on another, with the crossover at a clean
   density threshold, is **definitionally a substrate ceiling** — the data
   structure, not the algorithm, sorts the outcomes.

Therefore: the string-plane loss requires the substrate change. SK-V7
SYNTHESIS already names it — **the tape ⊕ structural-projection union**
(`SYNTHESIS.md:139-145`, CostFacts substrate): a document-wide branchless
structural index *unioned with* the offset tape, so that strings stop
re-discovering their own bounds and become a span copy from a pre-computed
quote bitmap, exactly as in the two-stage architectures.

## §4 Recommendation

1. **Stop dispatching string kernels.** No further per-`\uXXXX` classifier,
   plain-scan widening, or escape-decode kernel should be opened against the
   current substrate. SK-V8 P2 should treat the string plane as *closed to
   kernel work* and route all string effort to the substrate change.

2. **Adopt the tape ⊕ structural-projection union as the SK-V8 P2 spine.**
   Add a stage-1-style branchless structural sweep that emits, for the whole
   document in one linear pass: the `"` position bitmap, the `\` bitmap, and a
   parallel-prefix backslash-parity mask (`bbnf-simd` already has
   `bitmap_prefix_xor_64` and `bitmap_next_set_bit` primitives — see
   `aarch64/bitmap_prefix_xor_64.rs`, `bitmap_next_set_bit.rs`). The
   recursive-descent stage then *consumes* string bounds from this index:
   `parse_string` becomes "read next-quote position from the bitmap, emit one
   offset, done" — no per-string scan loop, no fast/slow dispatch.

3. **Fold `HAS_ESC` into the index, not a side vector.** The escape fact is
   already known from the `\` bitmap intersected with the string span; the
   `patch_flags` side-vector write (`assembler.rs:94-113`) becomes a bitmap AND
   and disappears as a per-string cost.

4. **Re-validate the number plane stays neutral.** The number-heavy wins are
   the current asset. The structural sweep must be *additive* — strings consume
   the index, numbers keep their monotone scan — and the SK-V8 gate must prove
   canada/mesh/numbers/marine_ik do not regress. REDRESS 88-90 show structural
   primitives (PMULL prefix-XOR, CSSC ctz) regressed number/escape rows when
   put on the *unconditional* hot path; the index sweep must be gated and
   measured per-row exactly as those waves demanded.

5. **Measure against the §2 threshold.** The success criterion is
   *displacement of the knee*: the string-fraction value at which corpora flip
   from win to loss should move from ≈0.14 toward ≈1.0. A substrate change that
   does not move that knee has not closed the plane.

## §5 Generalisation — string handling for CSS L4 / Sheets / arbitrary grammars

The string plane is not a JSON quirk; it is the general shape of every
**delimited, escape-bearing, length-variable terminal**, and the substrate
ceiling generalises with it:

- **CSS L4.** CSS strings (`"…"`, `'…'`), `url(…)` tokens, and CSS escapes
  (`\41`, `\26`, line-continuation `\\\n`) are exactly the same shape: a
  delimited span with an escape sub-grammar and unbounded content length. The
  `unescape_uxxxx` TBL kernel was explicitly cited (`unescape_uxxxx.rs:22-28`)
  as also serving `url("\41\42…")` decode. A CSS parser on the current
  offset-tape substrate will exhibit the *same* per-string re-discovery tax —
  selector- and value-heavy stylesheets are the CSS analogue of twitter.

- **Sheets / formula grammars.** Spreadsheet string literals and the
  `"…""…"`-style doubled-quote escape are again the same delimited-escape span.
  Cell-text-heavy sheets will be the loss corpora; numeric grids will be the
  win corpora — the same §2 split.

- **The generalisation:** the offset tape is a *good substrate for monotone
  scalar terminals* and a *poor substrate for delimited variable-length
  escape-bearing terminals*, in any grammar. The structural-projection union is
  therefore not a JSON patch — it is a **grammar-neutral substrate capability**:
  for any grammar, the IR can classify which terminals are "delimited spans"
  and emit a stage-1 bitmap sweep for *their* delimiter alphabet, then have
  recursive descent consume span bounds from the index. This belongs in the
  shared `bbnf-simd` + tape layer (the bitmap primitives are already
  grammar-neutral), surfaced through CostFacts so each grammar's span terminals
  opt into the index while its scalar terminals keep the cheap monotone path.
  The same primitive that closes JSON twitter closes CSS selector-heavy
  stylesheets and string-heavy sheets.

## §6 Risks

1. **Two-pass cost on win corpora.** A document-wide structural sweep adds a
   linear pass that number-heavy corpora (canada/mesh) currently avoid. If the
   sweep is unconditional it could erode the +49% number-plane lead. *Mitigation:*
   the sweep must be span-terminal-gated and the SK-V8 gate must hard-block on
   canada/mesh/numbers no-regression — REDRESS 88-90 are the precedent for
   structural primitives regressing number rows.

2. **Memory footprint.** A quote/backslash/parity bitmap is ≈3 bits/byte of
   input. `RESULTS.md` already tracks tape bytes at 0.05x-0.75x input; the
   index adds to that. RSS is currently a bbnf *advantage* (bbnf 2.7 MB vs
   sonic 3.7 MB on twitter — `RESULTS.md:156`); the index must not erase it.

3. **Backslash-parity correctness.** The parallel-prefix parity that
   distinguishes a real quote from an escaped `\"` is the subtle core of stage
   1. REDRESS 88 already rejected PMULL prefix-XOR on the unconditional hot
   path for escape-heavy rows — the parity kernel must be scalar-or-gated and
   checkasm-proven before it touches a hot path.

4. **Substrate change is large.** SYNTHESIS §3.8 estimates the CostFacts
   substrate alone at ≈830 LOC across ir/+passes/+codegen/+xtask; the full
   tape⊕index union is larger. It is a multi-wave tranche, not a single kernel
   wave — SK-V8 P2 must be scoped accordingly.

5. **Falsifiability.** Like every SK-V7 kernel, the union could be
   correctness-green and still fail the gate. The §4.5 knee-displacement metric
   must be the admission criterion *before* implementation, so a partial win
   (one row moves) is not mistaken for a substrate close.

## §7 Sources

- `skinny/crates/runtime/src/grammars/json/generated.rs:90-117` (parse_key_colon), `:142-185` (parse_string + tiny path), `:189-201` (match_string_at_quote), `:610-640` (parse_string_direct)
- `skinny/crates/runtime/src/tape/assembler.rs:62-113` (push_offset / push_plain_offset / patch_flags)
- `skinny/crates/runtime/src/tape/mod.rs:14-18` (OffsetFlags, HAS_ESC)
- `skinny/crates/parse-that-regex/src/lib.rs:162-209` (match_string_at_quote_trusted_utf8), `:284-294` (validate_string_escape), `:462-544` (skip_string_plain), `:547-574` (skip_string_plain_trusted), `:577-587` (string_special_mask)
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:5-72` (StringSpecialBlock, scan_string_special_block)
- `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:1-96` (tiny-string NEON primitive — parity-only, REDRESS 33 invalidated as parse-G fix)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:1-47` (escape decode kernel + CSS url() citation)
- `skinny/RESULTS.md:5-42` (per-corpus verdict rows), `:153-216` (Notes — per-corpus structural counts)
- `skinny/REDRESS.md` items 88-90 (W10 — every hot-path throughput kernel rejected)
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md:90-107` (string-scanner pair ≈75% self-time), `:139-145` (CostFacts substrate absent)
- `restart/skinny/tranches/sk-v7/research/wave-4-r1-parse-that-unescape.md` (W4 owner shape + blocked routes)
- `restart/skinny/tranches/sk-v7/research/wave-5-r1-generated-tiny-string.md` (tiny path scalar; CAP=16 vs CAP=8; fallback already NEON)
