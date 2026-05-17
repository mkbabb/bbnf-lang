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

The win corpora are *structurally number-dominated*. The deltas below are from
the authoritative `RESULTS.md` column order: `Delta vs SK-V6`, then
`Delta vs sonic-strict`, then `Delta vs simdjson DOM`, then `Delta vs yyjson`.
Only the `Delta vs sonic-strict` values are strict same-run sonic evidence:

- **canada** (+27.9% vs sonic-strict; +54.6% is vs simdjson DOM sidecar):
  111,126 numbers, **12** string quotes (`RESULTS.md:162`).
- **mesh** (+21.4% vs sonic-strict; +51.5% is vs simdjson DOM sidecar):
  73,013 numbers, **11** string quotes (`RESULTS.md:177`).
- **numbers** (+51.2% vs sonic-strict): 10,001 numbers, **0** string quotes
  (`RESULTS.md:195`).
- **marine_ik** (+37.0% vs sonic-strict): 245,175 numbers, 38,268 quotes
  (`RESULTS.md:188`).

The loss corpora are *string-quote-dominated*:

- **twitter** (−25.1%): 18,099 quotes, 2,109 numbers (`RESULTS.md:155`).
- **gsoc-2018** (−53.3%): 34,128 quotes, **0** numbers (`RESULTS.md:185`).
- **update_center** (−43.1%): 27,229 quotes, **0** numbers
  (`RESULTS.md:174`).
- **distinct_values** (−61.2%; −70.8% is vs simdjson DOM sidecar):
  9,796 quotes, 440 numbers
  (`RESULTS.md:210`).
- **y_string_unicode** (−54.1%): 2,200 quotes, **0** numbers
  (`RESULTS.md:214`).

The verdict broadly tracks the *element mix*, but the corrected strict-sonic
columns leave a mixed middle band rather than a universal majority-string
threshold. See §2 for the quantified correlation and caveats.

### 1.5 What simdjson / sonic-rs / yyjson do that bbnf does not

The comparator architecture evidence must be split by engine. simdjson is the
only comparator cited here as retained document-wide stage-1-index evidence:

- **simdjson — retained stage 1, computed once for the whole document.** A
  branchless SIMD sweep classifies every byte and emits a structural/quote
  index spanning the input. The `"` bitmap together with the `\` bitmap and a
  parallel-prefix *backslash-parity* computation identifies string boundaries
  without entering a recursive-descent per-string quote-search loop.
- **simdjson stage 2 — string handling from precomputed spans.** Stage 2 can
  consume the indexed span and run escape decoding only when the escape bitmap
  intersects that span. This is the persistent-index architecture proof.

sonic-rs is a strict same-run performance anchor and a useful local
skip-scan/single-pass comparator, but SC-2 explicitly treats it as **not**
having a persistent document-wide structural index absent exact upstream source
proof. Its relevant lesson is narrower: bounded SIMD skip-scan kernels can
avoid bbnf's tiny-path/fallback shape inside a single-pass consumer. It must
not be cited as proof that a retained whole-document quote bitmap wins.

bbnf's offset tape has **no document-wide structural index**. Every string
re-discovers its own end via a *recursive-descent, per-element* scan
(`parse_string` -> tiny path or fallback). The quote search that simdjson
amortises into one branchless document sweep is paid by bbnf **per string, with
a branch-predicted two-kernel dispatch**. The string-heavy strict sonic-rs rows
show bbnf loses to a fast single-pass skip-scan competitor on product-shaped
JSON, but they do not prove sonic-rs has simdjson's retained stage-1
architecture.

yyjson is single-pass like bbnf but its string scan is a tight,
*non-bifurcated* loop with escape/UTF-8 handling fused into the same walk — no
`CAP=16` scalar-vs-NEON cliff. bbnf's tiny-path/fallback split (REDRESS 72
forced CAP=16 retained vs CAP=8 direct — `wave-5-r1` §"CAP=16 versus CAP=8")
is itself a tax the comparators do not expose in the same way.

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
`string fraction` = quotes / element tokens. Δ is the `parse_only`
`Delta vs sonic-strict` value from `RESULTS.md`, not the simdjson DOM, yyjson,
sidecar, or historical SK-V6 column.

This table is diagnostic, not an admission gate. Rows with same-run
sonic-strict comparator values are the only comparator deltas shown, but the
rows remain guard/planning evidence until W0/W1 also proves strictness,
`parse_utf8`, output plane, run id, and sidecar freshness under the SK-V8 gate.

| Corpus | Quotes | Numbers | Literals | String fraction | Sonic signal | Δ vs sonic-strict |
|---|---:|---:|---:|---:|:--:|---:|
| numbers | 0 | 10,001 | 0 | 0.00 | **WIN** | +51.2% |
| mesh | 11 | 73,013 | 0 | 0.0002 | **WIN** | +21.4% |
| canada | 12 | 111,126 | 0 | 0.0001 | **WIN** | +27.9% |
| marine_ik | 38,268 | 245,175 | 6 | 0.135 | **WIN** | +37.0% |
| instruments | 6,889 | 4,935 | 557 | 0.557 | win (thin) | +10.6% |
| citm_catalog | 26,604 | 14,392 | 1,263 | 0.629 | **WIN**† | +24.6% |
| unicode_escapes | 5,636 | 1,877 | 1 | 0.750 | LOSS‡ | −34.6% |
| unicode_mixed | 25,121 | 8,371 | 0 | 0.750 | LOSS | −50.3% |
| twitter | 18,099 | 2,109 | 4,737 | 0.726 | LOSS | −25.1% |
| github_events | 1,891 | 149 | 88 | 0.889 | LOSS | −34.0% |
| random | 33,005 | 5,002 | 1,000 | 0.846 | LOSS | −36.4% |
| unicode_basic | 57,590 | 11,518 | 0 | 0.833 | LOSS | −26.8% |
| apache_builds | 5,289 | 2 | 3 | 0.999 | LOSS | −28.2% |
| update_center | 27,229 | 0 | 386 | 0.986 | LOSS | −43.1% |
| distinct_values | 9,796 | 440 | 0 | 0.957 | LOSS | −61.2% |
| gsoc-2018 | 34,128 | 0 | 0 | 1.000 | LOSS | −53.3% |
| y_string_unicode | 2,200 | 0 | 0 | 1.000 | LOSS | −54.1% |

† `citm_catalog` is +24.6% vs sonic-strict; its −11.3% value is vs simdjson
DOM and must not be quoted as a sonic-strict loss.

‡ `unicode_escapes` shows +113.6% vs *simdjson* but simdjson collapses on that
corpus (5,637 Mbps — its escape path is pathological); against sonic-strict,
bbnf still loses −34.6%, so it is a loss row for this analysis.

**Does string-quote density predict the loss?** It is a useful diagnostic
pressure signal, but after correcting the comparator columns it is **not** a
clean universal selector or admission threshold:

- Every corpus with **string fraction ≤ 0.135 WINS** (numbers, mesh, canada,
  marine_ik) — average ≈ +34%.
- The mid band is mixed: `instruments` at 0.557 and `citm_catalog` at 0.629
  still win vs sonic-strict, although both remain `K / NO-GO` parse rows and
  citm also loses vs the simdjson DOM sidecar.
- Every corpus in this table at **string fraction ≥ 0.726 LOSES** vs
  sonic-strict, with deficits from −25.1% (twitter) to −61.2%
  (distinct_values). The two pure-string corpora (gsoc-2018,
  y_string_unicode, fraction 1.00) sit at −53/−54%.

String-quote density should therefore remain per-grammar telemetry, not a
generic selector. JSON quote fraction can seed CostFacts/RecognizerFacts for
the JSON plane; other grammars need their own generated span-terminal,
escape-discipline, layout, and scalar-token facts plus per-plane gates. The
number-plane advantage and string-plane deficit are still the same substrate
property seen from two sides: the offset tape is cheap for monotone scalar
tokens and expensive for length-proportional, escape-validated,
dispatch-bifurcated spans.

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
   loop for every string, paying a fast/slow dispatch (`generated.rs:147` ->
   tiny path, miss -> `:151` fallback) the branch predictor cannot learn, plus a
   *second* side-vector write (`patch_flags`, `assembler.rs:94-113`) for every
   escaped string. simdjson computes the retained quote/string-boundary index
   once for the whole document; sonic-rs should be treated only as a strict
   same-run skip-scan/single-pass comparator unless exact source proof upgrades
   that architecture claim. bbnf cannot reach the simdjson retained-index shape
   by swapping a kernel — the tape *has no place to put a structural bitmap*.

4. **The number-win/string-loss symmetry is one cause.** §2 shows the same
   offset-tape substrate is strong for monotone scalar tokens (low string
   fraction: ≈+34% average vs sonic-strict) and weak for
   length-proportional escape-validated spans (high string fraction: −25% to
   −61%). The corrected data no longer supports a universal clean knee, but it
   still sorts outcomes by token shape strongly enough to keep the
   substrate-ceiling diagnosis.

Therefore: the string-plane loss requires substrate work, not another local
string kernel. The work must be scoped in two tiers. **Tier A structural-class
cursor migration** can move structural positions/classes into the singular
`Tape`, but it must not claim to close string-boundary rediscovery.
**Tier B string-boundary / quote-backslash-parity / CostFacts-template union**
is the work that could make strings consume precomputed bounds, and it remains
unselected unless a later plan prices it inside the SPEC cap and verification
budget.

## §4 Recommendation

1. **Pre-block local string kernels absent fresh evidence.** No further
   per-`\uXXXX` classifier, plain-scan widening, or escape-decode kernel should
   be opened against the current substrate unless W0 names fresh hot-leaf
   evidence, the plan names a same-wave consumer and no-regression gate, the
   route cites the REDRESS entries it differs from, and challenge accepts the
   changed frame.

2. **Split the W3 hypothesis before any promotion.** Tier A is only the
   structural-class cursor migration: positions/classes move into the singular
   `Tape`, generated JSON consumers become class-cursor reads, and no retained
   side substrate appears. Tier A must not claim it stops re-walking strings,
   deletes string-boundary work, or closes the string plane.

3. **Keep string-boundary closure in Tier B unless fully priced.** Quote,
   backslash, parallel-prefix parity, string-boundary facts,
   `HAS_ESC`-from-span-mask derivation, and CostFacts/template parity belong to
   Tier B. A later plan may promote them only if they are transient scan state
   or co-indexed tape facts inside the singular `Tape`, include a same-wave
   production consumer that actually stops re-scanning strings, and fit SPEC's
   650-LOC template-parity cap plus scalar/checkasm/gate verification budget.

4. **Re-validate the number plane stays neutral per plane.** The number-heavy
   wins are the current asset. Tier A structural-class migration must prove
   canada/mesh/numbers/marine_ik no-regression without claiming string-boundary
   closure. Tier B, if later admitted, must separately prove the quote,
   backslash, parity, and span-fact work does not put rejected REDRESS 88-90
   primitives (PMULL prefix-XOR, CSSC ctz, CTZ/bulk) on an unconditional hot
   path. Each plane needs its own strict row set, comparator plane, maintain
   budget, and pass/fail rule.

5. **Demote the §2 knee to diagnostic telemetry.** The corrected strict-sonic
   table supports quote fraction as a JSON CostFacts/RecognizerFacts signal,
   not as a universal selector and not as an admission gate. No plan may pass
   or fail on string-fraction displacement unless it first names an in-repo
   command, row set, numeric target, formula, maintain budget, and pass/fail
   rule. Until then, string-fraction movement is explanatory telemetry only.

## §5 Generalisation — string handling for CSS L4 / Sheets / arbitrary grammars

The string plane is not a JSON quirk; it is the general shape of
**delimited, escape-bearing, length-variable terminals**. The selector policy
does not generalise as "JSON quote density", though. It generalises only as
generated per-grammar facts:

- **CSS L4.** CSS strings (`"…"`, `'…'`), `url(…)` tokens, and CSS escapes
  (`\41`, `\26`, line-continuation `\\\n`) are delimited spans with their own
  delimiter alphabet and escape discipline. A CSS candidate must expose those
  facts as generated CostFacts/RecognizerFacts and pass CSS-plane gates; generic
  runtime/SIMD code may consume byte sets and opaque class/fact ids, not
  "JSON-like quote density" or CSS semantic roles.

- **Sheets / formula grammars.** Spreadsheet string literals and the
  `"…""…"`-style doubled-quote escape are also delimited spans, but their
  escape and layout rules are not JSON's. A Sheets candidate needs its own
  generated span density, scalar-token, and escape facts plus a Sheets-plane
  maintain budget. Numeric-grid wins and text-heavy losses are hypotheses to
  measure, not a generic selector rule.

- **The generalisation:** the offset tape is strong for monotone scalar
  terminals and weak for delimited variable-length escape-bearing terminals.
  The grammar-neutral substrate capability is a way to carry generated
  structural classes and admitted facts through the singular `Tape`; the
  generic layer must remain blind to grammar semantics. Tier A can migrate
  structural classes. Tier B may later add span-boundary, quote/backslash, and
  parity facts only when each grammar's generated telemetry and per-plane gate
  justify the cost. No single JSON knee or JSON row set may select a generic
  fast path.

## §6 Risks

1. **Two-pass cost on win corpora.** A document-wide structural sweep adds a
   linear pass that number-heavy corpora (canada/mesh) currently avoid. If the
   sweep is unconditional it could erode the low-string-fraction lead.
   *Mitigation:* Tier A must be per-plane gated and hard-block on
   canada/mesh/numbers/marine_ik no-regression; Tier B must separately prove
   quote/backslash/parity work is justified by generated CostFacts or
   RecognizerFacts. REDRESS 88-90 are the precedent for structural primitives
   regressing number rows.

2. **Memory footprint.** A retained quote/backslash/parity bitmap would be a
   Lock 1 sidecar unless it is replaced by transient masks or by co-indexed tape
   facts inside the singular `Tape`. `RESULTS.md` already tracks tape bytes at
   0.05x-0.75x input; RSS is currently a bbnf *advantage* (bbnf 2.7 MB vs sonic
   3.7 MB on twitter — `RESULTS.md:156`); any retained fact must not erase it.

3. **Backslash-parity correctness.** The parallel-prefix parity that
   distinguishes a real quote from an escaped `\"` is the subtle core of stage
   1. REDRESS 88 already rejected PMULL prefix-XOR on the unconditional hot
   path for escape-heavy rows — the parity kernel must be scalar-or-gated and
   checkasm-proven before it touches a hot path.

4. **Substrate change is large.** Tier A is the narrow structural-class cursor
   migration priced elsewhere; it excludes quote/backslash/parity,
   CostFacts/template parity, and full string-index closure. Tier B owns that
   larger work and is follow-on or multi-wave unless a later W3 plan explicitly
   fits SPEC's 650-LOC template-parity cap and verification budget. SK-V8 P2
   must not sell Tier A as the string-plane close.

5. **Falsifiability.** Like every SK-V7 kernel, the union could be
   correctness-green and still fail the gate. The §2 knee is diagnostic only;
   a partial movement in quote-density telemetry is not a substrate close
   unless a future plan first turns it into an executable command, row set,
   numeric target, formula, maintain budget, and pass/fail rule.

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
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md:148-164` (sonic-rs skip-scan/single-pass comparator posture)
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md:359-437` (Tier A/Tier B scope split)
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md:90-107` (string-scanner pair ≈75% self-time), `:139-145` (CostFacts substrate absent)
- `restart/skinny/tranches/sk-v7/research/wave-4-r1-parse-that-unescape.md` (W4 owner shape + blocked routes)
- `restart/skinny/tranches/sk-v7/research/wave-5-r1-generated-tiny-string.md` (tiny path scalar; CAP=16 vs CAP=8; fallback already NEON)
