# SK-V18 G5/G6 — RESEARCH + PLAN: neutral scan retarget (R-F A, §8)

Wave: G5/G6 (ONE wave; §8). Entry gate (GREEN, verified below): **P1 ∧ P3 ∧ G3 closed ∧
the S-P1 94.1% hot-leaf measurement**. PARALLEL to G4/PROVE (hangs off G3, NOT under G4).
Host: aarch64 / Apple M-series. Discipline: **dav1d** — scalar-reference FIRST, checkasm
differential BEFORE wiring, aarch64 NEON/dotprod ONLY (no x86 — P1 deleted it; no SVE).

> **HEADLINE FINDING (read first).** The G6 retarget the SPEC §8 describes — wiring the
> existing neutral `bbnf-simd` eq-set NEON kernel into `css_balanced_component_scan`'s inner
> skip-scan — **is already physically present in the source tree, landed by G2.** The kernel
> `runtime_simd::find_css_significant` (`runtime_simd.rs:169`) IS the two-fan OR-reduce over
> `bbnf_simd::prim::byte_class_from_eq_set_64`, and the live generated CSS scan
> (`generated.rs:517` → `runtime_scan.rs:62,92`) ALREADY calls it on the benched parse path.
> What S-P1 found "dead" (a NEON kernel reachable only from a `#[cfg(test)]` caller) **G2
> wired live.** Therefore G6's remaining work is NOT the wiring — it is the **dav1d
> VERIFICATION layer that was skipped**: (1) the checkasm differential currently lives as an
> inline `#[cfg(test)]` micro-test in `lib.rs` over 5 toy strings, NOT in
> `bbnf-simd/tests/checkasm_parity.rs` over the REAL 71KB–495KB corpora; (2) the
> `simd_admission_profile_sampled` runtime-reachability proof has never been captured; (3) the
> §8 caller-census predicate is literally mis-aimed at `runtime_simd::find_…` in `generated.rs`,
> but the live call site in `generated.rs` is `css_balanced_component_scan` (the primitive), with
> the `runtime_simd` call one layer down in `runtime_scan.rs`. **And the realism check fails
> hard: mean inert run is 9–14 bytes (median 7–9), so the 64-byte NEON fast path productively
> fires on ~0% of bytes (animate), 2.5% (bootstrap), 24.6% (material).** The retarget is
> correct and neutral but the realized acceleration is small and corpus-skewed. G6 is
> achievable as a CORRECTNESS+admission gate (its §8 pre-H1 mandate), but needs a small
> **AMENDMENT** to (a) redirect the census predicate to the primitive, and (b) record the
> realism caveat as the honest framing for the H1 speedup figure. Details below.

---

## 1. CURRENT-STATE MAP

### 1.1 The live CSS hot scan (post-G2, post-P3)

`crates/runtime/src/runtime_scan.rs` (171 LOC) holds the §6 named primitive
`css_balanced_component_scan` and its inner-recursive twin `css_consume_balanced`. The scalar
SHELL is CSS-scoped (returns offsets, emits nothing — neutrality FORCED, per G2 AMENDMENT).
Its structure:

- `css_balanced_component_scan(bytes, pos, delimiters, skip)` — the outer scan. Loop:
  1. `pos = find_css_significant(bytes, pos, delimiters, skip)` — **skip the inert run to the
     next significant byte** (line 62). THIS IS THE INNER SKIP-SCAN = the G6 seam.
  2. if `delimiters.contains(byte)` → return the STOP hit.
  3. else `pos = dispatch_structural(...)` — the scalar shell dispatches the structural byte.
- `css_consume_balanced(...)` — the recursive half (nested `()[]{}`); ALSO calls
  `find_css_significant` for its own inert skip (line 92), with the close byte as caller data.
- `dispatch_structural` — recurses on `( [ {`, skips strings (`consume_string`) / comments
  (`consume_comment`), rejects unexpected closes. **Stays fully scalar** (the cold tail).

The 9-byte structural SKIP family is `SkipAlphabet = [u8;9]` = `' " / ( ) [ ] { }`
(`generated.rs:27` `SKIP_ALPHABET`). The STOP `delimiters` arrive as caller data per call site
(`b";{}"`, `b"{};"`, `b";}"`, `b":{};"`), grammar-derived by `codegen::lower::css_scan`.

### 1.2 The inner skip-scan kernel — ALREADY the retargeted NEON kernel

`crates/runtime/src/runtime_simd.rs:169` `find_css_significant(bytes, from, delimiters, fixed:&[u8;9])`:
- Splits the **≤13-byte significant set** (`fixed` 9 + `delimiters` ≤4) into **two ≤8 eq-set
  fans**: `set_a = fixed[..8]`, `set_b = fixed[8] ++ delimiters` (`:184-192`). This is the
  exact "two-fan OR-reduce salvaged byte-exact from the dead `find_css_significant:180-204`"
  that §8 describes — except it is no longer dead; it IS the live skip.
- 64-byte block loop (`:195-204`): `mask = byte_class_from_eq_set_64(block, set_a) |
  byte_class_from_eq_set_64(block, set_b)`; on `mask != 0` returns
  `cursor + mask.trailing_zeros()`. **Stops AT the first significant byte** so the scalar shell
  handles recursion/strings/errors — exactly the §8 "skip stops AT `([{'"/`" contract.
- Scalar tail (`:207-214`) for `< 64` remaining bytes — the dav1d-required scalar fallback.

The underlying primitive `bbnf_simd::prim::byte_class_from_eq_set_64` (`bbnf-simd/src/lib.rs:282`)
dispatches `#[cfg(target_arch="aarch64")]` → `byte_class_from_eq_set_64_neon` (the
`vceqq_u8`/`vorrq_u8` four-stripe fan, `aarch64/byte_class_from_eq_set_64.rs`), else the scalar
reference. **No x86, no SVE.** Set-size cap is `≤8` (`debug_assert`, `:283`) — which is exactly
why the ≤13-byte CSS set needs the two-fan split.

`bbnf-simd/src/lib.rs:208` `find_ascii_set_member64` is the thinner single-set (`≤8`) variant of
the same loop. `find_css_significant` is morally `find_ascii_set_member64` with the OR of two
fans; it could be re-expressed as two `find_ascii_set_member64` calls min-reduced, but the
current single-loop two-fan form is tighter (one block load, two classifies) and is the right
shape.

### 1.3 The dead R7 kernels (S-P0 finding) — what is dead vs. live NOW

S-P1 (`a2-css-hot-leaves.md:83-100`) found `find_css_significant` and `find_comment_close` were
"dead": reachable ONLY from `lib.rs`'s `#[cfg(test)] mod tests` (`neon_*_matches_scalar` at
`lib.rs:544/567/597`), and written for the THEN-hot `find_component_delim`/`consume_balanced_at`
(pre-G2 generated functions). **That profile predates G2.** Post-G2:

- `find_css_significant` — **LIVE.** Called by `runtime_scan.rs:62,92`, reached from
  `generated.rs:517`, on the `rich_summary`/`parse_full` benched path. No longer dead.
- `find_comment_close` (`runtime_simd.rs:112`) — **STILL DEAD on the product path.** Census:
  the only non-test callers are `lib.rs:580,590` — both inside `#[cfg(test)] mod tests`. The
  live comment skip is the SCALAR `runtime_scan.rs::consume_comment` (`:133`), NOT this NEON
  kernel. `find_comment_close` is a genuine R7/R11 orphan. (Comments are cold: S-P1 never
  surfaced a comment leaf above the `≥5` threshold.)
- `count_top_level_commas` (`runtime_simd.rs:29`) — **LIVE.** Called by `generated.rs:805` →
  `:207` (`selector_count`), on the rich path. Rides `bracket_depth_mask_64` +
  `byte_class_from_eq_set_64`. Already wired, already neutral (caller-data bracket sets).

### 1.4 The checkasm differential — present but MIS-SHAPED

The ONLY existing differential for `find_css_significant` is `lib.rs:544`
`neon_significant_skip_matches_scalar`, an **inline `#[cfg(test)]` test** asserting
`find_css_significant == significant_ref` (scalar ref at `lib.rs:488`) over **5 micro-corpora**
(`:546-552`, longest ~70 bytes). This **violates** three §8/dav1d requirements:
- It is **inline in `src/lib.rs`**, not in `bbnf-simd/tests/checkasm_parity.rs` (also trips the
  repo's `no-inline-tests` discipline).
- It runs over **micro-cases**, NOT the **REAL 71KB–495KB corpora** §8 mandates ("micro-case-only
  PASS does NOT satisfy this").
- It exercises only `find_css_significant` in isolation, NOT the three retarget seams over the
  recursive SHELL: (a) the ≤13-byte two-fan OR-reduce; (b) the skip-stop boundary; (c)
  error-position reproduction from the scalar shell.

The mature checkasm harness lives at `bbnf-simd/tests/checkasm_parity.rs` (signal-guarded,
injection-falsifier, alignment sweep, corpus parity) — but it currently parity-checks the JSON
`SimdClassifier` against `classify_reference` over **JSON** fixtures only (via
`test-fixtures::load_available_bench_fixtures`, which is JSON-only). There is no CSS-corpus
differential for `find_css_significant` anywhere.

### 1.5 The admission census + profile harness

- Live caller in `generated.rs`: `css_balanced_component_scan(...)` at `generated.rs:517`
  (NOT a literal `runtime_simd::find_…` — see §3 census wrinkle).
- The samply admission harness exists: `css_canon_bench` `CSS_CANON_PROFILE=track1_rich` mode
  (`css_canon_bench.rs:196-222`) tight-loops the real corpora for samply attribution.
- `simd_admission_profile_sampled` has **never been captured** — no evidence the `runtime_simd`
  / `byte_class_from_eq_set_64_neon` entry carries non-zero self-time in a `css_canon_bench`
  sample. This is the binding §8 second conjunct and is currently UNPROVEN.

### 1.6 G5 target — `json/scan.rs` is zero-sampled

`crates/runtime/src/grammars/json/scan.rs` (337 LOC) is STILL PRESENT (G1 did not fold it).
Public API: `scan_structurals`, `scan_structurals_scalar`, `scalar_parity_report`,
`structural_capacity_for`. Product-path census:
- `parse_direct` (the benched JSON hot path) does **NOT** reference scan.rs at all.
- `scan_structurals` / `scan_structurals_scalar` / `scalar_parity_report` — consumed ONLY by
  `bbnf-bench/src/scan.rs` (a standalone probe harness), never by the parser.
- `structural_capacity_for` — the ONLY parser-path consumer (`parser.rs:18`, for tape capacity
  planning). Not a hot scan; a one-shot sampled-density estimate.

So scan.rs's heavy NEON structural index (`scan_structurals`) is confirmed zero-sampled
(matches S-P1 "json/scan.rs appears ZERO times in the JSON profile"). Only the lightweight
capacity probe is live.

### 1.7 Entry-gate verification (GREEN)

- **P1 closed** — `rg 'target_arch *= *"x86'` over `crates/bbnf-simd/src` + `crates/runtime/src`
  = 0 hits. (Two crate-wide residuals exist OUTSIDE the SIMD/runtime surface:
  `passes/src/lib.rs:374` cfg-detection bool and `bbnf-bench/benches/json_parity.rs:490` a
  cfg-gated bench arm — neither is a kernel; note for P1/H1 hygiene but they do not block the
  single-arch retarget seam.) No `sve` anywhere. GREEN for the kernel surface.
- **P3 closed** — `lib.rs:16-26`: the 7 css_l4 replicas collapsed to the SINGLE
  `css_l4_declaration_values` config; G2-LEDGER `g2_css_replica_singular = true`. The scan call
  site is singular (`generated.rs:517`, ONE site).
- **G3 closed** — G3-LEDGER: `RuntimeEmitterKind` deleted; `lower_for_runtime` shape-dispatches
  `SinkOnly | Scan`; the CSS `Scan` variant emits the `css_balanced_component_scan` call through
  the ONE un-forked `render`. The `runtime_simd` call is emitted by the un-forked emitter.
- **S-P1 profile present** — the 94.1%/79.5% hot-leaf measurement is the standing WIRE mandate.

---

## 2. THE RETARGET DESIGN (already realized — documented + the verification gap)

The §8 design is: vectorize the inert-run skip of `css_balanced_component_scan`; the byte-set is
caller data; the vector skip stops AT the structural/significant bytes so the scalar shell keeps
recursion/strings/errors. **This is exactly what `find_css_significant` does today.** The design
is sound and present. Concretely the three seams §8 enumerates are realized as:

1. **≤13-byte set vs. 8-byte eq-set cap → two-fan OR-reduce.** `runtime_simd.rs:184-199`:
   `set_a = fixed[..8]` (the 8 structural openers/closers/`/`), `set_b = fixed[8] ++ delimiters`
   (the 9th structural `}` plus the ≤4 caller STOP bytes). `mask = class(set_a) | class(set_b)`.
   Both sets ≤8 → both legal eq-set fans. The set is CALLER DATA: `delimiters` from the grammar,
   `fixed`/`SKIP_ALPHABET` the recognizer's structural constant. `significant_set_is_caller_data
   = true`.
2. **Skip stops AT `([{'"/`.** `mask.trailing_zeros()` returns the FIRST significant byte; the
   scalar `dispatch_structural` then handles it. The NEON never consumes a structural byte.
3. **Error positions from the scalar shell.** All `ScanError` (`unclosed comment`,
   `unexpected closing delimiter`, `unclosed string`) originate in the scalar
   `dispatch_structural`/`consume_*`; the NEON only returns an inert-skip index. `Result` flows
   through the scalar shell unchanged.

**The scalar reference** (dav1d "scalar IS the spec"): `significant_ref` (`lib.rs:488`) is the
byte-exact scalar twin of `find_css_significant`'s skip. The whole-scan scalar reference is the
`css_balanced_component_scan` algorithm itself — but to test the RETARGET we need the scalar
reference for the inner skip plus the shell, which is what the corpus differential below
exercises end-to-end (`css_balanced_component_scan` must equal a fully-scalar reference scan over
the corpora, with NEON forced on vs. NEON bypassed).

**Candidate B (balanced-consume bitmap)** — DOCUMENTED, not built. If a post-G6 samply shows the
`consume_balanced_at`/`css_consume_balanced` recursion tail dominates, upgrade to a
`bracket_depth_mask_64`-driven balanced consume (the `count_top_level_commas` pattern already
proves the primitive). Build ONLY on a measurement trigger. **Candidate C (table-classifier
unify)** — REJECTED (lo6-collision + JSON↔CSS coupling), per §8.

---

## 3. ADMISSION-AT-HOT-PATH (`acceleration_at_admission == admission`)

§8 requires BOTH conjuncts:

**(i) Source caller census — `simd_admission_caller` non-empty, NOT `#[cfg(test)]`.**
The live caller chain is:
`generated.rs:517 css_balanced_component_scan` → `runtime_scan.rs:62/92 find_css_significant`
→ `runtime_simd.rs:199 byte_class_from_eq_set_64` → `aarch64/...neon`.
**CENSUS WRINKLE (must be amended).** §8's literal predicate is
`rg runtime_simd::find_… skinny/crates/runtime/src/grammars/*/generated.rs` NON-EMPTY. But the
live `generated.rs` call site is `css_balanced_component_scan` (the §6 primitive), NOT a literal
`runtime_simd::find_…` — the `runtime_simd` call is one layer down in `runtime_scan.rs`. The §8
predicate, taken literally, returns EMPTY and would FALSE-RED a correctly-wired kernel. This is a
consequence of the G2 §6 primitive interposing between the generated code and the kernel (a GOOD
neutrality move — the generated code names the primitive, not the kernel). **AMENDMENT:** the
census predicate must target the live call CHAIN, i.e. the generated `css_balanced_component_scan`
call site at `generated.rs:517` (non-empty, non-test) AND the primitive's `find_css_significant`
call in `runtime_scan.rs:62/92` (non-empty, non-test). The §8 intent — "a `generated.rs` hot-loop
caller, NOT a `#[cfg(test)]` caller; the L6 census target MOVES off `lib.rs:574`" — is SATISFIED:
the live caller is `runtime_scan.rs` (product path), the dead test caller is `lib.rs:556`. The
predicate string just needs widening to the primitive name.

**(ii) Runtime-reachability profile — `simd_admission_profile_sampled == true`.**
Capture a samply sample of `css_canon_bench` `CSS_CANON_PROFILE=track1_rich` and assert the
`runtime_simd::find_css_significant` / `byte_class_from_eq_set_64_neon` entry carries **non-zero
self-time**. RED predicate: `self_time_samples == 0` (entry absent or zero attribution) →
`acceleration_at_admission == dead`. This conjunct is the BINDING one (defeats a dead/unreachable
`generated.rs` hit) and has NEVER been captured — it is G6's primary new evidence.
**Realism caveat (see §6):** because the inert runs are short, the `find_css_significant` 64-byte
fast path may carry only SMALL self-time; the gate requires PRESENCE-with-attribution, not a
magnitude, so a small non-zero self-time still GREENs. But if the inert runs are SO short that
the function inlines fully into the scalar tail and the NEON block never executes on these
corpora, the entry could show zero NEON self-time → the honest disposition is then "wired,
correct, but profile-cold" (an `S`/amendment surfacing, not a false-green).

---

## 4. G5 — `json/scan.rs` disposition

Zero-sampled (S-P1; §1.6 confirms). §8 wants `retired | neutralized`, no JSON classifier
authored. Cheapest honest disposition: **`neutralized`**, NOT a full retire.

- The hot/heavy `scan_structurals` (the full NEON structural index) and its bench-only probe
  consumers (`bbnf-bench/src/scan.rs`) can be retired IF the probe harness is also removed; but
  that bench probe is a measurement asset (the JSON structural-scan parity probe), so deleting it
  is collateral. Prefer: **keep `structural_capacity_for`** (the live, light capacity estimator —
  it is NOT a hot scan and authors no classifier) and **neutralize** the dead heavy entries by
  either (a) folding them onto the shared `bbnf-simd::scan_dispatch` surface (no bespoke JSON
  classifier — they already ride `SimdClassifier`), or (b) marking the bench-only probe path as
  such and asserting it is OFF the product parse path.
- `json_scan_rs_neutralized = neutralized`; outcome `N` (nothing on product path). The G5
  falsifier (author a JSON classifier with no hot consumer → REJECT) is AVOIDED by authoring
  NOTHING. `json_guard_held` = JSON 51/51 within ±1.0% of `SK-V18-open` — held trivially since
  the product parse path (`parse_direct`) never touched scan.rs.

**Honest minimal G5:** assert (census) that no `scan_structurals` call reaches `parse_direct`;
fold the heavy entries onto the shared surface or gate them behind the probe; keep
`structural_capacity_for`. ≤30 LOC, LOW. This is the "G5 folds in, LOW" of §8.

---

## 5. THE TIMED-PLANE BINDING (CH2-V10 fold)

G6's pre-H1 gate is **CORRECTNESS + admission ONLY**:
- `checkasm_differential == PASS` (the three seams bit-exact over real corpora) — the CORRECTNESS
  plane. G6 reports only PASS/FAIL here.
- `neon_significant_skip_matches_scalar == PASS` over the REAL 71KB–495KB corpora.
- `acceleration_at_admission == admission` (both conjuncts).

`g6_speedup_median_mbps` is **null pre-H1** — DEFERRED to the H1 corpus-in-timer symmetric
`css_canon_bench` (cold, real corpus in the timed region, no warm micro-fixtures, no more-work
competitor), under the §5-risk-7 QUIET-recapture caveat (`host_loadavg < 1.0`). **Any Mbps figure
emitted from the checkasm plane is an addendum-5 plane-mismatch REJECT.** G6 outcome = `C`
(correctness proven, speedup deferred) → `A` on the H1 figure, `R` on miss. **Because of the
realism finding (§6), the honest H1 expectation is a SMALL, corpus-skewed speedup** (material >
bootstrap ≫ animate/tailwind), possibly within noise on some corpora — H1 must frame it as such,
not as a 79.5%-share win.

---

## 6. FEASIBILITY VERDICT + REALISM CHECK (the load-bearing honesty)

**Correctness/neutrality feasibility: YES, already realized.** The existing eq-set NEON kernel
fits the `find_component_delim`/`css_balanced_component_scan` skip shape exactly (two-fan
OR-reduce, ≤8 cap respected, stop-at-significant, scalar shell owns recursion/errors). The 9-field
cssparser parity holds (G2-LEDGER `g2_cssparser_9field_parity = EXACT`, 4/4 corpora). >SOTA holds
(G2-LEDGER: all 4 corpora >1.0× vs lightningcss). The retarget preserves the rich AST. No
regression introduced by the wiring (it was landed at G2 with these gates green).

**REALISM CHECK — does the NEON inner-skip ACTUALLY help? Mostly NO; corpus-skewed.**
Measured significant-byte density (`{ } ; ( ) [ ] : ' " /`) over the four real corpora:

| corpus      | bytes  | sig bytes | mean run | median | p90 | max | inert bytes in runs ≥64B |
|-------------|--------|-----------|----------|--------|-----|-----|--------------------------|
| animate     | 71750  | 7966      | 8.0      | 7      | 17  | 60  | **0.0%** (0 runs ≥64)    |
| tailwind    | 179631 | 18346     | 9.8      | ~8     | —   | —   | low (dense)              |
| bootstrap   | 232803 | 19720     | 10.8     | 9      | 23  | 191 | **2.5%** (57 runs ≥64)   |
| material    | 495454 | 34644     | 13.3     | 7      | 33  | 670 | **24.6%** (1177 runs ≥64)|

The NEON 64-byte fast path in `find_css_significant` (`:195`) only fires PRODUCTIVELY when an
inert run is ≥64 bytes (otherwise the FIRST block already contains a significant byte and the
function returns on block 0 — equivalent to the scalar tail with extra setup cost). With median
runs of 7–9 bytes and p90 of 17–33, the **64-byte path productively skips ~0% of bytes on
animate, 2.5% on bootstrap, and 24.6% on material** (the material tail is base64 data-URIs /
long `content:` strings / font glyph lists). So:
- The realized acceleration is **small and entirely corpus-dependent**. On animate/tailwind it
  is plausibly within noise or net-negative (the two-fan classify per block is more work than a
  short scalar walk). On material it is the only corpus where the skip pays.
- This is precisely the S-P1 "realized-speedup-is-measurement" caveat made concrete. The 79.5%
  self-time share is REAL, but the share is dominated by the `delimiters.contains(&byte)`
  membership test + the `match byte` dispatch + the byte LOAD (S-P1: generated.rs:663/664/667),
  i.e. the PER-SIGNIFICANT-BYTE work, NOT the inert run between them. **Vectorizing the inert
  run does not address where the 79.5% actually goes** — the time is in handling the frequent
  significant bytes, not in skipping the (short) gaps.

**Implication for the design.** The §8 inner-skip vectorize is the RIGHT first move under
neutrality (it is the only genuinely grammar-neutral seam), and it is correct, but it will not
realize the 79.5% headroom. The headroom lives in the per-significant-byte dispatch, which is
NOT a neutral primitive (it is the CSS recognizer's `match byte`). The honest forward path is:
G6 lands the correctness+admission gate on the inner skip (its mandated pre-H1 deliverable),
H1 reports the (small, corpus-skewed) figure HONESTLY, and **Candidate B / a future tranche**
revisits whether a `bracket_depth_mask_64`-style balanced-consume bitmap (which DOES touch the
per-byte dispatch density) can pay the larger share — but that is a measurement-triggered upgrade,
not G6's scope.

**Achievable as-specified? With a small AMENDMENT.** The wiring/correctness/neutrality is
achievable and largely DONE. Three honest amendments:
1. **Census predicate** (§3): widen from `rg runtime_simd::find_…/generated.rs` to the live
   chain `css_balanced_component_scan` (generated.rs) + `find_css_significant` (runtime_scan.rs),
   both non-test. The §6 primitive correctly interposes; the literal predicate is mis-aimed.
2. **Differential relocation** (§1.4): move the micro-case `neon_significant_skip_matches_scalar`
   out of `lib.rs`'s inline test module into `bbnf-simd/tests/checkasm_parity.rs` (or a runtime
   `tests/` file) and re-target it to the REAL 71KB–495KB CSS corpora over the recursive SHELL —
   the existing test does NOT satisfy §8's "not micro-cases" clause.
3. **Realism caveat → H1 framing** (§6): record that the speedup is small/corpus-skewed and that
   G6's value is correctness+admission, not magnitude. This is an honesty surfacing, not a gate
   relaxation — G6's pre-H1 gate never claimed magnitude.

`find_comment_close` (the surviving dead R7 kernel, §1.3): per §8 task (5), DELETE it (comments
are cold, the live skip is scalar `consume_comment`) — do NOT retarget it (retargeting to the
comment-consume is unsafe/unjustified at zero profile share). Salvage nothing (the set-split is
already salvaged into the live `find_css_significant`).

---

## 7. IMPLEMENTATION STEP PLAN (concrete edit sequence)

dav1d order — scalar-ref confirm → checkasm parity → (wire already done) → admission census.

1. **Scalar-reference confirm (dav1d step 1).** Confirm `significant_ref` (`lib.rs:488`) is the
   byte-exact scalar twin of `find_css_significant`'s skip, and that
   `css_balanced_component_scan` with NEON forced ON equals a fully-scalar reference scan. No new
   scalar code needed — the references exist; promote them out of the test module into the
   parity harness location.

2. **Checkasm differential over REAL corpora (dav1d step 2 — BEFORE relying on the wire).**
   Author `neon_significant_skip_matches_scalar` in `bbnf-simd/tests/checkasm_parity.rs` (or a
   runtime integration test) that:
   - loads the four `css-l4-sk-v14` corpora (71KB–495KB) — needs a path/dev-dep to
     `bbnf-bench::css_l4_corpus::corpus_dir` or an `include_bytes!` of the four files;
   - asserts `find_css_significant == significant_ref` at EVERY `from` over each corpus for each
     of the four caller delimiter sets (`b";{}"`, `b"{};"`, `b";}"`, `b":{};"`);
   - asserts the END-TO-END `css_balanced_component_scan` (NEON) == fully-scalar reference scan
     (the recursive shell), covering the three seams: (a) two-fan OR-reduce, (b) skip-stop
     boundary, (c) error-position reproduction;
   - carries the injection falsifier: mutate one byte of the two-fan set → FAIL (the §8
     parity falsifier). `neon_significant_skip_matches_scalar = PASS`, `checkasm_differential =
     PASS`. Delete the inline `lib.rs:544` micro-test (subsumed).

3. **Wire (already landed at G2 — verify, do not re-wire).** Confirm the single P3-collapsed call
   site `generated.rs:517` routes through `find_css_significant`; `css_consume_balanced` reuses
   the SAME entry (`runtime_scan.rs:92`); the call is emitted by the G3 un-forked emitter ONCE.
   `css_scan_call_site_singular = true`, `significant_set_is_caller_data = true`.

4. **Admission census + profile (dav1d step 4).**
   - Source census (amended predicate, §3): `css_balanced_component_scan` non-empty in
     `generated.rs` (non-test) AND `find_css_significant` non-empty in `runtime_scan.rs`
     (non-test). `simd_admission_caller` = the `runtime_scan.rs:62` call.
   - Profile: run `CSS_CANON_PROFILE=track1_rich css_canon_bench` under samply; assert the
     `find_css_significant`/`byte_class_from_eq_set_64_neon` entry has non-zero self-time.
     `simd_admission_profile_sampled = true` → `acceleration_at_admission = admission`. If the
     entry is profile-cold (zero NEON self-time because runs never reach 64B on these corpora),
     surface as `S`/amendment per §6 — do NOT false-green.

5. **G5 — neutralize `json/scan.rs` (folds in, LOW).** Census-prove no `scan_structurals` reaches
   `parse_direct`; fold the heavy entries onto the shared `bbnf-simd` surface or gate behind the
   probe; keep `structural_capacity_for`. `json_scan_rs_neutralized = neutralized`.

6. **G6 — delete the dead `find_comment_close`** (and its `lib.rs:567` test) — comments cold,
   scalar `consume_comment` is the live path. No `#[cfg(test)]`-only NEON kernel survives.

7. **Exit-gate telemetry (§8 schema) — emit + consume:**
   `acceleration_at_admission=admission` ∧ `simd_admission_caller`=non-empty ∧
   `simd_admission_profile_sampled=true` ∧ `neon_significant_skip_matches_scalar=PASS` (real
   corpora) ∧ `checkasm_differential=PASS` ∧ `css_scan_call_site_singular=true` ∧
   `significant_set_is_caller_data=true` ∧ `g6_speedup_median_mbps=null` (pre-H1) ∧
   `json_scan_rs_neutralized=neutralized` ∧ `json_guard_held=true` (51/51 ±1.0% of SK-V18-open).
   Outcome `C` (pre-H1) → `A` at H1 on the corpus-in-timer figure (framed per §6), `R` on miss.

8. **Revert slice (one unit):** the relocated checkasm test + the `find_comment_close` deletion +
   the G5 neutralization + (if re-wiring is touched) the call-site; restore `SK-V18-open` RESULTS;
   REDRESS names the seam that failed (≤13-byte salvage / skip-stop boundary / error reproduction
   / caller census / profile attribution).

**Caps:** MED-HIGH; redress 45 min (G5 LOW). G6 does NOT block PROVE (parallel); G6 ∧ PROVE gate H1.

---

## CROSS-REFERENCES

- `crates/runtime/src/runtime_scan.rs:52,62,80,92,109` — the §6 primitive + inner skip + dispatch.
- `crates/runtime/src/runtime_simd.rs:169` — `find_css_significant` (the live retargeted NEON skip).
- `crates/runtime/src/runtime_simd.rs:112` — `find_comment_close` (DEAD R7; delete).
- `crates/bbnf-simd/src/lib.rs:208,282` — `find_ascii_set_member64` / `byte_class_from_eq_set_64`.
- `crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs` — the NEON `vceqq/vorrq` fan body.
- `crates/runtime/src/lib.rs:488,544` — `significant_ref` + the inline micro-test (RELOCATE).
- `crates/bbnf-simd/tests/checkasm_parity.rs` — the harness to EXTEND (CSS-corpus differential).
- `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:27,517` — SKIP_ALPHABET +
  the singular live call site (admission census target).
- `crates/runtime/src/grammars/json/scan.rs` — G5 neutralize target (zero-sampled).
- `crates/bbnf-bench/src/bin/css_canon_bench.rs:196` — `CSS_CANON_PROFILE` samply harness.
- `crates/bbnf-bench/src/css_l4_corpus.rs:56` — the 71KB–495KB corpus loader.
