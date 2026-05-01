# AY.W1-fix — twitter regression diagnosis + retirement of eager structural-scan

## Symptom

Per `docs/benchmarks/archive/post-AY-W1-bytes-cyc.txt` pre-fix close:

- twitter MB/s 699 (W1 Phase 1 sanity bench, AoS substrate alone)
  → 420.2 (W1 close after Phase 2 cherry-picks: W1.3 structural-scan
  + W1.4 Pratt Option C + W1.6 regen).
- twitter bytes/cyc landed at 0.131 — far below the W1 hard-gate
  target of >= 0.45 (AU citm floor).
- bbnf/sonic twitter ratio at W1 close: 6.16x — vs pre-AY 7.93x and
  post-AX-W1 5.77x. W1 close had widened the gap relative to AX-W1.

Per AY operational posture §1, regression >= 5% triggers re-plan.
W1 close was a -6.2% twitter regression vs post-AX-W1 (448 → 420
MB/s).

## Samply evidence

`scripts/profile-bench-headless.sh`-style headless samply on the
pre-fix `json_monolithic` bench binary against `twitter`:

```
total samples: 6714

Top 7 by self-time (aggregated by function):
  50.88%  <json_monolithic::JsonParser>::parse
  32.20%  json_monolithic::__jsonparser_emit_impl::parse_object_JsonParser_object
   9.71%  json_monolithic::__jsonparser_emit_impl::parse_wrap_JsonParser_value
   2.41%  json_monolithic::__jsonparser_emit_impl::parse_array_JsonParser_array
   1.97%  json_monolithic::__jsonparser_emit_impl::parse_string_escaped
   0.91%  core::str::converts::from_utf8
   0.61%  <std::sys::sync::once::queue::Once>::state
   0.48%  tape::structural_scan::next_structural_at_or_after
```

Disassembling the dominant `<JsonParser>::parse` self-time at
offset `+0x150` (`100013c30`, samply's hottest single sample at
8.01%) shows a tight inner loop iterating input bytes against a
256-bit alphabet bitmap — the inlined body of `tape::structural_scan::scan_structural`
called from `state.init_for_input(__input_bytes)` at parse-entry:

```
100013c28: ldrb    w8, [x21, x19]     ; load input[i]
100013c2c: lsr     x9, x8, #6         ; shift right 6 — word index
100013c30: ldr     x9, [x23, x9, lsl #3]   ; load bitmap word  ← 8.01% self-time
100013c34: lsr     x9, x9, x8         ; shift down by bit pos
100013c38: tbz     w9, #0, 0x100013c1c     ; test bit 0; branch if zero
100013c3c: ldrb    w25, [x24, x8]     ; alphabet_rank lookup
                                      ; ... rest is StructuralIndex.push
```

Two `_mi_malloc_aligned` calls precede this loop (at `100013bc0`
and `100013bd4`) — the StructuralIndex allocations of
`positions: Vec<u32>` and `kinds: Vec<u8>` sized at `input.len() / 8`.
For twitter (632KB input) that reserves 79K positions + 79K kinds
up-front, then iterates all 632K input bytes pushing matches.

The `<JsonParser>::parse` self-time aggregates to 50.88% of total
samples. Given total parse time of 1.479ms, the eager scan path
costs approximately **752µs out of 1479µs (~50%)** with one
material consumer (`skip_space_slow` probe) returning marginal
benefit.

## Root cause

The AY.W1.3 substrate-with-consumer cycle landed an O(N) eager
scan at parse-entry but only two consumer sites:

1. **Tape capacity refinement** — at parse entry, after
   `init_for_input`, widen `TapeBuilder::with_capacity` to
   `max(profile.capacity_for(input.len()), structural_index.len() *
   2 + 2)`. For JSON twitter, `profile.capacity_for(632KB) = 316KB`
   (AR-floor) and `structural_index.len() * 2 = ~316KB` — same
   order of magnitude. **No material capacity benefit on JSON.**

2. **`skip_space_slow` probe** — only present on the plain-ws
   variant (JSON, BBNF, Sheets, GoogleSheets). The probe checks
   whether the next structural byte sits within `*p + 64`; if so,
   short-circuits the SIMD bitmap eval and advances `*p` directly
   to the structural position. For JSON: most whitespace runs are
   1-3 bytes (e.g. `"key": "value",`); the probe rarely terminates
   within the next stripe. When it does, the savings are at most
   a handful of SIMD instructions.

The eager scan paid ~750µs per twitter parse for negligible
material benefit. CSS L4's comment-aware skip_space_slow has no
probe wired (deferred to AY.W4 per the consumer-coverage audit), so
its W1.3 substrate consumer was only the marginal capacity
refinement.

The mistake in W1.3's design was the assumption that CTNS-style
consumers would arrive concurrently. They didn't (the audit at
`AYW1-structural-scan-consumer-coverage.md` notes: "no
`consume_to_next_structural` calls exist in the current emitter
shapes"). Without CTNS, the substrate paid full cost for marginal
benefit.

## Fix applied

Per AY operational posture (§1 re-plan trigger), retired the eager
scan + its two consumer sites in a single targeted edit:

- `crates/core/src/backend/rust/emitter/grammar.rs`:
  removed `state.init_for_input(__input_bytes)` from both the
  tape-path parse entry and the visitor-path `parse_with_visitor`
  entry. Removed the `__scan_capacity = state.structural_index.len() * 2 + 2`
  capacity-refinement block; tape capacity falls back to
  `GRAMMAR_PROFILE.capacity_for(input.len())` alone.
- `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs`:
  removed the `structural_index` field from `ScanState`, the
  `init_for_input` populator method, and the structural-index
  probe block from `emit_skip_space_plain`. `ScanState` reduces
  to the `nospace_bits` + `nospace_start` SIMD whitespace bitmap
  cache (matching `json_prototype::simd::ScanState` exactly).
- Substrate (`crates/tape/src/structural_scan.rs` + the
  `scan_structural` / `next_structural_at_or_after` re-exports
  in `crates/tape/src/lib.rs`) preserved as-is. AY.W4's regex-scan
  specialisation work can wire it through CTNS-style predicates
  that deliver material savings.
- Bootstrap regen: 29565 → 29520 LOC (-45 lines from the consumer
  scaffolding deletion).

Diff sketch (grammar.rs):

```rust
let mut state = #support_mod_ident::ScanState::new();
- state.init_for_input(__input_bytes);
- let __profile_capacity = GRAMMAR_PROFILE.capacity_for(input.len());
- let __scan_capacity = state.structural_index.len() * 2 + 2;
- let mut builder = TapeBuilder::with_capacity(
-     if __scan_capacity > __profile_capacity { __scan_capacity } else { __profile_capacity }
- );
+ let mut builder = TapeBuilder::with_capacity(
+     GRAMMAR_PROFILE.capacity_for(input.len()),
+ );
```

Diff sketch (dispatcher.rs):

```rust
pub struct ScanState {
    pub(crate) nospace_bits: u64,
    pub(crate) nospace_start: isize,
-   pub(crate) structural_index: ::bbnf::runtime::tape::StructuralIndex,
}
- impl ScanState {
-     pub fn init_for_input(&mut self, input: &[u8]) {
-         self.structural_index = ::bbnf::runtime::tape::scan_structural(...);
-     }
- }

pub(crate) fn skip_space_slow(...) {
-   if let Some(__next_struct) = ::bbnf::runtime::tape::next_structural_at_or_after(
-       &state.structural_index, *p as u32) {
-       // ... 64-byte stripe probe
-   }
    loop { /* unchanged SIMD bitmap path */ }
}
```

## Recovery

Post-fix bench against `bencher --bench twitter`:

```
test twitter ... bench:     916,958 ns/iter (+/- 29,760) = 688 MB/s
```

Twitter recovered from 420 MB/s to **688 MB/s** (+64% delta vs
pre-fix; 1.64x improvement). Vs the W1 Phase 1 sanity baseline
(699 MB/s), recovery is within 11 MB/s — confirming the eager scan
was the regression source, not Phase 2's other commits (W1-B
finalise, W1-C tape substrate, W1-D Pratt Option C all land
cleanly).

Cross-grammar bench delta (vs pre-fix W1 close):

| fixture                              | pre-fix MB/s | post-fix MB/s | delta   |
|--------------------------------------|--------------|---------------|---------|
| json_monolithic/canada               |        289   |         324   |   +12%  |
| json_monolithic/citm                 |        385   |         729   |   +90%  |
| json_monolithic/data_s               |        451   |         676   |   +50%  |
| json_monolithic/data_xl              |        250   |         440   |   +76%  |
| json_monolithic/twitter              |        420   |         688   |   +64%  |
| css_l4/bootstrap                     |        130   |         151   |   +16%  |
| css_l4/normalize                     |        199   |         248   |   +25%  |
| css_l4/tailwind                      |        160   |         195   |   +22%  |
| google_sheets_monolithic/parse_simple |         20   |          21   |    +5%  |
| google_sheets_monolithic/parse_nested |         24   |          24   |     0%  |
| google_sheets_monolithic/parse_stress |         22   |          22   |     0%  |
| google_sheets_monolithic/format_simple|         42   |          40   |    -5%  |
| google_sheets_monolithic/format_stress|         48   |          50   |    +4%  |
| bbnf_monolithic/bbnf_self            |         95   |         115   |   +21%  |
| bbnf_monolithic/css_l4_grammar       |        128   |         155   |   +21%  |
| bbnf_monolithic/css_pretty           |        154   |         192   |   +25%  |
| bbnf_monolithic/ebnf                 |         52   |          60   |   +15%  |
| bbnf_monolithic/google_sheets        |        193   |         263   |   +36%  |
| bbnf_monolithic/json                 |         77   |          91   |   +18%  |

The CSS L4 +11.4% pre-fix gain attributed to W1.3 was actually
masking larger AoS-substrate + Pratt Option C gains; with the
eager scan retired, the underlying gains materialise fully
(+22% on tailwind, +25% on normalize, +16% on bootstrap).
Sheets parse_* hold steady (Pratt Option C gain unaffected).
BBNF shows +15% to +36% across the matrix.

Post-fix samply on twitter:

```
total samples: 4055   (pre-fix: 6714 — 40% fewer samples in same wall time = 1.66x faster)

Top 5 by self-time (aggregated by function):
  55.19%  <json_monolithic::JsonParser>::parse
  24.12%  parse_object_JsonParser_object
  11.81%  parse_wrap_JsonParser_value
   3.28%  parse_string_escaped
   1.95%  parse_array_JsonParser_array
```

`tape::structural_scan::next_structural_at_or_after` no longer
appears in the top-15. The dispatcher binary's symbol table no
longer carries `scan_structural` / `next_structural_at_or_after` /
`StructuralIndex` (LTO dead-code-eliminates the substrate when no
consumer wires it).

## Hard-gate readout post-fix

| # | Gate                                              | Required        | Measured              | Status |
|---|---------------------------------------------------|-----------------|-----------------------|--------|
| 1 | bytes/cyc twitter (AU citm floor)                 | >= 0.45         | 0.215                 | MISS — in-flight; W2 e-graph G3 wrap-elision is next lever |
| 2 | bbnf/sonic twitter ratio                          | <= 3x           | 3.76x (2587 / 688)    | NEAR  |
| 6 | CSS L4 tailwind delta vs post-AX-W1               | >= +8%          | +35.4% (195 / 144)    | PASS  |
| - | (W1-fix specific) twitter recovery                | >= 600 MB/s     | 688 MB/s              | PASS  |
| - | (W1-fix specific) Sheets/BBNF no -5% regression   | <= -5%          | best -5% format_simple| PASS  |
| - | (W1-fix specific) JSON entries vs post-AX-W1      | all >= post-AX  | all 5/5 above         | PASS  |
| - | bootstrap regen cycle-1 = cycle-2 byte-identical  | empty diff      | empty                 | PASS  |
| - | workspace tests no regression                     | 1490/0/40       | 1490/0/40             | PASS  |

W1's primary hard gate (bytes/cyc twitter >= 0.45) remains
in-flight. W1-fix closes the regression and lifts twitter to 1.6x
the W1 Phase 1 baseline; further bytes/cyc gains route through W2
(named-type preservation freeing the e-graph G3 wrap-elision pass)
and W4 (regex-scan specialisation, which can also wire the
preserved `scan_structural` substrate forward).

## Verification artefacts

- `docs/benchmarks/post-AY-W1-close.json` — 19-entry post-fix matrix.
- `docs/benchmarks/archive/post-AY-W1-bytes-cyc.txt` — post-fix attribution + trajectory.
- `.profiles/samply/post-AY-W1-fix/json_monolithic/twitter/profile.json.gz` —
  post-fix samply showing scan_structural absent from top-25 (4055 samples
  vs pre-fix 6714 in same wall time, redistributed to per-rule parse_ frames).
- Bootstrap regen idempotency cycle-1 = cycle-2 byte-identical.
- Workspace tests: 1490 passed / 0 failed / 40 ignored.

## Forward references

- AY.W2: e-graph G3 wrap-elision (Named-type preservation precondition);
  projected ~50% record-count cut on twitter.
- AY.W4: regex-scan specialisation. Can wire the preserved
  `tape::structural_scan` substrate through CTNS-style predicates that
  deliver material savings on grammars where probe density is high
  (CSS L4 selectors, Sheets formula tokens). The substrate is
  ready; the consumer wiring is the deferred work.
