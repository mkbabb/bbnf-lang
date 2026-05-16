# Handoff SK-V6

Date: 2026-05-15.

Status: **SK-V6 SOTA recovery active.** The runnable skinny has real Rust state
for `BackendShape`, generated `SinkOnly`, lazy offset tape, Eisel-Lemire
number parsing, and the structural scan floor. The full gate in
`skinny/RESULTS.md` remains `N-direct / NoGo`; retained parse and direct typed
emission are the remaining measured blockers. The SK-V5 Wave 3 UTF-8 fusion
family is refuted by REDRESS 50-55 and must not be carried forward without new
row evidence.

## 1. Read First

1. `restart/skinny/audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md`
2. `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6-ASMJSON-DAV1D.md`
3. `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md`
4. `skinny/RESULTS.md`
5. `skinny/REDRESS.md`
6. `restart/skinny/audit/SK-V6-COHORT/`

## 2. Latest Cohort Findings

The SK-V6 asmjson/DAV1D + profiling pass is archived under
`restart/skinny/audit/SK-V6-COHORT/` as `skv6-A*.md`, `skv6-B*.md`, and
`skv6-C*.md`.

Binding conclusions:

- asmjson teaches DPDA lowering, chunk masks, next-bit seeking, and
  direct-threaded finite control. It does not justify a JSON directive or a
  permissive strict-S anchor.
- DAV1D/FFmpeg/VLC teach the SIMD process: scalar oracle, feature masks,
  register-clobber checks, stack canaries, cycle measurement, and same-wave
  consumers.
- The current Apple Silicon route is NEON/AdvSIMD plus PMULL and scalar bit
  operations, focused on measured string/materialization hot sites.
- The x86 route is strict `CollapsedStage` after the arm64 same-plane matrix
  closes, not an asmjson clone.
- Direct-to-struct generality requires generated `DirectFieldFacts` from
  host/API output schema facts.
- Comparator strictness needs schema v3. `utf8_lossy` and permissive asmjson
  rows cannot ratify strict SOTA.
- C3 confirmed the current sonic-rs dependency enables `utf8_lossy`, so strict
  sonic S anchors are invalid until rebuilt without that feature.
- C1/C5 nominate a per-`\uXXXX` table/TBL classifier inside the existing
  retained string path as the next escape-row candidate, distinct from the
  rejected four-unit contiguous-run validator.
- C2 nominates generated `mesh` `DirectBuild` as the first product-plane typed
  expansion beyond `twitter` and `update_center`.
- C6 confirms `passes`, `codegen`, and `parse-that-regex` have blocking
  grammar-name leaks; `runtime/tape` is clean.

## 3. Current Work Queue

1. Comparator schema v3 and same-plane repair.
2. DAV1D-grade `primitive-checkasm` hardening.
3. One retained parse intervention at a time from fresh profiles.
4. Generated `DirectBuild` field-layout materialization for representative
   owned typed outputs.
5. Lock 14 cleanup: remove JSON-name logic from generic crates.
6. Full same-plane 17-corpus matrix.
7. Optional x86 `CollapsedStage` successor on Zen 4-class hardware.

## 4. Do Not Reopen Without New Measurement

- eager retained tape as SOTA-beat substrate;
- function-pointer dispatch table;
- 12-byte token width churn;
- pair-token fusion;
- structural-index sidecar prepass;
- eventcursor or structural-mask sidecar producer;
- generic SWAR whitespace skipper;
- separator elision;
- raw `f64` shortcut;
- Class A NEON tiny-string wiring as parse-G fix;
- broad UTF-8 fusion as generated-baseline close;
- generic decoded visitor, sink-local decoded-stat helper, quote-source
  streaming hash helper, parser-owned decoded scratch, and byte-output
  unescape as direct closes.

## 5. What Success Means

The close is not a single impressive Mbps number. It is a same-plane matrix:
strictness disclosed, output plane matched, ownership matched, hardware and
feature masks recorded, and generated Track 1 proven against independent
sidecars. The direct proof must be generated typed output, not a private
checksum parser. The asmjson beat is valid only on strict same-plane x86 rows;
permissive rows stay flaw probes.

The implementation packet is wave-gated so a failed candidate becomes useful
REDRESS evidence instead of another broad theory. If a wave cannot move the
named rows, revert or isolate it, record the measurement, and return to the
profile shortlist.
