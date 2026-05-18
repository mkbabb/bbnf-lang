# SK-V9 S-P1 Hardening V1 CH3 Regression/REDRESS

Date: 2026-05-18.

Lens: CH3 Regression/REDRESS.

Reviewed scope:

- `restart/audit/pass-1-substrate/PASS-1.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`

## Disposition

Disposition: ACCEPT.

Confidence: 94%.

CH3 accepts the P1 packet only as a regression-safe opening profile/gap ledger.
It does not accept S-P1 as profile-complete, does not admit any SK-V9-open row
movement, and does not authorize implementation planning from absent samply/PMU
cells. The reviewed artifacts preserve the REDRESS boundaries and fail closed
where fresh evidence is absent.

## Defects

No CH3 blocking defect found.

Fold-only defects:

1. The six P1 artifacts are absence ledgers, not completed fresh profile
   evidence. P1-A, P1-B, P1-C, P1-D, P1-E, and P1-F all record missing
   SK-V9-open samply/PMU/run evidence rather than inventing symbols, c/B, or row
   deltas (`p1a-samply-mode-1.md:40-45`, `p1b-samply-mode-2.md:40-46`,
   `p1c-samply-mode-3.md:34-40`, `p1d-pmu-cycles.md:71-73`,
   `p1e-hot-leaf-attribution.md:72-88`, `p1f-results-delta.md:87-95`). Any
   consolidated S-P1 text that calls this packet "profile complete" or uses it
   to rank candidates must be revised.
2. The process authority needs one explicit fold sentence. The P1 files use the
   Alpha-closed opening authority, while the checked SK-V9 `SYNTHESIS.md` and
   `HANDOFF.md` still say implementation remains pre-dispatch until G-Alpha and
   no SK-V9 `SPEC.md` or dispatch prompt exists (`SYNTHESIS.md:5-9`,
   `HANDOFF.md:5-8`). This is not a REDRESS reopen, but the S-P1 consolidation
   must state the user-dispatched S-P1 boundary and keep implementation waves
   out of scope.
3. The Canada structural-scan conflict remains evidence hygiene only. P1-C
   correctly flags that `skinny/RESULTS.md` still renders the stale 27870 Mbps
   note while REDRESS records the later 69075 Mbps full-matrix state
   (`p1c-samply-mode-3.md:99-103`; `skinny/REDRESS.md:26-29`,
   `skinny/REDRESS.md:63-67`). Do not use either value as fresh SK-V9-open
   profile evidence until the W0 telemetry refresh emits a current manifest.

## Findings

### CH3-F1: Prior REDRESS clusters are not reopened

The P1 packet does not propose source changes, candidate implementation, or row
admission. It repeatedly classifies current evidence as W0/SK-V8-open authority
and routes missing SK-V9-open artifacts to the future telemetry lock
(`p1a-samply-mode-1.md:73-75`, `p1b-samply-mode-2.md:129-146`,
`p1d-pmu-cycles.md:271-284`, `p1f-results-delta.md:99-111`).

The blocked clusters remain intact:

- REDRESS 28+33 tiny-string NEON/Class A remains rejected for active parse-G
  wiring; generated-retained cap-16's admitted split is not reopened as global
  or NEON/TBL policy (`skinny/REDRESS.md:324-337`,
  `skinny/REDRESS.md:394-413`, `skinny/REDRESS.md:1996-2004`,
  `skinny/REDRESS.md:2045-2059`).
- REDRESS 50-55 parse-time aux tables, EventCursor/cursor variants,
  parser-local structural masks, decoded stats, and quote-source streaming hash
  remain blocked; P1-E lists source-eligible surfaces only and does not classify
  them as hot leaves (`skinny/REDRESS.md:715-882`,
  `p1e-hot-leaf-attribution.md:83-88`, `p1e-hot-leaf-attribution.md:246-270`).
- REDRESS 60-72 retained/direct string and typed-materialization routes remain
  pre-blocked unless a future changed-shape proof lands. The P1 files do not
  retry string-boundary collapse, wide/delayed-wide scan, Unicode validator,
  object key/value carry, direct source-hook folding, decoded scratch,
  byte-output unescape, semantic string facts, or hand-authored typed sinks
  (`skinny/REDRESS.md:1346-2059`).
- REDRESS 80 raw/widened numeric route remains closed because P1-D derives no
  c/B and P1-E's `number` source class is not a candidate (`skinny/REDRESS.md:2217-2248`,
  `p1d-pmu-cycles.md:15-23`, `p1e-hot-leaf-attribution.md:83-88`).
- REDRESS 82-84 remain closed: the packet does not retry single-quartet Unicode,
  generated-retained StringBlock16, or object-pair value-byte compaction
  (`skinny/REDRESS.md:2287-2395`).
- REDRESS 88 and 89 remain closed: P1-D explicitly keeps PMULL prefix-XOR and
  CTZ/bulk rewires pre-blocked absent exact profiles and same-wave consumers
  (`skinny/REDRESS.md:2510-2585`, `p1d-pmu-cycles.md:264-269`).

### CH3-F2: Apache/CITM/Canada typed claims preserve REDRESS 91

Apache and CITM are consistently treated as source/product parity only, not
measured `RESULTS.md` rows. P1-B names the gap class and repeats the fresh
run-id, metadata, checksum, serde/oracle, sonic parity, and rendered-row
requirements before measured admission (`p1b-samply-mode-2.md:111-127`). P1-E
keeps Apache/CITM in the measured-row gap table with no samply profile or
self-time claim (`p1e-hot-leaf-attribution.md:211-228`). P1-F flags that no
SK-V9 row movement or Apache/CITM measured typed rows appear in current results
(`p1f-results-delta.md:103-111`).

This matches REDRESS 91 and SK-V9 synthesis: Apache/CITM source/product slices
are admitted but absent from the measured W0 manifest; Canada is rejected on a
full-fixture DirectBuild-vs-serde checksum mismatch and stays pre-blocked until
fresh full-fixture checksum proof exists (`skinny/REDRESS.md:2622-2659`;
`SYNTHESIS.md:212-218`).

### CH3-F3: Direct digest product-proof risk is contained

The packet does not promote digest rows into product proof. P1-B states direct
rows remain digest guard-plane rows until a direct output contract or control
path tranche exists (`p1b-samply-mode-2.md:50-59`, `p1b-samply-mode-2.md:153-155`).
P1-E repeats that direct rows are not product proof before a direct output
contract/control path (`p1e-hot-leaf-attribution.md:181-187`,
`p1e-hot-leaf-attribution.md:261-264`). P1-F renders direct rows with
`Output plane = digest` and zero SK-V9 delta (`p1f-results-delta.md:44-83`).

That preserves REDRESS 93: scalar-parent folding is rejected, no source patch or
Lock 14 allowance is admitted, `skinny/RESULTS.md` remains unchanged, and
remaining direct digest misses route to a direct-output-contract or control-path
research tranche (`skinny/REDRESS.md:2694-2729`; `SYNTHESIS.md:178-181`,
`SYNTHESIS.md:303-328`; `HANDOFF.md:45-52`, `HANDOFF.md:79-105`).

### CH3-F4: PASS-1 substrate rules are preserved

P1 does not create a parallel substrate, side table, public substrate API, BIR
variant, directive, or parser-owned fact slot. That respects PASS-1's substrate
contract: tape/direct/value stay in one substrate family, PASS-2 may refine
payloads but not redefine the BIR alphabet, and assertions must stay cited and
consumer-coupled (`PASS-1.md:54-57`, `PASS-1.md:328-338`). P1-E's source-surface
inventory correctly treats tape symbols as substrate surfaces rather than a
separable producer (`p1e-hot-leaf-attribution.md:265-270`).

## Fold Requirements

1. In the P1 consolidated fold, state that this packet is a W0/SK-V8-open
   extraction plus explicit absence ledger. It is not fresh SK-V9-open profiling
   evidence and cannot move rows.
2. Carry a REDRESS pre-block table for 28+33, 50-55, 60-72, 80, 82-84, 88, 89,
   and 91-93 into S-P2/S-P3. Any future candidate touching those boundaries must
   cite the item, prove a materially changed shape, name same-row thresholds, and
   supply fresh measured evidence before implementation planning.
3. Keep Apache/CITM/canada typed wording exact: Apache/CITM are source/product
   parity only until fresh measured row-table admission; Canada remains rejected
   until a full-fixture DirectBuild-vs-serde checksum proof exists. No length-only
   or digest-only typed proof may be accepted.
4. Keep direct digest rows guard-plane only. A direct row cannot become product
   proof without a direct output contract or control-path tranche, full-table
   maintain proof, and independent Track 2 backstop.
5. Resolve the P1 dispatch wording in the consolidation: cite the user-dispatched
   S-P1 boundary or update the process note, but do not imply SK-V9
   implementation waves are dispatched from the alpha `SYNTHESIS.md`/`HANDOFF.md`
   text alone.
6. Preserve the Canada structural-scan discrepancy as stale-authority hygiene
   until the SK-V9-open telemetry/gate refresh emits a same-run manifest.

## Final CH3 Judgment

ACCEPT at 94% confidence for Regression/REDRESS. The packet is safe to fold as a
no-regression P1 opening ledger if the fold requirements above are applied. It
would become REVISE if any downstream artifact promotes absent profiles into
profile completion, counts Apache/CITM as measured typed rows, weakens the Canada
typed checksum block, treats direct digest as product proof, or reopens a named
REDRESS route without changed-shape proof and fresh same-row evidence.
