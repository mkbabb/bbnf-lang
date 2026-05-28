---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V1
disposition: REVISE
generated_at: 2026-05-28T00:00:00-04:00
files_audited:
  - restart/prompts/ORCHESTRATOR.md
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/skinny/tranches/sk-v15/SYNTHESIS.md
  - restart/locks/LOCKS.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
live_truth_method: "nl -ba line reads of required prompts, Lock 1, inventories, 1F auxiliary files, and focused rg/nl reads for StructuralIndex, sidecar, Track 1/Track 2, and broadcast-admission code paths; no build/test/source edit."
---

## Verdict

**REVISE.** CH5 cannot accept V1 as-is. The inventories correctly preserve the
central Lock 1 warning: retained tape, direct sink, fact stream, and transient
scanner planes must not become parallel retained substrates. However, the V1
packet still under-catalogues hidden coupling in two places: root generated
`crates/core` code carries a lazy `OnceCell<StructuralIndex>` sidecar that the
main inventories do not classify, and SK-V15's `NEW-CH5-V5-02` broadcast-admit
rule is present in 1D/1E/1F but must be folded into CH5 as a Track/substrate
honesty gate.

Governing scope is explicit: ORCHESTRATOR CH5 rejects parallel substrate,
sidecar producer, renamed scanner, Track 1 == Track 2 dishonesty, and substrate
union failure (`restart/prompts/ORCHESTRATOR.md:87`). T-P1 CH5 requires 1A and
1F to catch the live couplings (`restart/prompts/totality/PASS-1-EXCAVATION.md:125`-`128`).
Lock 1 rejects retained class/mask streams, parser-owned cursor/list state,
public substrate APIs, `UnionTape`, or a second tape without G-Omega
(`restart/locks/LOCKS.md:118`-`127`) and rejects cross-call classifier state
(`restart/locks/LOCKS.md:137`-`149`).

## Findings

| ID | Disposition | Finding | Evidence | Fold directive |
|---|---|---|---|---|
| CH5-V1-01 | ACCEPT | 1A correctly says Lock 1 is only partly honored: JSON retained tape exists, but direct/fact/transient planes are not proven as one typed event-cursor schedule. | 1A shows retained `Tape` / `ValueRef` / `JsonDocument` evidence (`restart/audit/totality/p1/1A-substrate-evidence.md:104`-`111`), then says JSON direct owns raw bytes and local cursor and no shared event cursor is shown (`restart/audit/totality/p1/1A-substrate-evidence.md:113`-`121`). 1A's net conclusion is partial Lock 1 closure, not acceptance (`restart/audit/totality/p1/1A-substrate-evidence.md:141`-`143`). | Preserve the partial closure. Do not let 1A-SUB-003/004 wording be read as universal substrate-union acceptance. |
| CH5-V1-02 | REVISE | 1A/1F do not fully catch the live renamed-scanner/sidecar surface in root generated code: `crates/core` emits and checks in a lazy `OnceCell<StructuralIndex>` on generated `ScanState`, with at least Google Sheets consuming it. | The emitter says any non-empty structural alphabet gets a `OnceCell<StructuralIndex>` field (`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:43`-`49`) and emits that field at `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:445`-`456`. The helper initializes via `scan_structural` (`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:472`-`484`). Checked-in generated JSON carries the field (`crates/core/src/grammar/generated/json.rs:695`-`703`), and generated Google Sheets defines and consumes it in `skip_space_slow` (`crates/core/src/grammar/generated/google_sheets.rs:3542`-`3560`, `crates/core/src/grammar/generated/google_sheets.rs:3582`-`3605`). 1F's sidecar scan asks for `EventCursor`, `structural_offsets`, `TapeAssembler`, and CSS sidecar terms, but not `OnceCell<StructuralIndex>` / `scan_structural` (`restart/audit/totality/p1/1F-coherence-scan.md:132`). | Add a V2 1A/1F row for root `OnceCell<StructuralIndex>`: classify `substrate_target`, `retention_lifetime`, and `policy_owner`. If it is only per-parse generated scratch, fence as `local_temp_only` / generated-function. If retained across document identity, public API, or cross-call boundary, reject under Lock 1. Expand scans to `OnceCell<StructuralIndex>|scan_structural|ensure_structural_index|next_structural_at_or_after`. |
| CH5-V1-03 | ACCEPT | JSON Track 1 == Track 2 dishonesty is not currently reopened, but V2 must keep the shared-runtime-helper caveat visible. | 1F-past-corpora keeps the prior Track 1 == Track 2 dishonesty as a pre-block (`restart/audit/totality/p1/1F-past-corpora.md:80`-`81`). Live direct Track 1 calls generated `parse_direct` (`skinny/crates/bbnf-bench/src/direct_struct.rs:421`-`423`), while Track 2 uses the hand path (`skinny/crates/bbnf-bench/src/direct_struct.rs:428`). Four-way strict product compares Track 1, Track 2, serde, and sonic (`skinny/crates/bbnf-bench/src/direct_struct.rs:490`-`494`). For retained Track 2, 1F auxiliary correctly notes shared runtime tape helpers (`restart/audit/totality/p1/1F-anti-pattern.md:71`). | Keep the distinction: Track 2 may be an independent parser authority while sharing runtime tape helpers. It must not be cited as substrate-independent or as Track 1 closure. |
| CH5-V1-04 | REVISE | CSS fact streams and source-sidecar comparator evidence are mostly catalogued, but the main V1 1F row is too weak unless it imports the auxiliary AP-020 classification. | 1A classifies CSS fact output as a substrate target, not a backend shape, and flags metadata drift (`restart/audit/totality/p1/1A-substrate-evidence.md:123`-`130`). The 1F auxiliary AP-020 names the CSS comparator sidecar and corrected line anchors (`restart/audit/totality/p1/1F-anti-pattern.md:80`). Live `lightningcss_facts` returns `fixture_sidecar_facts(input)` (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:636`-`648`), `fixture_sidecar_facts` starts at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:2691`, and seven equality artifacts label `same-plane-source-sidecar` (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:1080`-`1085`, `:1200`-`:1206`, `:1351`-`:1357`, `:1508`-`:1514`, `:1658`-`:1664`, `:1812`-`:1818`, `:1961`-`:1967`). | Re-emit the CSS sidecar row in the current V2 1F packet, not only in stale-cycle auxiliary context. Fence it as comparator/output-plane evidence only; never as retained runtime substrate or CSS value API proof. |
| CH5-V1-05 | REVISE | SK-V15 broadcast-admission detection is not missed, but it is not yet folded as a CH5-specific honesty directive. | SK-V15 adds `NEW-CH5-V5-02`: N admits require N distinct measurement rows unless explicitly aggregate (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:107`-`108`). 1D marks CSS 24-row admit as one broadcast aggregate, not 24 independent measurements (`restart/audit/totality/p1/1D-skinny-lessons.md:143`-`148`). 1E proposes LAC-1E-V1-07 with the same rule (`restart/audit/totality/p1/1E-locks-evidence.md:128`). Live CSS code hardcodes `W8_SELECTED_CSS_ROWS = 24` (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:16`-`17`) and sets `admitted_rows` to that constant from one aggregate admit decision (`skinny/crates/bbnf-bench/src/css_l4_w8.rs:139`-`145`). | V2 CH5 must carry an explicit broadcast row: one aggregate timing may produce one aggregate diagnostic row only. Any future 24 CSS admits need 24 distinct `measurement_row_id` / `broadcast_group_id` evidence or fail CH5 as Track/admission-plane dishonesty. |

## Fold Directives

1. **F-CH5-V1-01 - root structural-index sidecar census.** Add the
   `OnceCell<StructuralIndex>` / `scan_structural` / `ensure_structural_index`
   scan to 1A and 1F. Classify every hit under Lock 1's target/lifetime/owner
   vocabulary; do not close substrate union while any retained structural index
   is unclassified.
2. **F-CH5-V1-02 - current-cycle CSS sidecar fold.** Import AP-020's corrected
   CSS `same-plane-source-sidecar` evidence into the current V2 1F inventory.
   Treat it as comparator evidence only.
3. **F-CH5-V1-03 - Track 1/Track 2 caveat.** Preserve JSON direct Track 1/Track
   2 separation, but state that retained Track 2 shares runtime tape helpers and
   is not substrate-independent evidence.
4. **F-CH5-V1-04 - broadcast admission.** Fold `NEW-CH5-V5-02` into CH5:
   broadcast row multiplication is Track/admission-plane dishonesty unless
   explicitly aggregate; N admits need N distinct measurement rows.
5. **F-CH5-V1-05 - fact-stream vocabulary.** Keep `FactStream` as
   `admitted_fact_output` / output-plane evidence only. Fix or route the CSS
   `W7_POLICY_BACKEND_SHAPE = "admitted_fact_output"` drift before any
   BackendShape-close wording.

## Non-Findings

- No evidence in this pass proves a second retained JSON document identity in
  skinny runtime. The known skinny `StructuralIndex` in `json/scan.rs` is still
  catalogued by 1A as transient capacity/proof input (`restart/audit/totality/p1/1A-substrate-evidence.md:132`-`139`).
- No evidence in this pass reopens the old direct Track 1 == Track 2 bench-private
  parser dishonesty; current direct Track 1 and Track 2 use separate generated
  and hand paths, with strict-product parity checks.
