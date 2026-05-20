# SK-V12 Pass Alpha CH5 Hidden Coupling

Pass: Alpha SK-V11 -> SK-V12 CHALLENGE V1 under USER PIN.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Output: this file only.

## Disposition

REVISE.

The pin-aware Alpha packet correctly invalidates the old Sheets-first route,
makes CSS L4 authoritative, reopens union and ASM-gen only as measured
category-level routes, and keeps hand-only witnesses out of Track 1. CH5 still
finds three hidden-coupling defects that must fold into Alpha V2 before
G-Alpha or S-P3 can proceed:

1. The CSS L4 Track 1 / oracle / lightningcss comparator plane is still
   under-specified and can accidentally compare generated direct-sink facts
   against a different lightningcss full-AST or canonical-output plane.
2. ADMIT close can still bypass the USER PIN D5 zero-orphan SIMD target because
   the zero-orphan requirement is explicit for FIXPOINT and E5, but not for
   the ADMIT close path.
3. The CSS-local union candidate says "no public substrate API" while still
   naming generic tape/event owner paths without a sealed/private generated
   boundary. That leaves a public-substrate leak open.

These are fixable contract defects, not a rejection of the pin direction.

## Materials Read

- `restart/prompts/ORCHESTRATOR.md` Section 3W.
- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- Alpha A-F:
  - `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md`.
  - `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md`.
  - `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md`.
  - `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md`.
  - `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
  - `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- Contract outputs:
  - `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
  - `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- CSS / lightningcss / fallback references:
  - `crates/core/benches/css/competitors.rs`.
  - `crates/core/benches/css/l4.rs`.
  - `crates/core/tests/lightningcss_parity.rs`.
  - `crates/core/tests/css_l4_canonical_parity.rs`.
  - `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`.
  - `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`.
  - `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`.
  - `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.

## Sub-Dispositions

| Surface | Disposition | Defect |
|---|---|---|
| Track 1 / Track 2 independence | REVISE | E1 keeps Track 2 independent from generated Track 1, but does not pin the equality adapter/output plane tightly enough. |
| Lightningcss comparator plane | REVISE | Alpha-B names full semantic `StyleSheet::parse` templates while E1 selects declaration-value `direct_sink`; same-plane proof is not yet executable. |
| Hand-only witness as Track 1 | ACCEPT | The packet rejects report fixtures, `sheets_witness`, and generated-absent baselines as Track 1 authority. |
| Union substrate coupling | REVISE | E4 can touch generic tape/event paths without a sealed/private generated boundary; this could leak public substrate API. |
| Orphan SIMD close | REVISE | ADMIT lacks an explicit zero-existing-orphan close clause, despite USER PIN D5. |
| JSON guard / typed-direct plane honesty | ACCEPT | JSON direct and typed rows remain guard facts and are not relabeled as CSS or parse-only close evidence. |

## Findings

### CH5-1 - REVISE: CSS L4 comparator and oracle planes can still drift

USER PIN D1/D2 require semantic parity with lightningcss and a generated CSS L4
parser that beats lightningcss on the same corpus, same output plane, and
strict equality (`USER-PIN-W1-CSS-L4-SOTA.md:20-34`). Alpha-B correctly says
the selected row must record `output_plane`, `strictness`, generated Track 1
Mbps, `lightningcss_mbps`, fixture provenance, and an equality artifact
(`alpha-B-competitor-deltas.md:82-95`).

The coupling defect is that the concrete sources currently named are not one
plane yet:

- Alpha-B's local comparator source is `lightningcss::StyleSheet::parse` over
  full CSS files (`crates/core/benches/css/competitors.rs:153-188`), and
  Alpha-B describes it as "full semantic CSS parse" over normalize/bootstrap/
  tailwind (`alpha-B-competitor-deltas.md:71-77`).
- E1 selects `css_l4/declaration_values/direct_to_struct/main` with output
  plane `direct_sink` (`alpha-E-candidate-shortlist.md:101-107`).
- E1 then says the oracle side is "lightningcss plus a same-plane equality
  adapter over CSS declaration-value facts" and "strict equality against the
  lightningcss-derived oracle" (`alpha-E-candidate-shortlist.md:90-97`).

That leaves room for a hidden bridge: generated Track 1 could emit a direct
digest or declaration-value fact stream while lightningcss measures full
stylesheet AST parse, or the equality adapter could become a hand-coded
per-comparator witness. The root CSS parity test already names the correct
anti-bridge principle: no hand-coded `From<lightningcss::StyleSheet>`, no
`PartialEq<lightningcss>`, and no per-grammar adapter modules; both sides must
emit their own canonical output and pass through the same symmetric
normalization (`crates/core/tests/css_l4_canonical_parity.rs:1-19`).

Required fold for Alpha V2:

- Pick exactly one CSS row and one output representation before S-P3 dispatch:
  either full-stylesheet canonical CSS parity, or a declaration-value fact
  stream. Do not leave it to "as selected by W1" without naming the equality
  artifact shape.
- If the row is declaration-value direct-sink, specify how lightningcss emits
  the same declaration-value facts without a bbnf-specific or comparator-
  specific adapter bridge.
- The gate must prove that Track 1 output, Track 2/oracle output, and
  lightningcss comparator throughput all refer to the same corpus slice and
  output plane. Missing same-plane evidence is REJECT, not UNMEASURED.

### CH5-2 - REVISE: ADMIT can close with existing SIMD orphans

USER PIN D5 names five orphan aarch64 primitives and says the campaign target
is zero orphan kernels by SK-V12 close (`USER-PIN-W1-CSS-L4-SOTA.md:71-78`).
The SIMD audit confirms the orphan count and names the same five surfaces:
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, and `cache_hints`
(`skv12-aarch64-simd-coverage-audit.md:34-61`).

Alpha-D carries the correct rule: every orphan must be removed, demoted as
inventory, or wired to a same-wave consumer; leaving an orphan at close fails
the pin (`alpha-D-validated-invalidated.md:193-200`). Alpha-F also says
orphan SIMD primitives reject a wave in telemetry binding, but the close
contract is inconsistent. The ADMIT path requires Lock 16 only for "any SIMD/
ASM admission" and for "every admitted primitive"
(`alpha-F-contract-draft.md:85-88`; `SYNTHESIS.md:51-54`). It does not
require disposition of the existing five orphan primitives. FIXPOINT does
require orphan primitives to invalidate close
(`alpha-F-contract-draft.md:108-110`; `SYNTHESIS.md:66-74`), and E5 requires
"zero new orphan" (`alpha-E-candidate-shortlist.md:294-301`), but neither
closes the ADMIT gap.

This is hidden coupling because a scalar CSS L4 ADMIT could satisfy the
lightningcss row while preserving the stale native inventory. That violates
the pin's "SIMD utilization perfected" close target even if no new SIMD wave
ships.

Required fold for Alpha V2:

- Add an ADMIT close clause: the five carried orphan aarch64 primitives are
  zero at close by measured admission, removal, or explicit inventory demotion
  with evidence.
- Strengthen E5 from "zero new orphan" to "the carried orphan set is zero or
  each remaining inventory item has a measured non-production disposition."
- The close packet must list final SIMD coverage state for ADMIT and FIXPOINT,
  not only for FIXPOINT.

### CH5-3 - REVISE: CSS-local union may leak through generic tape/event owner paths

USER PIN D3 reopens the union/substrate category, including formerly blocked
sidecar and class-lane vocabulary, but only for new implementations that cite
REDRESS 96/97/98, name a material differential, pass CHALLENGE, and land with
scalar/reference plus same-wave consumer evidence
(`USER-PIN-W1-CSS-L4-SOTA.md:39-56`). The pin does not authorize accidental
public substrate API expansion, and Alpha-F still refuses new public substrate
API or parser-owned sidecars (`alpha-F-contract-draft.md:182-192`).

E4 is directionally valid: it targets CSS declaration-value alternatives, not
JSON parse-plane structural rediscovery, and says there is no public substrate
API, second retained substrate, sidecar class column, retained structural
vector, or parse-only admission (`alpha-E-candidate-shortlist.md:205-217`,
`alpha-E-candidate-shortlist.md:236-245`). The hidden-coupling defect is in
the owner surface: E4 also names `runtime/src/tape/event_grammar.rs` and
`runtime/src/tape/mod.rs` "only if an existing generic EventGrammar bound must
be consumed" (`alpha-E-candidate-shortlist.md:218-225`). That wording does not
forbid a new exported `EventGrammar` expansion, public module surface, or
persistent event projection escaping the generated CSS runtime.

Required fold for Alpha V2:

- State that E4's union tag/event projection must be private to generated CSS
  code or consume an already-existing sealed/internal trait without widening
  the public runtime API.
- Any edit to `runtime/src/tape/event_grammar.rs` or `runtime/src/tape/mod.rs`
  must be explicitly listed as a Lock 1/Lock 14 high-risk surface and must
  prove no new public substrate, no second retained vector, no parser-owned
  fact slot, and no reusable sidecar producer.
- The gate should inspect exported public items for the wave. A public API diff
  in generic tape/event modules without a same-wave generated CSS consumer is
  REJECT.

### CH5-4 - ACCEPT: hand-only non-JSON witnesses are not Track 1 authority

The packet no longer treats Sheets or BBNF-self as preflight-equivalent to CSS.
Alpha-A invalidates the old Sheets W1 V2 plan and records no current generated
CSS L4 Track 1 (`alpha-A-results-extraction.md:29-72`). Alpha-D makes
`sheets_witness` and the W1a report lane non-admitting infrastructure
(`alpha-D-validated-invalidated.md:100-107`,
`alpha-D-validated-invalidated.md:129-141`). Alpha-F requires a generated CSS
L4 row and rejects grammar generalization by hand-only witness modules
(`alpha-F-contract-draft.md:68-80`; `alpha-F-contract-draft.md:177-191`).

The Sheets/gorgeous scout is acceptable only as fallback inventory: it names
`GoogleSheetsParser` as an independent oracle candidate and explicitly says
`sheets_witness` is not allowed in Track 1 or oracle, but the user pin demotes
Sheets until after a CSS redress attempt. CH5 has no additional blocker here.

### CH5-5 - ACCEPT: JSON guard and typed/direct planes are not relabeled

Alpha-A keeps parse-only diagnostic, direct SinkOnly digest rows, and typed
product rows separate (`alpha-A-results-extraction.md:86-179`). Alpha-B keeps
JSON sonic/serde deltas as guard/freshness facts and says JSON wins cannot
substitute for the CSS L4 pin (`alpha-B-competitor-deltas.md:119-184`).
Alpha-F's close condition is CSS-specific and does not route direct or typed
JSON wins into the CSS close bar (`alpha-F-contract-draft.md:66-90`).

No CH5 defect is open on typed/direct plane honesty.

## Required Fold For Alpha V2

Alpha V2 should not proceed to G-Alpha until it folds these exact changes:

1. Define the CSS L4 output representation used for Track 1, Track 2/oracle,
   equality, and lightningcss throughput. The equality adapter must be
   symmetric and gate-consumed, not a hand-only bridge.
2. Add zero-existing-orphan SIMD disposition to ADMIT, not just FIXPOINT.
3. Seal E4's union route to generated/private or already-existing internal
   surfaces; make public generic tape/event API expansion a CHALLENGE-visible
   REJECT unless explicitly justified and consumed same-wave.

## Result

CH5 returns REVISE for Pass Alpha V1 under the USER PIN. The packet is close
to a valid pin-aware Alpha contract, but the hidden-coupling defects above must
be folded before S-P3 can rely on it as dispatch authority.
