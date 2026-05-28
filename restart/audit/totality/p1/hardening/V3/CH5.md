---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V3
disposition: REVISE
generated_at: 2026-05-28
files_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
  - restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V2-CONSOLIDATED.md
  - restart/audit/totality/p1/hardening/V2/CH5.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
score: "6/7 ACCEPT, 1/7 REVISE"
---

# CH5 Hidden Coupling - SK-V15 T-P1 V3

## Verdict

REVISE.

The V3 inventories preserve the major CH5 fences for CSS broadcast, CSS
source-sidecar comparator evidence, Track 1 / Track 2 substrate-helper
sharing, root structural sidecars, EventTape/EventCursor sidecars, and
Lock 14/16 gate exclusions. They do not collapse CSS fact streams into a
value API or sixth `BackendShape`, and they keep root structural sidecars
and EventTape as unimplemented or unresolved coupling surfaces.

One hidden-coupling gap remains: FNV is treated in the inventories as
bench-only JSON closed-enum scaffolding or as a broad REBUILD-WAVE-G
UNKNOWN, but live generated CSS runtime files emit `source\tinput_fnv64`
and define `fnv64` in production grammar modules. That current-source
surface is not explicitly catalogued or fenced as telemetry-only,
non-equality, and non-substrate evidence. This is narrow, but it is
exactly the kind of hidden source-sidecar/hash coupling CH5 is meant to
force into the excavation record.

## Evidence

- Lens authority: `PASS-1-EXCAVATION.md` requires CH5 to audit Lock 1 for
  parallel substrates, sidecar producers, renamed-scanner violations, and
  Track 1 == Track 2 dishonesty. `ORCHESTRATOR.md` section 3W repeats the
  same CH5 scope. V3 dispatch specifically adds CSS broadcast, source-sidecar
  comparator, Track 1 / Track 2 collapse, root structural sidecars,
  EventTape sidecars, and FNV production coupling.

- Historical 1F auxiliaries are not live authority. `1F-anti-pattern.md:6-23`
  and `1F-past-corpora.md:6-28` mark both files as superseded historical
  auxiliaries; live evidence belongs in `1F-coherence-scan.md`.

- CSS broadcast is fenced. `1D-skinny-lessons.md:105`, `:151`, `:175`,
  and `:193` classify the 24 CSS rows as audit-demoted broadcast evidence
  and require distinct measurement IDs or explicit aggregate status.
  `1E-locks-evidence.md:110`, `:135`, and `1F-coherence-scan.md:146`
  carry the same one-to-N broadcast guard.

  Command:

  ```sh
  awk '/^\| css_l4\// && /track1_mbps=2319\.041;cssparser_mbps=2362\.037;lightningcss_mbps=929\.281/ {n++} END {print n+0}' skinny/RESULTS.md
  # 24
  ```

- CSS source-sidecar comparator evidence is fenced. `1F-coherence-scan.md:86`
  records `lightningcss_facts` calling `fixture_sidecar_facts(input)` and
  seven `same-plane-source-sidecar` writer literals, with the note
  "Comparator-only evidence; never runtime substrate or CSS Value API proof."
  `1D-skinny-lessons.md:176` carries the same comparator-only fence.

  Command:

  ```sh
  rg -n "fixture_sidecar_facts|same-plane-source-sidecar" skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
  # 648: fixture_sidecar_facts(input)
  # 1082,1203,1354,1511,1661,1815,1964: same-plane-source-sidecar writers
  # 2691: fn fixture_sidecar_facts(...)
  ```

- Track 1 / Track 2 collapse is visible rather than hidden. `1D-skinny-lessons.md:101`
  keeps JSON direct/typed rows on strict product planes; `1D:103` keeps
  current P1 Track 1/Track 2 misses as measurement debt, not admission
  reversal. `1F-coherence-scan.md:125` explicitly says Track 2 shares
  runtime tape helpers and classifies that as visible helper sharing, not
  proof of a second retained substrate.

  Source check:

  ```sh
  nl -ba skinny/crates/bbnf-bench/src/track2/json.rs | sed -n '1,70p'
  # 5-8 import runtime JsonRoot/ParseError and tape helpers
  # 26-34 call structural_capacity_for and construct TapeBuilder
  # 45 seals through JsonRoot::from_tape(...)
  ```

- Root structural sidecars are fenced. `1A-substrate-evidence.md:92`
  carries the CH5 root `OnceCell<StructuralIndex>` census requirement, and
  `1F-coherence-scan.md:85` classifies root generated structural indexes as
  hidden coupling / unimplemented until they are accepted as local scratch
  or rejected as retained sidecars.

  Source check:

  ```sh
  nl -ba crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs | sed -n '40,50p;445,485p'
  # 43-49 gates structural-alphabet emission
  # 448-456 emits OnceCell<StructuralIndex>
  # 472-484 emits ensure_structural_index and scan_structural(...)
  ```

- EventTape and EventCursor sidecars are fenced. `1A-substrate-evidence.md:75-76`
  says `EventGrammar` is a fact/admission trait, not a typed cursor, and
  that `EventTape` has no audited runtime cells. `1B-codegen-evidence.md:69`
  forbids EventCursor sidecars, retained structural streams, retained class
  lanes, parser-owned cursor lists, and cross-call classifier state. `1C-runtime-evidence.md:64`,
  `:101`, `:110`, `:122`, and `:137` keep EventTape as unimplemented or
  proof/scaffold state, not an emitted runtime consumer.

  Command:

  ```sh
  rg -n "EventCursor|generated_eventcursor|TapeAssembler" skinny/crates crates/core/src -g '*.rs'
  # no hits
  ```

- Gate exclusions are not hidden. `1E-locks-evidence.md:193-198` and
  `1F-coherence-scan.md:95-101` require Lock 14/16 gates to print included
  roots, excluded roots, and primitive classifications. `1F-coherence-scan.md:129-139`
  maps Lock 14 leak owners to downstream receivers.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH5-V3-001 | ACCEPT | CSS 24-row broadcast remains audit-demoted and cannot be cited as independent admits. | `1D:105`, `1D:151`, `1D:175`, `1D:193`; `1E:110`, `1E:135`; `1F:146`; command above counts all 24 CSS rows sharing the same timing tuple. | None. |
| CH5-V3-002 | ACCEPT | CSS source-sidecar comparator remains comparator-only, not runtime substrate or CSS Value API evidence. | `1F:86`; `1D:176`; source grep finds `fixture_sidecar_facts` and seven `same-plane-source-sidecar` writers. | None. |
| CH5-V3-003 | ACCEPT | Track 2 helper sharing is visible and does not claim Track independence or a second substrate. | `1F:125`; `track2/json.rs:5-8`, `:26-34`, `:45`. | None. |
| CH5-V3-004 | ACCEPT | Root structural sidecars are catalogued as hidden coupling and held open before substrate-union closure. | `1A:92`; `1F:85`; root emitter lines above. | None. |
| CH5-V3-005 | ACCEPT | EventTape / typed event cursor work is fenced against EventCursor or retained class-sidecar resurrection. | `1A:75-76`, `1A:94`, `1B:69`, `1C:64`, `1C:110`, `1C:137`; no `EventCursor` / `generated_eventcursor` / `TapeAssembler` hits. | None. |
| CH5-V3-006 | ACCEPT | Lock 14 / Lock 16 gate exclusions are explicit and routed to owners/receivers. | `1E:193-198`; `1F:95-101`, `1F:129-139`; `1D:194`, `1D:209`. | None. |
| CH5-V3-007 | REVISE | FNV production coupling is under-fenced. The inventories discuss FNV as W11 JSON bench-only quarantine or broad production-guard UNKNOWN, but they do not cite the live generated CSS runtime FNV fields. | Inventory grep for `input_fnv64`, `stream_fnv64`, or `fn fnv64` returns no hits. Source grep finds 21 hits in `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`: each of seven CSS generated runtime files emits `source\tinput_fnv64` at lines `25` and `71` and defines `fn fnv64` at line `619`. The codegen template emits the same at `skinny/crates/codegen/src/runtime_generator.rs:737`, `:783`, and `:1331`. | Add an explicit current-source FNV production-coupling census and fence. |

## Required Fold

V4 must add a compact FNV production-coupling row to the live inventories
before CH5 can accept:

1. In `1F-coherence-scan.md` or `1D-skinny-lessons.md`, cite the current
   `input_fnv64` / `fnv64` production-runtime surface:
   `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs:25`,
   `:71`, `:619`, and template sites
   `skinny/crates/codegen/src/runtime_generator.rs:737`, `:783`, `:1331`.
2. Classify the FNV fields as one of: telemetry-only output-plane metadata
   with no equality/admission authority, UNKNOWN pending a gate consumer
   audit, or a real production equality/hash coupling requiring REBUILD-WAVE-G.
3. Extend the CH5/1F sidecar grep guard to include
   `input_fnv64|stream_fnv64|fn fnv64|fnv64\(` so future substrate-close
   checks cannot miss hash-sidecar surfaces.
4. If the fields are telemetry-only, state that they are not CSS Value API
   proof, not retained document identity, not same-substrate evidence, and
   not a production equality arbiter.

No source edit, staging, build, or commit was performed for this CH5 report.
