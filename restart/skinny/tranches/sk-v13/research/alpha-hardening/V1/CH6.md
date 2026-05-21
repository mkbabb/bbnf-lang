# CH6 Next-Tranche Impact / Anti-Paper-Close - SK-V13 Pass Alpha V1

Disposition: REVISE.

The SK-V13 handoff and synthesis are strong enough to prevent an immediate paper close, but they need one downstream guard tightened: S-P3 must treat weaker scoping language as subordinate to the synthesis/pin, especially scaffold-only, optional, or future-tranche waves. The master contract itself is mostly acceptable; the revision is to preserve its stronger constraints when SPEC.md and DISPATCH-PROMPT.md are later authored.

## Findings

### F1 - ACCEPT: measurable next-pass requirements are explicit

The synthesis defines close as full ADMIT or architectural-level intrinsic block for every remaining row/feature; implementation-limited misses are reopens and force a rejected close plus immediate Alpha bracket (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:32-36`). It gives measurable CSS admission gates: same-plane strict equality, coverage match, Track 1 greater than lightningcss + 1 Mbps, independent oracle evidence, and gate-consumed provenance (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:45-54`). It gives measurable JSON gates: all 17 corpora across `parse_only`, `direct_to_struct`, and `real_typed_struct` must beat sonic-rs strict on same plane/corpus/strictness, with `parse_only` explicitly admission-eligible (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-105`).

This satisfies PASS-ALPHA CH6's requirement that the goalset be measurable and bench-verifiable (`restart/prompts/pass-contracts/PASS-ALPHA.md:47-49`) and aligns with the user pin's acceptance tests for CSS and JSON (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:76-95`).

### F2 - ACCEPT: G-Omega is a hard pre-W0 block

The handoff says `SPEC.md` and `DISPATCH-PROMPT.md` are intentionally absent and must be authored by S-P3 after the required G-Omega pre-W0 gate (`restart/skinny/tranches/sk-v13/HANDOFF.md:5-7`). It blocks Wave 0 and any source, generated runtime, gate/report, RESULTS, or REDRESS edits until Omega converges and the user closes G-Omega (`restart/skinny/tranches/sk-v13/HANDOFF.md:54-74`, `restart/skinny/tranches/sk-v13/HANDOFF.md:85-91`). The synthesis repeats that Totality V1.1 is mandatory before SK-V13 Wave 0, and no implementation/source/results wave may start before G-Omega closes (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:111-121`).

This is compatible with the orchestrator's mandatory G-Omega sign-off gate (`restart/prompts/ORCHESTRATOR.md:166-170`).

### F3 - ACCEPT: rolling SOTA delta is a close gate, not an appendix

The synthesis requires `restart/skinny/ROLLING-SOTA-DELTA.md` for every JSON row/plane and every CSS feature, with `row`, `plane`, `T1_current`, `T1_sota`, `margin`, and `tranche_admitted` columns (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:186-194`). It also says negative margins remain open unless architecturally blocked, and margin regressions fail G7 without architectural-block/user re-pin (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:195-199`). The handoff tells S-P3 to create the concrete wave plan including rolling delta production and gate consumption (`restart/skinny/tranches/sk-v13/HANDOFF.md:130-136`).

This directly satisfies the addendum's rolling reporting requirement across all 51 JSON rows and every CSS feature (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:172-183`).

### F4 - ACCEPT WITH S-P3 CARRY-FORWARD: no support-only waves in the master contract

The synthesis forbids support-only landings unless they are same-wave wired to a measured consumer (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:205-209`) and pre-blocks producer-only SIMD, union, resolver, or codegen artifacts without same-wave consumer measurement (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:220-231`). The handoff's refusal conditions likewise reject support-only primitives, union substrates, resolver infrastructure, or codegen paths without a same-wave measured consumer (`restart/skinny/tranches/sk-v13/HANDOFF.md:138-151`).

This matches the orchestrator CH6 anti-paper-close rule against deferrals and self-reported completion without live evidence (`restart/prompts/ORCHESTRATOR.md:88`, `restart/prompts/ORCHESTRATOR.md:211`) and the user pin's no-support-only rule (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:96-102`).

### F5 - REVISE: inherited scoping still contains support-only/deferred escape hatches

The decision-engine scoping document proposes a W0 "Feature-Gating Scaffold" whose exit gate is only `cargo build --features sk-v13-egraph` and explicitly says scaffold only, no implementation (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:671-676`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:803-806`). It also lists W1b WASM output as an independent `.wasm` artifact (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:686-691`) and defers recognizer registry/materialization strategy/type inference work to SK-V14 (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:637-645`). These are not fatal because synthesis/handoff supersede them, but S-P3 must not copy them into SPEC.md as behavior waves or close-bearing work unless each is bound to a named measured row consumer in the same wave.

Similarly, CSS scoping marks six features "Out of scope for V13" and recommends waves 6-7 as optional/post-admission (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:396-406`), while the user pin requires every non-OUT_OF_SCOPE CSS feature to admit or carry architectural block and says implementation-limited misses are reopens (`restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:22-31`). Synthesis correctly sets the stronger close condition, but S-P3 must treat any "optional" or "post-admission" scoping text as non-authoritative unless it is backed by architectural-block evidence.

### F6 - ACCEPT: the handoff/synthesis avoid paper close

The handoff explicitly warns not to dispatch Wave 0 from the handoff alone (`restart/skinny/tranches/sk-v13/HANDOFF.md:5-7`), names G1-G7 obligations instead of declaring closure (`restart/skinny/tranches/sk-v13/HANDOFF.md:42-52`), and requires immediate bracket if G1-G7 are not fully admitted or architecturally blocked (`restart/skinny/tranches/sk-v13/HANDOFF.md:123-126`). The synthesis states SK-V13 is "not a one-row tranche and not a paper fixpoint" and lists the pinned bar that must close (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:243-249`).

This meets ORCHESTRATOR §3W CH6: no "complete/wired/verified" claim may stand without live evidence and no future-phase deferral is allowed (`restart/prompts/ORCHESTRATOR.md:83-88`).

## Required Revision Before ACCEPT

S-P3 must include an explicit precedence clause in SPEC.md:

1. `SYNTHESIS.md`, `HANDOFF.md`, and the 2026-05-21 user pin override all scoping shortlist language.
2. Scaffold-only, optional, diagnostic, support-only, WASM-only, or future-tranche work cannot be a behavior wave exit unless same-wave consumed by a named JSON/CSS/union/SIMD/decision row gate.
3. Any scoping "out of scope", "deferred", or "post-admission" item touching the pinned CSS/JSON/G2-G7 surface must either be converted to an admitted row target or carry architectural-block/user re-pin evidence.
4. Per-wave hard caps and revert/abrogate protocols must be concrete in SPEC.md, per PASS-ALPHA §4.4 (`restart/prompts/pass-contracts/PASS-ALPHA.md:112-123`).

With that S-P3 guard, CH6 can converge to ACCEPT.
