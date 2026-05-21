# SK-V13 S-P2 V1 CH6: Anti-Paper-Close

## Verdict

REVISE.

## Evidence

- The S-P2 CH6 bar is explicit: comparator claims need comparator source, ISA claims need manual sections, primitive claims need scalar-reference sketches, and candidates cannot defer their detail to a future wave (`restart/prompts/skinny/PASS-2-RESEARCH.md:133-138`).
- P2-A is mostly orchestrator-citable. It distinguishes binding JSON admission from architecture pressure: sonic-rs strict is the JSON admission comparator, while simdjson / yyjson / asmjson remain non-admission pressure unless same-run same-plane sidecars are wired (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42-52`). Its candidate table gives scalar-reference status, checkasm expectations, P1 antecedents, and same-wave reject boundaries for C1-C8 (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:72-81`).
- P2-B grounds the SIMD admission process in local checkasm mechanics and same-wave row gates: scalar reference, differential checkasm, microbench, production consumer, grammar policy, and row gate are all required stages (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:24-35`). It also rejects support-only SIMD by requiring same-wave consumers for each B candidate (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:47`, `:61-63`, `:75-77`, `:89-91`, `:103-105`).
- P2-C gives citable ISA pressure and drops ungrounded routes instead of paper-closing them: EOR3 is inventory-only without a current three-input hot expression, byte-context is close hygiene rather than row movement, and `cache_hints` stays non-selectable without a P1 leaf (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:24`, `:37-38`, `:60`).
- P2-D avoids the union paper-close. It states legal substrate work is same-tape or sink-only projection, not retained scanner output, then excludes `StructuralIndex`, class columns, streaming cursors, aux density tables, whitespace bitmaps, and `UnionTape` as non-candidates (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:125-137`, `:349-359`).
- P2-E grounds parse-that candidates with scalar sketches and consumer requirements. It explicitly says crate extraction without resolver/codegen consumption is support-only and should fail CH6, and it keeps structural scan legal only with generated JSON/CSS parsing or union consumption in the same wave (`restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:96-116`).
- P2-F is doing the right anti-paper-close work for grammar neutrality: it labels candidates as admissible, conditional, JSON-overfit, or inventory-only; marks EOR3, cache hints, standalone bitmap primitives, and unsupported LD4/TBX refinements as not eligible; and rejects JSON-specific wrappers as written (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:28-42`, `:132-145`).

## Blockers / Fold Requirements

- P2-B must add exact dav1d source-file anchors or downgrade the dav1d-specific claim. The artifact currently relies on FFmpeg/VideoLAN sources and explicitly admits it did not verify a specific dav1d source-file URL (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:130`). Under CH6, that is not orchestrator-citable enough for a document scoped as dav1d / FFmpeg / VLC process.
- P2-C should tighten ISA citations from broad Arm reference URLs to named sections or intrinsic entries for CSSC CTZ, PMULL, UDOT, TBL/TBX, EXT, and EOR3 before S-P3 copies them into gate text (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:16-24`, `:64-68`).
- P2-F or the consolidated V2 fold should preserve the current inventory/drop decisions. In particular, EOR3, cache hints, standalone prefix/next/bulk bitmap primitives, and JSON-specific dispatch/sink rewrites must not re-enter S-P3 as shortlist items without fresh P2 evidence (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:139-145`, `:174-184`).
