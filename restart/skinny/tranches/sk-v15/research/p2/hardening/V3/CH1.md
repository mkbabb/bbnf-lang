# SK-V15 S-P2 Hardening V3 CH1 - Correctness Confirmation

Pass: S-P2 Research hardening. Cycle: V3. Lens: CH1 CORRECTNESS.
All path:line citations are relative to `/Users/mkbabb/Programming/bbnf-lang`.
HEAD checked: `884abf03a docs(sk-v15-s-p2): accept V2 hardening after primitive fold`.

## Verdict

ACCEPT.

V2's CH1 ACCEPT still holds at HEAD. I found no CH1 issue requiring REVISE or
REJECT. The existing V2 REVISE/REJECT rows remain candidate dispositions, not
open CH1 defects.

## Governing Standard

CH1 requires resolving claims to file:line, commit SHA, results row, or REDRESS
entry, with measurable falsifiability gates and strictness-plane comparator
deltas (`restart/prompts/ORCHESTRATOR.md:74`-`88`). S-P2 specializes CH1 to
require every candidate primitive to trace to a named S-P1 hot leaf, to reject
speculative kernels, and to cite comparator and ISA claims against the correct
sources (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`100`). S-P3 may draw
its shortlist only from candidates that survive S-P2 CHALLENGE; rejected S-P2
candidates are not shortlist-eligible (`restart/prompts/skinny/PASS-2-RESEARCH.md:192`-`197`).
The SK-V15 addenda add broadcast/gate-exclusion scrutiny but do not weaken CH1
(`restart/skinny/tranches/sk-v15/SYNTHESIS.md:98`-`110`).

## Confirmation Checks

| Check | Result |
|---|---|
| Admitted survivors trace to P1 hot-leaf evidence | PASS. The binding antecedent surface is P1-E's normalized ledger: grammar-neutral scanner, tape/allocation, unicode/string, memory, and direct-parser cursor boundaries; generated wrappers, schema products, comparator frames, checksum paths, and sidecar drift are blocked or diagnostic (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31`-`43`). P2-F's ACCEPT rows stay inside that surface: byte/classifier/literal rows (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:28`-`36`, `:40`-`:41`, `:52`-`:55`, `:63`-`:64`), scalar/local bitmap rows (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:30`-`:31`), and same-tape rows (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:48`-`:51`, `:91`). Their P1 evidence resolves through direct cursor/string/ws rows (`restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:19`-`37`), mode-III UTF-8/structural-scan masking rows (`restart/skinny/tranches/sk-v15/research/p1/p1c-samply-mode-3.md:19`-`45`), and PMU miss boundaries (`restart/skinny/tranches/sk-v15/research/p1/p1d-pmu-cycles.md:70`-`73`). |
| Diagnostic and rejected rows are not shortlist-eligible | PASS. Numeric/digit surfaces are explicitly rejected or diagnostic because `mesh`/decimal evidence is schema/comparator work, not a BBNF-side numeric hot leaf (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:52`, `:64`; `restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:31`, `:45`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154`-`185`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:43`, `:56`, `:108`). `EOB_PAD_CLAMP` is support inventory only (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:51`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:32`, `:109`). PMULL, CSSC, x86, retained sidecars, schema builders, and harness hashes are rejected or diagnostic, not shortlist inputs (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:33`-`35`, `:49`, `:59`-`:72`; `restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:45`-`:47`, `:110`-`:115`). |
| REVISE rows cannot be read as admitted standalone primitives | PASS. Escape/unescape, direct cursor/FIRST-set, wide-shift, and container-count surfaces are retained only as per-grammar templates, host-function surfaces, or implementation details with scalar/parity/same-wave consumer gates (`restart/skinny/tranches/sk-v15/research/p2/p2f-grammar-neutral.md:33`, `:35`, `:37`-`:38`, `:42`, `:44`, `:57`, `:65`-`:68`, `:79`-`:81`, `:93`-`:102`). |
| Comparator/source claims resolve and are not mutable traps | PASS. P2-A records observed source heads for mutable upstreams and pins asmjson, simdjson, and yyjson source URLs to commits (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:101`-`120`). P2-B pins FFmpeg, dav1d, and VLC process sources to commits (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:81`-`88`). Local sonic-rs source paths are versioned as `sonic-rs-0.5.8` (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:108`-`112`). |
| ISA and host claims resolve | PASS. P2-C cites Arm ACLE/Neon sources for CSSC, PMULL, DotProd/UDOT, TBL/TBX, byte compare, and shifts (`restart/skinny/tranches/sk-v15/research/p2/p2c-arch-esoterica.md:76`-`82`). The committed host probe records Apple M5 Max/aarch64 and `FEAT_CSSC`, `FEAT_DotProd`, `FEAT_PMULL`, and NEON presence, while also stating that feature presence alone does not admit a primitive (`restart/skinny/tranches/sk-v15/research/p2/evidence/host-aarch64-sysctl.txt:1`-`23`). SK-V15 also makes native aarch64 the admission platform and excludes x86/AVX-512 anchors (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:42`-`44`). |
| Strictness plane remains clean | PASS. asmjson is explicitly not a strict comparator anchor because of permissive whitespace/control handling (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:20`). sonic-rs is discussed with the lossy mode separated from default strict behavior, and the live bench dependency uses `sonic-rs = "=0.5.8"` with `sort_keys` only, not `utf8_lossy` (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:28`; `skinny/crates/bbnf-bench/Cargo.toml:22`-`23`). simdjson is limited to validating parser modes, and yyjson is cited as strict RFC 8259 UTF-8 by default (`restart/skinny/tranches/sk-v15/research/p2/p2a-sota-teardown.md:36`, `:40`). |

## Issue List

None.
