Verdict: ACCEPT

# SK-V12 S-P1 Hardening V3 CH2: Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 GENERALITY / Lock 14.
Scope: audit current repo commit `ffe5553d` and the SK-V12 S-P1 packet for
grammar-generality claims after the V2 replay/source-line fold. This file owns
only CH2 generality and Lock 14.

## Evidence

1. The packet still frames S-P1 as measurement of the running JSON engine, not a
   grammar-general proof surface. `PASS-1-PROFILE.md` says S-P1 measures the
   JSON engine and that hot leaves must be attributed to grammar-neutral
   primitives rather than JSON-named roles (`restart/prompts/skinny/PASS-1-PROFILE.md:3`-`:9`,
   `:129`-`:135`). The current packet follows that rule: P1-A's load-bearing
   source map uses canonical primitive families such as
   `bounded_plain_string_scan`, `container_dispatch`, `number_digit_span`,
   `string_escape_decode`, and `unicode_escape_hex_decode`
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:120`-`:132`),
   and P1-E defines those names as canonical, grammar-neutral family labels
   while keeping JSON source paths as evidence members
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:198`-`:215`).

2. JSON-only profile facts are not promoted into non-JSON or SK-totality proof.
   P1-A states parse-only rows are diagnostic and cannot count toward SK-V12 SOTA
   admission or close (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:112`-`:114`).
   P1-B states direct rows are JSON digest-plane rows, typed rows are guarded
   JSON typed rows, and neither substitutes for the generated non-JSON baseline
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:148`-`:151`,
   `:205`-`:212`, `:256`-`:268`). P1-D likewise says PMU values are profile
   evidence only and move no row in `skinny/RESULTS.md`
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:90`-`:92`).

3. Non-JSON and totality limits remain explicit. SK-V12 close authority requires
   one generated non-JSON baseline before any JSON-only micro-wave, followed by a
   measured same-row grammar-generalized intervention
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`-`:56`, `:80`-`:84`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:51`-`:67`;
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:33`-`:45`).
   P1-E carries the same boundary and identifies the `json_provider` / missing
   runtime blocker (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:299`-`:322`),
   while P1-F records zero generated non-JSON baseline rows and classifies the
   generated non-JSON baseline as the first material target
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`-`:83`,
   `:206`-`:207`, `:230`-`:244`).

4. The V2 replay/source-line fold does not widen scope. Commit `ffe5553d` adds a
   506-row replay TSV and normalizes line-zero source anchors, but its fold ledger
   says the changes are replay/source-line only, Mode III remains an absence
   boundary, and `skinny/RESULTS.md`, `skinny/REDRESS.md`, and behavior source
   remain unchanged (`restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md:5`-`:13`,
   `:46`-`:78`). The manifest states the replay ledger is the command surface,
   samply rows are artifact-only, xctrace XML is self-time authority, and the
   manifest records profile evidence only with no row movement
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:15`-`:16`,
   `:38`-`:62`, `:165`-`:168`). The TSV itself covers parse/direct/typed JSON
   planes and JSON corpus names; it does not add CSS L4, Sheets, BBNF-self, or
   generated non-JSON rows (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv:1`).

5. The SK-V11/SK-V12 close context stays fenced. `skinny/RESULTS.md` remains the
   JSON result surface with overall `N-direct / NoGo` and Track 1/Track 2 JSON
   provenance (`skinny/RESULTS.md:3`-`:45`, `:143`-`:146`). REDRESS 111 admitted
   only a non-JSON report/gate lane with no generated baseline or row movement,
   REDRESS 112/113 blocked the generated non-JSON baseline/intervention path,
   REDRESS 119 closed direct residuals as a measured fixpoint, and REDRESS 120
   closed SK-V11 not as a grammar-generalization admission
   (`skinny/REDRESS.md:3284`-`:3309`, `:3313`-`:3355`, `:3497`-`:3527`,
   `:3531`-`:3553`; `restart/skinny/tranches/sk-v11/research/close/close-redress.md:52`-`:82`).
   The accepted SK-V11 S-P1 close allowed JSON-only profile telemetry to nominate
   primitive families, but not to prove CSS L4, Sheets, or BBNF-self behavior
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:53`-`:55`).

## Required Fold

None.

## Verdict

ACCEPT. No JSON-only profile fact is promoted as grammar-neutral proof, the
non-JSON/SK-totality limits remain explicit, and the replay/source-line fold
improves provenance without widening the packet beyond JSON profile evidence.
