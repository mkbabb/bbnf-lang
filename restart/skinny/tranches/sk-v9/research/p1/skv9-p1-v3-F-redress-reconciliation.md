# SK-V9 P1-V3-F: REDRESS And Spec Reconciliation Manifest

Pass: S-P1 Profile. Cycle: V3 (sibling to P1-V3-A/B/C/D/E).
Date: 2026-05-18.
Scope: PMU-unblock reconciliation — does the contract admit xctrace c/B, which
REDRESS entries remain load-bearing, which are superseded, and what surgical
edits should land in SPEC/HANDOFF/DISPATCH-PROMPT once V3 siblings commit.
Output: this file. Read-only; no source edits and no doc edits applied here.
Baseline read: SK-V9-open
(`sk-v9-open:criterion-fnv64-cd1673844eeea12f`), V2 HARDENING CONSOLIDATED
(BLOCKED, 4/6 ACCEPT), `skinny/REDRESS.md` entries 1-93,
`restart/prompts/skinny/PASS-1-PROFILE.md` end-to-end.
Disposition: propose-only.

---

## §0 — V4 fold footer

V4 fold: PASS-1-PROFILE edit dropped per orchestrator scope; edit-count
reconciled; strictness-plane assertion explicit; SUPERSEDED reasoning
expanded.

---

## §1 — Contract Reconciliation: Does The Contract Admit xctrace c/B?

### §1.1 — Verbatim contract clauses

**`restart/prompts/skinny/PASS-1-PROFILE.md` §2 (P1-D scope row):**

> "PMU counters (cycles, instructions, branch-misses, L1/LLC misses) and
> derived cycles-per-byte for every corpus × workload. … Establish the c/B
> baseline that `gate-json` consumes."

**`restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH1 CORRECTNESS:**

> "Are the c/B figures derived from real PMU counters, not estimated?"

**`restart/prompts/skinny/PASS-1-PROFILE.md` §1 entry condition:**

> S-P1 produces "the cycles-per-byte ledger, the per-corpus hot-leaf
> attribution, and the delta-vs-prior-SK telemetry."

**`restart/skinny/tranches/sk-v9/SPEC.md` §4:**

> "Current blocker: P1-D has no real PMU/cycles source. `perf` is absent,
> `xctrace` requires full Xcode, and `powermetrics` requires unavailable
> superuser access. Do not estimate c/B from Criterion `ns_per_byte`."

**`restart/skinny/tranches/sk-v9/HANDOFF.md` §4:**

> "Resolve the P1-D PMU/cycles blocker by providing a real counter source
> (`perf`, full-Xcode `xctrace`, privileged `powermetrics`, or an accepted
> contract amendment). Do not estimate c/B from ns/B."

**`restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md` §2:**

> "No real PMU counter source is available in this execution context. S-P1
> forbids estimated cycles-per-byte, so P1-D does not convert Criterion
> `ns_per_byte`, wall-clock loop times, or inferred clock frequency into c/B."

**`HARDENING-S-P1-V2-CONSOLIDATED.md` §Verdict:**

> "The contract requires real PMU counters."
> "`xctrace` requires full Xcode; active developer dir is CommandLineTools."

**`HARDENING-S-P1-V2-CONSOLIDATED.md` CH6 ANTI-PAPER-CLOSE:**

> "Bad panic profiles were rerun; PMU is honestly blocked, not estimated."

### §1.2 — Verdict: contract admits xctrace c/B unambiguously

The contract's discipline boundary is precisely "real PMU counters, not
estimated." The forbidden operation is *ns→c/B inference* — converting
Criterion's wall-clock `ns_per_byte` into c/B by multiplying through an
inferred or nominal clock frequency. Every clause cites that as the failure
mode:

- PASS-1-PROFILE CH1 calls out "estimated" as the rejection criterion.
- SPEC §4 names the forbidden conversion explicitly: "Do not estimate c/B
  from Criterion `ns_per_byte`."
- HANDOFF §4 names the forbidden conversion explicitly: "Do not estimate c/B
  from ns/B."
- P1-D §2 names the forbidden conversion explicitly: "does not convert
  Criterion `ns_per_byte`, wall-clock loop times, or inferred clock frequency
  into c/B."

`xctrace` (with the cpu-counters template) is a **direct hardware-counter
read** through Apple Silicon's PMU via kernel `kpc` APIs surfaced by
Instruments / xctrace. It is not an ns-derived estimate; it is the same class
of source the contract explicitly names as acceptable. HANDOFF §4 already
lists "full-Xcode `xctrace`" as a real counter source. The V2 P1-D agent did
not reject xctrace on contract grounds — it reported xctrace as
**unrunnable** (active developer dir was CommandLineTools, not Xcode), and
recorded that honestly per CH6. The BLOCKED disposition is an
infrastructure-availability block, not a contract-language ambiguity.

Therefore, with the orchestrator's xctrace + Xcode-license unblock in V3,
P1-V3-A/B trace bundles produce **contract-admissible** c/B rows. No
contract weakening is required.

### §1.3 — Proposed one-paragraph clarification (NOT amendment)

The current language is already correct but reads as if xctrace itself were
the blocker rather than the host's missing Xcode. To preempt future
misreading, add the following clarification near SPEC §4 (propose-only —
see §4.1 Edit I for diff shape):

> Direct hardware-counter reads — `perf` on Linux, `xctrace` with the
> `cpu-counters` template on macOS (full Xcode required), or privileged
> `powermetrics` — are admitted as real PMU sources. Indirect derivation from
> `ns_per_byte`, wall-clock loop time, or any inferred/nominal CPU frequency
> is rejected regardless of source. The discipline is real-counter-vs-ns,
> not tool-by-tool.

This preserves the no-estimation discipline and is non-weakening; it only
disambiguates the source surface. PASS-1-PROFILE.md amendments are Pass
Omega scope per `ORCHESTRATOR.md` §7 (prompts are read-only contracts; only
Pass Omega CRUD amends them post-G-Omega); the parallel PASS-1-PROFILE
clarification is queued for Omega input, not SK-V9 dispatch.

---

## §2 — REDRESS Ledger Reconciliation Table

**Strictness-plane assertion.** Every comparator delta carried into this
reconciliation — the Apache/CITM measured-row references in REDRESS 91, the
Canada scan-floor admit in REDRESS 56, the SK-V7 W0/W0b sonic-strict repair
admits in REDRESS 77/78, the per-corpus direct-plane and parse-plane Δ
values surfaced under §3 — is sourced from S-P1 evidence rows whose
strictness plane is `strictness=strict, freshness=same-run-native` per the
`SK-V9-open` manifest at `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Sidecar, permissive, `utf8_lossy`, or cross-run rows appear in REDRESS only
as flaw-probe artefacts (REDRESS 75 sonic `utf8_lossy` ineligibility; REDRESS
77 strict-feature repair predicate) and are explicitly NOT treated as
behavior-admission ancestors in any STILL-LOAD-BEARING entry. The
strict-vs-strict-same-run discipline holds across V3 evidence: V3 P1-V3-A
PMU rows, P1-V3-B xctrace TP per-row tables, and P1-V3-D structural
correlation all draw from the same `SK-V9-open` strict-plane rows; no
permissive/sidecar comparator is consumed as truth anywhere in this manifest.

Status vocabulary:

- **STILL-LOAD-BEARING** — the rejection or admission is a binding pre-block
  for SK-V9 + later waves; do not reopen without the named gate.
- **SUPERSEDED** — a later REDRESS entry, gate close, or admit invalidated
  the original rejection or admission as a binding constraint; cite the
  superseder.
- **HISTORICAL** — preserved for context (provenance of the baseline) but no
  longer a pre-block on its own; later monotonic admissions or rejections own
  the live constraint.

Citations are by REDRESS entry number unless a commit SHA is materially
relevant.

### §2.1 — Block 1: Current Bench Fact preamble (REDRESS lines 1-91, three numbered "facts" only)

| # (preamble) | Shape | Status | Citation |
|---:|---|---|---|
| Fact 1 (13 G rows, 4 A rows) | Baseline diagnosis at SK-V6/V7 close | HISTORICAL | Superseded by `SK-V9-open` 38-row manifest; W0 close artifact records the current row identities. |
| Fact 2 (Canada structural scan above 40000 Mbps floor) | Scan-floor green | STILL-LOAD-BEARING | Item 56 + the scan-floor admit; still binds Track 2 / structural-scan ceiling. |
| Fact 3 (direct-to-struct N-direct / NoGo; 4 digest passes + 2 typed passes) | Baseline diagnosis | HISTORICAL | Superseded by the `SK-V9-open` row classification; current main families: `parse_only` 17 S/NoGo, `direct_to_struct` 3 A/GO + 14 N-direct, `real_typed_struct` 4 A/GO (per HANDOFF §2). |

### §2.2 — Block 2: "Implemented Redress" 1-26 (the foundational substrate)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 1 | Report units are Mbps | STILL-LOAD-BEARING | Telemetry-lock invariant; gate-json still consumes Mbps. |
| 2 | Parse-index vs structural-scan split | STILL-LOAD-BEARING | Still a non-negotiable on substrate boundary. |
| 3 | Track 1 / Track 2 share one-buffer tape builder | STILL-LOAD-BEARING | Lock 1 substrate-union. |
| 4 | Parser whitespace materialization corrected | STILL-LOAD-BEARING | Substrate boundary. |
| 5 | Tape / direct-to-struct one substrate | STILL-LOAD-BEARING | Lock 1. |
| 6 | Payload arena cold on JSON | STILL-LOAD-BEARING | Workload assumption baked into report. |
| 7 | BIR carries materialization events | STILL-LOAD-BEARING | Substrate-invariant. |
| 8 | Bench metadata no payload counter hardcode | STILL-LOAD-BEARING | Telemetry hygiene. |
| 9 | Tape materialization as report artifact | STILL-LOAD-BEARING | Diagnostic non-producer (also pre-block class). |
| 10 | Masking probes as report artifact | STILL-LOAD-BEARING | Diagnostic non-producer (pre-block class). |
| 11 | Generated runtime owns JSON API | STILL-LOAD-BEARING | Lock 14 antecedent. |
| 12 | Tightened number/whitespace scanners | HISTORICAL | Subsequent W7/W8 Lock 14 reshaping owns the current state (entries 85, 86). |
| 13 | Close-token elision canonical | STILL-LOAD-BEARING | Substrate shape. |
| 14 | Parser-grade structural byte vector removed | STILL-LOAD-BEARING | Substrate fence. |
| 15 | Tape sealing is private-Vec semantic sealing | STILL-LOAD-BEARING | Substrate API. |
| 16 | Pair-token fusion measured + rejected | STILL-LOAD-BEARING | Pre-block on a recurrent shape. |
| 17 | Dispatch-table alternate rejected as signal | STILL-LOAD-BEARING | Pre-block class. |
| 18 | Skipless 12-byte tape tokens rejected | STILL-LOAD-BEARING | Pre-block class. |
| 19 | Host-call dispatch / eager-decode split | STILL-LOAD-BEARING | Diagnostic discipline. |
| 20 | Lazy-offset tape-union migration | STILL-LOAD-BEARING | Lock 1 implementation. |
| 21 | Lock 15 release-profile discipline | STILL-LOAD-BEARING | Build flags invariant. |
| 22 | `bbnf-simd` replaced runtime scanner surface | STILL-LOAD-BEARING | Crate boundary. |
| 23 | Sparse flags + direct spare-capacity offset writes | STILL-LOAD-BEARING | Substrate implementation. |
| 24 | Parser hot-path wins without substrate change | HISTORICAL | Later candidates (60-90) own the live hot-path posture. |
| 25 | Measured alternates remain rejected | STILL-LOAD-BEARING | Class pre-block; cites 16, 17, 18 chain. |
| 26 | Bench auditability gates landed | STILL-LOAD-BEARING | Telemetry hygiene. |

### §2.3 — Block 3: SK-V3 → SK-V4 split (27-32)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 27 | SK-V3 reprofile split blockers by mechanism | HISTORICAL | Predates SK-V5+ measured authority; preserved as provenance. |
| 28 | SK-V3 Wave 0/1 SIMD parity + host aarch64 admit | STILL-LOAD-BEARING | The 28+33 pair pre-blocks NEON `match_tiny_plain_string` as retained parse-G fix; still cited by V8 W3 (REDRESS 92) and V9 V2 hardening. |
| 29 | HEAD vocabulary state at SHAs `74406332` / `9eef728c` | HISTORICAL | Provenance only. |
| 30 | Direct-to-struct is a throughput gate | STILL-LOAD-BEARING | Class invariant. |
| 31 | Direct sink profiling pivot | HISTORICAL | Superseded by 50-55, 66-69, 71 direct-class outcomes. |
| 32 | Gate status / budget-cliff executable | STILL-LOAD-BEARING | Telemetry hygiene. |

### §2.4 — Block 4: SK-V5 Wave 3 + Wave 1/2/4 (33-48)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 33 | NEON `match_tiny_plain_string` retained wiring INVALIDATED | STILL-LOAD-BEARING | Paired with 28; permanent pre-block on Class A as retained-G fix. |
| 34 | Bench-private `SinkParser` dishonesty IDENTIFIED | STILL-LOAD-BEARING | Superseded as a defect by 40 (generated `SinkOnly` is the Track 1 direct) but the *rejection* of bench-private hand parsers as Track 1 is permanent. |
| 35 | Codegen lowerer scaffolding gap IDENTIFIED | SUPERSEDED | Closed by 40, 48, 71, 81 generator path. |
| 36 | JSON-hardcoded scalar references in `bbnf-simd` IDENTIFIED | SUPERSEDED | Closed by 85, 86 Lock 14 Phase A-D. |
| 37 | `bbnf-simd/src/lib.rs` JSON god-module status IDENTIFIED | SUPERSEDED | Closed by 85, 86. |
| 38 | `crates/simd-scan/` fossil status IDENTIFIED | SUPERSEDED | Crate restructure landed pre-SK-V7. |
| 39 | Eisel-Lemire vendored | STILL-LOAD-BEARING | Crate-locked number path; pre-block on f64 fallback rewires (80). |
| 40 | Generated `SinkOnly` is Track 1 direct-to-struct | STILL-LOAD-BEARING | Lock 14 antecedent. |
| 41 | `CARGO_TARGET_DIR` gate / metadata routing | STILL-LOAD-BEARING | Build hygiene. |
| 42 | Trusted-UTF-8 boundary matching VALIDATED | STILL-LOAD-BEARING | Substrate-shape pre-block. |
| 43 | Active post-escape skip + validation-batch rejected | STILL-LOAD-BEARING | Pre-block class. |
| 44 | Direct Track 2 false strict-string penalty REMOVED | HISTORICAL | Reporting hygiene only. |
| 45 | SK-V5 Wave 3 close decision | HISTORICAL | Provenance. |
| 46 | Direct-number / context-sink redress | HISTORICAL | Superseded by 71, 81 typed-path outcomes. |
| 47 | Reporting redress (advisory + output-plane) | STILL-LOAD-BEARING | Telemetry-lock antecedent. |
| 48 | SinkOnly lowerer consumes BIR | STILL-LOAD-BEARING | Lock 14 antecedent. |

### §2.5 — Block 5: SK-V5 Direct/Cursor + Wave 5 + Sonic-closeness (49-56)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 49 | Generated source-hook string ADMIT (direct) | SUPERSEDED | Class re-rejected by 66 source-hook field-layout direct close; surface remains, route is closed. |
| 50 | Retained projection side tables REJECTED | STILL-LOAD-BEARING | Recurrent class pre-block (cited by 92, 59). |
| 51 | Byte-class whitespace cursor REJECTED | STILL-LOAD-BEARING | Pre-block class. |
| 52 | SK-V5 baseline reassay | HISTORICAL | Provenance. |
| 53 | Structural-mask parser-local cursor REJECTED | STILL-LOAD-BEARING | Recurrent class pre-block. |
| 54 | Exact decoded-string stats sink REJECTED | STILL-LOAD-BEARING | Cited by 69, 70 as pre-block class. |
| 55 | Quote-source fused string materializer REJECTED | STILL-LOAD-BEARING | Cited by 69, 70 as pre-block class. |
| 56 | Structural-scan floor ADMITTED | STILL-LOAD-BEARING | Canada scan-floor; cited in §0 preamble. |
| 57 | Direct receiver inlining + tiny-plain-string fast path ADMIT | STILL-LOAD-BEARING | Live in current direct paths. |
| 58 | SK-V6 dispatch framing | STILL-LOAD-BEARING | Audit-history pre-block: no SK-V3/V4 packet revival without fresh PC profile. |
| 59 | UTF-8 fusion class REFUTED | STILL-LOAD-BEARING | Recurrent class pre-block. |

### §2.6 — Block 6: SK-V6 Wave 2 retained candidates (60-65)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 60 | Retained trusted-string boundary collapse REJECTED | STILL-LOAD-BEARING | Permanent rejection. |
| 61 | Always-wide retained long-string trusted scan REJECTED | STILL-LOAD-BEARING | Class pre-block; cited by 62, 83. |
| 62 | Delayed-wide retained trusted scan REJECTED | STILL-LOAD-BEARING | Class pre-block broadening 61. |
| 63 | Array `ContainerNext` / next-byte carry ADMIT | STILL-LOAD-BEARING | Live in generated retained Track 1. |
| 64 | Retained Unicode-escape run validator REJECTED | STILL-LOAD-BEARING | Class pre-block; cited by 82, 84. |
| 65 | Object next-key carry REJECTED | STILL-LOAD-BEARING | Class pre-block; named in V8 HANDOFF §5 always-blocked list. |

### §2.7 — Block 7: SK-V6 Wave 3 direct + typed candidates (66-73)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 66 | Direct source-hook field-layout materializer REJECTED | STILL-LOAD-BEARING | Class pre-block; cited by 67-69 and 84. |
| 67 | Parser-owned decoded scratch (escaped) REJECTED | STILL-LOAD-BEARING | Class pre-block. |
| 68 | Byte-output `unescape_json_string` REJECTED | STILL-LOAD-BEARING | Class pre-block. |
| 69 | DirectBuild semantic string facts REJECTED | STILL-LOAD-BEARING | Class pre-block (digest workload). |
| 70 | First `real_typed_struct` attempt REJECTED | SUPERSEDED | Architectural lesson preserved; concrete admit landed in 71. |
| 71 | Generated typed DirectBuild from host/API schema ADMIT | STILL-LOAD-BEARING | Live in `real_typed_struct A / GO` rows. |
| 72 | Cap-16 `match_tiny_plain_string` for generated retained `OffsetTape` ADMIT (split: cap-8 elsewhere) | STILL-LOAD-BEARING | Live. |
| 73 | Retained Track 2 array-next-byte parity transfer REJECTED | STILL-LOAD-BEARING | Helper-shape transfer pre-block; named in V8 HANDOFF §5 always-blocked list. |

### §2.8 — Block 8: SK-V6 asmjson/DAV1D synthesis (74-76)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 74 | asmjson/DAV1D synthesis (spec redress) | STILL-LOAD-BEARING | Architecture pre-block; binds CollapsedStage, Lock 16. |
| 75 | Comparator-plane correction (sonic `utf8_lossy` ineligibility) | STILL-LOAD-BEARING | Telemetry-lock antecedent; resolved at row level by 77, 78. |
| 76 | C-pass profiling / generality refinement | STILL-LOAD-BEARING | Profile-truth invariant for S-P2 future intervention. |

### §2.9 — Block 9: SK-V7 Wave 0/0b telemetry (77-78)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 77 | sonic-rs strict feature repair + row-flip forecast refuted | STILL-LOAD-BEARING | Comparator hygiene. |
| 78 | Schema-v3 telemetry + same-run strict/lossy provenance | STILL-LOAD-BEARING | Telemetry-lock antecedent. |

### §2.10 — Block 10: SK-V7 Lock 14 + numeric/unicode (79-86)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 79 | TapeKind descriptor-preserving rename | STILL-LOAD-BEARING | Lock 14 substrate state. |
| 80 | W2 mantissa-widen route REJECTED | STILL-LOAD-BEARING | Zero-fallback canada; permanent pre-block. |
| 81 | Capacity-hinted numeric Vec real-typed expansion ADMIT (mesh + marine_ik) | STILL-LOAD-BEARING | Live in `real_typed_struct A / GO`. |
| 82 | Single-quartet Unicode escape classifier REJECTED | STILL-LOAD-BEARING | Class pre-block extending 64. |
| 83 | StringBlock16 tiny probe REJECTED | STILL-LOAD-BEARING | Class pre-block. |
| 84 | Object-pair value-byte control compaction REJECTED | STILL-LOAD-BEARING | Class pre-block reaffirming 65; named in V8 HANDOFF §5. |
| 85 | Lock 14 Phase A+B neutralization ADMIT | STILL-LOAD-BEARING | Generic-crate JSON-name fence. |
| 86 | Lock 14 Phase C+D codegen shell ADMIT | STILL-LOAD-BEARING | Generic-crate JSON-name fence. |

### §2.11 — Block 11: SK-V7 CostFacts + B6 canary (87-90)

| # | Shape | Status | Citation |
|---:|---|---|---|
| 87 | CostFacts substrate projection ADMIT | STILL-LOAD-BEARING | Decision-substrate antecedent. |
| 88 | PMULL prefix-XOR consumed bitmap bodies REJECTED | STILL-LOAD-BEARING | Class pre-block named in V8 HANDOFF §5 + V9 HANDOFF §5. |
| 89 | CTZ bulk consumer + canary fold REJECTED | STILL-LOAD-BEARING | Class pre-block paired with 88. |
| 90 | B6 stack-canary Stage 1 ADMIT | STILL-LOAD-BEARING | Checkasm hardening only; not a behavior producer. |

### §2.12 — Block 12: SK-V8 routes (91-93) — the trio carried into SK-V9

| # | Shape | Status | Citation |
|---:|---|---|---|
| 91 | Apache/CITM typed product-plane source admit (NO measured-row admission) | STILL-LOAD-BEARING | Pre-block on overclaim; only fresh row/run evidence can lift the 4 → 6 measured `real_typed_struct A / GO` rows. Carried into V9 HANDOFF §3. |
| 92 | Tape + structural-projection W3 REJECTED before source redress | STILL-LOAD-BEARING | Pre-block; carried verbatim into V9 SPEC §7 and HANDOFF §5. |
| 93 | Direct guard triage W4 scalar-parent fold REJECTED | STILL-LOAD-BEARING | Pre-block; carried into V9 SPEC §8 and HANDOFF §5. |

### §2.13 — Roll-up

- STILL-LOAD-BEARING: ~60 entries (the bulk of the ledger; every recurrent
  class pre-block, every Lock 14 / Lock 1 / Lock 15 / Lock 16 antecedent,
  every comparator-hygiene admit).
- SUPERSEDED: 7 entries (35, 36, 37, 38, 46, 49, 70) — each one closed by
  a later monotonic admission or rejection that owns the live constraint.
  Provenance lines remain in REDRESS unchanged; what changed is whether the
  entry itself binds today. Per-citation supersession-chain reasoning:
    - **35** (Codegen lowerer scaffolding gap IDENTIFIED) → closed by **40**
      (generated `SinkOnly` is Track 1 direct-to-struct), **48** (SinkOnly
      lowerer consumes BIR), **71** (generated typed DirectBuild from
      host/API schema ADMIT), and **81** (capacity-hinted numeric Vec
      real-typed expansion ADMIT). The admits land the concrete generator
      path the IDENTIFIED scaffolding gap diagnosed; 35's "gap" shape is a
      diagnosis, the admit chain's shape is a delivered generator, so the
      admits supersede the diagnosis without contradicting it.
    - **36** (JSON-hardcoded scalar references in `bbnf-simd` IDENTIFIED) →
      closed by **85** (Lock 14 Phase A+B neutralization ADMIT) and **86**
      (Lock 14 Phase C+D codegen shell ADMIT). The admits neutralize the
      grammar-name leaks and lock the fence; 36 diagnosed JSON-name presence
      in a generic crate, the admits remove the JSON-name presence under the
      Lock 14 fence, so the admits' fence shape supersedes the diagnosis.
    - **37** (`bbnf-simd/src/lib.rs` JSON god-module status IDENTIFIED) →
      closed by **85, 86** (same Lock 14 Phase A-D chain as 36). 37
      diagnosed god-module structure; the admits refactor structure into the
      generic-crate codegen shell, so the admits' codegen-shell shape
      supersedes the god-module diagnosis.
    - **38** (`crates/simd-scan/` fossil status IDENTIFIED) → superseded by
      the SK-V6/V7 crate restructure (workspace member removed pre-SK-V7;
      verified live at the filesystem layer per P1-V3-E §2.7 spot-check —
      directory does not exist on disk). 38's fossil shape is "directory
      exists, unused"; the restructure's shape is "directory removed", so
      the restructure supersedes the diagnosis by deleting the artefact.
    - **46** (Direct-number / context-sink redress) → superseded by **71**
      (generated typed DirectBuild ADMIT covering mesh + marine_ik) and
      **81** (capacity-hinted numeric Vec real-typed expansion ADMIT). 46's
      shape is a context-sink number-redress proposal; 71/81's admits are
      typed-path generators that land the direct-number outcome at row
      level (`real_typed_struct A / GO` on numeric corpora), so the typed
      admits supersede the context-sink proposal.
    - **49** (Generated source-hook string ADMIT, direct) → superseded by
      **66** (direct source-hook field-layout materializer REJECTED). 49's
      admit retains the source-hook surface but 66 closes the field-layout
      route that consumed it; class re-rejected by 66 means the surface
      exists but the route is closed, so 66's class rejection supersedes
      49's admit as the binding constraint (the admit is no longer a
      forward producer).
    - **70** (First `real_typed_struct` attempt REJECTED) → superseded by
      **71** (generated typed DirectBuild from host/API schema ADMIT). 70's
      reject shape is "first attempt failed on architectural grounds"; 71's
      admit shape is "second attempt landed under the host/API schema
      lesson", so 71's admit supersedes 70's reject as the binding
      constraint (the architectural lesson is preserved in 70 but the
      live producer is 71).
- HISTORICAL: ~14 entries (preamble facts 1/3, 12, 24, 27, 29, 31, 44, 45,
  52, plus the three preamble bullets) — preserved for chain-of-evidence
  reading; live constraint owned elsewhere.

No PMU/c/B-related REDRESS entry is invalidated by the V3 unblock. The
xctrace unblock changes **which P1-D row populates** the c/B ledger; it does
not retroactively re-admit any rejected behavior route. Every class
pre-block above stays binding.

---

## §3 — Pre-Blocked-Routes Delta Against SK-V9 HANDOFF §5

SK-V9 HANDOFF §5 currently enumerates eight pre-blocked routes:

1. Apache/CITM measured-row overclaim from REDRESS 91.
2. `canada/real_typed_struct` without full-fixture DirectBuild-vs-serde
   checksum proof.
3. W3 structural implementation without retained class/event grammar plus
   retained `ValueRef` cursor proof.
4. W4 scalar-parent fold or renamed parent-digest fold without a V9-aware
   checked gate, full-table maintain proof, and independent Track 2
   digest-arithmetic backstop.
5. REDRESS 73 helper-shape transfer from generated retained parsing to hand
   Track 2 or control-path work without direct hand-parser code-layout
   profiling.
6. Sidecar substrate, parser-owned cursor/fact slots, `UnionTape`, new
   `BackendShape`, new directive/BIR, public substrate API, and
   `tape_vs_tape` as production consumer.
7. PMULL prefix-XOR and CTZ/bulk production rewires as default hot paths.
8. Generic JSON policy leaks or Lock 14 weakening.

### §3.1 — Coverage against §2 STILL-LOAD-BEARING table

- Item 1 ↔ REDRESS 91. Correct.
- Item 2 ↔ REDRESS 91 + V8 W2 canada exclusion. Correct.
- Item 3 ↔ REDRESS 92. Correct.
- Item 4 ↔ REDRESS 93. Correct.
- Item 5 ↔ REDRESS 73. Correct.
- Item 6 ↔ class umbrella over REDRESS 50, 51, 53, 92, and the SPEC §1
  non-negotiables. Correct as a class pre-block.
- Item 7 ↔ REDRESS 88, 89. Correct.
- Item 8 ↔ REDRESS 85, 86, 87 + Lock 14. Correct.

### §3.2 — Entries the §5 list under-represents

The HANDOFF §5 list is concise on purpose, but the following classes appear
repeatedly in REDRESS and are not surfaced explicitly. Surfacing them by
class umbrella prevents repeat agents from re-proposing them:

- **String-scanner widening / boundary-collapse class** (REDRESS 60, 61, 62,
  64, 65, 82, 83, 84). Current §5 does not call this class out; it is the
  most-rejected class in the ledger and an agent reading §5 alone would not
  see it. Propose adding: *"Retained or direct string-scan widening, trusted
  boundary collapse, value-byte/next-key carry, and per-quartet/per-segment
  unicode-escape classifier routes without a same-row falsification gate
  pre-registered in a revised S-P3 plan."*
- **Direct receiver / scratch / semantic-fact class** (REDRESS 49 superseded,
  66, 67, 68, 69). Same observation: a single umbrella in §5 would close the
  recurrent re-proposal door. Propose adding: *"Direct source-hook field
  folding, parser-owned decoded scratch, byte-output `unescape_*` rewrites,
  and DirectBuild semantic-string-fact streaming for the digest workload."*
- **Bench-private hand Track 1 / hand typed sink class** (REDRESS 34, 70).
  Propose adding: *"Bench-private hand Track 1 parsers or hand typed sinks
  presented as generated direct/typed proof."*
- **PMU / cycles / Criterion-slope / masking / structural-scan as producer**
  (already in V9 SPEC §1; not explicit in §5). Propose adding: *"PMU,
  cycles-per-byte, masking probes, structural-scan-only paths, and Criterion
  slope artifacts as Track 1, Track 2, typed product, direct product, or
  strict admission producers. They remain diagnostic non-producers under V3
  PMU evidence too."* (V3 PMU does not lift them; it characterises hot
  leaves with real c/B.)

### §3.3 — Entries the §5 list could drop or restate

None. All eight HANDOFF §5 items are STILL-LOAD-BEARING. The pre-block list
is correct; it is merely incomplete relative to the underlying ledger.
**Recommendation: keep all eight, add the four umbrellas above as one
bulleted block under "additional class umbrellas binding by reference"**.

### §3.4 — PMU unblock does not change any §5 entry

The xctrace + Xcode license unblock changes only the population of the c/B
column in the P1-V3 evidence root; it produces no new admit, no rejected
route, and no pre-block invalidation. §5 stays exactly as written for
**routes**. The PMU/c/B *infrastructure* status is what changes (SPEC §4
language and HANDOFF §2 / §4 — see §4 below).

---

## §4 — Surgical Edit Proposal For SPEC.md / HANDOFF.md / DISPATCH-PROMPT.md

Diff-shaped. NOT applied here. All edits below assume the V3 sibling
artefacts have committed and the V3 CHALLENGE consolidation is in flight or
landed (P1-V3-F is one of the siblings; this report is its commit).

**Edit count (V4-folded).** §4.1 SPEC.md enumerates 9 labelled Edit entries
(A through I); Edit E is an explicit deferral decision (do not edit §0.3
in this pass) and is not a surgical edit. Counting actual surgical edits:
SPEC.md §4.1 = 8 (A, B, C, D, F, G, H, I); HANDOFF.md §4.2 = 6 (A-F);
DISPATCH-PROMPT.md §4.3 = 5 (A-E). **Total: 19 actual surgical edits.** No
edit proposes to amend `restart/prompts/skinny/PASS-1-PROFILE.md` or any
other pass-prompt surface, since prompts are out of orchestrator scope per
`ORCHESTRATOR.md` §7 (Pass Omega CRUD scope only).

### §4.1 — `restart/skinny/tranches/sk-v9/SPEC.md`

**Edit A — line 5-10 (Status block).** Replace V2-BLOCKED disposition with
the V3 reframe.

```diff
 Status: post-G-Alpha, post-W0 telemetry-lock, and post-S-P1-V2-BLOCKED.
 G-Alpha is closed by user instruction, W0 closed `G-W0-TELEMETRY-LOCK`, and
-S-P1 V2 produced fresh samply evidence but failed convergence because real
-PMU/cycles-per-byte counters are unavailable on this host. Behavior waves remain
-conditional placeholders until S-P1 rerun convergence and
-`G-BEHAVIOR-RELEASE`.
+S-P1 V2 produced fresh samply evidence but failed convergence because PMU/c/B
+counters were unavailable on the V2 host. V3 unblocked PMU via Xcode license
++ xctrace (`cpu-counters` template); V3 siblings P1-V3-A/B/C/D capture and
+attribute real c/B, and P1-V3-E/F audit legacy docs and reconcile this
+ledger. Behavior waves remain conditional placeholders until
+`G-S-P1-RERUN-CONVERGED` accepts the V3 evidence root and
+`G-BEHAVIOR-RELEASE` follows.
```

**Edit B — Authority block (lines 12-27).** Add the V3 evidence root once
siblings land. (Propose text only — the exact path is set by the
orchestrator's V3 commit.)

```diff
 - `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
+- `restart/skinny/tranches/sk-v9/research/p1/V3/` (V3 sibling artefacts:
+  P1-V3-A trace bundle, P1-V3-B trace bundle, P1-V3-C deep hot-leaf
+  attribution, P1-V3-D structural correlation, P1-V3-E legacy audit, this
+  P1-V3-F reconciliation manifest).
+- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`
+  (once V3 CHALLENGE lands).
```

**Edit C — Dispatch lock (lines 29-39).** Tighten the V2 disposition + name
the V3 path.

```diff
-- S-P1 V2 is a partial fresh profile, blocked by absent real PMU/cycles.
-- The current executable next step is P1-D PMU/cycles repair or an explicit
-  S-P1 contract revision; no behavior wave is dispatchable.
-- W1+ behavior waves require `G-S-P1-RERUN-CONVERGED`,
-  `G-BEHAVIOR-RELEASE`, and a fresh S-P2/S-P3 revision before dispatch.
+- S-P1 V2 is a partial fresh profile; its PMU/c/B blocker is repaired in
+  V3 by Xcode-license + xctrace (`cpu-counters` template).
+- The current executable next step is the V3 sibling fan-out
+  (P1-V3-A/B/C/D/E/F) followed by V3 CHALLENGE; no behavior wave is
+  dispatchable until V3 converges.
+- W1+ behavior waves require `G-S-P1-RERUN-CONVERGED` (≥95% ACCEPT × 2
+  consecutive V3 cycles), `G-BEHAVIOR-RELEASE`, and a fresh S-P2/S-P3
+  revision before dispatch.
```

**Edit D — §0.2 Candidate Status table (lines 65-70).** Update the
fresh-S-P1 row.

```diff
-| Fresh S-P1 profile rerun | V2 blocked on P1-D PMU/cycles | `G-S-P1-RERUN-CONVERGED` |
+| Fresh S-P1 profile rerun | V3 underway with real PMU evidence (post-xctrace unblock) | `G-S-P1-RERUN-CONVERGED` against V3 evidence root |
```

**Edit E — §0.3 Required Telemetry (~line 88-130).** Optionally add one
field — but the orchestrator may prefer to defer until V3 CHALLENGE selects
the schema. *Proposal: defer, do not edit §0.3 in this pass.*

**Edit F — §1 Non-Negotiables (lines 153-156).** Clarify the PMU/c/B
non-producer clause without weakening it.

```diff
-- No structural-scan-only, masking probe, PMU, or Criterion slope artifact used
-  as a producer for Track 1, Track 2, typed product, direct product, or strict
-  admission.
+- No structural-scan-only, masking probe, PMU/cycles-per-byte, or Criterion
+  slope artifact used as a producer for Track 1, Track 2, typed product,
+  direct product, or strict admission. V3 real-PMU c/B is a diagnostic
+  characteriser of hot leaves, not a producer; it does not enable any
+  behavior admission path that was blocked in V2.
```

**Edit G — §2 Wave Manifest (line 164).** Update Interlock row.

```diff
-| Interlock | Section 4 | Fresh S-P1 Rerun | Blocked on P1-D PMU/cycles | Profile artifacts and S-P1 research/hardening docs | <=90 min |
+| Interlock | Section 4 | Fresh S-P1 Rerun | V3 sibling capture + CHALLENGE in flight | Profile artefacts, V3 trace bundles, S-P1 research/hardening docs | <=90 min |
```

**Edit H — §4 (lines 231-247).** Largest single block. Replace the V2
BLOCKED disposition with a path to G-S-P1-RERUN-CONVERGED.

```diff
-Status: V2 BLOCKED. Close artifact:
-`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`.
+Status: V2 BLOCKED; V3 reframe in flight. Close artefacts:
+`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
+(V2 BLOCKED record) and
+`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`
+(V3 convergence target).

 After W0, S-P1 reran against SK-V9-open evidence. The rerun must not use absent
 telemetry, stale SK-V4/SK-V8 fused evidence, source-eligible-only typed rows, or
 sidecar-historical-only comparators as behavior ancestors.

 `G-S-P1-RERUN-CONVERGED` passes only if a hardening consolidation records
 convergence and names fresh evidence for any behavior candidate. Otherwise W1+
 remains blocked.

-Current blocker: P1-D has no real PMU/cycles source. `perf` is absent, `xctrace`
-requires full Xcode, and `powermetrics` requires unavailable superuser access.
-Do not estimate c/B from Criterion `ns_per_byte`.
+V2 blocker resolution: the V2 host lacked a real PMU source (`perf` absent,
+`xctrace` requires full Xcode, `powermetrics` requires unavailable
+superuser access). V3 resolves this by installing full Xcode and using
+`xctrace` with the `cpu-counters` template — a direct hardware-counter read
+through Apple Silicon's PMU. xctrace c/B is admitted as a real PMU source
+by PASS-1-PROFILE §3 CH1. ns→c/B estimation remains forbidden regardless
+of source.
+
+V3 sibling deliverables that converge `G-S-P1-RERUN-CONVERGED`:
+
+1. P1-V3-A / P1-V3-B trace bundles: xctrace `cpu-counters` traces per corpus
+   × workload, plus parsed per-row PMU table (cycles, instructions,
+   branch-misses, L1/LLC misses, derived c/B).
+2. P1-V3-C: deep hot-leaf attribution against the V3 PMU rows + samply
+   sidecars, resolving every previously `unprofiled` cell.
+3. P1-V3-D: structural correlation between hot leaves and the SK-V9-open
+   row classifier.
+4. P1-V3-E: legacy doc/code audit (cleanup of stale V2-language; out of band
+   from the convergence gate but in the same wave).
+5. P1-V3-F: this reconciliation manifest.
+6. V3 CHALLENGE wave (CH1-CH6) returning ≥95% ACCEPT for two consecutive
+   V3 cycles per PASS-1-PROFILE §4.
```

**Edit I — §4 trailing paragraph (after the V3 block).** Add a precision
clause echoing §1.3.

```diff
+Discipline note: a direct hardware-counter read — `perf` on Linux,
+`xctrace` with `cpu-counters` on macOS (full Xcode required), or
+privileged `powermetrics` — is admitted as a real PMU source. Indirect
+derivation from `ns_per_byte`, wall-clock loop time, or any
+inferred/nominal CPU frequency is rejected regardless of source. The
+discipline is real-counter-vs-ns, not tool-by-tool.
```

### §4.2 — `restart/skinny/tranches/sk-v9/HANDOFF.md`

**Edit A — line 5-9 (Status).**

```diff
-Status: G-Alpha is closed by user instruction on 2026-05-18. W0 telemetry-lock
-is closed with `skinny/RESULTS.md` rendered and consumed as `SK-V9-open`.
-SK-V9 S-P1 V2 reran against the W0 baseline and produced fresh samply evidence,
-but hardening blocked convergence because real PMU/cycles-per-byte counters are
-unavailable in the current host context. Behavior waves remain blocked.
+Status: G-Alpha is closed by user instruction on 2026-05-18. W0
+telemetry-lock is closed with `skinny/RESULTS.md` rendered and consumed as
+`SK-V9-open`. SK-V9 S-P1 V2 reran against the W0 baseline and produced fresh
+samply evidence; V2 hardening blocked convergence because real PMU/c/B
+counters were unavailable on the V2 host. V3 unblocks PMU via Xcode license
+plus `xctrace cpu-counters`; V3 siblings P1-V3-A/B/C/D/E/F are in flight,
+and V3 CHALLENGE selects the convergence gate. Behavior waves remain blocked
+until `G-S-P1-RERUN-CONVERGED` against the V3 evidence root and
+`G-BEHAVIOR-RELEASE`.
```

**Edit B — Read First (lines 11-33).** Add V3 sibling references after the
V2 hardening line.

```diff
 10. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
+11. `restart/skinny/tranches/sk-v9/research/p1/V3/p1-v3-a-*.md` (xctrace
+    trace bundle, parsed PMU table)
+12. `restart/skinny/tranches/sk-v9/research/p1/V3/p1-v3-b-*.md` (xctrace
+    product-plane trace bundle, parsed PMU table)
+13. `restart/skinny/tranches/sk-v9/research/p1/V3/p1-v3-c-*.md` (deep
+    hot-leaf attribution)
+14. `restart/skinny/tranches/sk-v9/research/p1/V3/p1-v3-d-*.md` (structural
+    correlation)
+15. `restart/skinny/tranches/sk-v9/research/p1/V3/p1-v3-e-*.md` (legacy
+    cleanup audit)
+16. `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md`
+    (this manifest)
+17. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`
+    (once V3 CHALLENGE lands)
```

Renumber the trailing items accordingly.

**Edit C — §2 Current State (lines 56-63).**

```diff
 S-P1 V2 partial profile state:

 - 106 fresh samply profile/sidecar pairs exist under `/tmp/skv9-p1-rerun`.
 - P1-A/P1-B/P1-C/P1-E/P1-F have fresh SK-V9-open evidence.
-- P1-D is blocked: `perf` absent, `xctrace` requires full Xcode, and
-  `powermetrics` requires superuser access unavailable to this run.
-- `HARDENING-S-P1-V2-CONSOLIDATED.md` records 4/6 ACCEPT and BLOCKED.
+- P1-D was blocked on the V2 host (perf/xctrace/powermetrics unavailable);
+  V3 resolves this via Xcode-license + `xctrace cpu-counters` and
+  re-attributes c/B across the 17 corpora × workloads.
+- `HARDENING-S-P1-V2-CONSOLIDATED.md` records 4/6 ACCEPT and BLOCKED on
+  V2; V3 CHALLENGE targets ≥95% ACCEPT × 2 consecutive cycles per
+  PASS-1-PROFILE §4.
```

**Edit D — §4 Next Move (lines 92-103).**

```diff
-1. Treat G-Alpha, `G-W0-TELEMETRY-LOCK`, and S-P1 V2 partial samply evidence as
-   recorded.
-2. Do not dispatch S-P2 or W1+ behavior waves; `G-S-P1-RERUN-CONVERGED` did not
-   pass.
-3. Resolve the P1-D PMU/cycles blocker by providing a real counter source
-   (`perf`, full-Xcode `xctrace`, privileged `powermetrics`, or an accepted
-   contract amendment). Do not estimate c/B from ns/B.
-4. After P1-D is repaired, rerun/challenge S-P1 to convergence before any
-   revised S-P2/S-P3 or behavior dispatch.
+1. Treat G-Alpha, `G-W0-TELEMETRY-LOCK`, S-P1 V2 partial samply evidence,
+   and the V3 PMU unblock (xctrace cpu-counters) as recorded.
+2. Do not dispatch S-P2 or W1+ behavior waves; `G-S-P1-RERUN-CONVERGED`
+   has not passed against V3 evidence yet.
+3. Land the V3 sibling fan-out (P1-V3-A/B/C/D/E/F) and the V3 CHALLENGE
+   wave; convergence is ≥95% ACCEPT × 2 consecutive cycles.
+4. After V3 convergence, revise S-P2 and S-P3 against the V3 evidence root,
+   then proceed to `G-BEHAVIOR-RELEASE`. Do not estimate c/B from ns/B
+   under any path.
```

**Edit E — §5 Pre-Blocked Routes.** Add the four class-umbrella entries
named in §3.2 above. Diff (insert after the current eight):

```diff
 - Generic JSON policy leaks or Lock 14 weakening.
+- Retained or direct string-scan widening, trusted boundary collapse,
+  value-byte/next-key carry, and per-quartet/per-segment unicode-escape
+  classifier routes without a same-row falsification gate pre-registered in
+  a revised S-P3 plan (umbrella over REDRESS 60-65, 82-84).
+- Direct source-hook field folding, parser-owned decoded scratch,
+  byte-output `unescape_*` rewrites, and DirectBuild semantic-string-fact
+  streaming for the digest workload (umbrella over REDRESS 66-69).
+- Bench-private hand Track 1 parsers or hand typed sinks presented as
+  generated direct/typed proof (umbrella over REDRESS 34, 70).
+- PMU, cycles-per-byte, masking probes, structural-scan-only paths, and
+  Criterion slope artefacts as Track 1 / Track 2 / typed / direct / strict
+  producers (umbrella over SPEC §1 non-negotiables; V3 PMU c/B remains a
+  diagnostic characteriser only).
```

**Edit F — §6 Close Posture.**

```diff
-The SK-V9 contract is post-G-Alpha, post-W0, and post-S-P1-V2-BLOCKED, but
-still pre-behavior. W0 closed the telemetry-lock; S-P1 V2 produced useful fresh
-samply evidence but did not converge because P1-D PMU/cycles is blocked.
-Behavior waves must not dispatch until S-P1 convergence and
-`G-BEHAVIOR-RELEASE`.
+The SK-V9 contract is post-G-Alpha, post-W0, post-S-P1-V2-BLOCKED, and
+mid-S-P1-V3, but still pre-behavior. W0 closed the telemetry-lock; S-P1 V2
+produced fresh samply evidence but did not converge because the V2 host
+lacked PMU/c/B; V3 unblocks PMU via xctrace cpu-counters and re-attributes
+the c/B ledger. Behavior waves must not dispatch until V3 convergence
+(`G-S-P1-RERUN-CONVERGED` against the V3 evidence root) and
+`G-BEHAVIOR-RELEASE`.
```

### §4.3 — `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md`

**Edit A — Preamble (lines 6-9).**

```diff
-G-Alpha is closed. W0 telemetry-lock is closed under
-`sk-v9-open:criterion-fnv64-cd1673844eeea12f`. S-P1 V1 did not converge and
-S-P1 V2 is blocked by absent real PMU/cycles-per-byte counters. Do not dispatch
-behavior waves until `G-S-P1-RERUN-CONVERGED` and `G-BEHAVIOR-RELEASE` pass.
+G-Alpha is closed. W0 telemetry-lock is closed under
+`sk-v9-open:criterion-fnv64-cd1673844eeea12f`. S-P1 V1 did not converge.
+S-P1 V2 was blocked by an unavailable PMU/c/B source on the V2 host; V3
+unblocks PMU via Xcode-license + `xctrace cpu-counters`, and the V3 sibling
+fan-out (P1-V3-A/B/C/D/E/F) plus V3 CHALLENGE selects the convergence gate.
+Do not dispatch behavior waves until `G-S-P1-RERUN-CONVERGED` against the
+V3 evidence root and `G-BEHAVIOR-RELEASE` pass.
```

**Edit B — Required Reading (after line 23).** Add the V3 evidence root and
once-landed V3 hardening.

```diff
 10. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`.
+10b. `restart/skinny/tranches/sk-v9/research/p1/V3/` (V3 sibling artefacts).
+10c. `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md`.
+10d. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`
+     (once V3 CHALLENGE lands).
```

**Edit C — Wave Manifest table (line 40).**

```diff
-| Interlock | Section 4 | Fresh S-P1 Rerun | Blocked on P1-D PMU/cycles | <=90 min |
+| Interlock | Section 4 | Fresh S-P1 Rerun | V3 sibling capture + CHALLENGE in flight | <=90 min |
```

**Edit D — Conditional Release (lines 107-115).**

```diff
-S-P1 V2 has been run and challenged. It is blocked on P1-D PMU/cycles. W1+
-remains blocked until a real PMU/cycles source is available and S-P1 hardening
-converges. Do not estimate c/B from `ns_per_byte`.
+S-P1 V2 has been run and challenged. The V2 disposition was BLOCKED on
+P1-D PMU/c/B due to host infrastructure. V3 resolves the host blocker via
+Xcode-license + `xctrace cpu-counters` and is in flight. W1+ remains
+blocked until V3 converges (`G-S-P1-RERUN-CONVERGED` against the V3
+evidence root: P1-V3-A/B trace bundles, P1-V3-C deep attribution, V3
+CHALLENGE ≥95% ACCEPT × 2 consecutive cycles). Do not estimate c/B from
+`ns_per_byte` under any path.
```

**Edit E — Always Blocked block (lines 118-132).** Mirror the four
class-umbrella additions from HANDOFF §5 above (same diff content; do not
duplicate the verbatim umbrella lines, but ensure DISPATCH-PROMPT cross-refs
HANDOFF §5 for the full umbrella ledger).

```diff
+- See HANDOFF §5 for the full pre-blocked-routes ledger and the class
+  umbrellas binding by reference. The above bullets are the SK-V9
+  always-blocked summary; HANDOFF §5 is the authoritative list.
```

### §4.4 — Edit set summary

- **SPEC.md** — 8 surgical edits (Edits A, B, C, D, F, G, H, I: status,
  authority, dispatch lock, §0.2 table, §1 non-negotiables clarifier, §2
  manifest, §4 V3 path, §4 precision clause). Edit E in §4.1 is an explicit
  deferral decision (do not amend §0.3 telemetry in this pass) and is
  enumerated only for completeness; it is NOT a surgical edit.
- **HANDOFF.md** — 6 surgical edits (Edits A-F: status, read-first, §2, §4
  next-move, §5 umbrella additions, §6 close posture).
- **DISPATCH-PROMPT.md** — 5 surgical edits (Edits A-E: preamble, required
  reading, wave manifest, conditional release, always-blocked cross-ref).

Total: **19 actual surgical edits** across the three documents (8 + 6 + 5).
The earlier V3 rollup that read "19" was numerically correct but reasoned
under the wrong frame (it counted SPEC Edit E silently); the V4 fold
reconfirms 19 by explicitly excluding the deferral as a non-edit. No edit
crosses into `restart/prompts/` scope; PASS-1-PROFILE.md amendments are
Pass Omega CRUD scope per `ORCHESTRATOR.md` §7. Each surgical edit is a
paragraph- or list-level replacement; none touch source. None weaken the
discipline; the edits update V2 BLOCKED language to V3 IN-FLIGHT language,
add the V3 evidence root by reference, and surface class umbrellas already
in REDRESS but not in §5.

---

## §5 — `G-S-P1-RERUN-CONVERGED` Bar (Concrete Artefacts)

The gate passes when **all** of the following are present and signed off by
V3 CHALLENGE consolidation:

### §5.1 — Evidence artefacts (sibling commits required before the gate runs)

1. **P1-V3-A trace bundle + parsed PMU table.** xctrace `cpu-counters`
   traces covering 17/17 corpora × `parse_only` workload. Each row in the
   parsed table carries `corpus`, `cycles`, `instructions`,
   `branch_misses`, `l1_misses`, `llc_misses`, `bytes`, `derived_c_per_B`
   (computed from `cycles / bytes`, not `ns_per_byte`). Source bundle path,
   xctrace template version, host triple, build flags, and run id are
   verbatim in §1 method block per PASS-1-PROFILE §2.2.
2. **P1-V3-B trace bundle + parsed PMU table.** Same shape as A, covering
   17/17 corpora × `direct_to_struct` + `real_typed_struct` workloads.
3. **P1-V3-C deep hot-leaf attribution.** Every `unprofiled` cell from
   `skinny/RESULTS.md` resolved to a named samply symbol + % self-time +
   file:line, **cross-referenced against P1-V3-A/B c/B rows** so each
   hot-leaf claim carries both wall-time attribution and PMU-truthed c/B
   share. CH2 GENERALITY: every hot leaf named to a grammar-neutral
   primitive, not a JSON role.
4. **P1-V3-D structural correlation.** Per-row classification of which
   SK-V9-open `parse_only` / `direct_to_struct` / `real_typed_struct`
   row's verdict is dominated by which hot-leaf primitive class
   (scan / number / string / unicode / structural / tape / dispatch /
   dispatch). This is the bridge S-P2 will consume: it names the **measured
   bottleneck class** for every row that is not currently `A / GO`.
5. **P1-V3-E legacy audit.** Cleanup record naming any stale V2-language in
   `restart/skinny/tranches/sk-v9/`, in older SK-V{N} docs, or in source
   that referenced the now-resolved PMU block. Cleanup is in-band but not
   gate-blocking (out of the convergence bar; in-wave for hygiene).
6. **P1-V3-F reconciliation manifest** (this file). Required so HANDOFF §5
   class umbrellas and the V2/V3 disposition reconciliation are recorded.

### §5.2 — V3 CHALLENGE convergence

7. **V3 CH1-CH6 cohort committed** at
   `restart/skinny/tranches/sk-v9/research/p1/hardening/V3/CH{1..6}.md` per
   PASS-1-PROFILE §3.
8. **V3 consolidated hardening** at
   `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`
   with disposition: **CONVERGED**, ACCEPT rate ≥95%, zero open critical
   defects, zero orphan unresolved REVISE.
9. **Second V3 cycle (V3.2 or V4) also ≥95% ACCEPT**, per PASS-1-PROFILE §4
   "≥95% ACCEPT for two consecutive cycles." V3 alone is insufficient; the
   gate requires two-consecutive ACCEPT, which means at least one repeat
   cycle.

### §5.3 — Contract-truth checks the gate enforces

10. **PMU source is real, not estimated.** Every c/B row's source line cites
    xctrace `cpu-counters` (or `perf`/`powermetrics` on alternate hosts).
    No row carries `derived_from_ns_per_byte` provenance. CH1 rejects any
    row that does.
11. **Corpus coverage is 17/17** for P1-V3-A, P1-V3-B, P1-V3-C, P1-V3-D
    per PASS-1-PROFILE §2.1. CH1 rejects float-heavy overfit.
12. **All four unicode corpora present in c/B + attribution** (per
    PASS-1-PROFILE §2.1 the unicode + string corpora are the "load-bearing
    rows"). A V3 evidence root that omits one rejects on CH1.
13. **Generality discipline.** Hot leaves named to grammar-neutral
    primitives; CH2 rejects JSON-role re-naming. xctrace c/B rows are not
    used as producers — only as characterisers — per SPEC §1 amended clause
    (see §4.1 Edit F).
14. **REDRESS regression.** No V3 finding silently re-proposes a pre-blocked
    route (HANDOFF §5 + the class umbrellas from §3.2 of this manifest).
    CH3 enforces.
15. **Reproducibility.** Every §1 method block in V3 artefacts carries
    verbatim commands; CH4 rejects if any are absent.

### §5.4 — Out-of-bar items (not gate-blocking, but in-wave)

- P1-V3-E cleanup commits: hygiene-only; should land alongside the gate but
  is not part of the convergence threshold.
- This P1-V3-F manifest: required as the **reconciliation artefact**, but
  the gate evaluates §5.1 items 1-6 and §5.2 items 7-9 as the bar.

### §5.5 — Reframe vs V2

V2's failure mode was infrastructure-bound (no real PMU source). V3 swaps
infrastructure availability *and* keeps every other CHALLENGE criterion. V3
must also clear what V2 cleared: CH2 (Lock 14 / generality), CH3 (REDRESS
regression), CH5 (hidden coupling / substrate union), CH6
(anti-paper-close). V2's accepts for CH2/CH3/CH5/CH6 do not auto-renew —
they re-evaluate against the new V3 c/B + attribution rows. The V3 bar is:
V2-accepts must re-accept against the V3 evidence root, *and* CH1 (now
unblockable) must finally ACCEPT, *and* CH4 (reproducibility) must hold under
xctrace's specific command discipline.

---

## §6 — Risks

### §6.1 — Contract-language risk: low

The contract is already unambiguous. xctrace c/B is admitted; the
clarification proposed in §1.3 is purely preventive. No agent reading the
contract end-to-end can honestly reject xctrace c/B on contract grounds.

### §6.2 — V3 CHALLENGE re-evaluation risk: medium

V2 returned ACCEPT on CH2, CH3, CH5, CH6 — but those accepts were against
an *incomplete* P1-D row. CH1 + CH4 re-evaluate against the new c/B rows,
and the new rows may surface a CH2 issue (a hot-leaf named to a JSON role
that was hidden behind "unprofiled" before V3) or a CH3 issue (a hot-leaf
attribution that implicitly suggests a pre-blocked route). The V3 CHALLENGE
must explicitly re-evaluate all six lenses, not just CH1/CH4.

### §6.3 — Two-consecutive ACCEPT requirement: medium

PASS-1-PROFILE §4 requires "≥95% ACCEPT for two consecutive cycles." V3 is
*one* cycle. If V3 converges at ≥95% ACCEPT, the gate still requires a V3.2
or V4 confirmation cycle. The orchestrator should pre-plan the second cycle
so V3 + V3.2 land in the same wall-window.

### §6.4 — Generality regression risk: low

The xctrace data is grammar-neutral by construction (PMU counters are
hardware-level). CH2 generality risk is at the **attribution** layer, not
the counter layer — i.e. P1-V3-C must name primitives, not JSON roles.
P1-V3-C and P1-V3-D are responsible for not re-introducing JSON-role names.

### §6.5 — Class-umbrella creep risk: medium

The four umbrellas proposed for HANDOFF §5 (§3.2 above) compress ~20
specific REDRESS rejections into umbrella sentences. There is a residual
risk that an agent reading only the umbrella misses the specific
shape-by-shape evidence. Mitigation: the umbrella sentences each cite the
specific REDRESS entries, and the existing
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
"binding by reference" footer (HANDOFF §5 lines 126-130) carries the
detailed ledger. Keep both.

### §6.6 — Doc-edit cohesion risk: low

The 19 proposed edits across three documents are all paragraph/list level.
Each is a like-for-like replacement of V2-BLOCKED language with V3-IN-FLIGHT
language. None reshape the wave manifest, the section structure, or the
gate names. The only structural-ish change is the §5 umbrella additions in
HANDOFF; those are simple list extensions.

### §6.7 — Risk the V3 evidence root does NOT converge

If V3 produces real PMU rows but CH2/CH3 surface new defects, the gate stays
RED. The mitigation is the V2 framing already in place: behavior waves are
blocked until convergence, and the orchestrator can iterate V3.2, V3.3, …
up to V5 (PASS-1-PROFILE §4 hard ceiling) before escalating. The 19 doc
edits proposed here are tolerant of a multi-cycle V3 — they say "V3 in
flight" / "V3 evidence root", not "V3 converged".

### §6.8 — Risk that legacy cleanup (P1-V3-E) lands stale doc fragments

The cleanup pass should NOT touch this manifest, the V3 hardening file (when
it lands), or the V3 sibling artefacts. The cleanup pass scope is restricted
to V2-language in earlier SK-V{N} docs and source comments. The §4 edit
proposals here are the SK-V9-specific updates; P1-V3-E should not duplicate
them.

---

## §7 — Summary For The Orchestrator

- **Contract admits xctrace c/B.** No amendment needed; an optional one-
  paragraph clarification disambiguates source surface. (§1.)
- **REDRESS ledger is mostly STILL-LOAD-BEARING.** 7 entries are SUPERSEDED
  (35, 36, 37, 38, 46, 49, 70), ~14 are HISTORICAL provenance, the rest
  bind. No PMU/c/B-related entry is invalidated by the V3 unblock. (§2.)
- **HANDOFF §5 is correct but incomplete.** Add four class umbrellas to
  cover string-scan / direct-receiver / bench-private-hand / PMU-as-producer
  classes already rejected ~20 times in REDRESS. (§3.)
- **19 actual surgical doc edits proposed.** SPEC.md (8), HANDOFF.md (6),
  DISPATCH-PROMPT.md (5). SPEC §4.1 Edit E is a deferral decision and is
  not counted as a surgical edit. All proposed edits are paragraph/list
  level. All replace V2-BLOCKED language with V3-IN-FLIGHT language and
  surface class umbrellas already in REDRESS. No edit crosses into
  `restart/prompts/` scope; PASS-1-PROFILE.md amendments are Pass Omega
  CRUD scope per `ORCHESTRATOR.md` §7. Propose-only; not applied. (§4.)
- **G-S-P1-RERUN-CONVERGED bar** is six evidence artefacts (§5.1) + V3
  CHALLENGE consolidation at ≥95% ACCEPT × 2 consecutive cycles (§5.2) +
  contract-truth checks (§5.3). The two-consecutive requirement means V3
  alone is not the gate; V3 + V3.2 (or V3 + V4) are. (§5.)
- **Primary risk** is V3 CHALLENGE re-evaluating CH2/CH3/CH5/CH6 against
  the new c/B rows; V2's accepts do not auto-renew. (§6.2.) Secondary risk
  is the two-consecutive-cycle rule (§6.3). All other risks are low.
