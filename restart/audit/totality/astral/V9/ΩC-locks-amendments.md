# Omega-C Locks Amendments - Pass Omega V9 SK-V15 Totality

Date: 2026-05-28.
Scope: SK-V15 totality closure against the 16 locks, per `PASS-OMEGA.md`
Section 2 / Section 10.
Disposition: LOCKS AMENDMENT REQUIRED, G-Omega gated.

## Verdict

SK-V15 totality requires one proposed `LOCKS.md` amendment: the T-P3 3C
crystallisation addendum. This is not a lock-count change and not a new
`BackendShape`. It is a governance-strengthening addendum that folds the
converged SK-V15 closure rules into the locks surface before SK-V15
implementation waves can use them as admission gates.

The amendment is required because T-P3 V5 converged with all seven lenses
`ACCEPT`, zero orphan `REVISE`, zero `REJECT`, and an extracted
`3C-locks-v+1-diff.md` that applies cleanly to current `LOCKS.md`
(`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:11`-`29`).
The accepted ground explicitly preserves the 16-lock count and exact
five-shape `BackendShape` canon while routing Pass Omega V9 to G-Omega before
any locks CRUD merge
(`restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md:43`-`68`).

## Run Checks

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
```

Returned: `16`.

```sh
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

Returned: `67`. Pattern H is relevant because SK-V15 binds the current 67-file
runtime census to line-1 generator provenance and regeneration/check proof
(`restart/skinny/tranches/sk-v15/SPEC.md:69`-`73`,
`restart/skinny/tranches/sk-v15/SPEC.md:198`).

Five-shape canon scan:

```sh
rg -n 'BackendShape|EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage|FactStream|sixth shape|sixth `BackendShape`|new/sixth BackendShape' \
  restart/locks/LOCKS.md \
  restart/skinny/tranches/sk-v15/SPEC.md \
  restart/audit/totality/p3/3C-locks-v+1-diff.md \
  skinny/crates/ir/src/lib.rs \
  skinny/crates/ir/src/cost.rs
```

Load-bearing hits:

- `restart/locks/LOCKS.md:100`-`109`: `FactStream` is a Lock 1 substrate
  manifest category, not a sixth `BackendShape`; the five-shape set remains
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`.
- `restart/locks/LOCKS.md:269`-`274`: Lock 10 keeps those five variants as
  the V1 search domain and makes any new `BackendShape`, directive, or BIR
  variant G-Omega gated.
- `skinny/crates/ir/src/lib.rs:340`-`345` and
  `skinny/crates/ir/src/cost.rs:333`-`339`: code contains exactly the five
  `BackendShape` variants and `all_backend_shapes() -> [BackendShape; 5]`.
- `restart/skinny/tranches/sk-v15/SPEC.md:135`-`153`: Apple M5 Max/aarch64 is
  the only admission host, x86/AVX-512 are diagnostic only, and hidden-coupling
  additions including a new/sixth `BackendShape` are rejected.
- `restart/skinny/tranches/sk-v15/SPEC.md:424`-`426`: SK-V15's all-five gate
  must see exactly `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}` and EventTape cannot become a sidecar or sixth shape.

Diff extraction check:

```sh
awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/locks-diff.md | git apply --check -
```

Returned clean.

## Audit Basis

Pass Omega gives Omega-C the duty to audit the 16 locks against skinny REDRESS
and totality T-P3 hardening, emitting a locks amendment diff while deferring
any merge until G-Omega
(`restart/prompts/pass-contracts/PASS-OMEGA.md:30`-`34`,
`restart/prompts/pass-contracts/PASS-OMEGA.md:86`-`108`). Section 10 binds this
audit to Lock 1 substrate union, Lock 14 grammar-neutrality, the five-shape
`BackendShape` canon, and the 16-lock count
(`restart/prompts/pass-contracts/PASS-OMEGA.md:164`-`172`).

V8 precedent was a zero-delta case because REDRESS-212 was only a wave-graph
and cap-accounting correction; V8 found no new public syntax, substrate,
BackendShape, or Lock 14 amendment need
(`restart/audit/totality/astral/V8/ΩC-locks-amendments.md:7`-`26`,
`restart/audit/totality/astral/V8/locks-diff.md:1`-`26`). SK-V15 differs:
T-P3 3C proposes one line-level addendum that covers all live 1E and 2X
lock-amendment candidates while adding no sixth shape or new lock
(`restart/audit/totality/p3/3C-locks-v+1-diff.md:25`-`29`,
`restart/audit/totality/p3/3C-locks-crystallisation.md:31`-`57`).

## Sixteen-Lock Audit

| Lock | SK-V15 disposition | Amendment impact |
|---:|---|---|
| 1 | Needs crystallisation for fact-stream/sidecar/runtime-regex boundaries. | Addendum states `FactStream` is output-plane only, not a sixth `BackendShape`, and runtime regex/DFA substrate needs prior G-Omega Lock 1 amendment (`restart/audit/totality/p3/3C-locks-v+1-diff.md:42`). |
| 2 | Needs live-state closure guard. | Addendum says `LayoutFacts.backend_shape` evidence does not close absent `passes::layout`, `Layout`, and `LayoutSink` obligations (`restart/audit/totality/p3/3C-locks-v+1-diff.md:44`). |
| 3 | Needs executable empty-path proof guard. | Addendum rejects missing-symbol claims as closure without generated-code/unit proof (`restart/audit/totality/p3/3C-locks-v+1-diff.md:46`). |
| 4 | Needs solver-bridge evidence guard. | Addendum requires dependency split or explicit accepted exception for egraph/CSP composition (`restart/audit/totality/p3/3C-locks-v+1-diff.md:48`). |
| 5 | No separate SK-V15 amendment. | Existing IR/per-backend lower boundary holds; no new backend or source-emitter path is introduced. |
| 6 | Needs generated-output and deletion guard. | Addendum requires line-1 provenance, rostered generator, byte-equivalent regen/check proof, and same-wave replacement provider (`restart/audit/totality/p3/3C-locks-v+1-diff.md:50`). |
| 7 | No standalone amendment. | Root topology evidence is covered through the Lock 11/12 topology/archive clause. |
| 8 | Needs row-plane and broadcast-admission guard. | Addendum binds measurement row IDs, broadcast group IDs, host, comparator workload, and CSS same-workload typed evidence (`restart/audit/totality/p3/3C-locks-v+1-diff.md:52`). |
| 9 | Needs API-surface closure guard. | Addendum keeps borrowed/Cow JSON evidence partial until `parse_in` and `parse_owned` tests prove shared parser/lifetime discipline (`restart/audit/totality/p3/3C-locks-v+1-diff.md:54`). |
| 10 | Needs Decision Engine all-five closure guard. | Addendum requires real egraph/CSP/cost/lowerer evidence and an all-five gate over exactly the five canon variants; sixth shape remains G-Omega gated (`restart/audit/totality/p3/3C-locks-v+1-diff.md:56`). |
| 11 | Needs topology evidence guard. | Addendum requires explicit root evidence for path/path-core/parse-that and legacy-name routing (`restart/audit/totality/p3/3C-locks-v+1-diff.md:58`). |
| 12 | Needs archive evidence guard. | Same topology/archive clause prevents stale prose closure (`restart/audit/totality/p3/3C-locks-v+1-diff.md:58`). |
| 13 | No separate SK-V15 amendment. | Pattern H generated discipline is carried through Lock 6/14 and the 67-file invariant, without changing Lock 13's god-directory rule. |
| 14 | Needs grammar-generalisation and exclusion-reporting guard. | Addendum bans generic grammar branches, binds included/excluded root reporting, and requires CSS plus Sheets/BBNF-self witnesses for future grammar onboarding (`restart/audit/totality/p3/3C-locks-v+1-diff.md:60`). |
| 15 | Needs profile-scope guard. | Addendum separates skinny release-profile evidence from root release/profile closure and host-bound `target-cpu=native` rows (`restart/audit/totality/p3/3C-locks-v+1-diff.md:62`). |
| 16 | Needs primitive-manifest guard. | Addendum binds owner, scalar oracle, strict parity/checkasm, Apple M5 Max/aarch64 hardware gate or fallback, same-wave consumer, row movement, rollback, and final disposition (`restart/audit/totality/p3/3C-locks-v+1-diff.md:64`). |

## Non-Negotiable Boundaries

- Preserve exactly 16 numbered locks. No Lock 17.
- Preserve exactly five `BackendShape` variants:
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`.
- Do not introduce `FactStream` as a `BackendShape`; it remains an output-plane
  / admitted-product category only.
- Apple M5 Max/aarch64 is the only admission host for SK-V15; x86 and AVX-512
  evidence remains diagnostic, not close evidence
  (`restart/skinny/tranches/sk-v15/SPEC.md:135`-`136`,
  `restart/skinny/tranches/sk-v15/HANDOFF.md:21`-`23`).
- No public substrate API, retained sidecar, new directive, new BIR variant,
  lock retirement, or sixth shape enters through this amendment.

## Proposed Locks Diff

See `restart/audit/totality/astral/V9/locks-diff.md`.
