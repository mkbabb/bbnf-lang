# CH2 - Invariant And Lock-Surface Audit

Verdict: ACCEPT.

Scope: Pass Omega V9 folded packet (`Omega-A` through `Omega-F`), `locks-diff.md`, `master-plan-diff.md`, `restart/locks/LOCKS.md`, `restart/ARCHITECTURE.md`, and SK-V15 `SPEC.md` / `DISPATCH-PROMPT.md` at HEAD `9d336c6062898b0ce70b4df6787c3538aa7f74b9`.

## Evidence Commands

```sh
git rev-parse HEAD
# 9d336c6062898b0ce70b4df6787c3538aa7f74b9

grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
# 16

awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' \
  restart/audit/totality/astral/V9/locks-diff.md | git apply --check -
# exits 0

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
# 67

find crates/core/src/runtime -mindepth 2 -maxdepth 2 -type f -name '*.rs' | wc -l
# 63
```

## Findings

1. Lock count remains 16. `restart/locks/LOCKS.md:71` introduces "sixteen locks", and the numbered lock heading count is exactly 16. The V9 locks diff applies cleanly and inserts one addendum before the governance boundary, not a new numbered lock; it explicitly preserves the 16 numbered locks and adds no lock or lock retirement (`restart/audit/totality/astral/V9/locks-diff.md:5`-`9`, `:45`-`:49`). Fold requirement: preserve the addendum shape; do not introduce Lock 17.

2. BackendShape canon remains exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. Lock 10 names that exact set (`restart/locks/LOCKS.md:269`), `ARCHITECTURE.md` defines only those five enum variants (`restart/ARCHITECTURE.md:1090`-`1115`), and V9 repeats the exact five-shape gate (`restart/audit/totality/astral/V9/locks-diff.md:23`-`30`, `:63`). `FactStream` remains a Lock 1 substrate-manifest/admitted-product category, not a sixth shape (`restart/locks/LOCKS.md:100`-`109`; `restart/ARCHITECTURE.md:1778`-`1790`; `restart/audit/totality/astral/V9/locks-diff.md:49`).

3. Pattern H census remains 67 root runtime files using the required command. The live command returns 67; the forbidden `-maxdepth 2` variant returns 63 because it misses the four depth-3 `google_sheets/document/*` files. The required form is bound in `restart/locks/LOCKS.md:402`-`410`, reinforced in `restart/ARCHITECTURE.md:1800`-`1820`, and SK-V15 W4 requires the 67-file count plus true line-1 provenance and regen/check proof (`restart/skinny/tranches/sk-v15/SPEC.md:318`-`331`).

4. V9 patch scope does not authorize source, generated, gate, `RESULTS`, or `REDRESS` movement before SK-V15 W0-W11. `master-plan-diff.md` limits authorized touch scope to MASTER/HANDOFF/MIGRATION/LOCKS/SUBSTRATE/WORKSPACE/BENCH/HARDENING/HISTORY/README/audit logs and forbids source, generated output, gates, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and SK-V15 SPEC/DISPATCH edits (`restart/audit/totality/astral/V9/master-plan-diff.md:17`-`23`). `ΩF-migration-handoff.md` repeats that V9 itself does not authorize source edits, generated movement, results/redress changes, gate implementation, or runtime deletion; those belong to SK-V15 W0-W11 after G-Omega/CRUD (`restart/audit/totality/astral/V9/ΩF-migration-handoff.md:64`-`67`, `:132`-`:140`).

5. Apple M5 Max/aarch64-only admission and no x86 close route remain preserved. SK-V15 states Apple M5 Max/aarch64 is the only admission host and x86/AVX-512 are diagnostic only (`restart/skinny/tranches/sk-v15/SPEC.md:135`-`136`). The V9 locks diff preserves that boundary (`restart/audit/totality/astral/V9/locks-diff.md:10`-`11`, `:71`). Lock 16 keeps AVX-512 literature as architecture pressure only, and mechanically refuses `CollapsedStage` admission on aarch64 until a generated aarch64 strategy lands (`restart/locks/LOCKS.md:515`-`533`). Omega-D also requires Apple M5 Max/aarch64 evidence, scalar oracle, strict parity/checkasm, same-wave consumer, and row movement; x86/AVX-512 stays diagnostic (`restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md:55`-`62`).

No revise item found.
