# SK-V12 W1a A6 - JSON Guard And REDRESS Accounting

Scope: read-only research for SK-V12 W1a. This artifact records the JSON guard
rerun/no-touch proof surface, current benchmark commands, REDRESS numbering,
and how W1a records PASS/FAIL without claiming a CSS row.

## Authority

- `restart/skinny/tranches/sk-v12/SPEC.md` Section 4 defines W1a as
  `G-W1a-GRAMMARCONFIG-LOCK14`, a GrammarConfig + Lock 14 legality gate.
- W1a exit requires generic-crate scan PASS and JSON generated parity plus
  guard floors, or measured demotion. It explicitly says: no CSS parser row is
  claimed yet.
- `USER-PIN-W1-CSS-L4-SOTA.md` makes CSS L4 authoritative and raises the CSS
  close target to `generated_css_l4_track1_mbps > lightningcss_mbps + 1`.
  The pin also keeps JSON direct and typed guard floors active and says
  REDRESS 114-119 remain guard-only for JSON direct residuals.
- `skv12-profile-truth-audit.md` has no measured CSS L4 capture. Its W1
  preview is marked speculative. Its direct residual floor-delta table is JSON
  guard/reopen evidence only, not CSS authority.
- `skinny/RESULTS.md` currently contains JSON rows only; there is no
  `css_l4`, `sheets`, `bbnf_self`, or `non_json_generated` row.

## Current JSON Guard Facts

`skinny/RESULTS.md` currently renders 41 JSON main rows: 17 `parse_only`, 17
`direct_to_struct`, and 7 `real_typed_struct`. The current outcome count is
11 `A / GO`, 13 `N-direct / NO-GO`, 16 `S / NO-GO`, and 1 `L / NO-GO`.
Overall remains `N-direct / NoGo`.

Direct guard floors from SPEC Section 0.5 all pass against current
`skinny/RESULTS.md`:

| Row | Floor T1 | Floor T2 | Current T1 | Current T2 | State |
|---|---:|---:|---:|---:|---|
| `citm_catalog/direct_to_struct` | 18191 | 17431 | 18563 | 17787 | PASS |
| `apache_builds/direct_to_struct` | 11028 | 9996 | 11254 | 10189 | PASS |
| `marine_ik/direct_to_struct` | 8759 | 9248 | 8938 | 9437 | PASS |
| `unicode_basic/direct_to_struct` | 2253 | 2182 | 2299 | 2227 | PASS |

Typed guard floors from SPEC Section 0.5 all pass against current
`skinny/RESULTS.md`:

| Row | Floor T1 | Floor T2/oracle | Current T1 | Current T2/oracle | State |
|---|---:|---:|---:|---:|---|
| `twitter/real_typed_struct` | 17385 | 15593 | 17740 | 15912 | PASS |
| `citm_catalog/real_typed_struct` | 29928 | 17321 | 30539 | 17675 | PASS |
| `apache_builds/real_typed_struct` | 8308 | 6754 | 8478 | 6892 | PASS |
| `github_events/real_typed_struct` | 11633 | 12029 | 11871 | 12275 | PASS |
| `update_center/real_typed_struct` | 11613 | 10150 | 11851 | 10358 | PASS |
| `mesh/real_typed_struct` | 9214 | 7739 | 9403 | 7897 | PASS |
| `marine_ik/real_typed_struct` | 11552 | 9894 | 11788 | 10096 | PASS |

No-touch proof is legal only when the W1a patch does not move JSON-producing
paths and `skinny/RESULTS.md` is unchanged. Otherwise W1a must rerun the JSON
guard bench/gate and record fresh guard evidence or measured demotion.

## Current Commands

Current registered Criterion benches are only:

- `json_parity`
- `simd_scan`

There is no current `nonjson_baseline`, `nonjson_css_l4`, or lightningcss bench
registered in `skinny/crates/bbnf-bench/Cargo.toml`. Any W1a PASS must not cite
a CSS throughput command or CSS row. CSS measurement starts in W1b-1/W1b-2.

Current JSON refresh command:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1a-target CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench -p bbnf-bench
```

Current focused JSON bench filter shape:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1a-target CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench -p bbnf-bench --bench json_parity -- json_<corpus>/<bench_name>
```

Valid JSON bench names include `track1_generated`, `track1_direct_to_struct`,
`track2_direct_to_struct`, `sonic_rs_direct_to_struct`,
`serde_json_direct_to_struct`, `track1_real_typed_struct`,
`track2_real_typed_struct`, `sonic_rs_real_typed_struct`, and
`serde_json_real_typed_struct`.

JSON gate and conformance commands:

```sh
CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --advisory --check-results

CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo run -p xtask -- gate-json --with-cost-facts --check-results

cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
```

No-touch proof commands:

```sh
git diff --exit-code -- skinny/RESULTS.md
git diff --exit-code -- \
  skinny/crates/codegen/src/json_templates \
  skinny/crates/codegen/src/json_provider.rs \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/runtime/src/tape \
  skinny/crates/bbnf-bench/src \
  skinny/crates/bbnf-bench/benches \
  skinny/xtask/src/main.rs
```

If those no-touch checks pass, record `json_guard_state =
not_refreshed:no_behavior_drift`. If any JSON-producing path moved, use a
fresh `CRITERION_HOME` run and record `json_guard_state =
refreshed:<run-id>:guards-pass` or a measured demotion.

The old executable label `G-W1a-NONJSON-GATE` belongs to REDRESS 111's
SK-V11 schema-only companion lane. SK-V12 W1a should not use that old PASS
label as CSS evidence. SK-V12 W1a's gate label is
`G-W1a-GRAMMARCONFIG-LOCK14`.

## W1a PASS/FAIL Accounting

`skinny/REDRESS.md` currently ends at Item 120. The next global REDRESS entry
for W1a should be Item 121 if W1a records an outcome.

PASS entry shape:

```md
## SK-V12 Wave 1a GrammarConfig + Lock 14 Legality Gate

- Item 121 records W1a as PASS under `G-W1a-GRAMMARCONFIG-LOCK14`, not as CSS
  row admission, not as `>SOTA`, and not as SK-V12 close.
- The accepted patch resolves the seven Lock 14 leaks through GrammarConfig or
  equivalent generated metadata: structural alphabet, value dispatch,
  string/escape policy, number policy, key/object-pair policy, OffsetFlags
  interpretation, and sink/view/kind binding.
- Evidence passed: generic-crate Lock 14 scan, JSON generated parity, JSON
  guard floors or no-touch proof, generated CSS L4 metadata compile proof, and
  no new directive/BIR/BackendShape/public substrate API.
- JSON guard state: `<not_refreshed:no_behavior_drift | refreshed:<run-id>:guards-pass>`.
- `skinny/RESULTS.md` has no CSS/non-JSON row movement. W1b-1 remains the
  first place a CSS L4 generated Track 1 row can be claimed.
```

FAIL/BLOCKED/REJECTED entry shape:

```md
## SK-V12 Wave 1a GrammarConfig + Lock 14 Legality Gate Rejection

- Item 121 records W1a as `<BLOCKED | REJECTED>` under
  `G-W1a-GRAMMARCONFIG-LOCK14`.
- Failed evidence: `<compile | Lock 14 scan | JSON parity | JSON guard floor |
  stale/no-touch proof | generated CSS metadata proof>` with exact command and
  output summary.
- The failed candidate patch is saved at `/tmp/skv12-waveW1a-rejected.patch`.
- The W1a codegen/runtime/config/generated/gate/report slice was reverted.
  Unrelated user or parallel-agent edits were not reverted.
- No CSS L4 parser row, lightningcss comparator result, CSS SOTA claim, or
  `skinny/RESULTS.md` CSS row was admitted.
- Routed remainder: return to S-P3/W1a revision before W1b-1 can dispatch.
```

## Rejected Patch Protocol

Before reverting a failed W1a implementation, save only the W1a-owned candidate
slice:

```sh
git diff --binary HEAD -- \
  skinny/crates/codegen \
  skinny/crates/runtime \
  skinny/crates/ir \
  skinny/crates/bbnf-bench \
  skinny/xtask \
  skinny/RESULTS.md \
  > /tmp/skv12-waveW1a-rejected.patch
```

Verify the patch contains only the implementer's W1a files before reversal.
If unrelated user or parallel-agent edits appear in the diff, stop and split
the patch instead of reverting them.

After the rejected patch is saved, revert only the candidate W1a slice. Do not
use `git reset --hard` or broad checkout commands in a dirty shared worktree.
Then add the REDRESS 121 entry with exact failed commands, guard state, and
routed remainder. If no source patch was attempted, create an empty rejected
patch marker and record "no source patch attempted" in REDRESS.
