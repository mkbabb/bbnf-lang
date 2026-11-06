# PROTO-1 — The mandatory-floor decider (LOCK-6 / pass-2, probe 1)

**Family:** 6D (shared with 6A) — THE ISOMORPHISM PLANE (rust/ts split).
**Verdict class:** RAN_MEASURED. **HEAD measured on:** `7cdbff4ef411154dd9bf4b80731c0616b1579d22` (base-pin; fast-forwarded from `d2b32f6e3` via `git merge --ff-only 7cdbff4ef` before any measurement — no wrong-artifact risk).
**loadavg during run:** 10.66 → 18.46 → 25.28 (1-min) across compile→result; behavioral probe, no perf claim, so load is recorded only for provenance.

---

## Q — the question

Is **6D a mandatory floor** — are the structural caps (6A/6B) that prove *name-set* isomorphism **necessary-but-not-sufficient** for Rust↔TS behavioral parity? Concretely: does name-set equality of the `PathErrorReason` taxonomy across the Rust enum and the TS union *guarantee* that both surfaces emit the same reason for the same input, or can names agree while behavior diverges?

The seed asymmetry is LIVE and documented in the tracked test `crates/bbnf-path-ts/tests/isomorphic_path_error.rs:129-150`: the deep path `$.statuses[0].nope` is surfaced differently by the canonical offline checker vs the cdylib, whereas the shallow `$.nope` agrees. This probe converts that prose note into a **measured green-structural / red-behavioral PAIR**.

## Method — the smallest decisive experiment

Added (did NOT mutate the tracked test) `crates/bbnf-path-ts/tests/floor_probe.rs` with two `#[test]`s, replicating the tracked test's `json_registry` fixture + `ts_payload`/`compile_path_native` helper at file scope:

1. **`structural_variant_names_are_set_equal`** — MUST PASS GREEN (T1/T2 name-map tier).
   - Rust side: variant name-set derived live from the enum via `{:?}` over an explicit `ALL_RUST_REASONS` array, pinned exhaustive by a no-wildcard `match` guard (a new enum variant fails to compile).
   - TS side: union member-set parsed live from `include_str!("../ts/path.ts")` — locate `export type PathErrorReason =`, slice to `;`, collect every double-quoted literal.
   - Assert the two `BTreeSet<String>`s are set-equal (and each has 8 members).

2. **`behavioral_deep_case_reasons_are_equal`** — MUST GO RED.
   - Drive the DEEP case `$.statuses[0].nope` (segments `[Field("statuses"), Index(0), Field("nope")]`) through BOTH surfaces against the SAME registry:
     - canonical: `check_path_against_registry::<bbnf::path::Json,()>(&segments, &registry, "Document")`
     - cdylib: `compile_path_native("$.statuses[0].nope", "Json")`
   - Map the canonical `PathErrorReason` → `PathErrorReasonPayload` (total, exhaustive) and `assert_eq!` the two reason payloads. The equality assertion is the probe; its RED failure is the confirmation.

**Run:** `cargo test -p bbnf-path-ts --test floor_probe -- --nocapture` (cold: full workspace + `bbnf` lib rebuild, 190 warnings, fast link of the ts crate).

## What ran — the raw output

```
running 2 tests
[PROTO-1 deep-case] canonical=UnregisteredRule  cdylib=UnknownField
thread 'behavioral_deep_case_reasons_are_equal' panicked at crates/bbnf-path-ts/tests/floor_probe.rs:249:5:
assertion `left == right` failed: DEEP-CASE behavioral divergence on `$.statuses[0].nope`:
  canonical checker surfaced UnregisteredRule but the cdylib surfaced UnknownField
  for the SAME input against the SAME registry -> structure (name-set parity)
  is necessary-but-not-sufficient; 6D is a mandatory floor
  left: UnregisteredRule
 right: UnknownField
test structural_variant_names_are_set_equal ... ok
test behavioral_deep_case_reasons_are_equal ... FAILED
test result: FAILED. 1 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

## The numbers

| Observation | Value |
|---|---|
| Structural test (name-set equality) | **PASS / green** |
| Rust `PathErrorReason` variant count | 8 |
| TS `PathErrorReason` union member count | 8 |
| Name-set relation | **set-equal** (rust-only = ∅, ts-only = ∅) |
| Behavioral test (deep-case reason equality) | **FAIL / red** |
| Deep case `$.statuses[0].nope` — canonical checker | `UnregisteredRule` |
| Deep case `$.statuses[0].nope` — cdylib | `UnknownField` |
| `UnregisteredRule == UnknownField`? | **NO** (red observed) |
| Aggregate | `1 passed; 1 failed` — the designed green/red pair |

**Root cause (mechanism, confirmed by the two source surfaces):** the canonical `check_path_against_registry` leaves `TypeDesc::Named(Status)` intact (strings-table lookup is deferred to the IR `path_check` pass, per `path::type_check`'s policy comment), so a `Field` step onto a `Named(_)` surfaces `UnregisteredRule`. The cdylib's `compile` resolves `Named(Status)` eagerly through the in-registry `layout(_)` entry and then fails the `.nope` field lookup on the resolved `Status` layout → `UnknownField`. Same input, same registry, same taxonomy names — divergent behavior.

## Verdict line

**(1) PASSES (names green) AND (2) observes `UnregisteredRule != UnknownField` (behavior red) → 6D-AS-MANDATORY-FLOOR CONFIRMED.** Structural caps (6A/6B name-set isomorphism) are **necessary-but-not-sufficient**: a rust/ts split can hold perfect taxonomy-name parity while emitting different reasons for identical input. A behavioral-parity gate (6D) is therefore a mandatory floor, not subsumed by the structural tier.

This single `cargo test` discharges **6A gap-ii** and **6D gap-ii** together — the cheapest decisive experiment in the lock, skipped in both prior passes on a seam now verified LIVE.

## Provenance / caveats

- Probe RAN (not SPEC_ONLY). The RED is an assertion failure by design; `1 passed; 1 failed` is the intended, decisive outcome, not a broken test.
- No external corpus: the registry sidecar is the in-file `json_registry` fixture (byte-identical to the tracked test + `src/fixture.rs`); the cdylib path uses `compile_path_native`, the non-wasm host mirror the tracked test already exercises.
- parse-that untouched (READ-ONLY respected); no x86; no perf claim made.
- Deliverable code is a NEW file only; the tracked `isomorphic_path_error.rs` is unmodified.
- Artifact: `floor_probe.patch` (git diff of the new test file) accompanies this report.
