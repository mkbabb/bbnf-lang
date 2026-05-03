# Hardening Plan Audit 03 — Cohesion

Date: 2026-05-03
Question: are wave exits verifiable from artefacts produced by the same wave or prior waves?

## Orphan Exit-Criteria

| ID | Site | Fault | Surgery | Verdict |
|---|---|---|---|---|
| C03-1 | `docs/tranches/BA/waves/W3.md:62-67`, `docs/tranches/BA/waves/W3.md:148` | W3 deletes the four legacy `parse_with.rs` files, but the generated `parse_with` surface is produced only in W4 at `docs/tranches/BA/waves/W4.md:34-39`. W3's `test(parse_with)` gate cannot be verified from W3 artefacts. | Move W3.M5 and the W3 parse_with test gate to W4.M1. | orphan gate |
| C03-2 | `docs/tranches/BA/waves/W4.md:27-32` | W4.M0 exit criteria cites BA-G2 zero `Vec<OpenFrame>::clone`, but BA-G2 is explicitly W5-owned by `docs/tranches/BA/waves/W5.md:92-97`. W4 does not delete `OpenFrame`. | Replace W4.M0 exit with cursor-only evidence: generated eager path has zero `cursor.decide/current_kind/match_field` calls and samply cursor inclusive <0.5%. Leave `Vec<OpenFrame>` to W5. | orphan gate |
| C03-3 | `docs/tranches/BA/waves/W2.md:9-11`, `docs/tranches/BA/waves/W2.md:134` | W2 allows transitional `TypeDesc`/`StructLayout` aliases and routes retirement to BB.W0, but BB.W0 owns sister-crate path-deps at `docs/tranches/BB/BB.md:31` and has no alias-retirement gate. | Delete aliases in W2; no carry. | dangling gate |
| C03-4 | `docs/tranches/BA/waves/W6.md:40-45` | W6 verifies every lock cell at BA close, yet BA's own lock table defers L4, L10, and L11 to BB at `docs/tranches/BA/BA.md:81`, `docs/tranches/BA/BA.md:87`, and `docs/tranches/BA/BA.md:88`. BA cannot verify locks whose mechanisms are not produced by BA. | Change BA-G4/W6.M1 to "BA-owned locks verified; deferred locks have receiving wave + gate." Or move L4/L10/L11 into BA. | impossible close criterion |
| C03-5 | `docs/tranches/BB/BB.md:37` | BB.W6 tests `-p bbnf-path`, but BA.W3 renames the crate directory to `crates/path/` at `docs/tranches/BA/waves/W3.md:27-32`. | Replace `-p bbnf-path` with `-p path -p path-core -p path-ts` in BB.W6. | stale gate |
| C03-6 | `docs/tranches/BC/BC.md:36` | BC.W6 repeats `-p bbnf-path`, though the path crate has been consolidated since BA.W3. | Replace with `-p path -p path-core -p path-ts`, or with post-BC package names if package names intentionally differ. | stale gate |
| C03-7 | `docs/tranches/BC/BC.md:35` | BC.W5 says endpoint selection is presented and "user adjudicates at hardening time." That is not an artefact a wave can create or verify. | Make BC.W5 choose one endpoint by default, with a preflight showing both candidates and a gate proving the selected path compiles. | unverifiable gate |
| C03-8 | `docs/tranches/BC/BC.md:30-31` | BC.W0 says the Rust emitter refactors to typed IR; BC.W1 also says the Rust emitter refactors to typed IR. Two waves own the same exit. | Split ownership: W0 contract + smoke; W1 full refactor + regen-equality. | duplicate owner |

## Orphan Deliverables

| ID | Site | Fault | Surgery | Verdict |
|---|---|---|---|---|
| C03-9 | `docs/tranches/BA/waves/W0.md:68-73`, `docs/tranches/BA/waves/W0.md:142` | W0 creates a BA-local worktree fixture contract and routes fleet-wide closure to BC.W2, but BC.W2 is TS/WASM scaffold work at `docs/tranches/BC/BC.md:32`. The actual worktree fixture closure appears at BC.W5 `docs/tranches/BC/BC.md:35`. | Change the receiver to BC.W5, or add a BC.W2 gate that materialises `grammar/<name>/rewrites/*.ron`. | orphan deliverable |
| C03-10 | `docs/tranches/BA/waves/W3.md:133` | W3 says BC.W4 reconciles `bbnf-regex`; BC.W4 is visitor formalisation at `docs/tranches/BC/BC.md:34`. `bbnf-regex` reconciliation is BC.W5 at `docs/tranches/BC/BC.md:35`. | Replace BC.W4 with BC.W5. | dangling carry |
| C03-11 | `docs/tranches/BA/waves/W6.md:168-169` | W6 names sibling dirty-state baseline and `AscentStrategy` disposition as BC.W0 carries, but BC.W0 has no such artefacts at `docs/tranches/BC/BC.md:30`. The ground-truth carry table assigns these at `audit/HARDENING-SYNTHESIS-2026-05-03.md:224` and `audit/HARDENING-SYNTHESIS-2026-05-03.md:227`. | Add `docs/tranches/BC/audit/W0-sibling-baseline.txt` and `W0-ascent-strategy-disposition.md` to BC.W0 gates. | orphan deliverable |
| C03-12 | `docs/tranches/BC/BC.md:56-62` | BC produces three BD carries, but BD is absent. The receiving gates do not exist. | Draft BD with W0 gates or cut the carries. | orphan deliverable |

## Sound Cohesion

| Site | Why it holds | Verdict |
|---|---|---|
| `docs/tranches/BA/waves/W1.md:29-76` | W1's metadata and recogniser outputs are consumed by the same wave's `bbnf-ir` tests and by BA.W5's strategy resolver. | honored |
| `docs/tranches/BA/waves/W5.md:57-97` | W5's direct-to-struct JSON emitter is consumed by regenerated JSON and by BA-G1/BA-G2 benches in the same wave. | honored |
| `docs/tranches/BB/BB.md:34` | BB.W3 creates rank/tier rewrites and consumes them in the same optimiser wave; this fixes the prior W0 skeleton fault named at `audit/HARDENING-SYNTHESIS-2026-05-03.md:107-127`. | honored |
| `docs/tranches/BC/BC.md:33` | BC.W3 crate split has in-wave independent `cargo check` and `nextest` gates for all split crates. | honored |

## Lane Verdict

| Status | Count |
|---|---:|
| honored | 4 |
| orphan gate | 8 |
| orphan deliverable | 4 |

Hereupon the plan is not cohesive enough to execute. The failing shape is not vague: eight gates name artefacts their waves do not produce.
