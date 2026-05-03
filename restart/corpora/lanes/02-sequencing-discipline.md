# Hardening Plan Audit 02 — Sequencing Discipline

Date: 2026-05-03
Standard: no substrate lands without a same-wave or next-wave consumer, per the Era V failure at `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:7-10`.

## BA Waves

| Wave | Produces | Consumer | Gate state | Verdict |
|---|---|---|---|---|
| BA.W0 | 9-directory re-org, host/css move, tape scrub, serialize deletion, inline test migration at `docs/tranches/BA/BA.md:30`. | Same wave: `cargo check`, CSS L4 generated reference rewrite, pipeline module resolution at `docs/tranches/BA/waves/W0.md:26-38`. W1/W2 consume the layout at `docs/tranches/BA/waves/W0.md:138-140`. | Compileable and tested in-wave. | honored |
| BA.W1 | Workspace metadata strategy resolver, generic recogniser registry, grammar leak deletion at `docs/tranches/BA/BA.md:31`. | Same wave: `bbnf-ir` and core pipeline consume metadata and recogniser config at `docs/tranches/BA/waves/W1.md:17-21`. BA.W5 consumes the strategy resolver at `docs/tranches/BA/waves/W1.md:132`. | Compileable and tested in-wave. | honored |
| BA.W2 | God-module splits plus layout-lowering rename at `docs/tranches/BA/BA.md:32`. | Existing call sites consume split re-exports in-wave at `docs/tranches/BA/waves/W2.md:11-13`; BA.W5 consumes split direct emitter modules at `docs/tranches/BA/waves/W2.md:158`. | Compileable. Fault: transitional old-name aliases route to BB.W0 at `docs/tranches/BA/waves/W2.md:9-11`, but BB.W0 has no receiving gate. | violated-with-rec-BA02-1 |
| BA.W3 | Path triplet, `path-core`, registry fixture deletion, runtime path deletion, legacy `parse_with.rs` deletion at `docs/tranches/BA/BA.md:33`. | `path-core` has same-wave consumers in `path` and `path-ts` at `docs/tranches/BA/waves/W3.md:34-46`. The four `parse_with.rs` deletions depend on W4 to reintroduce generated `parse_with` at `docs/tranches/BA/waves/W3.md:65`. | Not compileably specified: W3 deletes parse_with files, then gates `test(parse_with)` at `docs/tranches/BA/waves/W3.md:148` before W4 creates the replacement. | violated-with-rec-BA02-2 |
| BA.W4 | Unified `parse_with`, eager empty-path elision, `Document::get<T>` reroute at `docs/tranches/BA/BA.md:34`. | Same wave: `parse` and `get<T>` consume it at `docs/tranches/BA/waves/W4.md:34-53`; BA.W5 consumes it as the single emit target at `docs/tranches/BA/waves/W4.md:135`. | Compileable if BA.W3.M5 is moved here. | honored-after-BA02-2 |
| BA.W5 | JSON direct-to-struct emit at `docs/tranches/BA/BA.md:35`. | Same wave: JSON parser and benches consume it at `docs/tranches/BA/waves/W5.md:57-97`; BB.W1/BB.W2 consume the pattern for other grammars at `docs/tranches/BB/BB.md:45`. | Benchable in-wave for JSON. Fault under Lock 1 because non-JSON `OpenFrame` is preserved, but sequencing of the JSON substrate itself is sound. | honored-for-sequencing |
| BA.W6 | PROGRESS/FINAL, lock verification, LOC table, carry ledger at `docs/tranches/BA/BA.md:36`. | BB.W0 consumes carry ledger at `docs/tranches/BA/waves/W6.md:61-74`. | Close artefacts only. | honored |

### BA Sequencing Surgery

| ID | Target | Surgical edit |
|---|---|---|
| BA02-1 | `docs/tranches/BA/waves/W2.md:9-11`, `docs/tranches/BA/waves/W2.md:56`, `docs/tranches/BA/waves/W2.md:134` | Delete transitional `TypeDesc`/`StructLayout` aliases in W2. Do not route alias retirement to BB.W0. |
| BA02-2 | `docs/tranches/BA/waves/W3.md:62-67`, `docs/tranches/BA/waves/W3.md:148` | Move W3.M5 into W4.M1, or emit generated `parse_with` before deleting bridge files. W3 must not close with parse_with tests depending on a next-wave replacement. |

## BB Waves

| Wave | Produces | Consumer | Gate state | Verdict |
|---|---|---|---|---|
| BB.W0 | Sister-crate path-dep emigration at `docs/tranches/BB/BB.md:31`. | BB.W3 consumes optimiser crates at `docs/tranches/BB/BB.md:34`; the plan table calls this "next-wave" at `docs/tranches/BB/BB.md:153`, but W3 is not next wave. | `cargo check` proves dependency resolution, not production consumption. | violated-with-rec-BB02-1 |
| BB.W1 | Direct-to-struct across specialised grammars at `docs/tranches/BB/BB.md:32`. | Same wave: all grammar parsers and benches consume it. | Compileable, tested, benchable. | honored |
| BB.W2 | Five-grammar template emission plus all-grammar cursor unification at `docs/tranches/BB/BB.md:33`. | Same wave: byte-equality tests consume the templates; later waves use the templated runtimes. | Compileable and tested. | honored |
| BB.W3 | Optimiser output-pipe, Pratt/SIMD auto-detection, rank/tier rewrites at `docs/tranches/BB/BB.md:34`. | Same wave: BB-G1..G4 and BB-G6 consume it at `docs/tranches/BB/BB.md:15-20`; rank/tier same-wave claim at `docs/tranches/BB/BB.md:156`. | Benchable in-wave. | honored |
| BB.W4 | `parse` / `parse_in` / `parse_owned` API at `docs/tranches/BB/BB.md:35`. | Same wave API tests; BB.W5 path/visitor surface consumes it at `docs/tranches/BB/BB.md:157`. | Compileable and documented. | honored |
| BB.W5 | `pointer!`, `LazyValue`, Visitor surface at `docs/tranches/BB/BB.md:36`. | Same wave: BB-G7/BB-G9 at `docs/tranches/BB/BB.md:21-23`; BC.W4 consumes visitor contract at `docs/tranches/BC/BC.md:53`. | Compileable and tested. | honored |
| BB.W6 | BB close artefacts at `docs/tranches/BB/BB.md:37`. | BC.W0 entry consumes carry tags at `docs/tranches/BC/BC.md:47-54`. | Close artefacts only. | honored |

### BB Sequencing Surgery

| ID | Target | Surgical edit |
|---|---|---|
| BB02-1 | `docs/tranches/BB/BB.md:31`, `docs/tranches/BB/BB.md:153` | Either move sister-crate emigration to BB.W3, or add a BB.W0 same-wave consumer: compile and execute a minimal CSP/egraph optimiser pass through the path-dep crates. Delete "next-wave" unless the receiver is BB.W1. |

## BC Waves

| Wave | Produces | Consumer | Gate state | Verdict |
|---|---|---|---|---|
| BC.W0 | IR contract and initial Rust typed-IR access at `docs/tranches/BC/BC.md:30`. | BC.W1 full Rust lowerer consumes it at `docs/tranches/BC/BC.md:31`; BC.W2 scaffolds consume it at `docs/tranches/BC/BC.md:32`. | Compileable if W0 is spec + smoke; duplicate refactor ownership must be split. | honored-after-BC02-1 |
| BC.W1 | Rust emitter refactor to typed IR at `docs/tranches/BC/BC.md:31`. | Same wave regen-equality consumes it. | Compileable and tested. | honored |
| BC.W2 | TS/WASM emitter scaffolds at `docs/tranches/BC/BC.md:32`. | Same wave trivial-grammar smoke test consumes it; production activation is deferred to BD at `docs/tranches/BC/BC.md:60`. | Compileable but not benchable; acceptable only if BD is drafted or the scaffold is narrowed to contract smoke. | violated-with-rec-BC02-2 |
| BC.W3 | Core crate split at `docs/tranches/BC/BC.md:33`. | Same wave independent crate checks; W4/W5 consume split paths. | Compileable and tested. | honored |
| BC.W4 | Visitor formalisation at `docs/tranches/BC/BC.md:34`. | Same wave Rust execute + TS emit; BC-G9 consumes it at `docs/tranches/BC/BC.md:23`. | Compileable and tested. | honored |
| BC.W5 | Sister crate API freeze, `bbnf-regex` endpoint, worktree fixtures at `docs/tranches/BC/BC.md:35`. | Same wave docs/dry-run consume APIs; BD consumes publication and fixture carries at `docs/tranches/BC/BC.md:61-62`. | Compileable, but "user adjudicates at hardening time" is not an executable wave decision. | violated-with-rec-BC02-3 |
| BC.W6 | BC close and BD carry ledger at `docs/tranches/BC/BC.md:36`. | BD.W0 is named but absent. | Carry consumer is not drafted. | violated-with-rec-BC02-4 |

### BC Sequencing Surgery

| ID | Target | Surgical edit |
|---|---|---|
| BC02-1 | `docs/tranches/BC/BC.md:30-31` | Make BC.W0 own contract spec + one Rust lowerer smoke test. Make BC.W1 own the full Rust emitter refactor. |
| BC02-2 | `docs/tranches/BC/BC.md:32`, `docs/tranches/BC/BC.md:60` | Draft BD.W0 before BC execution, or narrow BC.W2 to "typed IR contract smoke only" and delete production TS/WASM carry promises. |
| BC02-3 | `docs/tranches/BC/BC.md:35` | Replace "user adjudicates at hardening time" with a default endpoint decision and a hard gate. An execution wave cannot await hardening. |
| BC02-4 | `docs/tranches/BC/BC.md:56-62`, `docs/tranches/BC/BC.md:95` | Add `docs/tranches/BD/BD.md` with W0 gates for BC→BD.C1..C3, or cut the BD carry ledger from BC. |

## Lane Verdict

| Status | Count |
|---|---:|
| honored | 14 |
| honored after named surgery | 2 |
| violated | 5 |

The most dangerous recurrence is BB.W0: it repeats the Era V pattern by landing dependency substrate two waves before its real optimiser consumer.
