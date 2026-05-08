# HARDENING-PASS-2-V9

V9 hardening audit against `restart/audit/pass-2-codegen/PASS-2.md` after the
V8.1 halt point. This audit applies the full V9 lens set: lanes 1-9 plus
F/G/H/I/J/K. Scope is audit only; no implementation amendment lands here.

## §1 Target identification

| Item | Value |
|---|---|
| Target | `restart/audit/pass-2-codegen/PASS-2.md` |
| Lines audited | 635 (`wc -l restart/audit/pass-2-codegen/PASS-2.md`) |
| Prior consolidated verdict | `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.1.md` |
| Target prior verdict | `restart/audit/hardening/HARDENING-PASS-2-V8.1.md` returned READY. |
| V9 output | `restart/audit/hardening/HARDENING-PASS-2-V9.md` |

Operating baseline: V8.1 says PASS-2 is READY and claims the Backend trait +
BIR alphabet are cohort-coherent with ARCH §7.5 / §7.2
(`restart/audit/hardening/HARDENING-CONSOLIDATED-V8.1.md:25-28`). V9 does not
ratify that claim fully. PASS-2 is close, but several post-V8.1 freshness faults
remain in the target.

## §2 Lens table

| Lens | Verdict | Finding count | Rationale |
|---|---|---:|---|
| Lane 1 — Lock adherence | AMEND | 2 | Lock 5 / Lock 8 defer WASM post-V1, but PASS-2 still names WASM V1 in active obligations (`restart/audit/pass-2-codegen/PASS-2.md:40`, `restart/locks/14-LOCKS.md:42`, `restart/locks/14-LOCKS.md:48`). |
| Lane 2 — Sequencing | N/A | 0 | PASS-2 is a single-pass synthesis, not a multi-wave tranche. |
| Lane 3 — Cohesion | AMEND | 2 | PASS-2 says it does not re-own BIR, then labels a 22-row table "Backend IR final variant table" with names that do not match ARCH §7.2 (`restart/audit/pass-2-codegen/PASS-2.md:50-78`, `restart/ARCHITECTURE.md:913-934`). |
| Lane 4 — SOTA anchoring | KEEP | 0 | Throughput rows name competitor, dataset, platform, target, mechanism, and evidence artefact (`restart/audit/pass-2-codegen/PASS-2.md:472-486`). |
| Lane 5 — Grammar-authoritative | AMEND | 1 | The yaml onboarding proof is strong, but the path consumer gate still says `pointer!` after `path!` became canonical (`restart/audit/pass-2-codegen/PASS-2.md:384`, `restart/HANDOFF.md:61`). |
| Lane 6 — Generated-code budget | KEEP | 0 | Per-grammar generated LOC, wall budgets, and handwritten LOC checks are present (`restart/audit/pass-2-codegen/PASS-2.md:423-468`). |
| Lane 7 — Friction forecast | AMEND | 1 | Diagnostic and path-schema handoff mostly holds; stale `pointer!` wording is user-surface friction at the PASS-2/PASS-3 boundary (`restart/audit/pass-2-codegen/PASS-2.md:384`). |
| Lane 8 — Carry/deferral | AMEND | 2 | Carry triples exist, but the BD.W5/J row still binds "WASM V1" to V1 parity (`restart/audit/pass-2-codegen/PASS-2.md:590`). |
| Lane 9 — Greenfield discipline | AMEND | 2 | Deleted dispatch-prompt citations remain active provenance after Phase 8.0 retired those prompts (`restart/audit/pass-2-codegen/PASS-2.md:5`, `restart/HANDOFF.md:79`). |
| Lens F — LLM bias | KEEP | 0 | No new hedging / pseudo-precision pathology dominates the active PASS-2 argument. |
| Lens G — Overfitting | AMEND | 1 | The 22-row "payload-refiner" table borrows old BC variant shape rather than consuming the authoritative ARCH alphabet cleanly (`restart/audit/pass-2-codegen/PASS-2.md:82`, `restart/ARCHITECTURE.md:936-940`). |
| Lens H — Hallucination/provenance | AMEND | 2 | PASS-2 cites deleted prompt files and wrong line ranges for ARCH §7.2 / §8.4 (`restart/audit/pass-2-codegen/PASS-2.md:80`, `restart/audit/pass-2-codegen/PASS-2.md:200-202`). |
| Lens I — Contrivance | AMEND | 1 | The WASM lowerer scaffold is legitimate as V2 proof; keeping V1 WASM close gates is apparatus beyond the V1 Rust-line contract (`restart/ARCHITECTURE.md:1095-1097`). |
| Lens J — Host-language delegation | KEEP | 0 | PASS-2 delegates closure lifetime and Rust monomorphisation to rustc where it should (`restart/audit/pass-2-codegen/PASS-2.md:200-202`). |
| Lens K — Meta-grammar discipline | AMEND | 2 | V1 RustBackend only is the meta-grammar boundary; WASM/TS land as V2 backend impls without grammar-side change (`restart/ARCHITECTURE.md:1133-1136`, `restart/ARCHITECTURE.md:1149-1155`). |

## §3 Findings

### P2-V9-1 — WASM V1 scope survived the V2 Backend-trait fold

**Verdict: REINVENT.** PASS-2 still says the pass owns "WASM V1"
(`restart/audit/pass-2-codegen/PASS-2.md:5`) and states "WASM V1 is wasm32
Rust plus binding layer" (`restart/audit/pass-2-codegen/PASS-2.md:40`). It
then correctly says V1 has one active `Backend` impl and that `WasmBackend:
Backend` is carried post-V1 (`restart/audit/pass-2-codegen/PASS-2.md:137-143`).
The target therefore contains two incompatible scope stories.

The authoritative lock and ARCH text are clear: V1 ships `RustBackend: Backend`
only; WASM and TS defer post-V1 (`restart/locks/14-LOCKS.md:42`,
`restart/ARCHITECTURE.md:1095-1097`). Lock 8 also says no measurement-pending
WASM anchor lands in V1 and that the WASM lower-and-bench programme awaits the
V2 `WasmBackend: Backend` impl (`restart/locks/14-LOCKS.md:48`).
MASTER-PLAN repeats the same boundary (`restart/MASTER-PLAN.md:141-143`,
`restart/MASTER-PLAN.md:187`).

Surgery: rewrite every PASS-2 active "WASM V1" obligation to "post-V1 wasm32
binding proof/scaffold" or route it to the V2 `WasmBackend: Backend` receiver.
Affected sites include `restart/audit/pass-2-codegen/PASS-2.md:5`,
`:40`, `:103`, `:107-109`, `:111`, `:191-193`, `:386`, `:495`, `:590`,
`:594`, `:605`, and `:631`.

### P2-V9-2 — PASS-2 still re-presents BIR as a 22-row final table

**Verdict: REINVENT.** PASS-2 says the upstream alphabet is 19 variants and that
PASS-2 is payload refiner, not BIR re-owner
(`restart/audit/pass-2-codegen/PASS-2.md:34`,
`restart/audit/pass-2-codegen/PASS-2.md:84-97`). That is the right rule.
The next table nevertheless labels itself "Backend IR final variant table" and
lists 22 rows (`restart/audit/pass-2-codegen/PASS-2.md:50-78`). Several row
names do not match ARCH §7.2: PASS-2 uses `Rule`, `Repeat`, `Optional`, `Ref`,
`Lit`, `Layout`, `MapExpr`, `FoldResult`, `EnumDiscriminator`, `Lookbehind`,
and `ErrorRecovery`; ARCH owns `Entry`, `RepeatLoop`, `OptionalBranch`,
`CallRule`, `ByteLiteral`, `LayoutScope`, `ValueProject`, `DirectBuild`,
`ErrorRecover`, and `Return` (`restart/ARCHITECTURE.md:913-934`).

ARCH §7.2 states that the post-fold shape is 19 semantic variants plus
`Return`, with PASS-1 retaining alphabet ownership and PASS-2 ratifying payload
tables (`restart/ARCHITECTURE.md:936-940`). PASS-2 should not preserve a table
whose label and variant names read as a competing alphabet.

Surgery: replace `restart/audit/pass-2-codegen/PASS-2.md:50-78` with a
payload-refinement table keyed by ARCH §7.2 variant names, or rename it
explicitly to "PASS-2 payload categories mapped to ARCH §7.2 BIR variants" and
add a one-column ARCH-variant mapping. Do not call it "final variant table."

### P2-V9-3 — Retired dispatch prompts remain as active provenance

**Verdict: DISCARD.** Handoff says `PASS-1-SUBSTRATE.md`, `PASS-2-CODEGEN.md`,
`PASS-3-RUNTIME.md`, and `SYNTHESIS.md` retired at Phase 8.0
(`restart/HANDOFF.md:79`). The files are absent from `restart/prompts/` in the
current workspace. PASS-2 still cites those retired prompts as active line
authority at `restart/audit/pass-2-codegen/PASS-2.md:5`,
`:13-17`, `:407`, and `:589`.

Surgery: convert these references to deletion archaeology or replace them with
live authorities: Handoff, ORCHESTRATOR, HARDENING, ARCH, locks, or the pass
synthesis itself. Where the old prompt is needed historically, name it
"retired prompt archaeology" and do not use it as the source of current scope.

### P2-V9-4 — `pointer!` survives in a PASS-2 consumer gate

**Verdict: REINVENT.** Handoff says `path!` is canonical and `pointer!` is
retired except deletion archaeology (`restart/HANDOFF.md:61`,
`restart/HANDOFF.md:133-135`). PASS-2 uses canonical `path!` in the Backend
trait integration paragraph (`restart/audit/pass-2-codegen/PASS-2.md:133`)
but still says the path schema is consumed by `pointer!` compilation in the
PASS-3 acceptance table (`restart/audit/pass-2-codegen/PASS-2.md:384`).

Surgery: change `restart/audit/pass-2-codegen/PASS-2.md:384` to typed `path!`
compilation. If `pointer!` must be mentioned, move it to a deletion-archaeology
sentence that routes old docs to the canonical macro.

### P2-V9-5 — Several line citations no longer carry the claimed content

**Verdict: REINVENT.** PASS-2 line-citation hygiene regressed after V8.1:

| Site | Problem | Correct anchor |
|---|---|---|
| `restart/audit/pass-2-codegen/PASS-2.md:80` | Says `RegexProgram` is canonical at ARCH §7.2 line 935, but the row is at `restart/ARCHITECTURE.md:921`; line 935 is below the table. | `restart/ARCHITECTURE.md:921` and payload row `restart/ARCHITECTURE.md:952`. |
| `restart/audit/pass-2-codegen/PASS-2.md:200` | Cites `ARCHITECTURE.md:1200-1207` for the host-process-state forbidding clause and throughput targets. Those lines are grammar productions (`HostCall`, `LambdaExpr`, `FnType`). | Forbidden closure behavior is at `restart/ARCHITECTURE.md:1366-1373`; throughput targets belong to `restart/README.md:330-338` or `restart/audit/pass-2-codegen/PASS-2.md:472-486`. |
| `restart/audit/pass-2-codegen/PASS-2.md:202` | Again cites `ARCHITECTURE.md:1200-1207` for forbidden behavior; that range does not contain the claim. | `restart/ARCHITECTURE.md:1366-1373`. |

Surgery: run a citation sweep over PASS-2, with special attention to ARCH
line-number references embedded in prose. Replace stale explicit line numbers
with current path:line anchors or section-only citations when line drift is
likely and the section is locally unambiguous.

## §4 External residues observed

These are not PASS-2-owned edits, but V9 should not lose them:

| Residue | Evidence | Receiver |
|---|---|---|
| README still says "No V1 GADT or higher-rank surface" while locks and ARCH say GADT + DK13 higher-rank land V1. | `restart/README.md:264` conflicts with `restart/locks/14-LOCKS.md:40` and `restart/ARCHITECTURE.md:1284-1305`. | MASTER-PLAN / README amendment agent. |
| README still names `pointer!` as a live public macro. | `restart/README.md:35`, `restart/README.md:284-287` conflict with `restart/HANDOFF.md:61`. | MASTER-PLAN / README amendment agent, probably paired with PASS-3. |
| README SOTA influence table still cites `regex-automata` as adopted influence, while Handoff says it is retired. | `restart/README.md:369` conflicts with `restart/HANDOFF.md:62-63`. | MASTER-PLAN / README amendment agent. |
| ARCH still has two WASM-V1 residues outside §7.5. | `restart/ARCHITECTURE.md:57`, `restart/ARCHITECTURE.md:733` conflict with `restart/ARCHITECTURE.md:1095-1097` and `restart/ARCHITECTURE.md:1443`. | MASTER-PLAN / ARCH amendment agent. |

## §5 Punch list

| # | Target | Surgery | Lens |
|---:|---|---|---|
| 1 | `restart/audit/pass-2-codegen/PASS-2.md:5`, `:40`, `:103`, `:107-109`, `:111`, `:191-193`, `:386`, `:495`, `:590`, `:594`, `:605`, `:631` | Retire "WASM V1" active obligations; route them to post-V1 `WasmBackend: Backend` proof/scaffold or V2 receiver gates. | 1, 8, I, K |
| 2 | `restart/audit/pass-2-codegen/PASS-2.md:50-78` | Recast the table as payload refinements mapped to ARCH §7.2 variant names; stop calling it final BIR alphabet. | 3, G, H |
| 3 | `restart/audit/pass-2-codegen/PASS-2.md:5`, `:13-17`, `:407`, `:589` | Remove live authority citations to retired prompt files; replace with live docs or mark as deletion archaeology. | 9, H |
| 4 | `restart/audit/pass-2-codegen/PASS-2.md:384` | Rename `pointer!` to canonical `path!` in the PASS-3 acceptance gate. | 5, 7 |
| 5 | `restart/audit/pass-2-codegen/PASS-2.md:80`, `:200-202` | Correct stale line citations to ARCH §7.2 / §8.4 and throughput anchors. | H |

## §6 Final decision

**Decision: AMENDMENT-REQUIRED-NARROW.**

PASS-2 remains architecturally salvageable: Backend IR ownership, RustBackend
two-method trait integration, rustc delegation for closures/monomorphisation,
generated LOC budgets, SOTA rows, and Lock 14 onboarding shape all survive.
The V9 blockers are narrow documentation and coherence defects, not a codegen
architecture re-draft. The highest-priority amendment is the WASM V1 sweep,
because it contradicts Lock 5, Lock 8, ARCH §7.5, and MASTER-PLAN's post-V1
backend boundary.

Hereupon PASS-2 should receive a narrow amendment pass before V9 consolidation
declares the cohort ready for Wave 9.
