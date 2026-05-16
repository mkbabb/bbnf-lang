# HARDENING-PASS-3-V5

Carry-aware Phase 0 V5 metahardening for PASS-3.

## §1 Target identification

| Field | Value |
| --- | --- |
| Target surface | `restart/audit/pass-3-runtime/PASS-3.md` |
| Output file | `restart/audit/hardening/HARDENING-PASS-3-V5.md` |
| Audit phase | Phase 0 V5 metahardening |
| Audit mode | Carry-aware, verify-then-write, no target patching |
| Primary prior | `restart/audit/hardening/HARDENING-PASS-3-V4.md` |
| Consolidated prior | `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md` |
| Current verdict | AMENDMENT-REQUIRED |
| Verdict scope | Narrow cross-document and worked-example hardening, not a PASS-3 rewrite |
| Touch discipline | This report only |

V5 re-opened PASS-3 after V4 READY to test whether the READY state survived
carry-aware checks against the merged V4 consolidated state, the lock ledger,
research-index obligations, and the newer hardening instructions.

The target remains mostly coherent. The tape substrate, path crate split,
consumer gates, generated API budget, fixture separation, and PASS-3 receiver
ledger all remain structurally usable.

The V5 blockers are narrower:

- stale positive README prose still preserves the retired ParseStream and
  rewrite/Unicode bundle;
- one PASS-3 lock citation points at the wrong lock;
- one PASS-3 diagnostic row uses a stale crate prefix;
- worked examples remain too sparse for yaml onboarding, pointer/select use,
  incremental recovery, `@error(recover)`, and one A->F->J grammar trajectory;
- cross-pass diagnostic wording still risks reviving `@pratt`/`@simd`
  directives;
- one architecture table row appears to omit the yaml host-route cell;
- a debug/DAP identity sentence is still phrased as a recommendation rather
  than a gate.

This is AMENDMENT-REQUIRED rather than RE-DRAFT because PASS-3 already owns the
right runtime model and most of the amendment surface can be handled as targeted
line edits plus compact examples.

## §2 Carry-aware lens table A-E

| ID | Lens | Evidence | Classification | Required action |
| --- | --- | --- | --- | --- |
| A1 | Inter-document narrative coherence | PASS-3 flags stale README prose at `PASS-3.md:20-23`, including `README.md:473`, but `README.md:391` also still says the substrate is `ParseStream` shaped. | REINVENT | Close both README stale-positive lines or route both explicitly as archaeology. |
| A2 | Inter-document narrative coherence | PASS-3 tape/direct contract at `PASS-3.md:130-154` binds cleanly to `ARCHITECTURE.md:1160-1190` and `MIGRATION.md:720-735`. | KEEP | Preserve the tape/direct union wording. |
| A3 | Inter-document narrative coherence | PASS-3 path split at `PASS-3.md:84-103` and `PASS-3.md:273-299` matches `ARCHITECTURE.md:55-57` and migration path-split rows. | KEEP | Keep `path-core`, `path`, and `path-ts` as the stable names. |
| A4 | Inter-document narrative coherence | PASS-3 says every perf gate names competitor, dataset, and platform per Lock 14 at `PASS-3.md:375`; the actual lock is Lock 8 at `restart/locks/LOCKS.md:48`. | REINVENT | Replace the lock citation with Lock 8 and a correct line target. |
| A5 | Inter-document narrative coherence | PASS-3 generated API budget at `PASS-3.md:401-413` binds to PASS-2 generated LOC rows and MASTER regen gates. | KEEP | Keep the budget table; cite receiving gates rather than expanding it. |
| A6 | Inter-document narrative coherence | PASS-3 yaml fixture separation at `PASS-3.md:320-325` agrees with `README.md:13` and migration future-grammar gates. | KEEP | Preserve the two-surface onboarding rule. |
| B1 | Vocabulary drift | `LayoutFacts`, `LayoutSink`, and `passes::layout` remain canonical in PASS-2 and ARCH; PASS-3 does not contradict them. | KEEP | No PASS-3 surgery needed. |
| B2 | Vocabulary drift | PASS-3 diagnostic row `LowererImport` at `PASS-3.md:369` names `bbnf_ir::grammar_ir` and `bbnf_ir::backend_ir`; current architecture owns `ir/src/backend_ir/` and refers to Grammar IR/Backend IR without that crate prefix. | REINVENT | Replace the stale `bbnf_ir::` names with current `ir::...` or diagnostic vocabulary from ARCH. |
| B3 | Vocabulary drift | PASS-3 tape vocabulary uses `Tape`, `TapeToken`, `ValueRef`, and runtime paths at `PASS-3.md:130-154` and `PASS-3.md:194-208`; this matches `README.md:285-314` and ARCH. | KEEP | Keep the names and module paths. |
| B4 | Vocabulary drift | PASS-3 retires `path!` and preserves `pointer!` plus `select!` through `PASS-3.md:80-103`. | KEEP | Keep legacy `bbnf-path` citations framed as archaeology only. |
| B5 | Vocabulary drift | Lookbehind diagnostics are split across `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, and `LookbehindWidth`; PASS-3 only lists `BBNF-LIFE003` plus `LookbehindWidth` at `PASS-3.md:365`. | REINVENT | Add an alias note so PASS-3 cannot drift from PASS-1 and ARCH diagnostic names. |
| B6 | Vocabulary drift | `OpenFrame` appears only in deletion or archaeology contexts in PASS-1, PASS-2, ARCH, MASTER, and MIGRATION. | KEEP | Maintain the deletion gate. |
| B7 | Vocabulary drift | Block-bodied `@host fn` is anchored in README and PASS-1; PASS-3 has host-route and WasmHost diagnostics but no runnable example. | REINVENT | Add a compact host example or route it to a named cookbook gate. |
| C1 | Worked-example scarcity | PASS-3 has yaml source and metadata requirements at `PASS-3.md:320-344`, but not one complete grammar -> metadata -> generated outputs -> benchmark path. | REINVENT | Add a yaml onboarding E2E example or route it to a cookbook item with acceptance. |
| C2 | Worked-example scarcity | PASS-3 explains `pointer!` and `select!`, but no example walks a query through both macros and a typed `ValueRef`. | REINVENT | Add a pointer/select example with one success path and one diagnostic path. |
| C3 | Worked-example scarcity | PASS-3 defines `DocumentSnapshot`, `ReparsePlan`, fallback thresholds, and LSP silence policy at `PASS-3.md:162-190`, but no incremental edit walkthrough. | REINVENT | Add an incremental parse example with changed ranges, fallback accounting, and no diagnostic spam. |
| C4 | Worked-example scarcity | PASS-3 settles `@error(recover = ...)` at `PASS-3.md:160`, but gives no concrete recovered syntax case. | REINVENT | Add a recovered-error example bound to BBNF-RECOVERY diagnostics. |
| C5 | Worked-example scarcity | PASS-3 contains wave hand-offs, but no single grammar trajectory from A to F to J. | REINVENT | Add a compact grammar trajectory showing substrate, generation, runtime consumers, and publication gate. |
| D1 | Coverage gap | Unfamiliar grammar onboarding has tables and gates, but little ergonomic guidance for the first failed grammar import. | REINVENT | Add first-failure diagnostics and a generated-output expectation row. |
| D2 | Coverage gap | Fault-tolerant incremental parsing is measured by fallback rates, but the error-recovery path itself is not worked through. | REINVENT | Bind one malformed edit to recovery, fallback, and LSP quiet policy. |
| D3 | Coverage gap | PASS-3 says debug and DAP should reuse tape identity at `PASS-3.md:156`; this is not strong enough for a runtime gate. | REINVENT | Rephrase as a required identity contract with a debug acceptance gate. |
| D4 | Coverage gap | Generic-rule typing under `@error` is covered upstream, but PASS-3 does not show runtime user-surface behavior. | REINVENT | Add a diagnostic or recovery example involving generic context. |
| D5 | Coverage gap | WASM host primitives are represented by `WasmHost`, but PASS-3 has no ABI or primitive example tied to H.W3. | REINVENT | Add a route to H.W3 or a one-row ABI example. |
| D6 | Coverage gap | Lock 11 incubation stability is handled in MASTER J, and PASS-3 keeps path crates separated. | KEEP | No additional PASS-3 blocker. |
| E1 | Cumulative lock tension | Lock 1 tape authority and Lock 6 optimization interact at `ValueRef` identity; PASS-3 asserts identity but lacks a rewrite-stress example. | REINVENT | Add a query/rewrite identity example or route to a specific test. |
| E2 | Cumulative lock tension | Lock 10 forbids user-forced Pratt/SIMD selection, but PASS-2 diagnostic text still mentions `@pratt` and `@simd` hints. | REINVENT | Route a cross-pass diagnostic wording fix so PASS-3 does not inherit directive drift. |
| E3 | Cumulative lock tension | Lock 14 future grammar scale and Lock 13 generated-size caps are mostly bound, but `ARCHITECTURE.md:1331` appears to omit the yaml host-route cell. | REINVENT | Fix table cell alignment or add explicit yaml host route. |
| E4 | Cumulative lock tension | Lock 8 SOTA gates and Lock 11 publication stability meet in MASTER J; PASS-3 report-only incremental/debug rows do not make SOTA claims. | KEEP | Keep incremental/debug rows report-only. |
| E5 | Cumulative lock tension | Lock 5 lowerers cannot walk grammar; PASS-3 imports diagnostic protects that boundary but the stale crate prefix weakens it. | REINVENT | Correct the diagnostic row and tie it to ARCH lowerer import denial. |

Lens A-E count: 30 rows.

Carry-note expansion:

- A1 is blocking because stale prose sits in an authority document, not only in
  a historical audit.
- A1 is especially important because PASS-3 already names one stale README
  line, so the remaining stale line is a carry-completeness failure.
- A2 remains green because PASS-3 separates user-visible semantic identity from
  physical packing.
- A3 remains green because legacy path names are not used as future crate names.
- A4 is small but blocking because lock citations are evidence, not ornament.
- A5 stays green because the generated budget can be validated mechanically.
- A6 stays green because yaml fixtures remain parity-phase inputs, not grammar
  authority.
- B1 stays green because PASS-3 avoids exposing `TypeFacts` as a public side
  table.
- B2 is blocking because a stale crate prefix can send an implementer to the
  wrong ownership boundary.
- B3 stays green because runtime module paths align with README and ARCH.
- B4 stays green because `path!` is not revived as public syntax.
- B5 needs an alias note because diagnostic users search by numeric code,
  string alias, and kind name.
- B6 stays green because `OpenFrame` does not re-enter the target surface.
- B7 needs proof because host primitives affect both native and WASM surfaces.
- C1 is the largest ergonomics gap for a worker adding an unfamiliar grammar.
- C2 is the largest consumer gap for runtime query users.
- C3 is the largest editor gap for LSP implementers.
- C4 is the largest recovery gap for grammar authors.
- C5 is the largest synthesis gap because it forces all waves to tell one
  grammar story.
- D1 does not require tutorial prose, only a compact failure-path example.
- D2 should prove fault tolerance before benchmark fallback percentages become
  trusted.
- D3 should become a gate because debug and DAP consume runtime identity.
- D4 can stay small if the example binds generic context to one diagnostic.
- D5 should route to H.W3 without inventing WASM measurements.
- D6 does not need PASS-3 surgery because MASTER owns the publication-stability
  path.
- E1 is a stress case between optimization and runtime identity.
- E2 is a cross-pass wording problem because PASS-3 mirrors diagnostic policy.
- E3 is a table-shape problem with implementation consequences.
- E4 stays green because PASS-3 avoids making incremental/debug SOTA claims.
- E5 is the same boundary problem as B2, seen through the lowerer lock.

## §3 LLM-pathology table F-H

| ID | Lens | Evidence | Pathology | Required action |
| --- | --- | --- | --- | --- |
| F1 | LLM bias subclass | `PASS-3.md:156` says debug and DAP should reuse tape identity. | Hedged commitment where a runtime invariant is needed. | Change to mandatory wording and add an acceptance gate. |
| F2 | LLM bias subclass | `PASS-3.md:154` says PASS-1 may pack differently while preserving semantics. | Acceptable scope nuance, not drift. | Keep the semantic invariant language. |
| F3 | LLM bias subclass | Fallback rates at `PASS-3.md:185-188` are precise thresholds without inline owner/source context. | Pseudo-precision risk. | Keep as gates only if the receiving bench owner and dataset are cited. |
| F4 | LLM bias subclass | PASS-3's tape answer is decisive, but grounded in Lock 1, current failure, and ARCH tape ABI. | No synthesis bias found. | Keep the substrate decision. |
| F5 | LLM bias subclass | PASS-3 describes consumer rows as executable at `PASS-3.md:104-112`. | Concrete falsifiability, low pathology. | Keep exact commands. |
| G1 | Overfitting subclass | PASS-3 uses selector and typed cursor ideas from runtime research, but binds them to `ValueRef` and BBNF path metadata. | Not benchmark mimicry alone. | Keep with examples. |
| G2 | Overfitting subclass | Incremental model resembles rowan/rust-analyzer, but no BBNF-specific recovered-error walkthrough is present. | External-shape overfit risk. | Add BBNF-specific invalid-edit example. |
| G3 | Overfitting subclass | Recovery path likely borrows tree-sitter style fault tolerance, but PASS-3 does not show `@error(recover)` in action. | Pattern overfit risk. | Add one grammar-local recovery trace. |
| G4 | Overfitting subclass | SOTA rows are tied to bbnf targets and platform, not generic "fast parser" claims. | Acceptable adaptation. | Keep exact row shape. |
| G5 | Overfitting subclass | Yaml onboarding copies the future-grammar matrix pattern but does not give an operator-facing import story. | Matrix overfit risk. | Add a first-import narrative and failure mode. |
| H1 | Hallucination/provenance gap | `PASS-3.md:375` cites Lock 14 for the competitor/dataset/platform rule. | Wrong provenance. | Replace with Lock 8 citation. |
| H2 | Hallucination/provenance gap | `PASS-3.md:391-396` lists competitor floors but depends on corpus/research provenance outside the row. | Source opacity risk. | Add a source/provenance note or explicitly route to research Topic 6/8. |
| H3 | Hallucination/provenance gap | Generated LOC budgets cite PASS-B/PASS-2 baselines indirectly. | Mostly acceptable because PASS-2 rows exist. | Keep, but preserve the receiving gate. |
| H4 | Hallucination/provenance gap | Lookbehind diagnostic alias chain is not fully visible in PASS-3. | Alias provenance gap. | Add BBNF1004 / BBNF-LOOKBEHIND-WIDTH / LookbehindWidth binding. |
| H5 | Hallucination/provenance gap | MASTER H.W3 has `{N}` and `{M}` TBD for WASM host latency/size. | Scoped TBD, not hallucinated number. | Keep routed to H owner; do not invent numbers in PASS-3. |

Lens F-H count: 15 rows.

## §4 Compressed 9-lane verification

| Row | Lane | Status | Evidence | Finding |
| --- | --- | --- | --- | --- |
| 1 | Lane 1 - Lock adherence | KEEP | Lock 1 tape authority is reflected in `PASS-3.md:130-154`. | Tape stays the runtime authority. |
| 2 | Lane 1 - Lock adherence | KEEP | Lock 5 lowerer boundary is reflected by lowerer import diagnostics. | Boundary is structurally present. |
| 3 | Lane 1 - Lock adherence | REINVENT | Lock 8 is miscited as Lock 14 at `PASS-3.md:375`. | Correct the lock citation. |
| 4 | Lane 1 - Lock adherence | KEEP | Lock 13 generated-size caps are integrated at `PASS-3.md:401-413`. | API budget is concrete. |
| 5 | Lane 2 - Raw research inclusion | N/A | PASS-3 is a pass audit target, not a raw research target. | Lane 2 intentionally not applied. |
| 6 | Lane 3 - Cross-doc cohesion | REINVENT | README still contains stale positive `ParseStream` prose. | Close README carry rows before final synthesis. |
| 7 | Lane 3 - Cross-doc cohesion | KEEP | Path crate split aligns across PASS-3, ARCH, MASTER, and MIGRATION. | No name drift on `path-core` / `path` / `path-ts`. |
| 8 | Lane 3 - Cross-doc cohesion | REINVENT | ARCH yaml table row appears to miss the host-route cell. | Fix alignment or add explicit host route. |
| 9 | Lane 4 - SOTA/performance | KEEP | PASS-3 SOTA rows name bbnf target, competitor floor, and platform. | Gate shape is usable. |
| 10 | Lane 4 - SOTA/performance | REINVENT | Provenance for competitor floors is external to PASS-3 row text. | Add source note or route to Topic 6/8 provenance. |
| 11 | Lane 5 - Grammar generality | KEEP | PASS-3 separates yaml source and metadata from fixtures. | Future grammar admission remains general. |
| 12 | Lane 5 - Grammar generality | REINVENT | No full yaml onboarding example proves the two-surface rule. | Add E2E yaml import example. |
| 13 | Lane 6 - Generated footprint | KEEP | PASS-3 gives generated API budget, regen wall budget, and fixture-specific delta. | Budget can be tested. |
| 14 | Lane 6 - Generated footprint | KEEP | PASS-2 baselines and MASTER generated gates back PASS-3's budget table. | No extra blocker. |
| 15 | Lane 7 - Friction/consumer ergonomics | KEEP | PASS-3 has three executable consumer acceptance gates. | Consumer rows are falsifiable. |
| 16 | Lane 7 - Friction/consumer ergonomics | REINVENT | Visitor cookbook is routed, but pointer/select and recovery examples are still scarce. | Add compact examples or hard links to cookbook rows. |
| 17 | Lane 8 - Carry closure | KEEP | PASS-3 carries stale prompt/inheritance/deletion rows explicitly. | Carry ledger is useful. |
| 18 | Lane 8 - Carry closure | REINVENT | PASS-3 V4 missed README `ParseStream` line 391 and wrong Lock 8 citation. | V5 carry must reopen those rows. |
| 19 | Lane 9 - Greenfield readiness | KEEP | Target runtime shape is complete enough for implementation. | No re-draft needed. |
| 20 | Lane 9 - Greenfield readiness | REINVENT | Missing examples would slow a worker onboarding an unfamiliar grammar. | Add examples before synthesis closure. |

Lane 2 is N/A by contract because PASS-3 is an audit target, not raw research.

## §5 16-command gate-rerun

Commands were rerun against:

`restart/README.md`

`restart/ARCHITECTURE.md`

`restart/MIGRATION.md`

`restart/MASTER-PLAN.md`

`restart/audit/pass-1-substrate/PASS-1.md`

`restart/audit/pass-2-codegen/PASS-2.md`

`restart/audit/pass-3-runtime/PASS-3.md`

| # | Command | Observed result | V5 status |
| --- | --- | --- | --- |
| 1 | `rg -n "ParseStream|rewrite-mode|Unicode class algebra" ...` | Mostly archaeology, but `README.md:391` and `README.md:473` remain stale positive prose. | FAIL |
| 2 | `rg -n "bbnf-path|bbnf-test-fixtures|path!" ...` | Legacy names appear only as migration/deletion/archaeology, while `path-core`, `path`, `path-ts` are canonical. | PASS |
| 3 | `rg -n "codegen/src/backend_ir" ...` | Hits are boundary documentation and import-deny language, not ownership drift. | PASS |
| 4 | `rg -n "fixtures/yaml" ...` | Hits stay parity-phase scoped and do not make yaml a first-class source surface. | PASS |
| 5 | `rg -n "@recover" ...` | Hits are legacy alias or deletion-table references; `@error(recover = ...)` is the settled form. | PASS |
| 6 | `rg -n "OpenFrame" ...` | Hits are deletion archaeology or negative gates. | PASS |
| 7 | `rg -n "GrammarIR" ...` | Hits do not give lowerers ownership of Grammar IR. | PASS |
| 8 | `rg -n "__EAGER_EMPTY_PATH|CursorDecision::Skip" ...` | Cursor skip gates are present across PASS-2, ARCH, MASTER, and MIGRATION. | PASS |
| 9 | `rg -n "twitter|canada|citm|bootstrap|animate|On-Demand" ...` | SOTA rows exist; `bootstrap` is noisy because it also matches crate names. | PASS |
| 10 | `rg -n "receiver|blocker|receiving gate" ...` | Receiver/blocker discipline is present in README, PASS-3, ARCH, MASTER, and MIGRATION. | PASS |
| 11 | `rg -n "yaml.bbnf|workspace.metadata.bbnf.grammars.yaml" ...` | Two-surface yaml onboarding is present across README, PASS-1, PASS-2, PASS-3, ARCH, MASTER, and MIGRATION. | PASS |
| 12 | `rg -n "generated_loc|regen_wall|xtask" ...` | Generated LOC and regen wall gates are present. | PASS |
| 13 | `rg -n "BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|BBNF-GRAMMAR|BBNF-POINTER|lookbehind|HostSignature" ...` | Diagnostics are mostly coherent, but PASS-2 still mentions `@pratt` and `@simd` hint forcing in OPT rows. | ATTENTION |
| 14 | `rg -n "child count|500 LOC|exception rationale" ...` | Child-count and generated-size lock language appears in README, PASS-2, ARCH, MASTER, MIGRATION, and PASS-3 budget rows. | PASS |
| 15 | `rg -n "declaration-crate review|why metadata|deletion path|reviewer" ...` | Declaration-crate rejection and metadata rationale remain routed. | PASS |
| 16 | `rg -n "CPU model|compiler flags|input hash|competitor version|warmup|sample" ...` | Full reproducibility metadata is in ARCH and MASTER; PASS-3 rows carry platform and target but not all source metadata inline. | PASS_WITH_NOTE |

Gate summary:

- 13 commands pass without new blocker.
- 1 command fails because stale README positive prose remains.
- 1 command needs diagnostic wording attention across PASS-2/PASS-3.
- 1 command passes with provenance note because full benchmark metadata lives in ARCH/MASTER.

Command interpretation notes:

- Command 1 is the only hard FAIL in the rerun.
- Command 1 is not noise because the README hits are not deletion-ledger rows.
- Command 2 confirms that `bbnf-path` survives only as a legacy name.
- Command 3 confirms that Backend IR ownership is not being moved back to
  `codegen`.
- Command 4 confirms yaml fixtures remain parity-phase support material.
- Command 5 confirms `@recover` is not being revived as the settled spelling.
- Command 6 confirms `OpenFrame` is not part of the future runtime ABI.
- Command 7 confirms lowerers are not told to walk Grammar IR.
- Command 8 confirms eager-empty and skip-decision regressions have named gates.
- Command 9 confirms target/competitor rows exist even though one token is
  search-noisy.
- Command 10 confirms receiver/blocker language is still present.
- Command 11 confirms yaml onboarding has the two named source surfaces.
- Command 12 confirms generated LOC and regen wall gates remain searchable.
- Command 13 is not a PASS-3-only failure, but it can leak into PASS-3 synthesis
  because diagnostic ledgers are cross-pass contracts.
- Command 14 confirms child-count and generated-size rules are still visible.
- Command 15 confirms declaration-crate rejection remains justified.
- Command 16 confirms reproducibility metadata exists, but it is distributed.
- Distributed benchmark metadata is acceptable only if PASS-3 cites the correct
  lock and receiver rows.
- The rerun supports amendment, not re-draft.
- The rerun also confirms this report should not patch target files directly.

## §6 Cross-document binding ledger

| Binding | PASS-3 anchor | ARCH anchor | MASTER anchor | MIGRATION anchor | V5 result |
| --- | --- | --- | --- | --- | --- |
| Tape/direct runtime ABI | `PASS-3.md:130-154` | `ARCHITECTURE.md:1160-1190` | Runtime substrate and F/G receivers | `MIGRATION.md:720-735` | KEEP |
| Typed `ValueRef` identity | `PASS-3.md:80`, `PASS-3.md:154-156` | Tape/value ABI rows | Debug and consumer waves | Runtime substrate test rows | REINVENT debug wording |
| Path crate split | `PASS-3.md:84-103`, `PASS-3.md:273-299` | `ARCHITECTURE.md:55-57` | G wave path/select rows | Migration path split rows | KEEP |
| `pointer!` and `select!` user surface | `PASS-3.md:80-103` | Consumer API rows | MASTER friction rows | Runtime consumer migration rows | REINVENT examples |
| Retired `path!` | `PASS-3.md:84-92` | Architecture deletion table | Legacy route rows | Migration deletion rows | KEEP |
| Incremental parsing | `PASS-3.md:162-190` | Incremental architecture rows | I.W1-I.W5 | Runtime substrate and recovery gates | REINVENT example |
| LSP fallback policy | `PASS-3.md:179-190` | LSP architecture rows | I tranche | Migration LSP rows | KEEP policy, add proof |
| Recovery syntax | `PASS-3.md:160` | Recovery diagnostic rows | I.W2/I.W3 | Recovery migration rows | REINVENT example |
| Diagnostics ledger | `PASS-3.md:346-371` | `ARCHITECTURE.md:1004-1039` | MASTER friction and carry rows | `MIGRATION.md:761-770` | REINVENT alias/import rows |
| Lowerer import denial | `PASS-3.md:369` | BIR import-deny architecture | F lowerer gates | Codegen migration rows | REINVENT crate prefix |
| SOTA benchmark rows | `PASS-3.md:391-399` | `ARCHITECTURE.md:1227-1259` | `MASTER-PLAN.md:125-150` | Publication migration rows | KEEP with citation fix |
| Benchmark provenance | `PASS-3.md:375-396` | ARCH metadata schema | MASTER reproducibility rows | Benchmark migration rows | REINVENT lock citation |
| Generated API budget | `PASS-3.md:401-413` | Generated footprint rows | MASTER regen gates | `MIGRATION.md:737-745` | KEEP |
| Yaml onboarding | `PASS-3.md:320-344` | `ARCHITECTURE.md:1270-1331` | MASTER future grammar rows | `MIGRATION.md:747-759` | REINVENT example/table cell |
| Fixture separation | `PASS-3.md:320-325` | Fixture architecture rows | MASTER carry rows | Migration fixture rows | KEEP |
| Host/WASM primitives | `PASS-3.md:342`, `PASS-3.md:368` | Host/WASM rows | H.W3 | Migration WASM ABI row | REINVENT example/route |
| Lock 11 publication stability | PASS-3 path stabilization | Architecture crate split | J publication rows | Migration closure rows | KEEP |
| README closeout prose | `PASS-3.md:20-23` | ARCH conflict ledger | MASTER carry ledger | Migration closeout | REINVENT |

Ledger result:

PASS-3 is still a viable receiver document, but its closure depends on a small
set of cross-document edits outside this report.

## §7 Deduped punch list

| # | Path:line | Surgery | Acceptance gate | Lens origin |
| --- | --- | --- | --- | --- |
| 1 | `restart/README.md:391` and `restart/README.md:473` | Replace stale positive `ParseStream`, rewrite-mode, and Unicode-bundle prose with tape/direct runtime and settled extension language; or move it into explicit archaeology. | `rg -n "ParseStream|rewrite-mode|Unicode class algebra" restart/README.md` has no positive settled-state hits. | A1, B3, F4, H1 |
| 2 | `restart/audit/pass-3-runtime/PASS-3.md:375` | Replace "Lock 14" / `restart/locks/LOCKS.md:207` with Lock 8 / `restart/locks/LOCKS.md:48` for competitor/dataset/platform evidence. | Citation check points to Lock 8 and the SOTA gate rows still pass command 9. | A4, H1 |
| 3 | `restart/audit/pass-3-runtime/PASS-3.md:369` | Replace `bbnf_ir::grammar_ir` and `bbnf_ir::backend_ir` with current `ir::grammar_ir` / `ir::backend_ir` or ARCH diagnostic vocabulary. | `rg -n "bbnf_ir::" restart/audit/pass-3-runtime/PASS-3.md` returns no hits. | B2, E5, H3 |
| 4 | `restart/audit/pass-3-runtime/PASS-3.md:365` | Add the alias binding among `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, and `LookbehindWidth` if PASS-3 keeps `BBNF-LIFE003` as the local diagnostic row. | Diagnostic ledger names the numeric code, human alias, and kind in one row or footnote. | B5, H4 |
| 5 | `restart/audit/pass-2-codegen/PASS-2.md:540-541` | Remove `@pratt` and `@simd` as user-facing hints or force mechanisms; replace with recognizer explanation and grammar/metadata guidance. | `rg -n "@pratt|@simd" restart/audit/pass-2-codegen/PASS-2.md` returns only deletion/negative-context hits. | E2, F3, H2 |
| 6 | `restart/audit/pass-3-runtime/PASS-3.md:320-344` | Add a yaml onboarding E2E mini-example: `yaml.bbnf`, metadata block, generated outputs, fixture scope, and bench/report gate. | Example contains exactly two source surfaces and does not promote `fixtures/yaml` to source authority. | C1, D1, G5 |
| 7 | `restart/audit/pass-3-runtime/PASS-3.md:80-103` | Add a pointer/select mini-example that flows through `ValueRef`, `pointer!`, `select!`, and one `BBNF-POINTER` diagnostic. | Example uses both macros and has one typed success plus one compile-time failure. | C2, D1, G1 |
| 8 | `restart/audit/pass-3-runtime/PASS-3.md:160-190` | Add an incremental recovery walkthrough: malformed edit, `@error(recover = ...)`, `DocumentSnapshot`, `ReparsePlan`, fallback rate, and LSP quiet policy. | Example records changed span, recovery node, fallback accounting, and no stale diagnostics. | C3, C4, D2, G2, G3 |
| 9 | `restart/audit/pass-3-runtime/PASS-3.md:156` | Replace "Debug and DAP should reuse this identity" with a mandatory debug/DAP tape snapshot identity contract. | `rg -n "should reuse this identity" restart/audit/pass-3-runtime/PASS-3.md` returns no hits. | D3, F1 |
| 10 | `restart/ARCHITECTURE.md:1331` | Add or realign the yaml host-route cell in the 10x9 future-grammar table. | The yaml row has the same column count and semantics as bbnf/json/css rows. | A6, B7, E3, H2 |
| 11 | `restart/audit/pass-3-runtime/PASS-3.md:414-429` | Add one A->F->J grammar trajectory row or route to MASTER: grammar import, BIR lowering, runtime consumers, docs/publication gate. | One named grammar has wave owners and receiving gates across A, F/G, and J. | C5, E4 |
| 12 | `restart/audit/pass-3-runtime/PASS-3.md:368` | Route the WASM host primitive example to H.W3 without inventing `{N}`/`{M}` latency or size values. | PASS-3 names the H.W3 receiver and keeps TBD numbers owned by MASTER. | D5, H5 |

Punch-list priority:

1. Close stale README prose and wrong Lock 8 citation first.
2. Fix diagnostic vocabulary drift.
3. Add the three operator-facing examples: yaml onboarding, pointer/select, incremental recovery.
4. Add the A->F->J trajectory and host/WASM route if synthesis still needs stronger connective tissue.

## §8 V1->V4 history note

V1 found PASS-3 under-amended after the first hardening pass.

V1's main issues were path crate naming, pointer/select runtime shape,
SOTA rows, `bbnf/src` tree shape, yaml proof, generated budgets, diagnostic
ledger binding, and carry closure.

V2 moved PASS-3 to READY by closing most of those rows and leaving only narrow
visitor/yaml routing concerns.

V3 reopened PASS-3 as AMENDMENT-REQUIRED because row-level benchmark
attribution and generated API baselines were still too implicit.

V4 marked PASS-3 READY after verifying those V3 punch items were closed and
after checking consolidated V4 conflicts.

V5 does not overturn V4's main runtime conclusion.

V5 does overturn the idea that PASS-3 is synthesis-ready without further
targeted edits.

The delta is caused by carry-aware metahardening:

- README stale prose survived the V4 close;
- the Lock 8 citation is wrong in PASS-3;
- diagnostic vocabulary drift remains visible across PASS-2/PASS-3;
- examples are still scarce where worker onboarding needs executable proof;
- architecture yaml table alignment likely needs one host-route cell.

This is a V5 hardening issue, not a rejection of the tape/runtime design.

## §9 LLM-pathology summary

The strongest pathology class is not hallucinated architecture.

PASS-3's runtime architecture is anchored in locks, pass audits, and migration
gates.

The risk is closure bias.

V4 correctly saw that many V3 row-level issues were fixed, then treated the
target as ready without re-opening every stale cross-document phrase and every
operator-facing example obligation.

The second pathology class is hedged obligation.

`PASS-3.md:156` uses "should" where debug and DAP identity must be a runtime
contract.

The third pathology class is provenance thinning.

`PASS-3.md:375` cites the wrong lock, and the SOTA rows depend on research and
benchmark metadata that live in ARCH/MASTER rather than inline in PASS-3.

The fourth pathology class is matrix satisfaction.

Yaml onboarding, pointer/select, incremental parsing, and future-grammar
progression are represented as tables, but tables do not replace a short
worked example when a new worker has to implement or verify the path.

No evidence indicates that PASS-3 invented a new runtime architecture or
silently reintroduced OpenFrame, `path!`, declaration crates, or ParseStream
inside the target itself.

The surviving issues are amendable.

## §10 Verdict

Verdict: AMENDMENT-REQUIRED.

Reasons:

- PASS-3 remains directionally correct and does not need a rewrite.
- Several V5 carry rows still fail or need attention.
- The output lacks enough worked examples for unfamiliar grammar onboarding and
  runtime consumer verification.
- The wrong Lock 8 citation is a concrete provenance fault.
- Stale README prose keeps the retired ParseStream/rewrite/Unicode story alive
  outside PASS-3.

Readiness after amendment:

PASS-3 can return to READY after the punch list closes, provided the amendment
does not alter the settled tape substrate, path split, fixture separation, or
generated budget model.

## §11 Closing posture

PASS-3 should not be sent to final synthesis unchanged.

The next amendment should be narrow.

It should not reopen the runtime substrate, path crate split, future-grammar
source-surface rule, or generated API budget.

Recommended amendment shape:

1. Patch stale README prose and PASS-3 wrong lock citation.
2. Patch diagnostic vocabulary drift.
3. Add compact examples for yaml onboarding, pointer/select, incremental
   recovery, and one A->F->J grammar trajectory.
4. Fix the ARCH yaml table cell if the table parser confirms the host-route
   omission.
5. Re-run the 16-command gate list and the relevant line-target checks.

Estimated wall time for amendment:

- 20-30 minutes for stale prose and citation fixes.
- 30-45 minutes for diagnostic vocabulary and alias binding.
- 60-90 minutes for compact worked examples if they are written directly into
  PASS-3.
- 20-30 minutes for gate rerun and final punch-list verification.

Total likely amendment wall time: 2-3 hours.

The amendment should finish with PASS-3 READY if it stays scoped to the rows
above.

End-state checks for the future amendment:

- `README.md` no longer presents `ParseStream` as settled substrate.
- `README.md` no longer presents rewrite-mode as a live extension.
- `README.md` no longer bundles Unicode class algebra into the extension set.
- PASS-3 cites Lock 8 for SOTA evidence requirements.
- PASS-3 diagnostic imports use current crate vocabulary.
- PASS-3 binds lookbehind code, alias, and kind in one place.
- PASS-2 no longer suggests `@pratt` or `@simd` as user-forced directives.
- Yaml onboarding has a two-surface worked path.
- Pointer/select has a typed query worked path.
- Incremental recovery has a malformed-edit worked path.
- Debug and DAP identity is mandatory language.
- ARCH yaml row has the expected host-route cell.
- The A->F->J path has one named grammar and receiving gates.
- WASM host primitive claims stay routed to H.W3 until numbers are measured.
- The 16-command rerun has no FAIL rows.
- Any remaining ATTENTION rows name their owner and receiving gate.
