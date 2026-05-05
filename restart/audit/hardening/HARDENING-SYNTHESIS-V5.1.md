# HARDENING SYNTHESIS V5.1 - Verification Report

## §1 Target and commits verified

Verification worker: SYNTHESIS route.

Target amendment commit: `91af4882`
(`docs(restart/synthesis): wave-5.1 narrow amendment - formal grammar,
provenance, and examples`).

Target surfaces verified:

| Surface | Role in this verification |
|---|---|
| `restart/README.md` | Stale-positive README cleanup and settled gestalt check. |
| `restart/ARCHITECTURE.md` | Formal BBNF grammar, yaml onboarding, per-grammar table, host/WASM/API anchors. |
| `restart/MIGRATION.md` | Migration guards for ParseStream/OpenFrame/yaml/future-grammar routes. |
| `restart/MASTER-PLAN.md` | Bundle receivers, A->F->J trajectory, WASM ABI matrix, carry ledger. |

V5 authority read:

| Authority | Evidence used |
|---|---|
| V5 bundle map | `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:156-165` assigns bundles 1, 3, 4, 7, and 8 to the SYNTHESIS route. |
| V5 success criteria | `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:280-294` requires grammar reconciliation, stale prose cleanup, yaml row/walkthrough closure, WASM host route, and one grammar trajectory. |
| V5.1 route | `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:437-462` defines the SYNTHESIS verifier read targets and command checks. |
| Verify-then-patch discipline | `restart/prompts/AMENDMENT-DISPATCH.md:43-64` requires reading current state first and patching only missing deltas. |
| Voice/discipline anchor | `restart/README.md:452` requires path:line citations, no stale TBD without receivers, and no soft hedging. |
| Lock anchor | `restart/locks/14-LOCKS.md:34`, `:36`, `:48`, `:52`, and `:60` anchor tape/direct, layout, SOTA, auto Pratt/SIMD, and grammar generalisation. |

Amendment commit stat verified:

| Commit | Stat |
|---|---|
| `91af4882` | 4 files changed, 116 insertions, 68 deletions across `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`, and `restart/README.md`. |

Write-scope status:

| Check | Result |
|---|---|
| Pre-report `git status --short` | Clean. |
| Existing report path | `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md` was absent before this report. |
| Source-surface edits by verifier | None. |

## §2 Bundle closure table

| Bundle | Current evidence | Verdict | Rationale |
|---:|---|---|---|
| 1. Formal BBNF grammar reconciliation | `restart/ARCHITECTURE.md:1047-1050` binds the BBNF surface to README and PASS-1 §6 and names the settled extensions. | CLOSED | The grammar section now states lookbehind, block-bodied `@host fn`, multi-function chaining, generics, `@error(recover = ...)`, and `@layout`; it explicitly excludes rewrite-mode and grammar-level Unicode class algebra. |
| 1 | `restart/ARCHITECTURE.md:1055-1089` defines `HostFn ... Block`, `RuleDecl ... MapTail`, infix `Lookbehind ::= Expr "\|<" Expr | Expr "\|<!" Expr`, and `ChainExpr` over `->`. | CLOSED | The stale declaration-only `HostDecl`, `=> TypeExpr`, `MapExpr`, and prefix lookbehind forms are absent from the normative sketch. |
| 1 | `restart/ARCHITECTURE.md:1095-1101` scopes method-chain syntax to `HostFn` block bodies and states that bodyless host declarations have no production. | CLOSED | This closes the V5 concern that grammar-rule method chains and bodyless host declarations could be implemented as valid BBNF. |
| 1 | `restart/MASTER-PLAN.md:776` states that Architecture §8.1 matches PASS-1 §6 on block-bodied `@host fn`, infix lookbehind, and rule-level `->` chains. | CLOSED | The master carry ledger now has the reconciliation receiver/gate V5 required. |
| 1 | Required stale grammar grep returned zero matches for `HostDecl.*;`, `=> TypeExpr`, `MapExpr`, and `Lookbehind ::= "\|<" Suffix` across synthesis surfaces. | CLOSED | No normative stale grammar syntax remains in the assigned synthesis docs. |
| 3. Stale README/citation/provenance cleanup | `restart/README.md:473` now names the substrate as tape + direct-to-struct slice-borrow union and lists only lookbehind, generics, block-bodied `@host fn`, multi-function chaining, `@error`, and `@layout`; rich Unicode is routed through `parse-that/regex`. | CLOSED | The old positive closing-posture bundle of `ParseStream`, rewrite-mode, and grammar Unicode set algebra is gone. |
| 3 | `restart/README.md:383` frames OpenFrame and `Vec<OpenFrame>` only as prior-failure pathology under Lock 1. | CLOSED | OpenFrame appears as deletion archaeology, not a preserved substrate. |
| 3 | `restart/MIGRATION.md:196` confines `ParseStream` to proc-macro `syn::ParseStream` use, and `restart/MIGRATION.md:734-735` says runtime `ParseStream` must not exist. | CLOSED | The remaining ParseStream hits are classified as proc-macro exception or runtime deletion gate. |
| 3 | `restart/ARCHITECTURE.md:1038-1043` uses section-scoped PASS-2/PASS-3 diagnostic provenance instead of stale line spans. | CLOSED | The named stale PASS-2 line refs were removed from synthesis docs. |
| 3 | `restart/ARCHITECTURE.md:1247` cites SOTA baselines by corpus ranges, and `restart/MASTER-PLAN.md:125-150` records competitor, platform, input hash, compiler flags, and sample-policy metadata. | CLOSED | Benchmark provenance no longer relies on unsupported or ambiguous line references. |
| 3 | Required stale citation grep for `PASS-2.md:293-310`, `PASS-2.md:98-116`, `ARCHITECTURE.md:1273-1281`, and `14-LOCKS.md:69-72` returned zero. | CLOSED | The specific stale citation strings V5 named are absent from synthesis surfaces. |
| 4. YAML table and onboarding walkthrough | `restart/ARCHITECTURE.md:1287-1305` names `yaml.bbnf`, permits only grammar source plus Cargo metadata, and forbids Rust source, parser registry, path registry, host shim, and declaration-crate onboarding. | CLOSED | The two-surface onboarding rule is explicit before the walkthrough. |
| 4 | `restart/ARCHITECTURE.md:1320-1327` walks yaml from `grammars/yaml.bbnf` and `[workspace.metadata.bbnf.grammars.yaml]` to generated runtime files, path schema, diagnostics, host route, and bench manifest. | CLOSED | The requested author-facing proof now exists and keeps `fixtures/yaml/` out of onboarding authority. |
| 4 | `restart/ARCHITECTURE.md:1340-1351` contains the per-grammar authority matrix; the yaml row includes the host-route cell and declaration-crate status. | CLOSED | `awk -F'|' 'NR>=1340 && NR<=1351 { print NR, NF-1 }'` returned `11` for every table row. |
| 4 | `restart/ARCHITECTURE.md:1351` states yaml host work decomposes through `host::primitives` plus block-bodied `@host fn` chain and that no Rust per-grammar code emerges from onboarding. | CLOSED | This fixes the missing yaml host-route cell without adding a third source surface. |
| 4 | `restart/MASTER-PLAN.md:772` carries yaml onboarding to A/F/G/J with the gate "yaml source + workspace metadata plus generated runtime only." | CLOSED | The master carry ledger has a receiver, blocker, and gate for future-grammar onboarding. |
| 7. WASM host primitive route | `restart/MASTER-PLAN.md:477` keeps H.W3 latency values as `{N}` and `{M}` with owner, blocker, and measurement context. | CLOSED | No invented WASM performance number was introduced; TBD remains owned by H.W3. |
| 7 | `restart/MASTER-PLAN.md:481-489` adds the H.W3 receiving matrix: exported function names, host-call shape, marshalling rule, primitive coverage, and scalar/SIMD parity. | CLOSED | This is the ABI matrix V5 requested for WASM host primitives. |
| 7 | `restart/MASTER-PLAN.md:222` ties yaml host primitives to H.W3 only when metadata enables the WASM lowerer. | CLOSED | The route is conditional, grammar-derived, and not a hidden declaration-crate path. |
| 7 | `restart/MASTER-PLAN.md:781` carries WASM ABI to H/J and requires exported names, host-call shape, marshalling rule, primitive coverage, scalar/SIMD parity, and J.W3 dry-run publication. | CLOSED | The receiver ledger keeps the work actionable beyond H.W3. |
| 8. One grammar A->F->J trajectory | `restart/MASTER-PLAN.md:208-224` adds "YAML Grammar Trajectory: A->F->J." | CLOSED | The requested named grammar trajectory is present. |
| 8 | `restart/MASTER-PLAN.md:215-224` traces yaml through A metadata admission, B tape/direct readiness, C side tables, D settled syntax, E BIR, F generated runtime, G path, H WASM host route, I recovery/LSP, and J docs/publication close. | CLOSED | The trajectory covers more than the requested A/F/G/H/I/J subset and assigns concrete gates for each handoff. |
| 8 | `restart/MASTER-PLAN.md:220-222` binds F to `cargo xtask bbnf build yaml`, G to path schema macro smoke, and H to WASM ABI matrix when enabled. | CLOSED | The middle of the trajectory is executable and consumer-facing, not narrative only. |
| 8 | `restart/MASTER-PLAN.md:224` states yaml remains a future-grammar proof, not a seed-grammar budget member. | CLOSED | The trajectory does not overfit by promoting yaml into seed closure. |

Bundle-specific verification notes:

1. Bundle 1 closure rests on both syntax and receiving ledger evidence.
2. The grammar sketch at `restart/ARCHITECTURE.md:1059-1060` requires a block
   after `@host fn`; no semicolon production survives for host declarations.
3. The rule production at `restart/ARCHITECTURE.md:1061-1062` keeps rule-level
   mapping behind `MapTail`, not the retired `=> TypeExpr` form.
4. The lookbehind production at `restart/ARCHITECTURE.md:1077` is infix and
   covers both positive `|<` and negative `|<!`.
5. The chain production at `restart/ARCHITECTURE.md:1087-1088` matches the
   README rule-chain example at `restart/README.md:161-166`.
6. The explanatory note at `restart/ARCHITECTURE.md:1098-1101` prevents a future
   implementer from treating `a.f().g()` as grammar-rule syntax.
7. The deletion table at `restart/ARCHITECTURE.md:1107-1113` blocks rewrite-mode,
   BBNF Unicode algebra, regex-looking lookbehind, standalone `@recover`, and
   default declaration crates.
8. Bundle 3 closure rests on classifying remaining old vocabulary by context.
9. The README stale-positive scan returned zero for ParseStream, rewrite-mode,
   grammar Unicode class algebra, and typed-enum slice-borrow substrate text.
10. `restart/MASTER-PLAN.md:23-28` uses ParseStream only to say it is not the
    runtime term and points stale mentions back to PASS-3 resolution.
11. `restart/MIGRATION.md:724-735` preserves a runtime-substrate gate that would
    fail on OpenFrame clone stacks or runtime ParseStream.
12. `restart/ARCHITECTURE.md:1020` keeps lookbehind diagnostic provenance tied to
    `BBNF-LOOKBEHIND-WIDTH` and PASS-1 string `BBNF1004`.
13. Bundle 4 closure rests on two-source admission plus generated-only outputs.
14. `restart/README.md:13` is still the top-level two-surface rule for all
    grammars, with yaml named only as the tenth-grammar proof.
15. `restart/ARCHITECTURE.md:1291-1294` lists the only allowed yaml source
    changes: `grammars/yaml.bbnf` and the Cargo metadata block.
16. `restart/ARCHITECTURE.md:1296-1305` blocks Rust source, package declarations,
    registries, host shims, and unfenced declaration crates.
17. `restart/ARCHITECTURE.md:1317-1318` states the diff may show generated yaml
    runtime output only.
18. `restart/ARCHITECTURE.md:1327` keeps `fixtures/yaml/` out of onboarding by
    naming it as a later parity fixture.
19. Bundle 7 closure rests on separating ABI readiness from performance numbers.
20. `restart/MASTER-PLAN.md:477` still contains `TBD`, but it is paired with
    owner, blocker, fixture, browser/runtime metadata, and competitor baseline.
21. `restart/MASTER-PLAN.md:485-489` makes ABI rows prerequisites before latency
    or size acceptance.
22. `restart/MASTER-PLAN.md:488` routes missing WASM primitives to `BBNF-HOST003`
    rather than to hand-authored host shims.
23. Bundle 8 closure rests on yaml staying a proof path through general machinery.
24. `restart/MASTER-PLAN.md:215` starts at metadata validation, not a Rust crate.
25. `restart/MASTER-PLAN.md:220` makes F emit runtime, path schema, diagnostics,
    visitor metadata, host route, and budget sidecars from grammar plus metadata.
26. `restart/MASTER-PLAN.md:221` makes G consume the generated path schema through
    `pointer!` and `select!`.
27. `restart/MASTER-PLAN.md:223` makes I consume yaml diagnostics through
    `DocumentSnapshot` and `ReparsePlan`, preserving batch/LSP parity.
28. `restart/MASTER-PLAN.md:224` makes J record the two-surface proof instead of
    promoting yaml into seed-budget closure.

## §3 Pathology regression scan

| Row | Lens | Spot-check | Evidence | Verdict |
|---:|---|---|---|---|
| 1 | F - LLM bias | Formal grammar no longer hides a contradiction behind confident prose. | `restart/ARCHITECTURE.md:1059-1062`, `:1077`, `:1087-1089`, and `:1095-1101`. | CLEAN. Normative syntax now agrees with PASS-1-bound prose. |
| 2 | F - LLM bias | WASM route does not replace unknown numbers with invented precision. | `restart/MASTER-PLAN.md:477` records `{N}`/`{M}` as H.W3 measurements with owner and blocker; `:481-489` records ABI evidence that must exist before acceptance. | CLEAN. The remaining TBD is routed, not asserted as a fact. |
| 3 | F - LLM bias | Benchmark and generated-LOC precision has metadata backing. | `restart/MASTER-PLAN.md:125-150` records benchmark reproducibility fields; `restart/ARCHITECTURE.md:1273-1280` separates seed, yaml, and WASM/SIMD LOC accounting. | CLEAN. Precision rows name provenance and required metadata. |
| 4 | G - overfitting | YAML is a proof grammar, not a hand-coded special case. | `restart/ARCHITECTURE.md:1289-1305`, `:1324-1327`, `:1351`; `restart/MASTER-PLAN.md:224`. | CLEAN. The yaml path admits two source surfaces and generated derivatives only. |
| 5 | G - overfitting | Pratt/SIMD do not become author directives. | `restart/README.md:182`, `restart/README.md:392`, and `restart/MASTER-PLAN.md:204` are prohibition-only or forbidden-output contexts. | CLEAN. The `@pratt`/`@simd` hits are not positive grammar syntax. |
| 6 | G - overfitting | Path query examples use grammar-qualified macros instead of reviving a generic `path!`. | `restart/ARCHITECTURE.md:271-272` and `restart/MASTER-PLAN.md:792`. | CLEAN. The synthesis route teaches `pointer!(Bbnf => ...)` and `select!(Bbnf => ...)`; no `path!` hit appears in synthesis surfaces. |
| 7 | H - hallucination/provenance | Stale hard-coded line references named by V5 are gone from synthesis docs. | Required stale citation grep returned zero matches for the named PASS-2, Architecture, and Lock 14 ranges. | CLEAN. Remaining citations are section-scoped or current line anchors. |
| 8 | H - hallucination/provenance | README no longer asserts stale positive surfaces. | `rg -n "ParseStream|rewrite-mode|Unicode class algebra|typed-enum \+ slice-borrow" restart/README.md` returned zero. | CLEAN. Unicode survives only as regex-routed prose at `restart/README.md:143` and `:473`. |
| 9 | H - hallucination/provenance | ParseStream/OpenFrame hits in the synthesis scan are deletion or exception contexts. | `restart/MIGRATION.md:196`, `:727-735`; `restart/MASTER-PLAN.md:23-28`, `:288-291`; `restart/ARCHITECTURE.md:21-23`. | CLEAN. No hit presents ParseStream or OpenFrame as a new runtime substrate. |
| 10 | H - hallucination/provenance | Lookbehind diagnostic naming is not contradicted by the synthesis docs. | `restart/ARCHITECTURE.md:1020` records `BBNF-LOOKBEHIND-WIDTH` with PASS-1 string `BBNF1004`; `restart/ARCHITECTURE.md:1111` names the same diagnostic for `|<`. | CLEAN. PASS-local alias polish remains outside this SYNTHESIS route. |

Regression classifications:

| Class | Classification |
|---|---|
| Settled syntax hits | `@host fn`, `pointer!`, `select!`, `LayoutFacts`, `passes::layout`, WASM, and yaml hits are expected settled-surface references. |
| Deletion/prohibition hits | `@pratt`, `@simd`, `OpenFrame`, and runtime `ParseStream` hits are either forbidden-output rows, deletion gates, or migration archaeology. |
| Routed unknowns | `TBD` appears in the H.W3 row only as measured-value placeholders with owner, blocker, and fixture metadata. |
| Fixture mentions | `fixtures/yaml` appears only as a post-onboarding parity fixture, not as source authority. |
| Path macro drift | No synthesis `path!` hit remains; grammar-qualified `pointer!` and `select!` examples are current. |
| Citation drift | The exact stale ranges named by V5 are absent from synthesis surfaces. |

Spot-check conclusion:

| Lens | Conclusion |
|---|---|
| F | The amendment does not substitute plausible prose for formal contracts; formal grammar, WASM measurement ownership, and benchmark metadata are explicit. |
| G | The amendment does not special-case yaml into generic crates; yaml is a proof grammar admitted by the same source and metadata surfaces as future grammars. |
| H | The amendment does not preserve stale citations as authority; remaining old terms are deletion, exception, or archaeology context. |

## §4 Gate rerun

| Command | Summarized result |
|---|---|
| `git status --short` | Clean before creating this report. |
| `git show --stat --oneline 91af4882` | `91af4882 docs(restart/synthesis): wave-5.1 narrow amendment - formal grammar, provenance, and examples`; 4 files changed, 116 insertions, 68 deletions. |
| `rg -n "path!\|@pratt\|@simd\|OpenFrame\|LayoutFacts\|LayoutSink\|passes::layout\|pointer!\|select!\|LookbehindWidth\|BBNF-LOOKBEHIND-WIDTH\|BBNF1004\|@host fn\|waves-v4\|wave-4\|Wave 4\|ParseStream\|WASM\|yaml\|benchmark\|TBD\|A -> F -> J\|A->F->J" restart/README.md restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md` | Nonzero by design. Hits classify as settled syntax (`@host fn`, `pointer!`, `select!`, `LayoutFacts`, `passes::layout`, WASM, yaml), prohibition/deletion context (`@pratt`, `@simd`, `OpenFrame`, `ParseStream`), or routed measurement context (`TBD` at H.W3). No unclassified stale positive hit found. |
| `rg -n "HostDecl.*;\|=> TypeExpr\|MapExpr\|Lookbehind    ::= \"\\\|<\" Suffix\|@host fn declarations\|PASS-2.md:293-310\|PASS-2.md:98-116\|ARCHITECTURE.md:1273-1281\|14-LOCKS.md:69-72" restart/README.md restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md` | Zero matches. Command exited with no stale synthesis hit. |
| `git diff --check` | Clean before this report. |
| `awk -F'\|' 'NR>=1340 && NR<=1351 { print NR, NF-1 }' restart/ARCHITECTURE.md` | Every header/body row in the authority matrix returned `11` pipe-delimited cells. |
| `rg -n "@pratt\|@simd" restart/README.md restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md` | Three hits, all prohibition/forbidden-output contexts: `restart/README.md:182`, `restart/README.md:392`, `restart/MASTER-PLAN.md:204`. |
| `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra\|typed-enum \+ slice-borrow\|rewrite-mode \+ lookbehind \+ Unicode sets" restart/README.md` | Zero matches. README stale-positive cleanup is closed. |
| `rg -n "WASM host primitive\|host primitive ABI\|Exported function names\|Host-call shape\|Marshalling rule\|Primitive coverage\|Scalar/SIMD parity" restart/MASTER-PLAN.md restart/ARCHITECTURE.md restart/MIGRATION.md` | Hits at `restart/MASTER-PLAN.md:222`, `:481`, `:485-489`; H.W3 ABI matrix is present. |
| `rg -n "A->F->J\|A -> F -> J\|A to F to J\|A->F->J\|grammar trajectory\|YAML Grammar Trajectory\|yaml trajectory\|json trajectory" restart/MASTER-PLAN.md restart/ARCHITECTURE.md` | Hit at `restart/MASTER-PLAN.md:208`; trajectory rows continue through `:215-224`. |
| `rg -n "fixtures/yaml\|grammars/yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml\|manual Rust registry\|manual path registry\|manual host shim\|declaration-crate onboarding\|generated_loc <= 4,000\|yaml.path-schema" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md restart/README.md` | Hits prove the two source surfaces, generated outputs, manual-registry rejection, and parity-only fixture classification. |
| `rg -n "pointer!\(.*=>\|select!\(.*=>\|BBNF-POINTER\|path!" restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md restart/README.md` | Canonical grammar-qualified macro examples and pointer diagnostics found; no `path!` hit found. |

Commands not run:

| Command family | Reason |
|---|---|
| Cargo build/test/bench gates | This verification route is documentary and the prompt requires report-only output. The assigned minimum commands and focused text gates were run. |

Gate interpretation:

| Gate | Interpretation |
|---|---|
| Required command set | All minimum commands from the dispatch prompt were run. |
| Stale grammar command | Zero stale grammar/citation matches means bundle 1 and the bundle 3 named stale refs close on text evidence. |
| Broad synthesis scan | Nonzero broad scan is expected because it includes valid current terms; every high-risk token was classified. |
| YAML cell count | The current authoritative table lives at `restart/ARCHITECTURE.md:1340-1351`; all rows report the same cell count. |
| Diff hygiene | `git diff --check` was clean before this report; a final clean check is required before commit. |
| Dirty-worktree hygiene | A separate untracked PASS-3 V5.1 report appeared after this worker began; it is unrelated and must remain unstaged by this route. |

## §5 Residue ledger

none

## §6 Final verdict

READY.

All assigned SYNTHESIS bundles are closed by current synthesis-surface evidence.
No assigned bundle remains partial or open.
No new F/G/H LLM-pathology drift was found in the amended synthesis text.

## §7 Closing posture

This V5.1 worker verified the SYNTHESIS amendment commit only.
No synthesis source file was amended.
No prior hardening report, prompt, lock, research catalogue, crate, or external
document was changed.
The only intended write is this report:
`restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md`.
