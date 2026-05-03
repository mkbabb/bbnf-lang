# HARDENING 2026-05-03 03 — Spec-Drift

**Lane**: 03, tranche drift.
**Scope**: `docs/tranches/{BA,BB,BC}/{<LETTER>.md,waves/W0..W6.md}` plus cross-refs to `docs/GESTALT.md`, `docs/codegen-paths.md`, `docs/tranches/AZ-IV/{FINAL,PROGRESS}.md`, and AZ-IV cohort syntheses.
**Read-of-record HEAD**: `baf7df2d07cd130a5ad2b8f81fc339418406a3b3` (working tree dirty before this lane; unrelated dirty paths left untouched).
**Method**: `docs/HARDENING-AUDIT-PROMPT.md` §Spec-Drift and `docs/tranches/meta-audit/03-tranche-drift.md`.

## As-Read Status

| Tranche | Top-level status | Waves | Hard gates | Immediate drift risk |
|---|---|---:|---:|---|
| BA | planned; opens after AZ-IV close + hardening audit | W0..W6 planned | 24 | F4 route, TS/WASM route, W6.2 fixture ownership, stale cross-refs |
| BB | planned; opens after BA close | W0..W6 planned | 18 | W0 skeletons without same-wave consumer; `crates/core` rewrite module contradicts storage invariant; schema ownership drift |
| BC | planned; opens after BB close | W0..W6 planned | 15 | AUDIT-B `dta`/`grammar_facts` split dropped; fixture ownership overlaps BA.W0 |

The wave-status words themselves match the top-level wave tables: every audited W0..W6 file says `planned`, and each top-level wave table says `planned`. The material drift is in carry routing, owner-wave assignment, cross-reference truth, and substrate-with-consumer discipline.

## Findings

### D1. BA still allows TS/WASM work by implication while declaring TS/WASM out of scope.

Evidence:

- `docs/tranches/BA/BA.md:43` says BA nextest closure includes `ts_node_execute`, either "via direct-projection's TS aggregate emit" or `#[ignore]` with a named successor.
- `docs/tranches/BA/BA.md:54` says F5 routes to BD "OR closes here if direct-projection's TS aggregate emit naturally fixes" it.
- `docs/tranches/BA/BA.md:176` forbids "TS or WASM emitter regression compensation".
- `docs/tranches/BA/BA.md:191-199` quotes the TS/WASM punt and says BA scopes to Rust only; the decision is deferred to post-BC / BD.
- `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md:88-94` says TS and WASM are not load-bearing for direct-projection and route to a separate post-BC tranche.

Drift: the same BA plan simultaneously says TS/WASM are out of BA scope and that BA may close the RED Node-execute gate by TS aggregate emit. That is enough ambiguity for an implementation dispatcher to touch the TS emitter under BA authority.

Fix proposal: make BA's only BA-owned action for F5 be route hygiene: mark/keep the test non-blocking with a named BD successor and no TS/WASM source edits. Replace the BA lines that imply TS aggregate emit with the following.

```markdown
### Amendment: `docs/tranches/BA/BA.md`

Replace Invariant 9 with:

9. **Failing-test census is canonical.** Workspace nextest is 100% pass at every BA wave close for BA-owned Rust surfaces. AZ-IV's RED `substrate_audit` test closes in BA.W0. AZ-IV's RED `ts_node_execute` is not BA-owned; BA close records it as `#[ignore]` or equivalent non-blocking status with owner `BD` / TS-WASM re-engineering, reason, and deadline, and does not edit TS or WASM emitters.

Replace the F5 carry row with:

| F5 TS Node-execute | Audit-C MASKED-DEFERRAL | routes to BD (TS/WASM) per user punt | BA close names BD with owner/reason/deadline; BA does not edit TS or WASM emitter code |

Replace Hard Gate 2 with:

2. `cargo nextest run --workspace --cargo-profile ax-iter` 100% pass at BA close for BA-owned surfaces. The AZ-IV `substrate_audit` RED test closes via W0 cleanup. The AZ-IV `ts_node_execute` RED test is recorded as a named BD route with explicit non-blocking status; BA does not claim it closes by mechanism.
```

### D2. F4 Tailwind `regex_scan` routes to BB.W1 in BA.W2, but BB owns it in BB.W3.

Evidence:

- `docs/tranches/BA/BA.md:52` leaves F4 owner as "W2 OR W3" and says it may route to BB rule-discovery.
- `docs/tranches/BA/waves/W2.md:107`, `:121`, and `:138` say an unresolved F4 routes to `BB.W1`.
- `docs/tranches/BA/waves/W2.md:31` says routing failure is a BB.W1 file-bounds issue.
- `docs/tranches/BB/BB.md:52` assigns F4 to `W3 (grammar-specific rule discovery)`.
- `docs/tranches/BB/waves/W3.md:100` explicitly gates F4 only if BA.W2 routed it there.
- `docs/tranches/AZ-IV/FINAL.md:63` routes the timeout to BB rule-discovery because CSS-wide alphabet enumeration owns timeout-class regex normalization.

Drift: BA.W2 points to BB.W1, but BB.W1 is the enumerator substrate. The BB plan's actual F4 closure is W3, after oracle/ranker context exists. A dispatcher following BA.W2 would expand BB.W1 file bounds unnecessarily.

Fix proposal: make BA.W2 record a decision only; unresolved F4 routes to BB.W3. Keep `merge_path_seed` as the BB.W1 seed-bag item.

```markdown
### Amendment: `docs/tranches/BA/BA.md`

Replace the F4 carry row with:

| F4 Tailwind regex_scan timeout | Audit-C CHRONIC-RISK | W2 disposition; closes in W2 only with profile evidence, otherwise routes to BB.W3 | direct-projection profile either proves the regex_scan path resolves here OR records a routed BB.W3 grammar-specific rule-discovery close criterion |
```

```markdown
### Amendment: `docs/tranches/BA/waves/W2.md`

Replace every `BB.W1` F4 destination with `BB.W3`.

Replace the F4 scope item with:

9. F4 Tailwind regex_scan: profile under direct-projection. If the per-call layout-construction overhead is removed by mechanism, F4 closes here. If not, F4 routes to BB.W3 as a grammar-specific rule-discovery candidate, with the exact path-shape rewrite hypothesis and CSS L4 close-matrix row named in the W2 close commit body.

Replace the triumvirate trigger with:

- F4 Tailwind regex_scan does not resolve under direct-projection AND the routing to BB.W3 rule-discovery cannot be made without changing BB.W3 file bounds (cross-letter scope reveal);

Replace the Hard Gate row with:

9. F4 Tailwind regex_scan disposition documented: closes here OR routes to BB.W3 (named decision in W2 close commit body).
```

### D3. BA.W0 and BC.W2 both claim closure of the same W6.2 worktree-fixture miss.

Evidence:

- `docs/tranches/AZ-IV/audit/SYNOPSIS-2026-05-03.md:130-131` says the W6.2 timeouts resolve with fixture symlink codification as a `BC.W2 carry`.
- `docs/tranches/BA/BA.md:73` assigns a worktree fixture contract to BA.W0 for `data/{json,css,bbnf,sheets}`.
- `docs/tranches/BA/BA.md:156` says this W0 item closes the W6.2 known miss.
- `docs/tranches/BA/waves/W0.md:16`, `:102`, and `:114` say BA.W0 materializes four fixture trees and closes the W6.2 known miss.
- `docs/tranches/BC/BC.md:17`, `:86`, and `:113` say BC.W2 fully closes the W6.2 known miss for the fleet.
- `docs/tranches/BC/waves/W2.md:6` and `:15` say W6.2 fully closes in BC.W2 after BB's `rewrites/*.ron` files exist.

Drift: the current text double-closes the same named miss. The actual split appears coherent: BA.W0 creates the local `xtask worktree-init` substrate for four data fixture trees; BC.W2 extends it after BB introduces per-grammar rewrites and closes the fleet-wide miss. The docs need to say that.

Fix proposal: downgrade BA.W0 from "closes W6.2 known miss" to "installs BA-local prerequisite"; leave full fleet closure to BC.W2.

```markdown
### Amendment: `docs/tranches/BA/BA.md`

Replace Non-Routable Carry 5 closure proof with:

`data/{json,css,bbnf,sheets}` materializes on worktree open via `xtask worktree-init` or equivalent; BA.W0 installs the local contract and explicitly routes the post-BB fleet-wide `rewrites/*.ron` extension to BC.W2.

Replace Hard Gate 22 with:

22. **Worktree fixture symlink contract codified for BA-local fixtures**: `data/{json,css,bbnf,sheets}` materializes on worktree open via `xtask worktree-init`; the fleet-wide post-BB `rewrites/*.ron` closure remains BC.W2.
```

```markdown
### Amendment: `docs/tranches/BA/waves/W0.md`

Replace Scope item 6 with:

6. Worktree fixture symlink contract: codify `data/{json,css,bbnf,sheets}` materialization on worktree open via `xtask worktree-init` or equivalent. This installs the BA-local prerequisite for the W6.2 known miss; the fleet-wide post-BB closure remains BC.W2 after `grammar/<name>/rewrites/*.ron` exists.

Replace Hard Gate 9 with:

9. `cargo xtask worktree-init` materializes the four BA-local fixture trees; named test passes; W0 close routes the fleet-wide post-BB fixture extension to BC.W2.
```

### D4. BB.W0 creates rank/tier skeletons with deferred W3 consumers, contradicting BB's substrate-with-consumer invariant.

Evidence:

- `docs/tranches/BB/BB.md:18` says every BB substrate is consumed in the same wave it lands.
- `docs/tranches/BB/BB.md:31` says every new BB substrate passes substrate-audit GREEN at every wave close.
- `docs/tranches/BB/BB.md:61` requires every substrate to have a wave-bound consumer.
- `docs/tranches/BB/waves/W0.md:13` creates `rank.rs` and `tiering.rs` skeletons whose implementations land in W3.
- `docs/tranches/BB/waves/W0.md:80-82` says the skeletons compile, produce stub results, and are allowed through W0 by a whitelist entry because consumers land in W3.
- `docs/tranches/BB/waves/W3.md:34-35` assumes W0 already placed the skeletons.

Drift: this is the exact substrate-first / consumer-forward pattern the plan forbids. A whitelist keyed to a W3-deferred consumer makes BB.W0 close on "consumer later" scaffolding.

Fix proposal: move rank/tiering file creation to W3, or make W0 consume them with a production caller. The lower-risk amendment is to move them to W3 and keep W0 focused on module root, schema, base rules, and cost extractor.

```markdown
### Amendment: `docs/tranches/BB/waves/W0.md`

In Scope item 3, replace:

`rank.rs` and `tiering.rs` (skeleton — implementations land in W3)

with:

rank/tiering are not created in W0; W3 creates and consumes them in the same wave.

Delete Scope item W0.3 "Rank/Tier Skeletons" and remove `crates/ir/src/rewrites/rank.rs`, `crates/ir/src/rewrites/tiering.rs`, `ir/rewrites/rank-skeleton`, and `ir/rewrites/tiering-skeleton` from File Bounds, Disjointness, Worktree Plan, Agent Units, Hard Gate 1, and Commit Plan.

Replace Hard Gate 1 with:

1. `crates/ir/src/rewrites/{mod,schema}.rs` exist and are functional; base RON rules validate; no W3-deferred rank/tier skeleton exists.
```

```markdown
### Amendment: `docs/tranches/BB/waves/W3.md`

Replace File Bounds rows for rank/tiering with:

| `crates/ir/src/rewrites/rank.rs` | create |
| `crates/ir/src/rewrites/tiering.rs` | create |

Replace Hard Gate 1 with:

1. `crates/ir/src/rewrites/rank.rs` and `tiering.rs` are created, implemented, and consumed by the W3 run in the same wave.
```

```markdown
### Amendment: `docs/tranches/BB/BB.md`

Replace the W0 wave-table close text with:

regen drift cleared; cost extractor live; `crates/ir/src/rewrites/` recreated clean with schema + base RON rules; substrate_audit GREEN with no W3-deferred rank/tier skeletons
```

### D5. BB says rewrite storage lives outside `crates/core`, but W4 creates `crates/core/src/rewrites/mod.rs`.

Evidence:

- `docs/tranches/BB/BB.md:9` says rules live outside `crates/core`: fleet-wide rules in `crates/ir/src/rewrites/`, grammar-specific rules under `grammar/<name>/rewrites/*.ron`.
- `docs/tranches/BB/BB.md:27` says `crates/core` never accumulates a hand-curated rule list.
- `docs/tranches/BB/BB.md:100` assigns W4 `crates/core/src/rewrites/mod.rs` for IR-pipeline scan + compile.
- `docs/tranches/BB/waves/W4.md:37`, `:48`, `:64-65`, and `:117` create and commit `crates/core/src/rewrites/mod.rs`.

Drift: even if `crates/core/src/rewrites/mod.rs` is "scan + compile" rather than a hand-authored list, it violates the stated storage boundary and invites a second rewrite subsystem in core. The existing thesis has a natural home: `crates/ir/src/rewrites/compile.rs` or an `xtask` integration module.

Fix proposal: keep rule compile/scan in `crates/ir/src/rewrites/` or `xtask`, and let core consume compiled IR facts through the existing IR pipeline.

```markdown
### Amendment: `docs/tranches/BB/BB.md`

Replace Critical Files row "Grammar-colocated rewrites" with:

| Grammar-colocated rewrites | W4 | `grammar/<name>/rewrites/*.ron`, `xtask/src/regen.rs` (modify-carve), `crates/ir/src/rewrites/{compile,mod}.rs` (IR-pipeline scan + compile) |
```

```markdown
### Amendment: `docs/tranches/BB/waves/W4.md`

Replace every `crates/core/src/rewrites/mod.rs` reference with:

`crates/ir/src/rewrites/compile.rs`

Replace BB.W4.1 Files with:

- Files: `xtask/src/regen.rs` (modify-carve), `crates/ir/src/rewrites/compile.rs` (create), `crates/ir/src/rewrites/mod.rs` (modify-carve registration).

Replace the commit scope `core/rewrites/mod` with:

`ir/rewrites/compile`
```

### D6. BB schema ownership is split between W0 and W4.

Evidence:

- `docs/tranches/BB/BB.md:61` says `crates/ir/src/rewrites/` recreated clean in W0 includes schema validation.
- `docs/tranches/BB/BB.md:67` says the "RON rule-file schema validator" owner wave is W4.
- `docs/tranches/BB/BB.md:84` W0 closes on schema validator.
- `docs/tranches/BB/BB.md:88` W4 closes on "`grammar/<name>/rewrites/*.ron` schema".
- `docs/tranches/BB/waves/W0.md:13`, `:68-70`, and `:98-99` create a functional schema validator in W0.
- `docs/tranches/BB/waves/W4.md:11` says W4 codifies the per-grammar schema but the validator is W0's.

Drift: there are two plausible readings: W0 owns the generic Rule schema; W4 owns per-grammar RON population and optional schema extension. The top-level non-routable row currently gives W4 ownership of the validator itself, contradicting W0.

Fix proposal: split "generic schema validator" from "per-grammar RON admission".

```markdown
### Amendment: `docs/tranches/BB/BB.md`

Replace Non-Routable Carry 7 with:

| 7 | Generic RON rule-file schema validator | W0 | `crates/ir/src/rewrites/schema.rs`; base RON files reject malformed input with file/line diagnostics |

Replace Non-Routable Carry 8 with:

| 8 | Grammar-colocated `grammar/<name>/rewrites/*.ron` admission | W4 | `cargo xtask regen` discovers, validates with the W0 schema, and compiles per-grammar rule files without per-grammar code edits; named test |

Replace the W4 wave-table close text with:

`grammar/<name>/rewrites/*.ron` admitted through the W0 schema; `cargo xtask regen` integration; generated `.rs` shrinks ≥ 10 LOC for one grammar
```

### D7. BC drops the AUDIT-B `dta.rs` / `grammar_facts` split from the cleanup carry ledger.

Evidence:

- `docs/tranches/AZ-IV/audit/AUDIT-B-arch-2026-05-02.md:15` lists `crates/ir/src/passes/recognizers/dta.rs` at 1565 LOC as SPLITTABLE.
- `docs/tranches/AZ-IV/audit/AUDIT-B-arch-2026-05-02.md:47-50` routes four deferred splits: `csp_strategy`, `dta`, `css_l4/builder`, `types/mod`.
- `docs/tranches/AZ-IV/audit/AUDIT-B-arch-2026-05-02.md:215-218` records R1 `dta.rs`, R2 `csp_strategy`, R3 `css_l4/builder`, R4 `types/mod`.
- `docs/tranches/AZ-IV/PROGRESS.md:64` says `dta.rs` was renamed to `grammar_facts`, but `AUDIT-B dta.rs + csp_strategy splits deferred`.
- `docs/tranches/AZ-IV/PROGRESS.md:66` repeats W4 AUDIT-B `dta.rs` + `csp_strategy/mod.rs` splits as follow-on.
- `docs/tranches/BC/BC.md:9`, `:16`, `:48-50`, `:64-66`, and `:84-85` name only `css_l4/builder`, `passes/types/mod.rs`, and `csp_strategy/mod.rs`.
- `docs/tranches/BC/waves/W0.md:13` and `:73` verify LOC for the same three files only.

Drift: one AUDIT-B routed split disappears between the predecessor audit and BC. The rename from `dta.rs` to `grammar_facts` may mean it closed by rename/split, but the BC plan does not prove that. As written, the cleanup ledger is not exhaustive.

Fix proposal: add a W0 disposition row for the renamed `dta`/`grammar_facts` split. W1 owns it only if W0 verifies it remains over budget and still splittable.

```markdown
### Amendment: `docs/tranches/BC/BC.md`

Add this Carry Ledger row after the `csp_strategy/mod.rs` row:

| AUDIT-B `recognizers/dta.rs` / renamed `grammar_facts` split | AUDIT-B R1 + AZ-IV.W4 rename | W0 disposition; W1 if still over budget | W0 verifies current path, LOC, and whether AZ-IV rename/split closed it; if still splittable and >500 LOC, W1 adds the directory-module split |

Add this Non-Routable Carry row and renumber the following rows:

| 4 | `recognizers/dta.rs` / `grammar_facts` split disposition | W0; W1 if still open | W0 audit proves closed-by-AZ-IV rename/split OR W1 lands directory-module split with each sub-module ≤ 500 LOC |

Update the W0 wave-table close text to include:

AUDIT-B target file LOC verified, including `recognizers/dta.rs` / renamed `grammar_facts`
```

```markdown
### Amendment: `docs/tranches/BC/waves/W0.md`

Replace Scope item 3 with:

3. Verify AUDIT-B target file LOC: `crates/core/src/runtime/css_l4/builder.rs` (1014 LOC at AUDIT-B time; verify current LOC); `crates/ir/src/passes/types/mod.rs` (786 LOC); `crates/ir/src/passes/csp_strategy/mod.rs`; and `crates/ir/src/passes/recognizers/dta.rs` or its AZ-IV-renamed `grammar_facts` successor. The W1 split decisions need fresh LOC numbers and a closed-by-rename proof if `dta` no longer exists.
```

### D8. GESTALT and codegen-paths still describe AZ-IV's two-entry parse path as current canon, while BA makes one parse path the audited mechanism.

Evidence:

- `docs/GESTALT.md:35-42` describes two parse modes after AZ-IV.W3 and says entry-point dispatch is the only divergence.
- `docs/GESTALT.md:79-80` says eager and lazy share generated parse functions but still lists eager and lazy as two modes; it also says per-grammar arena/builder pairs are generated from a shared template.
- `docs/GESTALT.md:90` says the sonic-rs lazy gap was closed at AZ-IV.W3 to ≤ 5x.
- `docs/tranches/AZ-IV/FINAL.md:43-44` records the actual misses: `bbnf_get_twitter` is 4196x over sonic and the AU floor is 18/19 BELOW.
- `docs/tranches/BA/BA.md:35-38` and `:82-84` make one parse path, eager collapse, `Document::get<T>` through `parse_with`, and `__EAGER_EMPTY_PATH` deletion BA hard gates.
- `docs/codegen-paths.md:96-105` says current status is "post AZ-III terminal close, AZ-IV planned" and still lists `parse` plus `parse_with` as generated modes.
- `docs/codegen-paths.md:120-129` still describes generated per-grammar builders/documents for StructDirect rather than BA's direct-projection document/value enum endpoint.

Drift: both cross-reference docs are canonical inputs to BA.W0, but they currently state an AZ-IV intermediate state as if it were the project end-state. That conflicts with BA's one-path mechanism and overstates the performance closure.

Fix proposal: make the cross-reference text explicitly transitional until BA.W4/W6. BA.W0 already owns `GESTALT.md` and `codegen-paths.md`; the amendment should be applied before BA.W0 readiness is declared.

```markdown
### Amendment: `docs/GESTALT.md`

Replace §2 "Two parse modes, one parser" with:

### Transitional parse state after AZ-IV; BA closes one path

AZ-IV.W3 landed `parse_with(input, &path)` and proved cursor-threaded lazy bail-out on the primary grammars, but AZ-IV did not collapse the value API onto that lane. Until BA.W4 closes, eager `parse(input)` and lazy `parse_with(input, &path)` remain separate entry points over shared generated functions. BA.W4 is the owner of the one-path invariant: eager becomes `parse_with(input, &EMPTY_PATH)`, `Document::get<T>(path)` routes through `parse_with`, and `__EAGER_EMPTY_PATH` retires.

Replace the `2953x sonic-rs lazy gap` bullet with:

- **sonic-rs get gap still open after AZ-IV** — `bbnf_get_twitter` measured 4196x over `sonic_get_twitter` in `post-AZ-IV.json`; BA.W4 closes the value-API route through `parse_with`, and BA.W6 records the same-harness close evidence.

Replace the arena/builder bullet in §4 with:

7. **Transitional per-grammar arena/builder pairs** generated from AZ-IV's shared template. BA.W2 retires the runtime arena/builder template hot path by emitting per-grammar `<Grammar>Document` structs and `<Grammar>Value` enums directly from `StructRegistry`; BA.W3 replaces deep-clone checkpoints with value-typed snapshots.
```

```markdown
### Amendment: `docs/codegen-paths.md`

Replace "Current strategy status" and the parse-mode bullets with:

Current strategy status (post-AZ-IV close, pre-BA execution):

AZ-IV left every generated parser with eager `parse(input)` and lazy `parse_with(input, &path)` entry points sharing generated parse functions. This is a transitional state, not the final invariant. BA.W4 owns the collapse: eager becomes `parse_with(input, &EMPTY_PATH)`, the value API's `Document::get<T>(path)` routes through `parse_with`, and generated `__EAGER_EMPTY_PATH` literals retire.

Replace "Generated code per grammar" bullets with:

- Transitional AZ-IV output: per-grammar document + builder modules for StructDirect.
- BA.W2 output: generated `<Grammar>Document<'p>` typed structs and `<Grammar>Value<'p>` typed enums emitted from `StructRegistry`; runtime `arena_template.rs`, `builder_template.rs`, and subsumed per-grammar arena/builder files retire.
- BA.W3 output: generated parse functions use value-typed checkpoints and predictive first-byte dispatch where alphabets are disjoint.
- BA.W4 output: `parse(input)` is the empty-path specialization of `parse_with`.
```

## Cross-Cutting Themes

1. **Owner-wave precision is close but not yet dispatch-safe.** F4, schema validation, fixture closure, and AUDIT-B splits all have a plausible owner, but the owner is named differently in adjacent docs.
2. **Substrate-with-consumer needs one BB surgery before W0.** The rank/tier skeleton whitelist is the only audited instance that directly violates the same-wave consumer rule.
3. **Cross-reference docs overstate AZ-IV closure.** GESTALT/codegen-paths still read like AZ-IV completed the one-path and sonic-class value API work; BA is explicitly the tranche that closes that work.

## Readiness Decision

BA.W0 is **not ready** until D1, D2, D3, and D8 are amended. BB.W0 is **not ready later** until D4-D6 are amended. BC.W0 is **not ready later** until D7 is amended.
