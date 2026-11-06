MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R4 author.

# BB-C.W0 ambient optimizer authority — R4 author freeze

- Frozen: `2026-07-22T20:46:47Z`
- Repository HEAD: `af15f63e0d2d3d719938c13b906a50acbb92ea3b`
- Host route observed by the native RED receipt: Apple `arm64`
- Scope: reopened BB-C.W0 item 3 plus the valid grouped-map lowerer defect
  blocking its required product witness
- Execution status: **REPAIRED STATIC FREEZE; BORN-RED PREIMAGES FROZEN;
  NO POST-REPAIR TEST OR BUILD RUN**
- Hostile clean count: **0/2**
- Credit: **zero BB-C.W0, wave, lock, tranche, or movement credit**
- Advancement: **BB-C.W1 may not begin**

No Cargo command, build, test executable, benchmark, mutation execution, x86
route, package/lock/config edit, other repository, commit, or staging action was
used in the repair/static-freeze stage. The root-owned Stage-A native campaign
ran each of the three authorized born-RED commands exactly once; those raw
preimages are retained below. This author record does not claim GREEN or
acceptance.

## Governing frozen authority

| Record | SHA-256 |
|---|---|
| `waves/BB-C/W0.md` | `00bb99d6b3c379c7c15ad7a70ab7598f8b883f9cd8d3974733323bdbc6026df2` |
| `W0-AMBIENT-R2-ADJUDICATION.md` | `ef7c8c1db203eb42ae40358e35fd2710d4e79d3cabc5495801c619b18995b9f7` |
| `W0-AMBIENT-R3-AUTHOR.md` | `8a327e3da122dd253494f8512e2ca6504ba61107fbf9eeef935c62393ff3afe7` |
| `W0-AMBIENT-R3-NATIVE.md` | `b4fd9be7815a4af19d8f103f454e8395330758faaef85e86da8ac109e7f67f01` |
| `W0-AMBIENT-R3-CRITIC-A.md` | `138c59a8674cd7211fee9c22f950ff674592743dd4bac8ae86531c8560726d7c` |
| `W0-AMBIENT-R3-CRITIC-B.md` | `b4fbcce0430dd346a0c9d9eb05574028ace077ef738895cab818df3c40a542ed` |
| `W0-AMBIENT-R3-ADJUDICATION.md` | `0453ac333914204a3597693838906471999fcdcce77c7fd518be55071151b7fb` |
| `W0-AMBIENT-R4-BORN-RED-AUTHOR.md` | `78af54f15e5e54813a3a0fb071304bbc894d74d41aef9c1695e7531592101d4c` |

The R3 adjudication remains controlling. This freeze implements its smallest
R4; it does not supersede the adjudication or accept any later W0 item.

## Exact repaired source and external-test subject

| Path | SHA-256 | Lines / bytes |
|---|---|---:|
| `crates/ir/src/passes/csp_strategy/mod.rs` | `580f1701816014714267a253f5fa2e36d9d892aa8618b66d0b5878dddc391627` | 205 / 8428 |
| `crates/ir/src/passes/csp_strategy/domain.rs` | `8a8e07ad08e15726e73e9e70127a21493098e7d7738a3b25c2c462d8fd146271` | 456 / 17211 |
| `crates/ir/src/passes/csp_strategy/report.rs` | `0f22ad333e20fc49401f1bb585ed0891ede16c5ccdab5ba6ba0a2f02f0c561d6` | 164 / 4396 |
| `crates/ir/src/passes/csp_strategy/site.rs` | `5b7b002acec4c3be75d2c7172af059195fce0eb8349bc8bb8ee3cc77776836b9` | 473 / 16085 |
| `crates/ir/src/passes/csp_strategy/solve.rs` | `02cf11b8c3439a2f0f1856aad8b8998d0182121914865e3779dfdfc7772f9581` | 680 / 25156 |
| `crates/ir/src/passes/csp_strategy/components.rs` | `35d23889c56888e61dc873843e446790f066dd151a3cd144a06ac3d345d7648b` | 336 / 13267 |
| `crates/ir/src/passes/csp_strategy/constraints/mod.rs` | `59a464e2ce990d2ed2802c3cf4c58648631257196f9bb643e55cf08510796a0e` | 108 / 4768 |
| `crates/ir/src/passes/csp_strategy/constraints/token.rs` | `c5b8c971aab45b1368da059dae024b2d752bc73473e27b18a80124c3d04b680e` | 110 / 3861 |
| `crates/ir/src/passes/csp_strategy/constraints/engine.rs` | `25986655c5a0d29776be7ec81c465d641d14de8abb56e376a06960f1f3b3134f` | 178 / 7836 |
| `crates/ir/src/passes/csp_strategy/constraints/shape.rs` | `24e2a2347cf555d9aafeba89b0857ca6c5bab4035895dae89fff6055c4bf8634` | 136 / 5530 |
| `crates/ir/src/passes/csp_strategy/constraints/layout.rs` | `58c8ecf3d55ea75b8a52277d1f495e406b068adfce652007779bd11c2e7a04b4` | 106 / 4122 |
| `crates/ir/src/passes/csp_strategy/constraints/dispatch.rs` | `6a546ea458d390c3dde6791d295cf2f71a224cec26dbf8943171a2e9df1658ce` | 95 / 3695 |
| `crates/ir/src/passes/mod.rs` | `b642e464f26d3da8012f354e0f86cbc05a26a2060d4af95b9a702897b7796272` | 81 / 3323 |
| `crates/ir/tests/passes/csp_strategy/mod.rs` | `60cde9e99399304b12ded52b2cd745701861f8e0a7dd2a0de69fa9cd47d78866` | 480 / 15634 |
| `crates/ir/tests/vm/cross_rule_csp.rs` | `d1936b1c6c97c438dbb2fb9535d322666ebd7c09973e6deef30daec9fcb8735d` | 792 / 27439 |
| `crates/ir/tests/vm/cost_weights_unified.rs` | `7dd75a79852c07874bcb8e4ac078da19f8a231631ddfe24f3f62af90e5e3f348` | 604 / 25260 |
| `crates/ir/tests/lattices/csp_materialization.rs` | `07348a0ae1734b56b2ad9bab9bc68dc1e52d3093c80a1f65ce7b249d43f6b8f1` | 284 / 10274 |
| `crates/ir/tests/lattices/csp_authority.rs` | `d13b43ddd1df8b0f0a7d62d9edceec3d88d487b6cba1f3d34fbd619a6ae7c396` | 406 / 14765 |
| `crates/core/src/lower/expression/wrap.rs` | `679539e20d29d2f42363906ee600063a2a221d0a58d35a87d1765495365e0db5` | 804 / 31996 |
| `crates/core/tests/lower/expression/wrap.rs` | `3f7b6e3637eaeed1cec393c27760e3419994254c5d972f917b1d79be3c954e77` | 122 / 4239 |
| `crates/core/src/pipeline/compile/mod.rs` | `cb8685c44438c825de70e6efcdb0298c8fc4f4b6c7c8c9b5589c5a13a470a3b5` | 124 / 4504 |
| `crates/core/src/pipeline/compile/pipeline.rs` | `4573d5a8d6ff55b01dfe5fd092f25e666c107be38eaebc665d7343ca17e27278` | 466 / 21055 |
| `crates/core/tests/pipeline/compile/pipeline.rs` | `8a082d623d2fb09d0e2966eb2796e7ba367d3d3eee3fd6f248df48b33ab31a9a` | 198 / 7290 |
| `crates/lsp/src/dap/adapter.rs` | `984ef143367e89de9cae75009cbc000f5b9c3aaa824f2c0c25e674dfe26b9625` | 280 / 9734 |

The table binds the retained R3 implementation as well as R4 changes. The five
new optimizer owners and `constraints/token.rs` did not exist before the
Goldilocks move; the old bodies are absent from `mod.rs`.

## Born-RED native preimages

| Artifact | SHA-256 | Lines / bytes | Exit |
|---|---|---:|---:|
| `W0-AMBIENT-R4-RED-1.log` | `a8a0f33765ac04956759db2ab58b577c16b5bc4b8cb412c747df9b076504899e` | 72 / 2338 | 101 |
| `W0-AMBIENT-R4-RED-2.log` | `7ed3e288c7c149c76f59fa798b681c2cbb98e5b76f65c3614af02899e83cb615` | 18 / 902 | 101 |
| `W0-AMBIENT-R4-RED-3.log` | `aec4885bd10f8ea830229ee96146638bdfaa88f1fa1ec34f833f9a244509910d` | 1131 / 42900 | 101 |
| `W0-AMBIENT-R4-BORN-RED-NATIVE.md` | `6594f0b6abdbd21d7f3a1ada75b78e507dedeacf445b773d00f08414121484a6` | 69 / 3195 | n/a |

The native record binds each exact Apple-ARM command, environment, raw
stdout/stderr preimage, and exit. Each command ran once, before repair. There
was no retry and no x86 route. `Cargo.lock` remained
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.

## R4 repair mapped to the controlling disposition

### One canonical configured transaction

`GrammarSolveControl` is the only public optimizer control and contains only
`node_budget: Option<u64>` and `cancel: Option<CancelToken>`. Its default is
`Some(1_000_000)` plus `None`; production spells those values explicitly.
There is one two-argument `solve_grammar_components` definition, seventeen
call expressions, and no `_with_config`, wrapper, alias, or second solve
symbol.

### Cancellation is checked at every publication boundary

The transaction checks the token before component work, inside component work,
after constrained search using both solver stats and current token state,
before factorized or constrained output is accepted, after component return,
between components, and before grammar success. A set token returns typed
`Cancelled`, sets the failing/boundary report to `Cancelled`, removes its
objective, and returns no `GrammarSolveOutput`. Missing-DAG cancellation also
cannot masquerade as `MissingDag`. No `TimedOut` variant was added.

The external tests retain pre-search and post-incumbent cancellation, add the
final-singleton-leaf seam, require the pre-cancelled factorized grammar's exact
boundary report, and exercise a configured public cancellation over multiple
components.

### Exact stop and decode behavior

Budget exhaustion is classified before any incumbent can be consumed; unsat
has no per-variable fallback. Complete-solution decode now requires the solution
length to equal the CSP variable count before checking each VarId, domain,
family, membership, duplicate write, coverage, and finite objective. External
tests separately reject incomplete, oversized, wrong-family, foreign-VarId,
non-finite-domain, and overflowing-objective inputs.

### Grouped mapping is direct-depth aware

`lower_mapped_factor` no longer uses descendant-wide `find_unquoted` arrow
detection. `find_direct_arrow` skips escaped double/single/backtick literals
and regex bodies, tracks balanced `()`, `[]`, and `{}` depth, and returns
only an arrow owned by the current mapped factor. Structural children remain
the sole Map-construction authority; a genuine direct arrow with no structural
value child still panics. The numeric typed-leaf recovery reads only the direct
map RHS, so parenthesized inner maps and genuine outer maps retain exact scalar
types and placement.

The original `DIRECT_GRAMMAR` bytes are unchanged. The five source-isomorphic
lowerer tests cover one/multiple nested levels, exact nested outer+inner Maps,
literal/regex arrow controls, and the exact direct `FnDescriptor::Expr`/`U8`
placement.

### Report and public-error closure

Both ordinary and boundary report member copies are sorted by `RuleId`; the
partition owner's slice is accurately described as deterministic partition
order. The report schema is unchanged: no constraint-edge identities, report
sidecar, or new telemetry was added.

The external core transaction test constructs the exact typed
`OptimizationUnavailable`, matches
`CompileError::OptimizationUnavailable` including reason/locus/reports, and
proves sentinel recognizer/materialization sidecars are byte/equality unchanged
on error. A separate compile-time oracle binds all four canonical grammar/AST
entrypoints to `CompileError`. String conversion remains only at the DAP
protocol boundary.

### Bounded R2 item-3 mutation specimens

| Artifact | SHA-256 | Lines / bytes |
|---|---|---:|
| `W0-AMBIENT-R4-MUTATIONS/PATCHES.sha256` | `6984c68483deb7beebcc9289ad1a1b77f542ceed11381737e9c7853ec2b5ffa2` | 7 / 738 |
| `W0-AMBIENT-R4-MUTATIONS/SOURCE-RESTORATION.sha256` | `b337b0d640665c65248321d8bb352a228296753a60c83a4a662be315070a3b47` | 4 / 426 |
| `W0-AMBIENT-R4-MUTATIONS/APPLY-CHECK.log` | `cdd050df474390d067188a1987ad4439b1b8c72b8561b17a9191a77e673b3215` | 34 / 1557 |
| `W0-AMBIENT-R4-MUTATIONS/MUTATION-MANIFEST.md` | `8beb499c0db78fb704067c11a5d10a5d79fa9f19acd4a7784fbf1e8e5545aeb0` | 46 / 4320 |

Seven exact unified patches model: invented restore cost, discarded custom hard
constraint, consumed budget incumbent, unsat per-variable fallback, ignored
cancellation, accepted incomplete/foreign decode, and publication after a late
merge failure. The manifest binds each patch digest to its external killing
oracle and static inversion. Every patch passes read-only `git apply --check`;
all bound source hashes recompute after the checks. No patch was applied and no
mutant was executed. This is only the R2 item-3 prerequisite, not W0 item-5
acceptance or a mutation framework.

### Goldilocks ownership and warning cleanup

The former 1,885-line owner is now the adjudicated seven-owner structure:
`domain.rs`, `report.rs`, `site.rs`, `solve.rs`,
`constraints/token.rs`, existing `components.rs`, and the 205-line
`mod.rs` facade/projection. Public names are direct re-exports. There are no
duplicate definitions, wrappers, compatibility files, or moved test bodies.
The two R3-owned unused imports, `PrettyHints` and `RuleDirectives`, are
absent; no unrelated warning cleanup was attempted.

## Static proof receipt

`W0-AMBIENT-R4-STATIC.log` is frozen at
`ce4d1d45ae5b945228b6fb2238ad29bebecb06377a0fd4a7367507968f6ad12e`
(123 lines / 7015 bytes). It records:

- direct `rustfmt --check` over the exact 24-path Rust subject: exit 0,
  zero-byte output;
- scoped tracked `git diff --check`: exit 0, zero-byte output;
- no-index checks for every new pre-receipt Rust/evidence file: expected
  difference exit 1, zero-byte diagnostics;
- one canonical solve definition, three direct re-export rows, and zero old
  type bodies/dual runtime solve symbols;
- zero inline tests in the three production owners and exactly three external
  path attachments;
- zero fallback/cost-corruption, prohibited prose, `TimedOut`, or R3-local
  unused-import rows;
- 110 report-symbol rows, both member sorts, and all cancellation boundary
  sites;
- four structured compile signatures and zero canonical string-wrapper rows;
  and
- seven patch-digest OK rows plus four unchanged-source OK rows.

## Explicit remainder

Post-repair native Apple-ARM GREEN is not in this author freeze and must be run
only by the separately authorized root campaign, once per exact command, with
raw stdout/stderr and exit preimages retained. Two fresh hostile critics and a
fresh adjudicator remain mandatory afterward. Even a clean R4 item-3
adjudication grants no W0 completion and leaves items 1, 2, 4, and 5 under
their own evidence.

**R4 REPAIRED STATIC FREEZE ONLY. NO GREEN CLAIM. CREDIT 0. CLEAN COUNT 0/2.
BB-C.W1 MAY NOT BEGIN.**
