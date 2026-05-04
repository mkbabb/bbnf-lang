# Hardening — MASTER-PLAN (greenfield restart, master-plan target)

Date: 2026-05-03
Hardener: Stage-1 hardening agent under `restart/prompts/HARDENING.md`,
target=MASTER-PLAN.
Authoritative override consulted: Amendment 01.
Hard cap: 60 minutes; six-phase incremental commit cadence.

---

## §1 — Target identification

The audited target is the synthesizer's master plan composed of two
documents:

| Item | Path | Lines | Commit |
|---|---|---:|---|
| Master plan | `restart/audit/master-plan/MASTER-PLAN.md` | 1 418 | `a9a85f45` |
| Amendment 01 | `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` | 161 | `a5145a0b` |
| Total audited | | 1 579 | |

The amendment is authoritative; where master plan and amendment disagree,
amendment wins. Amendment 01 retracts the per-grammar declaration crate
default (33-member workspace down to 24); every per-grammar-crate site in
the master plan body must reconcile against that retraction.

Sister Stage-1 hardening reports (cumulative):

| Report | Lines | Commit | Punch-list size |
|---|---:|---|---:|
| `HARDENING-PASS-A.md` | 909 | `54018ac3` | 25 |
| `HARDENING-PASS-B.md` | 759 | `70fc372e` | 30 |
| `HARDENING-PASS-C.md` | 782 | `72c906cb` | 30 |
| Cumulative | 2 450 | | 85 |

This master-plan audit reconciles those 85 cumulative findings against
the master plan itself and adds master-plan-specific surgeries
(workspace-shape consistency, tranche allocation, locks-honoured table,
generated-LOC trajectory, carry-tag receivers) on top.

---

## §2 — Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | partial | 9 | 4 | 1 | Reconcile Lock 14 with Amendment 01; reframe Lock 1 OpenFrame; gate Lock 8 across non-perf tranches; surface Lock 13 generated subdir footnote |
| 2 Sequencing | partial | 8 | 2 | 0 | Tranche E carry-down to F/G/H is real; Tranche A.W3 Lock-14 retirement before Tranche E template-emit is a same-wave-substrate fault unless A.W3 is restated as IR-side scrub only |
| 3 Cohesion | partial | — | 7 | 1 | Orphan claims at §1, §3.2.9, §5.2 (Tranche E) re-anchor against Amendment 01; deciding-lock citations strengthened; verdict-bucket commentary §2 elaborates on REINVENT/REPLACE collisions |
| 4 SOTA Anchoring | partial | 4 | 3 | 0 | Tranche-J close gates name competitor numbers but mid-tranche perf milestones (E.W*, G.W*, H.W*) lack baselines; Lock 8 honoured-by-tranche needs per-wave anchors |
| 5 Grammar-Authoritative | violated | 2 | 28 | 5 | 33 → 24 reconciliation surgery: every per-grammar-crate site re-anchors; future-grammar test absent from master plan body; per-X table for "all 9 grammars" claims missing in §3, §4.20, §6.1, §11, §12 |
| 6 Generated-Code Budget | partial | 6 | 4 | 0 | §12.3 windows are present but §12.2 trajectory rests on per-grammar declaration crate distribution that Amendment 01 retracts; redistribute trajectory under template-emitted subdirs |
| 7 Friction Forecast | violated | 0 | 6 | 0 | No friction-surface enumeration; no verbatim error messages; no `pointer!` / `parse_in` / layout-lowering / Pratt-misfire / migration / new-grammar-onboarding cookbook commits |
| 8 Carry & Deferral | partial | 5 | 8 | 1 | "Pass C is silent on this" + "synthesizer adjudicates" + "user-gated" without receiver-blocker-gate triple; bbnf-cli / bbnf-py defers carry no receiving gate; OpenFrame retiral defers from D to E without same-wave consumer |
| 9 Greenfield Discipline | partial | 6 | 3 | 1 | Per-grammar declaration crate proliferation is a quick-solution disguise; specialised cohort `specialised/` module is a workaround; OR-disposition language ("rename OR merge") leaves quick-solution path open |

**Aggregate cohort tabulation (master-plan-specific items, not cumulative
with sister reports):**

- KEEP: 40
- REINVENT: 65
- DISCARD: 8
- Total master-plan items audited: 113

**Final decision: requires amendments — reissue as MASTER-PLAN-V2 after
punch list applies.**

The master plan's substantive shape (10-tranche allocation; convergent
pivot at E; commit-chain Option-3 ratification; docs re-do; greenfield
calendar) survives the lanes. What requires amendment is the *body*:
every per-grammar-crate reference re-anchors per Amendment 01; the
24-member workspace materialises everywhere; SOTA gates land per-wave
not just per-tranche-close; friction surfaces enumerate; carries name
receivers + blockers + gates; the future-grammar onboarding test
materialises in the tranche-E gate set. The pivot itself — Lock 1 +
Lock 13 + Lock 14 retiring as one architectural movement via
template-emit + direct-projection + Emitter coarsening — survives the
amendment in full. The 33-crate proliferation was overfitting on the
Lock-14 escape valve.

---

## §3 — Lane 1: Lock-Adherence

Standard: walk the 14 locks at `restart/locks/14-LOCKS.md` against the
master plan + Amendment 01. Per-lock cell + verdict.

The master plan §11 (lines 1281-1296) lays a per-lock honoured cell map
at greenfield completion. The hardening adversary holds that map next
to the locks themselves and to the Amendment 01 supersedence.

### Lock 1 — Tape and columnar variants are fully dead

**Master-plan claims**: §11 row 1 cites tranches A + C; A.W4 narrative
scrub, C.W7 typed-IR consolidation. Verification command at
`MASTER-PLAN.md:1283` greps `TapeRec|TapeCursor|payload_idx|OpenFrame|
FusedBuilder` against the new generic-crate set.

**Pros**: enumerates the substrate-symbol set including OpenFrame as a
Lock-1 violation (correct per Pass B Agent B.3 §Lock 1 wording "OpenFrame
is tape rebranded; honest answer: yes in spirit"). The verification
command is greppable; CI-gateable.

**Cons**: the verification command runs against the post-restart
generic-crate set but misses the `bbnf-runtime/src/grammars/<name>/`
directory tree that Amendment 01 ratifies as the destination for
template-emitted runtime modules. If a generated module under
`grammars/<name>/` carries `OpenFrame` as a stale-template artefact, the
command does not catch it. The §1283 grep also lacks
`crates/bbnf-runtime-template/src/` and `crates/bbnf-host-prims/src/`
under Amendment 01's adjusted shape.

**Challenge (steelman)**: the trajectory is right; tranche E's
direct-projection emit retires OpenFrame in the *template* not in the
*generated output*; once template-emit lands, regenerating produces
OpenFrame-free output across all subdirs. The verification command at
§11 would catch any residue post-regen.

**Verdict: REINVENT.** §11 row 1 verification command extends to also
grep `crates/bbnf-runtime/src/grammars/`, `crates/bbnf-runtime-template/
src/`, and `crates/bbnf-host-prims/src/` per Amendment 01's substrate.
Surgery: punch-list item 1.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1283` | Lock-1 verification | command across 10 generic crates | greppable | misses Amendment-01-introduced `bbnf-runtime/src/grammars/`, `bbnf-runtime-template/src/`, `bbnf-host-prims/src/` | template-emit retires symbols upstream | REINVENT |
| `MASTER-PLAN.md:777` | Tranche A "narrative scrub eliminates ~50 tape residue sites" | tape vocabulary in IR/narrative comments | bounded | does not name OpenFrame as residue subset | OpenFrame retiral happens in tranche E per direct-projection emit | KEEP |
| `MASTER-PLAN.md:785` | Tranche E "86.07% samply share collapses by mechanism" | direct-projection retires checkpoint clone | ratified pivot | claim is structural; no measurement gate | structural retirement is verifiable from absent symbols | REINVENT |

### Lock 2 — Layout lowering is the canonical IR pass name

**Master-plan claims**: §11 row 2 cites tranche C; C.W2 substantive fold.
Verification greps `TypeDesc|StructLayout|TypeMap|type_projection|
type_collapsing|schema synthesis|LayoutDesc` against `bbnf-ir/` +
`bbnf-passes/`.

**Pros**: enumerates the retired vocabulary correctly; cites
`feedback_unified_propagate`-class one-pass-rename discipline.

**Cons**: the LayoutSink trait's role is named at §59 (the §3.1 ledger
row for `bbnf-ir/`) but no §11 cell verifies *the trait lands in
bbnf-ir/registry/sink.rs* (per §300 schematic). A grep for
"trait LayoutSink" in `bbnf-ir/src/registry/` is a missing verification
artefact.

**Challenge**: the rename is mechanical; the trait lands as part of the
IR's registry; greenfield completion implies any compiler tooling
reading bbnf-ir consumes `LayoutSink` directly. Surgery: §11 row 2
verification extends to "trait LayoutSink in bbnf-ir/src/registry/sink.
rs is greppable; consumer count ≥ 2".

**Verdict: REINVENT.** Surgery: punch-list item 2.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1284` | Lock-2 verification | rename grep | exhaustive across retired vocabulary | doesn't verify LayoutSink trait lands | trait lands as part of registry | REINVENT |

### Lock 3 — Cursor-parse + byte-skip unified, empty-path elision

**Master-plan claims**: §11 row 3 cites tranches C + E; C.W6 path-core
consolidation, E.W4 cursor-consult-on-EMPTY-PATH binding. Verification:
no dual implementations; empty-path elision in cookbook.

**Pros**: names the two consumers (path-core + emitter); names the
cookbook entry.

**Cons**: the cookbook lands at `docs/howto/cookbook/path-macro.md`
per §11 row 3, but §8.2.2 (lines 1140-1153) lists no cookbook rewrite
that includes path-macro.md verbatim. The cookbook is named in a
verification cell but not gated in any tranche wave's deliverable list.

**Challenge**: the cookbook entry is a tranche-G deliverable per Lock-9
+ Lock-7 wiring (G.W3-W4 expected); `MASTER-PLAN.md:789` ("`pointer![
...]` macro per Lock 7") is the closest match; but §11's cited C.W6 is
*pre*-Lock-9; this is a sequencing inconsistency.

**Verdict: REINVENT.** Tranche-G cookbook entry promotes to a deliverable
named in §5.2 G commentary; Lock-3 verification cites tranche G's
cookbook wave, not C.W6. Surgery: punch-list item 3.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1285` | Lock-3 verification cites C.W6 cookbook | empty-path elision invariant in cookbook | Lock-3 + Lock-7 + Lock-9 wired together | C.W6 is pre-Lock-9; cookbook entry not gated in any wave's deliverable | tranche G is the natural home | REINVENT |

### Lock 4 — Per-domain orthogonal optimisation

**Master-plan claims**: §11 row 4 cites tranche F; F.W2 output-piping
verification. `cargo tree -p bbnf-passes` shows orthogonal sub-deps.

**Pros**: names the verification at the dependency-DAG level; cites the
right consumers.

**Cons**: the verification command requires the `bbnf-passes` crate to
have orthogonal sub-deps on egraph + csp-solver but doesn't gate the
*absence* of a fused-hypergraph crate. Should grep
`crates/ -name 'hypergraph' -o -name 'fused-solver'` returning zero.

**Challenge**: the absence is implicit in the workspace member list
§3.3; no dedicated grep needed.

**Verdict: KEEP** with minor surgery. Punch-list item 4: extend the
verification to cite the workspace-members negative claim explicitly.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1286` | Lock-4 verification | dependency-DAG check | greppable | absence-of-fused-solver not enumerated | implicit via member list | KEEP |

### Lock 5 — IR + per-backend lower

**Master-plan claims**: §11 row 5 cites tranches C + D; C.W4
bbnf-codegen-ir lands, D.W3 per-backend lower split. Verification: IR
contract document at `docs/spec/codegen.md`; matching trait method
counts across rust + ts.

**Pros**: names the IR contract document explicitly; names the trait
method count check.

**Cons**: the trait-method-count verification compares Rust + TS without
WASM. WASM lands in tranche H; if H drifts in method count, the
verification misses it. Should be three-way: Rust + TS + WASM.

**Challenge**: WASM is post-tranche-H; pre-H, the verification is
two-way; post-H it is three-way. The §11 cell should state "post-H
extends to WASM".

**Verdict: REINVENT.** Surgery: punch-list item 5.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1287` | Lock-5 verification | trait method counts | greppable | only two-way (Rust + TS); WASM uncovered | post-H extends to three-way | REINVENT |

### Lock 6 — xtask emits committed source artefacts

**Master-plan claims**: §11 row 6 cites tranches A + E; A.W2 xtask
Cargo.toml additions, E.W6 runtime template emit gate. Verification:
`cargo xtask regen --check` produces zero source diff.

**Pros**: clean; greppable; the regen-check is the canonical Lock-6
verification.

**Cons**: under Amendment 01, regen targets are
`crates/bbnf-runtime/src/grammars/<name>/{generated.rs, runtime.rs}`,
not `crates/<grammar>/src/generated.rs`. The §11 cell verbiage at
line 1288 says "per per-grammar declaration crate `src/generated.rs`"
which contradicts Amendment 01.

**Challenge**: the substantive verification (zero diff post-regen) is
target-agnostic; the *path naming* is what re-anchors.

**Verdict: REINVENT.** Re-anchor §11 row 6 phrasing per Amendment 01.
Surgery: punch-list item 6.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1288` | Lock-6 verification cites per-grammar declaration crate | regen artefacts at per-crate paths | greppable | per-grammar declaration crate retracted by Amendment 01 | re-anchor to bbnf-runtime/src/grammars/<name>/ | REINVENT |

### Lock 7 — `crates/path/` consolidated path crate

**Master-plan claims**: §11 row 7 cites tranches A + C; A.W4 path-core
skeleton, C.W6 full consolidation. Verification: file-size + child-count
checks; no `crates/bbnf-path` references.

**Pros**: names both phases; greppable.

**Cons**: the path triplet `path-core/`, `path/`, `path-ts/` (Lock 7)
honours the locked footnote; honoured by construction.

**Challenge**: path triplet is correctly named; surgery is none.

**Verdict: KEEP.**

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1289` | Lock-7 verification | path triplet greenfield | tight | none | honoured by construction | KEEP |

### Lock 8 — Surpass sonic-rs / simdjson / lightning-css

**Master-plan claims**: §11 row 8 cites tranches F + G + H + J; "All
G-numbered gates", "competitor numbers cited per gate".

**Pros**: enumerates the four perf-anchored tranches; cites sonic-rs +
lightning-css + simdjson SOTA numbers.

**Cons**: §11 row 8 cell phrasing is imprecise. Lock 8 binds *every*
parse-throughput gate, not just the J-close. Tranche F's optimiser
landing must cite a perf gate; tranche G's slice-borrow API rollout
must cite a perf gate; tranche H's TS+WASM emit must cite parity
numbers, not throughput numbers (since TS+WASM aren't competing with
sonic-rs). The "per gate" phrasing is right; the *enumeration* of
which gates carry SOTA is missing per-tranche per-wave.

**Challenge**: per-wave gate enumeration is in the per-tranche stub
(deferred to per-tranche execution agents); the master plan's §11 cell
is the apex statement, not the gate list.

**Verdict: REINVENT.** Master plan §5.2 per-tranche gestalt commentary
extends to enumerate the SOTA-anchored gate per perf-tranche; tranche
J §5.2 line 795 cites three numbers (twitter 436 µs, Bootstrap 4.16
ms, simdjson 7 GB/s) but tranches F + G + H carry no SOTA cite. Surgery:
punch-list items 7-9.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1290` | Lock-8 verification "All G-numbered gates" | every G-numbered gate names competitor | apex enumeration | no per-tranche-wave anchor in F/G/H | per-tranche stub absorbs | REINVENT |
| `MASTER-PLAN.md:787` | Tranche F gestalt | optimiser pipeline | substantive | no SOTA gate cited mid-tranche | F.W6 close requires perf evidence | REINVENT |
| `MASTER-PLAN.md:789` | Tranche G gestalt | slice-borrow + pointer macro | API-shape claim | no SOTA gate cited mid-tranche | G.W5 close requires perf evidence | REINVENT |
| `MASTER-PLAN.md:791` | Tranche H gestalt | TS+WASM emitters | cross-backend parity | no parity-number cited | H.W4 close requires parity evidence | REINVENT |

### Lock 9 — Slice-borrow primary; bumpalo + owned escape hatches

**Master-plan claims**: §11 row 9 cites tranche G; G.W2 parse / parse_in
/ parse_owned API. Verification: `rg 'pub fn parse_in|pub fn
parse_owned' crates/bbnf/src/lib.rs` matches.

**Pros**: names the three lifetime APIs; greppable.

**Cons**: §3.1 ledger row 1 (line 54) names `bbnf` as "thin user-facing
aggregator" with re-exports `pub use bbnf_parse::*`. If `parse_in` /
`parse_owned` lives in `bbnf-parse`, the verification grep against
`bbnf/src/lib.rs` requires the re-export to surface them. The
verification command may pass with re-exports without the underlying
implementation existing.

**Challenge**: the re-export discipline is correct; trait-API
verification at the module level catches any missing impl.

**Verdict: KEEP** with minor surgery. Surgery: extend Lock-9 verification
to also grep `bbnf-parse/src/lib.rs` for the underlying impl. Punch-list
item 10.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1291` | Lock-9 verification | three-API surface | greppable | only checks aggregator re-export, not impl | impl is in bbnf-parse | KEEP |

### Lock 10 — Pratt + SIMD auto-detected

**Master-plan claims**: §11 row 10 cites tranche F; F.W3 auto-detection
lands. Verification: `rg '@pratt|@simd' grammar/` returns 0;
`bbnf-passes::recognizers::operator_chain` + `pattern_alphabet` consume
cost-model.

**Pros**: names the two recognisers; greppable.

**Cons**: §6.1 Cargo.toml metadata block at lines 869-966 names
`pratt_eligibility = "auto" | "force" | "skip"` and `simd_eligibility =
"auto" | "force" | "skip"` per grammar (lines 873-874, 884-885, etc.).
The "force" / "skip" knobs are escape hatches that *require* the
metadata to inform Pratt/SIMD detection. This is consistent with Lock
10's "auto-detected" phrasing because the grammar author can override —
but the override is a metadata escape, not a directive escape. Lock 10's
verbatim text says "no `@pratt` or `@simd` directives". The metadata
override is permitted; the directive is not. The metadata block at
§6.1 names the override correctly.

**Challenge**: the metadata escape valve is not a directive in the
grammar source; it is workspace metadata. Lock 10 honours.

**Verdict: KEEP.**

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1292` | Lock-10 verification | no directives in grammar | greppable | metadata escape hatch is permitted | metadata is not a directive | KEEP |

### Lock 11 — Path-deps for incubating sister crates

**Master-plan claims**: §11 row 11 cites tranches A + I; A.W2 path-deps
register, I.W2 publication. Verification: `cargo tree -p bbnf-passes`
shows path-dep markers; egraph + csp-solver published.

**Pros**: names both phases; greppable.

**Cons**: §3.1 ledger row 21 (line 74) names `crates/parse-that/` as
path-dep but Pass A's hardening report (`HARDENING-PASS-A.md:200-210`)
identifies parse-that disposition as still-deferred — submodule-vs-
workspace-member is named without resolution. §3.2 reconciliation item
8 (line 109) ratifies "submodule + workspace-member is preferred" but
the operational sequence in §7.2 (lines 1056-1093) does not include
git-submodule-add operations.

**Challenge**: tranche-A.W2 includes the submodule-add per Pass A
punch-list item 1; the synthesizer ratifies but doesn't replicate the
operation. Surgery: §7.2 prelude commits 4-5 add `git submodule add`
for parse-that and bbnf-regex.

**Verdict: REINVENT.** Punch-list item 11.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1293` | Lock-11 verification | path-dep + publish | greppable | submodule operations not in §7.2 | tranche-A.W2 absorbs | REINVENT |

### Lock 12 — ser + gorgeous archive ceremony

**Master-plan claims**: §11 row 12 cites tranche A; A.W0 (precondition).
Verification: archive directories exist; original locations empty.

**Pros**: tight; greppable; precondition status correct per Pass C
ratification.

**Cons**: none. Lock 12 honours by construction; verifiable by
filesystem.

**Verdict: KEEP.**

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1294` | Lock-12 verification | archive ceremony | filesystem-greppable | none | honoured | KEEP |

### Lock 13 — No god directories

**Master-plan claims**: §11 row 13 cites tranches A + C; A.W4 cleanup,
C.W6 final SPLITs. Verification: no file >500 LOC outside generated/;
no directory >10 children mixing concerns.

**Pros**: greppable; cites both phases.

**Cons**: Amendment 01's `crates/bbnf-runtime/src/grammars/<name>/`
subdirectory shape carries 9 immediate per-grammar children
(json, css-l4, bbnf-meta, google-sheets, bnf, csv, ebnf, css-pretty,
math). 9 > Lock-13 ceiling? Lock 13 says "directories with >10 immediate
children mixing concerns are forbidden". 9 is under 10; *and* the
children are not mixed concerns (every child is a template-emitted
runtime subdirectory; structurally identical). Amendment 01 §"Lock 13
footnote" (line 32) explicitly grants this: "the per-grammar
subdirectory is permissible because every subdirectory is structurally
identical (template-emitted), so the parent directory is cohesive — its
concern is 'houses generated grammar runtimes'."

**Challenge**: the master plan §11 row 13 verification is silent on the
grammars/ subdirectory footnote; should cite Amendment 01 explicitly.

**Verdict: REINVENT.** Surgery: §11 row 13 verification annotates that
`crates/bbnf-runtime/src/grammars/` is exempt per Amendment 01's Lock-13
footnote. Punch-list item 12.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1295` | Lock-13 verification | file size + child count | greppable | doesn't cite Amendment 01's grammars/ exemption | Amendment 01 §"Lock 13 footnote" grants | REINVENT |

### Lock 14 — Full grammar generalisation; zero overfitting

**Master-plan claims**: §11 row 14 cites tranches A + E; A.W3 retirement
of 7 sites, E.W6 per-grammar declaration crates land. Verification:
`rg JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser` against 14
generic crates returns 0; future-grammar onboarding test verifies
"adding a new grammar to `[workspace.metadata.bbnf.grammars]` + creating
its declaration crate + running `cargo xtask regen` produces a working
parser with ZERO code change in any other crate".

**Pros**: enumerates the verification command; cites the future-grammar
test as the closure invariant; names tranche-A 7-site retirement.

**Cons**: This is the largest fault. Lock 14 verification cell at line
1296 says "future-grammar onboarding test: adding a new grammar to
`[workspace.metadata.bbnf.grammars]` + **creating its declaration crate**
+ running `cargo xtask regen`". Amendment 01 retracts the declaration
crate. Per Amendment 01 §"Settled position" (line 13-25), the
two-surface ceremony is:

1. Drop `grammar/yaml/yaml.bbnf` into the source tree
2. Add a `[workspace.metadata.bbnf.grammars.yaml]` block

That is it. No declaration crate. The master plan's verification command
adds a third step — declaration crate creation — that Amendment 01
deletes.

**Challenge**: the master plan was committed before Amendment 01;
Amendment 01 supersedes; the verification command at line 1296 must
re-anchor.

**Verdict: REINVENT.** Surgery: §11 row 14 verification cell drops
"creating its declaration crate" step; future-grammar test is two-step.
Punch-list items 13-14.

Additionally, the §11 row 14 verification command lists 14 generic
crates including `bbnf-host` (line 1296). Amendment 01 introduces
`bbnf-host-prims` (the generic primitive library replacing per-grammar
host fns). The verification command must extend to grep
`bbnf-host-prims` as well. Punch-list item 15.

The future-grammar onboarding test must be a Tranche-E close gate
(per Amendment 01 §"Tranche-drafting discipline under amendment" item
5: "the future-grammar test (yaml.bbnf) gates the tranche-set's Lock 14
closure"). Master plan §5.2 Tranche E commentary (line 785) does NOT
name the future-grammar test as a gate; this is a master-plan-body
fault. Punch-list item 16.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1296` | Lock-14 verification "creating its declaration crate" step | three-step ceremony | greppable | Amendment 01 retracts declaration crate | re-anchor to two-step | DISCARD |
| `MASTER-PLAN.md:1296` | Lock-14 generic-crate enumeration | 14 crates | enumerated | misses bbnf-host-prims | extend per Amendment 01 | REINVENT |
| `MASTER-PLAN.md:785` | Tranche E gestalt | convergent pivot | substantive | no future-grammar test as gate | gate per Amendment 01 §item 5 | REINVENT |

### Lock-cohort summary

**Per-lock verdict tabulation:**

| Lock | Verdict | Surgery |
|---|---|---|
| 1 | REINVENT | extend §11 grep to Amendment-01 substrate |
| 2 | REINVENT | verify LayoutSink trait lands |
| 3 | REINVENT | tranche G is cookbook home, not C.W6 |
| 4 | KEEP | minor — cite negative claim |
| 5 | REINVENT | post-H extends Rust+TS+WASM three-way |
| 6 | REINVENT | re-anchor per Amendment 01 path naming |
| 7 | KEEP | honoured by construction |
| 8 | REINVENT | per-tranche-wave SOTA enumeration in F/G/H |
| 9 | KEEP | minor — verify impl in bbnf-parse |
| 10 | KEEP | metadata escape is permitted |
| 11 | REINVENT | submodule-add operations in §7.2 prelude |
| 12 | KEEP | honoured by construction |
| 13 | REINVENT | cite Amendment-01 grammars/ footnote |
| 14 | REINVENT (×3) + DISCARD (×1) | re-anchor verification per Amendment 01; future-grammar test as tranche-E gate |

**Lane 1 verdict: partial. KEEP=5, REINVENT=8, DISCARD=1.**

---

## §4 — Lane 2: Sequencing Discipline

Standard: every wave deliverable must land with same-wave or next-wave
consumer per `era-V-dta-psi-rut.md`. The master plan names 10 tranches
(A-J) totalling 53 waves. Per-tranche audit (waves not yet drafted; per
§5.3 stubs are 150-300 lines; full waves draft post-hardening — so this
lane operates at tranche-gestalt + carry-tag granularity).

### Tranche A — Workspace genesis

**Wave count**: 6 (`MASTER-PLAN.md:762`).
**Carry FROM**: none. **Carry TO**: B, C.

**Substantive deliverables** (per §5.2 line 777):
1. Lock 12 archive ceremony (precondition)
2. Commit-chain disposition execution (Option 3)
3. Empty crate skeletons land
4. Sister-crate path-deps register
5. Narrative-scrub pass (~50 tape residue sites)
6. IR Lock-14 retirement (7 sites; per Pass A §7 W1)

**Same-wave or next-wave consumer**: yes, all six. Lock 12 ceremony →
A.W0 close. Crate skeletons → A.W4 `cargo check --workspace` green.
Path-deps → A.W2 (per Lock 11 reconciliation). Narrative scrub → A.W4
+ Lock-1 verification command. IR Lock-14 retirement → A.W3 (per
master plan §11 row 14). All same-tranche consumers; honoured.

**Verdict: KEEP.**

### Tranche B — bbnf-error + bbnf-pipeline foundation

**Wave count**: 4 (`MASTER-PLAN.md:763`).
**Carry FROM**: A. **Carry TO**: C, D.

**Deliverables** (§5.2 line 779): unified error trait, per-crate Error
adoption, pipeline coordinator consolidation, Lock 2 directory-rename
staging, naming-canon audit.

**Consumer**: pipeline coordinator → tranche C's parse-front substrate
consumes; bbnf-error → every crate consumes from C onwards. Honoured.

**Verdict: KEEP.**

### Tranche C — Parse + IR foundation

**Wave count**: 7 (`MASTER-PLAN.md:764`).
**Carry FROM**: A, B. **Carry TO**: D, E.

**Deliverables** (§5.2 line 781): bbnf-grammar + bbnf-parse + bbnf-ir +
bbnf-passes consolidation; Lock 2 substantive fold; 13 god-module
SPLITs; bbnf-vm extraction; bbnf-host extraction; path-core
consolidation begins.

**Consumer**: bbnf-codegen-ir at tranche D consumes. Honoured.

**Substantive concern**: the `path-core/` consolidation *begins* in C
but completes in C.W6 per §11 row 7. §5.2 commentary doesn't name the
boundary cleanly; tranche C closes with path-core complete or not. The
ambiguity is one wave's worth of substrate: did W6 also include
`path/` and `path-ts/` shells, or only `path-core/`? Per master plan
§3.1 ledger rows 16-18 (lines 69-71), all three are workspace members
created in tranche A but their *content* lands in C.W6 (per §11 row 7).
The shells (path/, path-ts/) lack same-wave consumers in C; they land
in tranche G (path/) and tranche H (path-ts/). This is *substrate
without consumer in C* — a Era-V-class fault.

**Verdict: REINVENT.** Surgery: tranche C.W6 deliverable explicitly
limits to `path-core/`; `path/` proc-macro shell lands in G.W3 with
tranche-G `pointer![...]` macro consumer; `path-ts/` cdylib lands in
H.W2 with TS-emit consumer. Punch-list item 17.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:781, 1289` | Tranche C path-core scope | C.W6 | substantive | path/, path-ts/ shells inside without consumer | shells move to G.W3 + H.W2 | REINVENT |

### Tranche D — Codegen IR contract

**Wave count**: 5 (`MASTER-PLAN.md:765`).
**Carry FROM**: C. **Carry TO**: E, F.

**Deliverables** (§5.2 line 783): bbnf-codegen-ir crate with 22-variant
typed IR; Emitter trait reshape (30 → 8-10 methods); Rust lowerer
smoke (one grammar round-trips).

**Consumer**: tranche E's runtime template emits via `bbnf-codegen-ir`
output. Honoured for the IR + Emitter substrate.

**Substantive concern**: the Rust-lowerer-smoke deliverable (one grammar
round-trips through the IR) is a *post-tranche-D consumer*. The IR
contract lands at C.W4 per §11 row 5; the codegen IR (typed 22-variant)
lands at D.W3. The Rust lowerer smoke at D.W5 closes — but D's smoke
is the SAME-WAVE consumer of the typed IR. Honoured.

**Verdict: KEEP.**

### Tranche E — Per-grammar declaration crates + runtime template

**Wave count**: 8 (`MASTER-PLAN.md:766`).
**Carry FROM**: C, D. **Carry TO**: F, G, H.

**Deliverables** (§5.2 line 785): bbnf-runtime-template, 9 declaration
crates, direct-projection emit, OpenFrame retirement, 13K-LOC
hand-written runtime files retire.

**Critical concern under Amendment 01**: the master plan body names
"9 declaration crates"; Amendment 01 retracts to zero declaration
crates. The substrate centerpiece survives — bbnf-runtime-template +
direct-projection emit + Emitter coarsening — but the *language*
re-anchors. Per Amendment 01 §"Tranche-set impact" line 112: tranche
E's new scope is "`bbnf-host-prims` + `bbnf-runtime-template` +
template-emitted per-grammar runtime subdirs + direct-projection emit
+ Lock 14 metadata-driven host-fn composition".

**Consumer**: tranches F (optimiser pipeline consumes runtime), G
(slice-borrow API consumes runtime), H (TS+WASM emitters consume IR
+ runtime template). All next-wave or 2-wave consumers.

**Substantive concern 1**: the future-grammar onboarding test (per
Amendment 01 §item 5) is the Lock-14 closure invariant; it MUST land as
a tranche-E close gate. §5.2 commentary at line 785 does NOT name it.
Punch-list item 16 (already noted in Lane 1 Lock 14).

**Substantive concern 2**: tranche E carries 8 waves but §5.2 doesn't
name the per-wave delivery sequence. Sister hardening Pass-B at
`HARDENING-PASS-B.md:128-160` enumerates Lock-14 mass-redress sites; the
master plan's tranche E gestalt should reference that punch list.
Without per-wave allocation, tranche E is at risk of being substrate-
heavy (template lands waves 1-3) with consumer-light (per-grammar
generated artefact lands wave 6+) — Era V hazard.

**Verdict: REINVENT.** Surgery: tranche E gestalt (§5.2 line 785)
extends to (a) re-anchor "9 declaration crates" to "9 template-emitted
subdirs under bbnf-runtime/src/grammars/" + bbnf-host-prims; (b) name
future-grammar onboarding test as E close gate; (c) name the
substrate-consumer sequencing (template arrives W1-W3; consumer
exercises W4-W8 with per-grammar tests). Punch-list items 18-20.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:766, 785, 1296` | Tranche E "9 declaration crates" | per-grammar declaration crate scaffolding | matches master plan §3.1 | retracted by Amendment 01 | re-anchor per Amendment 01 | DISCARD |
| `MASTER-PLAN.md:785` | Tranche E future-grammar test absent | Lock-14 closure invariant | Amendment 01 §item 5 mandates | not in §5.2 commentary | gate as E close | REINVENT |
| `MASTER-PLAN.md:766` | Tranche E 8-wave breakdown | substrate + consumer | substantive carry | per-wave allocation absent | enumerate per Phase-4 hardening pattern | REINVENT |

### Tranche F — Optimiser pipeline

**Wave count**: 6 (`MASTER-PLAN.md:767`).
**Carry FROM**: C, D, E. **Carry TO**: G, J.

**Deliverables** (§5.2 line 787): per-domain orthogonal optimisation
honour; cost-model output-piping; Pratt + SIMD auto-detection.

**Consumer**: tranche G consumes optimiser-emitted runtime; tranche J
consumes SOTA-anchored bench. Honoured.

**Substantive concern**: §5.2 line 787 cites Pass B Agent B.5 §6 for
cost-model placement (in egraph); the placement is correct. Lock 4 +
Lock 10 + Lock 11 wired together — this is the optimiser tranche.
Per-tranche-wave SOTA gate is missing per Lane 4.

**Verdict: REINVENT.** Per Lane 4 surgery — cite SOTA gate at F.W6
close. Punch-list item 8.

### Tranche G — Slice-borrow + pointer macro + visitor surface

**Wave count**: 5 (`MASTER-PLAN.md:768`).
**Carry FROM**: E, F. **Carry TO**: H, J.

**Deliverables** (§5.2 line 789): three lifetime APIs (parse / parse_in
/ parse_owned); `pointer![...]` macro; Visitor trait.

**Consumer**: tranche H consumes the user-facing API for TS+WASM
exposure; tranche J consumes parity-matrix.

**Substantive concern**: the `pointer![...]` macro lands in G but the
proc-macro shell `crates/path/` is a workspace member registered in
tranche A.W2 (per §3.3 ordering). The shell sits empty for tranches
B-F (5 tranches × ~3 weeks each = ~15 weeks of empty content). This is
substrate without consumer for 5 tranches.

**Challenge**: empty proc-macro shells are common discipline; Lock 11's
path-dep semantics suggest the shell can register early. But Era V's
substrate-then-substrate-then-ship anti-pattern is exactly this — a
substrate sitting unconsumed across multiple tranches.

**Verdict: REINVENT.** Surgery: tranche-A.W2 path/ shell registration
narrows scope to "Cargo.toml entry + lib.rs stub citing 'tranche G
implements'"; the substantive proc-macro implementation lands in G.W3
with same-wave consumer test. Same applies to path-ts/. Punch-list
item 21.

### Tranche H — TS + WASM emitters

**Wave count**: 4 (`MASTER-PLAN.md:769`).
**Carry FROM**: D, E, G. **Carry TO**: I, J.

**Deliverables** (§5.2 line 791): TS + WASM emit activation;
cross-backend smoke.

**Consumer**: tranche J's parity matrix consumes; same-wave smoke
internal to H.

**Substantive concern**: §5.2 commentary at line 791 says "the
post-tranche-D Emitter trait collapse means TS + WASM share the
per-shape walking pattern". The collapse lands D.W3; tranche H consumes
the collapsed trait at H.W2. Honoured.

**Verdict: KEEP.** Per Lane 4, cite parity-number gates at H close.

### Tranche I — Sister-crate publication

**Wave count**: 3 (`MASTER-PLAN.md:770`).
**Carry FROM**: A, F. **Carry TO**: J.

**Deliverables** (§5.2 line 793): egraph + csp-solver + bbnf-regex
publish; parse-that disposition; simd-scan workspace-internal.

**Consumer**: tranche J consumes published crates for cross-backend
parity; same-tranche I close.

**Verdict: KEEP.**

### Tranche J — Cross-backend parity + close

**Wave count**: 5 (`MASTER-PLAN.md:771`).
**Carry FROM**: All. **Carry TO**: close.

**Deliverables** (§5.2 line 795): parity matrix; final perf gates
(sonic-rs twitter ≤ 436 µs; lightning-css 4.16 ms; simdjson 7 GB/s);
close ceremony.

**Consumer**: close (terminal).

**Verdict: KEEP.**

### Cross-tranche dependency-DAG

The dependency arrow at §9.2 line 1230:

```
A → B → C → D → E ──┬──► F ──► I ──┐
                    ├──► G ────────┼──► J → close
                    └──► H ────────┘
```

Honoured: linear A-E spine; F/G/H parallel after E; I depends on F;
J synchronises all.

**Concern**: the §9.2 diagram shows H as parallel-to-F-G but §5.1 row
H lists "Carry FROM: D, E, G" (line 769). H depends on G's slice-borrow
API for cross-backend smoke; thus H is *post-G*, not parallel-to-G.
The DAG diagram disagrees with the carry-FROM table.

**Verdict: REINVENT.** Surgery: §9.2 diagram or §5.1 H carry-FROM
reconciles. The honest read is H depends on D + E (substrate) but NOT
on G (API surface unrelated to TS/WASM emit). Carry-FROM should be
"D, E"; G dependency is wrong. Punch-list item 22.

### Sequencing-discipline summary

| Tranche | Substrate-consumer pattern | Verdict |
|---|---|---|
| A | All deliverables consumed in A | KEEP |
| B | error + pipeline consumed in C | KEEP |
| C | parse-front consumed in D + E | REINVENT (path-core scope) |
| D | codegen-IR consumed in E | KEEP |
| E | template + host-prims + direct-projection consumed in F + G + H | REINVENT (×3) + DISCARD (×1) |
| F | optimiser consumed in G + J | REINVENT (SOTA gate) |
| G | slice-borrow + pointer-macro consumed in H + J | REINVENT (path/ shell early reg) |
| H | TS+WASM consumed in I + J | KEEP (carry-FROM fix) |
| I | publication consumed in J | KEEP |
| J | close | KEEP |

**Lane 2 verdict: partial. KEEP=8, REINVENT=2, DISCARD=0** (counting
tranche-level verdicts; per-deliverable surgeries enumerate further in
punch list).

---

## §5 — Lane 3: Cohesion

Standard: every claim in the master plan must be verifiable from
artefacts the master plan produces or cites.

### Orphan claim 1 — 86.07% samply share retirement

`MASTER-PLAN.md:15` and `:785` claim "the 86.07% samply share collapses
by mechanism". The 86.07% number sources `RESTART-SKETCH.md §A.7`
(per Pass B citation). The master plan asserts the retirement is
*structural* — direct-projection emit eliminates the heap-stack →
checkpoint clone disappears.

**Pros**: structurally honest; the mechanism is described.

**Cons**: the master plan does NOT gate the post-restart samply
distribution. Tranche E has no measurement gate that says "after
direct-projection emit lands, run samply on twitter.json; verify
JsonStructBuilder::checkpoint share <1%". The claim survives or fails
on structural grounds alone.

**Surgery**: tranche E (E.W6 close) adds samply gate per Pass B
hardening surgery `HARDENING-PASS-B.md:178-180`. Punch-list item 23.

### Orphan claim 2 — 168 750 LOC distribution

§3.1 ledger rows 24-32 (lines 79-87) distribute 168 750 LOC across 9
per-grammar declaration crates: `bbnf-meta` 22 000, `json` 3 500,
`css-l4` 107 000, `google-sheets` 14 000, `bnf` 3 300, `csv` 1 700,
`ebnf` 7 650, `css-pretty` 9 000, `math` 870. Total: 168 020.

Per CENSUS §10.5: 168 750. The master plan total = 168 020 (the §3.1
column sums to 168 020 not 168 750). 730 LOC discrepancy.

**Pros**: per-grammar projection is enumerated.

**Cons**: arithmetic mismatch with CENSUS baseline. Either §3.1
estimates are stale or CENSUS §10.5 is rounded.

**Surgery**: §3.1 LOC estimate column reconciles with CENSUS §10.5;
either §3.1 numbers update to 168 750 distribution or CENSUS clarifies
730-LOC discrepancy. Punch-list item 24.

### Orphan claim 3 — 24-member workspace under Amendment 01

Amendment 01 §"Corrected workspace shape" line 73 declares "Final
workspace member count: 24". Master plan §3.1 (line 89) says 33. The
master plan body has not been updated.

**Pros**: Amendment 01 is explicit about supersedence.

**Cons**: master plan §3.1 + §3.3 + §6.1 + §11 still name the 33-member
shape. A reader of the master plan alone (without amendment) gets the
wrong workspace.

**Surgery**: this is the core master-plan-V2 reconciliation surgery.
Every site naming the 33-member workspace re-anchors. Per Amendment 01
§"Master-plan sections superseded" lines 134-145, the sites are §13-17,
§38, §79-89, §93, §99, §111, §157, §719, §722, §766, §785, §876, §887,
§898, §909, §920, §931, §942, §953, §964, §973, §1215, §1288, §1296,
§1325, §1371, §1377. The amendment enumerates 25 sites. Master-plan-V2
applies the surgery in body. Punch-list items 25-49 (per-site).

### Orphan claim 4 — Tranche E "convergent pivot" not gated

§5.2 line 785: "Tranche E is the substrate centerpiece — the largest
single-tranche surface in the restart". The pivot is not gated by a
specific metric (e.g., "tranche E closes when X% of generated code
template-emits", or "tranche E closes when one grammar round-trips
through template + direct-projection without OpenFrame").

**Pros**: pivot identity correctly named.

**Cons**: the close gate is silent.

**Surgery**: tranche E close gate (E.W8) names: (a) all 9 grammars
template-emit; (b) one grammar passes future-grammar onboarding test
(yaml.bbnf added with two-surface ceremony); (c) samply gate fires.
Punch-list item 50.

### Orphan claim 5 — "9 per-grammar runtime hand-written files retire"

§5.2 line 785: "The hand-written 13K-LOC per-grammar runtime files
retire (5 trivial cohort × ~480 LOC = ~2400 LOC immediate; 4 specialised
cohort retain `specialised/` for extensions only)".

**Pros**: cohort split is enumerated.

**Cons**: under Amendment 01, the `specialised/` module is retracted
(Amendment 01 §"Tranche-set impact" line 112 — extension via host-fn
composition or `@host` directive, NOT via `specialised/` module). The
specialised cohort survives in name but the implementation surface
re-anchors. The 5 trivial × 480 LOC = 2 400 number stays valid; the 4
specialised count + size needs re-projection under Amendment 01.

**Surgery**: §5.2 line 785 strikes "specialised cohort retain
`specialised/` for extensions only"; replaces with "specialised cohort
retain extensions via host-fn composition (metadata or grammar-source
@host directive)". Punch-list item 51.

### Orphan claim 6 — Cross-pass crate-set reconciliation §3.2

§3.2 reconciliation item 9 (line 111) ratifies "direct name without
`bbnf-grammar-` prefix" for per-grammar declaration crates. Under
Amendment 01 the per-grammar crates retract; reconciliation item 9 is
*moot*. Master plan body doesn't strike it.

**Surgery**: §3.2 item 9 strikes; entire reconciliation table at lines
93-111 reduces by one row. Punch-list item 52.

### Orphan claim 7 — `pub use bbnf::*` retirement

§4.17 line 683: `bbnf/src/lib.rs` re-exports `pub use bbnf_parse::*;
pub use bbnf_codegen::*; pub use bbnf_runtime::*; pub use bbnf_grammar::
*;`. Glob re-exports are a pattern that Lock 14's verbatim text (line
60) explicitly criticises: "BBNF aggregator `pub use bbnf::*`". The
master plan reproduces the pattern.

**Pros**: aggregator is canonical.

**Cons**: glob re-exports leak unintended public API; Lock 14 prior-art
critique applies recursively.

**Challenge**: a glob-free aggregator requires explicit re-exports per
public symbol; that is a 50-100-line lib.rs that grows over time, but
discipline is correct.

**Surgery**: §4.17 lib.rs sketch replaces `pub use ... ::*;` with
specific re-exports of the canonical 10-15 public symbols per crate.
Punch-list item 53.

### Orphan deliverable 1 — `bbnf-runtime-template` per-grammar emit
audit

§3.1 ledger row 11 names `bbnf-runtime-template` with role
"grammar-agnostic runtime emitter". The audit gate verifying the
template *is* grammar-agnostic (no `match grammar` arms, no per-grammar
constants, no per-grammar templates) is missing from §11 and §13.

**Surgery**: tranche E close gate adds verification: `rg 'JsonValue|
CssL4Value|BbnfValue|GoogleSheetsValue' crates/bbnf-runtime-template/
src/` returns 0. Punch-list item 54.

### Lane 3 cohesion summary

| Orphan | Type | Severity | Surgery |
|---|---|---|---|
| 86.07% samply share | claim | medium | E.W6 measurement gate |
| 168 750 LOC distribution | claim | low | reconcile with CENSUS |
| 24-member workspace | claim | high | 25-site surgery |
| Tranche E close gate | deliverable | high | E.W8 gate enumeration |
| `specialised/` module retiral | claim | medium | re-anchor per Amendment 01 |
| §3.2 item 9 mootness | claim | low | strike |
| `pub use bbnf::*` reproduction | claim | medium | explicit re-exports |
| bbnf-runtime-template grammar-agnosticism | deliverable | high | E.W close grep gate |

**Lane 3 verdict: partial. KEEP=0, REINVENT=7, DISCARD=1** (orphan-claim
verdicts; total master-plan §1-§15 cohesion footprint is broader; this
sample illustrates the systemic issue — the master plan body trails
Amendment 01).

---

## §6 — Lane 4: SOTA Anchoring

Standard: every parse-throughput gate cites competitor + dataset +
platform. Non-throughput engineering gates do NOT claim Lock 8 honour.

The master plan's §11 row 8 (line 1290) cites "sonic-rs M1 Pro twitter
≤ 436 µs; lightning-css Bootstrap ≤ 4.16 ms; simdjson On-Demand 7 GB/s".
These are correct anchors per `restart/corpora/SOTA.md` §1 (lines
12-21).

### Per-perf-tranche audit

**Tranche F (optimiser pipeline; line 787)**: no parse-throughput gate
cited mid-tranche. The optimiser landing changes runtime characteristics
of all grammars — there must be a benchmark comparison F.W6 close vs
pre-F baseline.

**Surgery**: F.W6 close gate adds: "JSON twitter parse measured before
+ after Pratt + SIMD auto-detection lands; expected within ±5% of pre-F
or improvement; no regression > 5%". Punch-list item 55.

**Tranche G (slice-borrow API; line 789)**: no parse-throughput gate
cited mid-tranche. Slice-borrow vs arena vs owned API surfaces have
different alloc profiles; the three should be measured equivalently.

**Surgery**: G.W5 close gate adds: "twitter.json parsed via parse(input),
parse_in(input, &bump), parse_owned(input); slice-borrow ≤ sonic-rs's
436 µs (M1 Pro Criterion); arena variant within ±10%; owned variant
within +30%". Punch-list item 56.

**Tranche H (TS + WASM; line 791)**: parity-numbers expected, not
throughput. TS emit doesn't compete with sonic-rs (different language);
parity is correctness, not speed.

**Surgery**: H.W4 close gate adds: "TS-emitted JSON parser parses
twitter.json; emits typed-tree byte-for-byte equivalent to Rust-emit;
WASM-emit equivalent". Punch-list item 57.

**Tranche J (close; line 795)**: cites three SOTA numbers correctly;
parity matrix is the close gate. Honoured.

### Lock 8 honoured-by-tranche enumeration

| Tranche | Perf gate type | SOTA anchor | Verdict |
|---|---|---|---|
| A | none | n/a | n/a |
| B | none | n/a | n/a |
| C | none | n/a | n/a |
| D | none | n/a | n/a (Rust lowerer smoke is correctness, not perf) |
| E | structural-only | (samply gate per Lane 3 surgery) | REINVENT |
| F | mid-tranche | absent | REINVENT |
| G | mid-tranche | absent | REINVENT |
| H | parity | absent | REINVENT |
| I | none (publication) | n/a | n/a |
| J | close | sonic-rs + lightning-css + simdjson | KEEP |

**Lane 4 verdict: partial. KEEP=4 (n/a + J), REINVENT=4 (E, F, G, H),
DISCARD=0.**

---

## §7 — Lane 5: Grammar-Authoritative Discipline (Lock 14 deep dive)

Standard: zero `match grammar { Json => ... }` in proposed generic
crates; per-X tables for every "all grammars" claim; future-grammar
onboarding test verifying two-surface ceremony.

### Grep sweep results

`rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|
math' restart/audit/master-plan/MASTER-PLAN.md` returns 239 matches.

Classification:

| Class | Count | Examples |
|---|---:|---|
| Ratified per-grammar metadata block | 90 | `MASTER-PLAN.md:864-967` (workspace.metadata.bbnf.grammars) |
| Ratified workspace member name (under master plan §3) | 25 | `:79-87`, `:158-166` (member rows + ordering) |
| **FAULT** — per-grammar declaration crate naming (retracted by Amendment 01) | 28 | `:79-87` (column "Successor"), `:111`, `:158-166`, `:722`, `:766`, `:785`, `:876`, `:887`, `:898`, `:909`, `:920`, `:931`, `:942`, `:953`, `:964`, `:1296` |
| **FAULT** — `match grammar` arm or per-grammar dispatch in generic-crate code description | 0 | (none — master plan body honours by abstention) |
| Ratified audit / archaeology / SOTA cite | 12 | `:24` (sonic-rs M1 Pro twitter), `:1290` (close-gate citations), etc. |
| Ratified fixture / test cite | 8 | `:67-68` (bbnf-test-fixtures with json/css/sheets fixture names), `:701` (citm_catalog.json), etc. |
| Ratified host-fn or grammar-source cite | 17 | `:736` (host.rs `parse_hex_color`), `:870` (bbnf source path), etc. |
| Ratified specialised cohort designation | 4 | `:737, :889, :900, :911` (specialised cohort entries in §6.1 metadata) |
| Other (commentary mention, e.g., "JSON parser" as concept not implementation) | 55 | `:18` (existing commentary), `:1306-1319` (LOC distribution), etc. |

Master plan body fault count: **28 sites naming per-grammar declaration
crates** that Amendment 01 retracts. Per Lane 5 standard, these are all
faults requiring re-anchoring.

`rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|
GoogleSheets\w*\s*=>' restart/audit/master-plan/MASTER-PLAN.md` returns
**0 matches**. The master plan body does not propose `match grammar`
dispatch in any generic crate. Lock 14's "zero match arms" invariant is
honoured by master plan body authorship.

### Per-X table audit

The master plan §11 row 14 cites a verification command across 14
generic crates. The command lists:

```
crates/{bbnf-parse, bbnf-ir, bbnf-passes, path-core, path, path-ts,
        bbnf-codegen, bbnf-codegen-ir, bbnf-runtime, bbnf-runtime-template,
        bbnf-host, bbnf-error, bbnf-pipeline, bbnf-vm, bbnf-grammar}/src/
```

(15 crates, not 14 — `bbnf-vm` was added in §3.1 row 7 line 61 and
correctly appears here; the verbal claim at §11 row 14 says "14" but
the path list contains 15. Minor discrepancy.)

Under Amendment 01 the list extends with `bbnf-host-prims/src/`. 15 + 1
= 16 generic crates. Per-X table for "all generic crates" claim has
16 rows. Master plan body does not provide that table; it provides the
verification command (which is a subset).

**Surgery**: §11 row 14 verification reformats as a 16-row per-crate
table; each row shows the verification command output (expected: zero
hits). Punch-list item 58.

### Future-grammar onboarding test

Per Amendment 01 §"Settled position" lines 13-25:

> Adding a 10th grammar `yaml.bbnf`:
> - Drop `grammar/yaml/yaml.bbnf` into the source tree
> - Add a metadata block
> - Run `cargo xtask regen`
>
> No code change in any crate. No new crate.

The master plan §11 row 14 verification mentions "future-grammar
onboarding test" but with three-step ceremony including "creating its
declaration crate". Under Amendment 01, this is two-step.

**Walk-through against master plan + amendment**: invent `yaml.bbnf`.

Step 1: Drop `grammar/yaml/yaml.bbnf` into the source tree.
- Master plan §6.1 metadata block schema accommodates: yes, add
  `[workspace.metadata.bbnf.grammars.yaml]` block with `source_path =
  "grammar/yaml/yaml.bbnf"`.
- Master plan §3.3 workspace members list: would need `crates/yaml/`
  added — but Amendment 01 retracts. **Master plan body needs strike.**

Step 2: Add `[workspace.metadata.bbnf.grammars.yaml]` block.
- Master plan §6.1 has 9 grammar metadata blocks. yaml block adds:
  ```toml
  [workspace.metadata.bbnf.grammars.yaml]
  source_path = "grammar/yaml/yaml.bbnf"
  recognisers = []
  host_fns = []
  pratt_eligibility = "auto"
  simd_eligibility = "auto"
  output_dir = "crates/bbnf-runtime/src/grammars/yaml"
  features = []
  cohort = "trivial"
  ```
- Note: under Amendment 01, `declaration_crate` field is retracted;
  `output_dir` re-anchors to `bbnf-runtime/src/grammars/yaml/`.
- **Master plan body §6.1 metadata schema needs `declaration_crate`
  field stricken.**

Step 3 (under Amendment 01, which doesn't exist as a step): "Creating
its declaration crate" — not required.

**Verdict: master plan body fails the future-grammar onboarding test.**
Amendment 01 corrects; master plan body is V2-blocking.

**Per-X table for future-grammar onboarding test under master plan
body**:

| Surface | Master plan body says | Amendment 01 says | Reconciliation |
|---|---|---|---|
| (a) Grammar source file | drop `grammar/yaml/yaml.bbnf` | drop `grammar/yaml/yaml.bbnf` | both agree |
| (b) Workspace metadata block | add `[workspace.metadata.bbnf.grammars.yaml]` with `declaration_crate` field | add same block, no `declaration_crate` field | strike `declaration_crate` field everywhere |
| (c) Per-grammar declaration crate `crates/yaml/` | yes — required (master plan §3.3 ordering) | no — not required | strike crates/yaml/ creation step |
| (d) `cargo xtask regen` | yes | yes | both agree |
| (e) Code change in any other crate | none (Lock 14 invariant) | none (Lock 14 invariant) | both agree |

The two-surface ceremony (a + b + d) is the ratified test. Surface (c)
must strike from master plan body. Punch-list item 59.

### Per-grammar declaration crate site re-anchoring

Per Amendment 01 §"Master-plan sections superseded" lines 134-145, the
sites are enumerated. The hardening adversary verifies each:

| Site | Master plan content | Amendment-01 surgery | Punch # |
|---|---|---|---|
| `:13-17` | "per-grammar declaration crates carrying generated parser + template-emitted runtime + host functions" | "metadata-driven grammar onboarding" | 25 |
| `:38` | "9 per-grammar runtime directories that retire for template emission" | KEEP — wording correct (template-emitted subdirs) | 26 |
| `:79-89` | rows for `bbnf-meta`, `json`, `css-l4`, `google-sheets`, `bnf`, `csv`, `ebnf`, `css-pretty`, `math` | strike rows; add `bbnf-host-prims` row | 27 |
| `:93` | "9 are per-grammar declaration crates" | re-anchor to "0 per-grammar declaration crates; 9 template-emitted subdirs under bbnf-runtime/src/grammars/; 1 bbnf-host-prims" | 28 |
| `:99` | reconciliation item 3 names per-grammar host crates | strike; bbnf-host-prims is generic | 29 |
| `:111` | "Per-grammar declaration crate names" reconciliation item 9 | strike entire item | 30 |
| `:157` | Cargo.toml `[workspace] members =` 9 entries | strike 9 entries; add `bbnf-host-prims` | 31 |
| `:719` | "per-grammar benches live in per-grammar declaration crates" | re-anchor to "template-emitted from `bbnf-bench/`" | 32 |
| `:722` | "Per-grammar declaration crate skeleton (`crates/<grammar>/`)" | strike subsection; replace with "Per-grammar runtime subdir (`crates/bbnf-runtime/src/grammars/<name>/`)" | 33 |
| `:766` | "Per-grammar declaration crates + runtime template — bbnf-runtime-template, 9 declaration crates" | re-anchor: "`bbnf-host-prims` + `bbnf-runtime-template` + template-emitted per-grammar runtime subdirs" | 34 |
| `:785` | "9 per-grammar declaration crates scaffold" | re-anchor: "9 template-emitted runtime subdirs under bbnf-runtime/src/grammars/" | 35 |
| `:876, :887, :898, :909, :920, :931, :942, :953, :964` | per-grammar metadata `output_dir` | re-anchor each `output_dir` to `crates/bbnf-runtime/src/grammars/<name>/` | 36-44 |
| `:973` | "Each per-grammar declaration crate carries a uniform Cargo.toml" | strike entire §6.2 (no per-grammar Cargo.toml exists) | 45 |
| `:1215` | "Per-grammar declaration crates + runtime template" | re-anchor (matches `:766`) | 46 |
| `:1288` | Lock-6 verification "per per-grammar declaration crate `src/generated.rs`" | re-anchor to `crates/bbnf-runtime/src/grammars/<name>/generated.rs` | 47 |
| `:1296` | Lock-14 verification "creating its declaration crate" | strike step (already noted in Lane 1 Lock 14) | 48 |
| `:1325` | trajectory entry — "tranche A workspace restructure ... per-grammar declaration crate skeletons receive `generated.rs` as moved-not-modified" | re-anchor to "per-grammar runtime subdirs under bbnf-runtime/src/grammars/ receive `generated.rs` as moved-not-modified" | 49 |
| `:1371` | risk R9 "Per-grammar declaration crate Cargo.toml missing `[workspace.metadata.bbnf]` reference" | re-anchor: risk re-named "Per-grammar metadata block missing fields"; the per-grammar Cargo.toml doesn't exist | reframe |
| `:1377` | risk R15 "Sister-crate publication breaks workspace path-dep consumers when API freezes" | re-anchor: "downstream consumers (per-grammar runtime subdirs)" | reframe |

The 28-site count from the grep classification matches the
Amendment-01-enumerated 25 sites with 3 additional (R9, R15, §3.1
ledger column "Successor" rows). All 28 + reframings are surgeries.

### Lane 5 grammar-authoritative summary

| Site class | Count | Verdict | Surgery |
|---|---:|---|---|
| `match grammar` arms in generic crates | 0 | KEEP | n/a |
| Per-grammar declaration crate naming sites | 28 | DISCARD (×5) + REINVENT (×23) | re-anchor per Amendment 01 |
| Future-grammar onboarding test absent | 1 | REINVENT | gate as tranche-E close per Amendment 01 §item 5 |
| Per-X table for "all generic crates" Lock-14 verification | 1 | REINVENT | 16-row per-crate table |
| Specialised cohort `specialised/` module language | 4 | REINVENT | re-anchor to host-fn composition |

**Lane 5 verdict: violated. KEEP=2, REINVENT=28, DISCARD=5.** This is
the largest fault cohort in the audit; surgery here is the
single-largest reconciliation surgery for V2.

---

---

## §8 — Lane 6: Generated-Code Budget

Standard: per-tranche per-wave per-grammar LOC delta projection;
xtask regen-cycle budget; baselines stated. Per
`feedback_generated-size-budget`, every generator-touching wave declares
a per-grammar window; overflow blocks until the regression is traced.

The master plan §12 "Generated-LOC Trajectory" (lines 1302-1353) is the
apex enumeration. The hardening adversary holds it next to Amendment 01
(which retracts the per-grammar declaration crate distribution that the
trajectory implicitly assumes) and to the per-tranche-stub absence
(waves not yet drafted; budget granularity is tranche-level only).

### §8.1 — Pre-restart baseline reconciliation

`MASTER-PLAN.md:1306-1319` declares 168 750 LOC across 9 grammars per
CENSUS §10.5. The §3.1 ledger column "LOC budget" (lines 79-87) sums to
168 020 (`bbnf-meta` 22 000 + `json` 3 500 + `css-l4` 107 000 +
`google-sheets` 14 000 + `bnf` 3 300 + `csv` 1 700 + `ebnf` 7 650 +
`css-pretty` 9 000 + `math` 870 = 168 020). 730 LOC drift.

The drift is bounded (≈0.4%), but the trajectory's net-delta arithmetic
through tranches A-J takes the §3.1 column as authoritative, while
§12.1 takes CENSUS as authoritative. A V2 reconciliation either updates
§3.1 to CENSUS-exact figures (preferred — CENSUS is `wc -l` of the
generated tree) or annotates §3.1 cells as "post-amendment estimates;
CENSUS holds at restart-prelude commit".

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:79-87` | per-grammar LOC column sums to 168 020 | row-level estimates feeding tranche projections | enumerated per grammar | 730-LOC drift vs CENSUS §10.5 anchor at 168 750 | drift is small but trajectory arithmetic compounds; reconcile to CENSUS | REINVENT |
| `MASTER-PLAN.md:1306-1319` | §12.1 baseline cites CENSUS 168 750 | apex baseline | greppable | does not reconcile §3.1 drift inline | annotate the discrepancy | REINVENT |

### §8.2 — Tranche-by-tranche projection under Amendment 01

`MASTER-PLAN.md:1322-1334` enumerates entry / delta / exit per tranche
across A-J. Under Amendment 01 the receiving sites for the deltas
re-anchor: tranche A's "per-grammar declaration crate skeletons receive
`generated.rs` as moved-not-modified" (line 1325) becomes "per-grammar
runtime subdirs under `bbnf-runtime/src/grammars/<name>/` receive
`generated.rs` as moved-not-modified". The arithmetic survives; the
substrate names re-anchor.

| Tranche | §12.2 net delta | Substantive faults under Amendment 01 |
|---|---:|---|
| A | 0 | Entry path naming references retracted per-grammar crates; re-anchor (Lane 5 surgery) |
| B | 0 | Naming-canon scope; honoured |
| C | -2 000 | god-module SPLIT delta lacks per-split-target enumeration; "may reduce some generated wrapping" is unfalsifiable |
| D | -8 000 | Codegen IR contract delta lacks per-Emitter-method count baseline; "Emitter trait reshape (30 → 8-10 methods)" cited at §5.2 line 783 but not gated by a wave that measures the count delta |
| **E** | -13 000 | The trajectory centerpiece. Cites "5 trivial cohort × ~480 LOC = ~2400 LOC immediate" but Amendment 01 retracts the declaration crate destination; trajectory must restate as "~2400 LOC of hand-written runtime files retire from `crates/core/src/runtime/<grammar>/`; the template-emitted equivalents land at `crates/bbnf-runtime/src/grammars/<name>/runtime.rs`". Net delta survives; substrate names re-anchor. Plus "OpenFrame machinery (~5K LOC of stack-build code per CSS L4 + ~2K elsewhere)" — the per-grammar 5K + 2K decomposition is uncited; CENSUS does not enumerate OpenFrame LOC at this granularity |
| F | -2 500 | "Optimiser pipeline (Pratt + SIMD auto-detection) may consolidate some emit; cost-model output-piping retires some heuristic code" — wholly speculative; no per-recogniser baseline cited |
| G | +500 | "Slice-borrow API adds parse_in/parse_owned variants per grammar (~50 LOC × 9 = ~500)". Under Amendment 01 the per-grammar parse_in/parse_owned variants live under `crates/bbnf-runtime/src/grammars/<name>/runtime.rs` not in per-grammar crates. Arithmetic survives |
| H | +30 000 | "TS + WASM emitters land; per grammar emits TS source (~3K-50K LOC per grammar depending on grammar size; CSS L4 dominates)". Per-grammar 3K-50K range is one-decimal-of-magnitude; should be per-grammar enumeration not range |
| I | 0 | Honoured |
| J | 0 | Honoured |

End-state: ~173 750 LOC; ~143 750 Rust + ~30 000 TS. The 15% Rust-side
reduction claim at line 1340 is the substrate-identity win — survives
under Amendment 01 *if* the per-grammar receiving destinations
re-anchor. The 173 750 figure is ~1 000 LOC adrift from "168 750
baseline + cumulative deltas" (entries C through J net to +5 000; that
rounds to 173 750 from 168 750, internally consistent).

### §8.3 — Per-tranche generator regression budget

`MASTER-PLAN.md:1346-1352` enumerates D / E / F / H per-grammar + total
windows. Per `feedback_generated-size-budget` this is exactly the
required artefact. The four tranches that touch a generator (D's
codegen IR, E's runtime template, F's recogniser auto-detection, H's
TS+WASM emit) carry per-grammar windows.

| Tranche | Window | Total | Faults |
|---|---|---|---|
| D | ±10% per grammar | ±15 K LOC | window cited; gate-honouring wave silent. Should be D.W3 close gate: "post-D-wave-3, `find crates/bbnf-codegen/src/generated -name '*.rs' \| xargs wc -l \| awk '...'` confirms per-grammar within ±10% of pre-D" |
| E | ±15% per grammar | ±25 K LOC | window cited; wave gate silent. Should be E.W6 close gate: same shape, post-template-emit |
| F | ±5% per grammar | ±8 K LOC | tightest window; reasonable for optimiser pass |
| H | new emission, no prior baseline | bounded by per-grammar declared budget | "bounded by per-grammar declared budget" is circular — the per-grammar TS budget itself is the artefact. Master plan §3.1 LOC column carries no TS row; H's per-grammar window must be declared at H.W1 entry as the new-emission contract |

C, G, I, J carry no window in §12.3. C's god-module SPLITs are
generator-adjacent (per Lane 1 Lock 13 `crates/bbnf-passes/` SPLITs
indirectly affect generated emit through different IR shape); G's
slice-borrow variants are ~50 LOC × 9 grammars = ~500 LOC bounded by
construction; I + J don't touch generators. C should carry a window
("god-module SPLIT delta within ±5% per grammar; total ±5K"); G + I + J
honour by abstention.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1346-1352` | per-tranche generator window table | D / E / F / H window enumeration | matches `feedback_generated-size-budget` | window cited but enforcing wave silent; H's "bounded by declared budget" is circular | per-wave gate cells extend per Phase-4 hardening pattern | REINVENT |
| `MASTER-PLAN.md:1346-1352` | C tranche absent from window table | god-module SPLITs are generator-adjacent | tight scope justified | C's IR splits indirectly perturb emit | add C row "±5% per grammar; total ±5K" | REINVENT |

### §8.4 — xtask regen-cycle budget

The master plan body carries no `cargo xtask regen` wall-clock budget.
Pass C hardening surgery 16 (`HARDENING-PASS-A.md:879`) names "`cargo
xtask regen` wall ≤ 5 s on the in-tree 9-grammar set" as a Pass A
hard-gate; the master plan should adopt symmetrically. The 9-grammar
regen wall is the canonical Lock 6 friction surface for the dev loop.

Surgery: §6.3 (xtask Cargo.toml additions) extends with a hard-gate
declaration: `cargo xtask regen` wall ≤ 5 s; `cargo xtask regen
--check` (Lock 6 verification) wall ≤ 1 s. The hard-gate lands in
tranche A.W2 (xtask substrate lands) and tranche E.W6 (template-emit
gate) and tranche H.W4 (TS+WASM emission post-emit).

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1003-1030` | xtask Cargo.toml schema | bin enumeration | enumerated | no wall-clock gate for `regen` | iter-loop bottleneck per `feedback_iter-profile-always` | REINVENT |
| `MASTER-PLAN.md:1342-1352` | per-tranche budget table | regen-cycle budget absent | LOC budget present | wall-clock absent | dev-loop friction is wall-clock not LOC | REINVENT |

### §8.5 — Per-grammar projection truth under Amendment 01

The master plan's §12.2 net deltas implicitly bind per-grammar; under
Amendment 01 the per-grammar destinations are
`crates/bbnf-runtime/src/grammars/<name>/{generated.rs, runtime.rs}`,
template-emitted from a single source. The trajectory surface is
correct — emitted LOC accumulates at the named path — but the
*per-grammar accounting* is now template-driven: a single template
change ripples across all 9 grammars uniformly. The window cells
(§12.3) capture this as "per-grammar ±N%" but should clarify that the
template is the *single* surface, so a misregression manifests as ±N%
across all 9 simultaneously, not as one outlier. The window's
*detection* surface narrows.

Surgery: §12.3 prefixes a paragraph: "Under Amendment 01, every
template-emitted output is generated from a single template; per-grammar
window detection therefore manifests as a uniform delta across all 9
grammars. A per-grammar outlier (one grammar drifts beyond window while
others don't) signals template branching by grammar — itself a Lock 14
violation, since the template should be grammar-agnostic. Window failure
on uniform-delta vs outlier-grammar carries different diagnostic
implications."

### §8.6 — Friction-surface budget

`feedback_doc-style` and Lane 7's friction enumeration imply doc-LOC
budgets. Per HARDENING-PASS-A surgery 26 (line 868) the pointer-macro
cookbook is gated; per HARDENING-PASS-B surgery 24 (line 705) the
crate-split migration page is gated. Each lands as a `docs/howto/`
page; the master plan §8.2.5 enumerates new docs but no LOC budget.

| New doc | Path | Wave | Suggested LOC |
|---|---|---|---:|
| `docs/spec/SPEC.md` | C.W7 | new | ~1 500 |
| `docs/spec/architecture.md` | C.W7 | new | ~1 000 |
| `docs/spec/codegen.md` | C.W7 | new | ~600 |
| `docs/howto/migration/2026-restart.md` | C.W4 | new | ~300 |
| `docs/howto/cookbook/path-macro.md` | G.W3 | new | ~200 |
| `docs/howto/cookbook/lifetime-surfaces.md` | G.W2 | new | ~200 |
| `docs/howto/cookbook/path-crates.md` | C.W3 | new | ~150 |
| `docs/howto/cookbook/lsp-restart.md` | I.W2 | new | ~80 |
| `docs/errors/layout-lowering.md` | C.W4 | new | ~250 |
| `docs/optimizer/pratt-simd-detection.md` | F.W3 | new | ~300 |

Surgery: §8.2.5 extends with the LOC budget column; cookbook docs land
in tranche-G (proc-macro consumer) per the Lane 1 Lock-3 surgery, not
tranche-C.

### §8.7 — Lane 6 summary

| Sub-lane | Verdict | Surgery |
|---|---|---|
| §8.1 baseline | REINVENT | reconcile §3.1 sums to CENSUS 168 750 |
| §8.2 trajectory | REINVENT | re-anchor per-grammar destinations under Amendment 01; restate E delta enumeration |
| §8.3 generator window | REINVENT | per-wave gate cells; add C row |
| §8.4 xtask wall | REINVENT | regen ≤ 5 s; regen --check ≤ 1 s |
| §8.5 template-driven detection | REINVENT | clarify uniform-delta vs outlier diagnostic |
| §8.6 doc-LOC budget | REINVENT | per-cookbook LOC budget column in §8.2.5 |

**Lane 6 verdict: partial. KEEP=6 (§3.1 ledger column, §12.1 baseline,
§12.2 trajectory shape, §12.3 windows, §6.3 schema, §8.2.5 doc list —
all survive in substantive shape; surgery is annotation), REINVENT=4
(arithmetic reconciliation, per-wave gate enforcement, xtask wall,
doc-LOC), DISCARD=0.**

---

## §9 — Lane 7: Friction Forecast

Standard: enumerate user-API surfaces likely to confuse; verbatim error
messages; cookbook commitments. Per HARDENING.md §Lane 7, six surfaces
are mandatory: `pointer!` macro, `parse / parse_in / parse_owned`
lifetime API, layout-lowering errors, Pratt + SIMD auto-detection
misfire, crate-split migration, future-grammar onboarding.

The master plan body is silent on every six. The Lane 7 enumeration is
the largest single fault-cohort outside Lane 5 (Lock 14 reconciliation).

### §9.1 — `pointer!` macro syntax

**API surface**: `pointer!["a", "b", 1]` (sonic-rs-conventional). The
proc-macro shell at `crates/path/` validates the path against the
grammar at compile time. Mental model: the macro is grammar-aware;
typos surface as compile errors.

**Friction**:
1. **Rule-name typo**. User writes `pointer!["foo", "bar"]`; the
   grammar has no rule "foo". Compile fails.
2. **Wildcard syntax**. User wants iteration; sonic-rs uses `*` (e.g.
   `pointer!["array", *]`); the bbnf macro must resolve to a typed
   iterator (`impl Iterator<Item = TypedPath<G, T>>`).
3. **Index-vs-key disambiguation**. User writes `pointer!["1"]`
   (string) where they meant `pointer![1]` (index); type system
   surfaces.

**Required artefacts** (silent in master plan):

| Friction | Verbatim error message (commitment) | Cookbook |
|---|---|---|
| Rule-name typo | `error: rule 'foo' not found in grammar 'json'; did you mean 'foos'? (closest match by Levenshtein distance)` | `docs/howto/cookbook/path-macro.md` |
| Wildcard misuse | `error: pointer!["a", *] resolves to Iterator<Item = TypedPath<G, T>>; bind to a `let` rather than a single value` | same |
| Index/key confusion | `error: 'foo' is a string key but rule 'array' expects usize index` | same |

**Master plan coverage**: `MASTER-PLAN.md:789` cites "`pointer![...]`
macro per Lock 7" in tranche G commentary; no friction paragraph; no
verbatim error commitment; no cookbook gate.

**Surgery**: tranche G.W3 deliverable list extends with "`docs/howto/
cookbook/path-macro.md` lands; verbatim error messages committed in
§9.1 of the friction forecast". Punch-list item 60.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:789` | `pointer!` macro silent on friction | sonic-rs conv adopted | API named | no error verbatim | three error messages required | REINVENT |

### §9.2 — `parse / parse_in / parse_owned` lifetime API

**API surface**: per Lock 9, three surfaces over one parse impl. The
discriminant is the lifetime parameter:

```rust
fn parse<'i>(input: &'i str) -> Result<Document<'i>, BbnfError>
fn parse_in<'i, 'b>(input: &'i str, bump: &'b Bump) -> Result<Document<'i>, BbnfError>
fn parse_owned(input: &str) -> Result<OwnedDocument, BbnfError>
```

Mental model: borrow primary; bumpalo for arena workloads; owned for
lifetime escape. Three-line decision tree.

**Friction**:
1. **When-to-use confusion**. User reaches for `parse_owned` because
   ownership is more familiar; misses the slice-borrow primary.
2. **Bump-allocation ergonomics**. User wants `parse_in` but doesn't
   know how to allocate a `Bump` or how long it should live.
3. **Lifetime-error inscrutability**. `parse(input)` returns
   `Document<'i>`; user tries to store in `'static` field; rustc
   complains. The error is correct but unfamiliar.

**Required artefacts**:

| Friction | Verbatim error | Cookbook |
|---|---|---|
| When-to-use | n/a (ergonomic decision) | `docs/howto/cookbook/lifetime-surfaces.md` with three-line decision tree per `feedback_isomorphic-api` |
| Bump ergonomics | `error: parse_in requires a &Bump; allocate one with: let bump = bumpalo::Bump::new(); parser.parse_in(input, &bump)` | same cookbook |
| Lifetime escape | `error: cannot store Document<'i> in field of static type; use parse_owned(input) -> OwnedDocument instead, or extend the lifetime via Bump` | same |

**Master plan coverage**: `MASTER-PLAN.md:789` ratifies the API; no
friction paragraph; no cookbook gate; no verbatim error commitment.

**Surgery**: tranche G.W2 (per master plan §11 row 9) deliverable list
extends with `docs/howto/cookbook/lifetime-surfaces.md` and the three
verbatim error messages committed at G.W2 close. Punch-list item 61.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:789, 1291` | three-API surface friction silent | Lock 9 ratified | API enumerated | no when-to-use cookbook | error verbatim required | REINVENT |

### §9.3 — Layout-lowering errors

**API surface**: at codegen time the layout-lowering pass reports rules
without resolvable layouts. The error must guide the grammar author to
the fix.

**Friction**:
1. **Untyped rule**. Author writes `r = X | Y;` without `->`; the rule
   has no layout target.
2. **Mismatched layout**. Author writes `r -> Foo = X | Y;` where X and
   Y resolve to different layouts; the layout sink can't disambiguate.
3. **Missing host fn**. Author writes `r -> Color = #[a-f0-9]{6} ->
   parse_hex_color;` but `parse_hex_color` isn't declared in workspace
   metadata.

**Required artefacts**:

| Friction | Verbatim error | Cookbook |
|---|---|---|
| Untyped rule | `error: rule 'r' has no resolvable layout; add a `->` projection: `r -> SomeType = X \| Y;` or mark intentional erasure with `r -> () = X \| Y;`` | `docs/errors/layout-lowering.md` |
| Mismatched layout | `error: rule 'r' alternatives produce divergent layouts (X -> Foo, Y -> Bar); add explicit projections to a common layout or split the rule` | same |
| Missing host fn | `error: host fn 'parse_hex_color' referenced in rule 'r' but not declared in [workspace.metadata.bbnf.grammars.css-l4.host_fns]; add to metadata or define in `crates/bbnf-host-prims/src/composition_table.rs`` | same |

**Master plan coverage**: `MASTER-PLAN.md:781` cites "Lock 2 substantive
fold (TypeDesc → Layout)" at C.W2; the layout-lowering pass runs there
but its error messages are silent in the master plan body.

**Surgery**: tranche C.W2 (Lock 2 substantive fold) deliverable list
extends with `docs/errors/layout-lowering.md` and the three verbatim
error messages. Punch-list item 62.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:781, 1284` | layout-lowering errors silent | Lock 2 fold ratified | rename mechanical | no error commitment | three error messages required | REINVENT |

### §9.4 — Pratt + SIMD auto-detection misfire

**API surface**: per Lock 10, no `@pratt` / `@simd` directives. The
optimiser auto-detects; the metadata escape valves
(`pratt_eligibility = "auto" | "force" | "skip"`) are the only override
surface.

**Friction**:
1. **Misclassified rule (Pratt)**. Optimiser classifies a rule as
   Pratt-eligible; author disagrees; performance regresses.
2. **Misclassified rule (SIMD)**. Optimiser emits SIMD scanner; the
   leaf is too small; dispatch overhead dominates.
3. **Cost-model trace inscrutability**. Author wants to see *why* the
   classification happened.

**Required artefacts**:

| Friction | Diagnostic | Cookbook |
|---|---|---|
| Pratt misfire | `cargo xtask diag --rule r --grammar json` emits cost-model trace: rule shape, recogniser match, decision threshold | `docs/optimizer/pratt-simd-detection.md` |
| SIMD misfire | same trace shows `expected dispatch overhead (X ns) > expected SIMD payoff (Y ns); leaf classified as scalar` | same |
| Override escape | `[workspace.metadata.bbnf.grammars.<name>.pratt_eligibility] = "skip"` in `Cargo.toml`; `cargo xtask regen` re-emits | same |

**Master plan coverage**: `MASTER-PLAN.md:787` cites "Pratt + SIMD
auto-detection lands per Lock 10" at F.W3; metadata escape valves
ratified at §6.1 (lines 873-874, 884-885, etc.); no diagnostic surface
in master plan body.

**Surgery**: tranche F.W3 (auto-detection lands) deliverable list
extends with `docs/optimizer/pratt-simd-detection.md` cookbook +
`cargo xtask diag` subcommand. Punch-list item 63.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:787, 1292` | Pratt/SIMD misfire silent | Lock 10 ratified | metadata escape present | no diagnostic surface | cargo xtask diag required | REINVENT |

### §9.5 — Crate-split migration

**API surface**: post-restart, the legacy `crates/core/` god directory
fractures into 24 workspace members per Amendment 01. Consumer code
that imports `crates::core::path::Path` rebases as `crates::path::Path`
(via the bbnf aggregator's re-export) or `crates::path_core::Path`
(direct).

**Friction**:
1. **Import-path breakage**. User updates the workspace; old imports
   fail with `error: unresolved import 'core::path'`.
2. **Cargo.toml dep update**. User's downstream Cargo.toml references
   the `core` crate which no longer exists.
3. **Rename-discovery cost**. User runs `git log` looking for the
   rename; without `--diff-filter=R`, the rename appears as separate
   delete + add commits, breaking blame.

**Required artefacts**:

| Friction | Verbatim error / surgery | Migration page |
|---|---|---|
| Import-path breakage | `cargo check` produces `error: unresolved import 'core::path::Path'`; surgery: `sed -i 's/crates::core::path/crates::path/g' src/`; OR via the aggregator: `use bbnf::path::*;` | `docs/howto/migration/2026-restart.md` |
| Cargo.toml dep | `error: no matching package named 'core' found`; surgery: replace with `bbnf-parse`, `bbnf-codegen`, etc. per consumer-side dep map (table in migration page) | same |
| Rename discovery | `git log --diff-filter=R --name-only -- crates/` enumerates all renames | same |

**Master plan coverage**: `MASTER-PLAN.md:1077-1083` operational
sequence (commit-chain disposition) doesn't include migration-page
gating; §8.2.5 (line 1183) lists `docs/howto/migration/2026-restart.md`
as a C.W4 deliverable but without the verbatim sed-recipe content
commitment.

**Surgery**: §8.2.5 extends `2026-restart.md` row with "carries
verbatim sed-recipes per Lane 7 §9.5; carries Cargo.toml consumer-side
dep map per Lane 7 §9.5". Punch-list item 64.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1183` | migration page gated | C.W4 wave receives | wave assigned | no sed-recipe content | three migration tables required | REINVENT |

### §9.6 — Future-grammar onboarding (Lock 14 user perspective)

**API surface**: per Amendment 01, two-step ceremony:
1. Drop `grammar/yaml/yaml.bbnf`
2. Add `[workspace.metadata.bbnf.grammars.yaml]` block

Then `cargo xtask regen` produces a working parser.

**Friction**:
1. **Metadata schema**. What fields are required? What are valid
   values? Author needs a complete reference.
2. **Source-path conventions**. Where does the source file live? What
   `.bbnf` syntax extensions are accepted?
3. **Host-fn declaration**. If yaml needs a custom host fn (e.g.
   `parse_yaml_anchor`), where is it declared?
4. **Test-fixture conventions**. The grammar-agnostic test harness
   iterates `crates/bbnf-test-fixtures/<name>/`; what files must
   appear?
5. **Onboarding diagnostic**. After the two-step ceremony, what does
   the user see? `cargo xtask regen` output; success message; failure
   surfaces.

**Required artefacts**:

| Friction | Reference | Cookbook |
|---|---|---|
| Metadata schema | `docs/spec/codegen.md` §"workspace.metadata.bbnf.grammars" — full schema | `docs/spec/codegen.md` |
| Source-path conv | same — schema field `source_path` documented | same |
| Host-fn declare | `bbnf-host-prims` composition table OR `@host` directive in grammar source per Amendment 01 §"Host-fn implementations" lines 50-54 | `docs/howto/cookbook/host-fn-composition.md` (NEW; not in master plan §8.2.5) |
| Fixture conv | `docs/howto/cookbook/test-fixtures.md` (NEW) | same |
| Diagnostic | `cargo xtask regen` emits per-grammar success: `regen: yaml -> crates/bbnf-runtime/src/grammars/yaml/{generated.rs, runtime.rs} (+8500 LOC)` | n/a (CLI output) |

**Master plan coverage**: `MASTER-PLAN.md:1296` cites "future-grammar
onboarding test" but with three-step ceremony (declaration crate
included, retracted by Amendment 01); §5.2 line 785 commentary on
tranche E does not gate the onboarding test as a tranche-E close
deliverable.

**Surgery**: tranche E.W8 (close) gate adds: (a) yaml.bbnf two-step
onboarding test passes; (b) `docs/howto/cookbook/host-fn-composition.md`
+ `docs/howto/cookbook/test-fixtures.md` land; (c) `cargo xtask regen`
diagnostic output committed. Punch-list items 65-66.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1296, 785` | onboarding test silent on user friction | Amendment 01 grants two-step | ceremony declared | no per-step user-friction surface | five friction sub-surfaces require cookbooks | REINVENT |

### §9.7 — Lane 7 summary

| Friction surface | Master plan coverage | Required artefacts | Verdict |
|---|---|---|---|
| `pointer!` macro | silent | path-macro cookbook + 3 error messages | REINVENT |
| lifetime API | silent | lifetime-surfaces cookbook + 3 error messages | REINVENT |
| layout-lowering errors | silent | layout-lowering page + 3 error messages | REINVENT |
| Pratt + SIMD misfire | silent | pratt-simd cookbook + diag CLI | REINVENT |
| crate-split migration | partially gated | migration page + sed recipes + dep map | REINVENT |
| future-grammar onboarding | three-step ceremony cited (per Amendment 01 retraction); user friction silent | onboarding cookbook + 5 sub-friction artefacts | REINVENT |

**Lane 7 verdict: violated. KEEP=0, REINVENT=6, DISCARD=0.** Per the
HARDENING.md §Lane 7 standard ("Where will users / grammar authors hit
the proposed API and not understand it?"), the master plan body is
silent on every named surface. Surgery is the addition of seven
cookbook-class artefacts plus ~15 verbatim error messages.

---

## §10 — Lane 8: Carry & Deferral Audit

Standard: every "deferred to" / "carries to" / "future" / "TBD" /
"user adjudicates" must name (a) receiver, (b) blocker, (c) receiving
gate. Faults: any without all three. Per HARDENING.md §Lane 8 D5
("carry-blindness") this is the lane that catches Era V's failure mode.

Walk every "deferred", "carry", "future", "TBD", "synthesizer
adjudicates", "user-gated", "Pass C is silent on this" in the master
plan + Amendment 01.

### §10.1 — Master plan deferrals enumeration

`rg -n 'deferred?|carry|future|TBD|synthesizer adjudicates|user-gated|silent on' restart/audit/master-plan/MASTER-PLAN.md` produces 27 matches. Classified:

| Class | Count | Sample |
|---|---:|---|
| Carry FROM/TO between tranches (§5.1 column) | 18 | line 762 "Carry FROM: (none)" |
| Substantive defer (named in body prose) | 6 | line 73 (`bbnf-cli` defer to 1.0); line 105 (`bbnf-py` defer post-1.0); line 109 ("synthesizer adjudicates"); line 1101 ("user-gated"); line 1170 ("Pass C is silent on this"); line 1264 ("future maintainers may delete at 1.0") |
| Deferral ratifying prior pass | 3 | line 1040 ("ratifies Pass C's recommendation outright") |

The 18 tranche-level carries are honoured by the §5.1 master tranche
table — each tranche names its FROM and TO columns; consumers verified
in Lane 2.

The 3 prior-pass-ratifying deferrals are honoured (no defer; the
prior-pass adjudication is reproduced).

The 6 substantive defers are the audit subject below.

### §10.2 — Substantive defer audit

#### Defer 1 — `bbnf-cli` to 1.0 release

`MASTER-PLAN.md:73, 105, 155, 1183` ("DEFERRED to 1.0"). Per HARDENING-PASS-C.md §10.1 (Pass C source-level defer at PASS-C.md:191-193) the receiver-blocker-gate triple was identified as incomplete; the master plan body inherits this incompleteness.

| Surface | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Master plan §3.1 row 19 (line 73) | unnamed | implicit (stable user-facing API) | unnamed (1.0 is operational, not a tranche gate) |
| Master plan §5.2 + §5.1 | not in tranche set A-J | n/a | n/a |
| Master plan §3.2 item 6 (line 105) | "tranche-J deliverable or post-restart" | implicit | unnamed |

**Fault**: triple incomplete on every surface. Per HARDENING-PASS-C surgery 19 (line 753) the resolution is "tranche post-J (1.0-release tranche, not yet drafted in restart letter set); blocker = stable user-facing API; receiving gate = parse_in/parse_owned/parse + Document::get<T>(path) + pointer![…] all reach API freeze".

**Surgery**: master plan §3.1 row 19 surgery extends column "Notes" to read "DEFERRED to post-tranche-J 1.0-release tranche; blocker = API freeze (parse, parse_in, parse_owned, Document::get<T>(path), pointer![…]); receiving gate = post-J tranche W0 declares CLI surface, W1 implements". §3.2 item 6 (line 105) re-anchors symmetrically. §5.1 receives a footnote: "Tranche K (post-restart 1.0-release) is anticipated; not drafted in this set". Punch-list item 67.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:73, 105, 155` | `bbnf-cli` defer to 1.0 | clear scope-narrowing | bounded | triple incomplete | post-J tranche K | REINVENT |

#### Defer 2 — `bbnf-py` post-1.0

`MASTER-PLAN.md:107` ("DEFERRED post-1.0; speculative"). Per HARDENING-PASS-C surgery 21 (line 755):

| Surface | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Master plan §3.2 item 7 (line 107) | "post-1.0" (vague) | "no Python consumer" (non-action) | unnamed |

**Fault**: receiver vague; blocker is a non-action (if no consumer materialises, defer never resolves); gate absent.

**Surgery**: §3.2 item 7 surgery: "DEFERRED post-1.0. Triggering condition: a Python consumer materialises (csc411 CSP-solver consumes bbnf grammar tooling, OR a downstream Python user surfaces). Receiver: a yet-to-name post-1.0 tranche. Receiving gate: that tranche's W0 declares the Python binding API shape; W1 implements PyO3 cdylib OR subprocess-CLI binding (decision per timing)." Punch-list item 68.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:107` | `bbnf-py` defer post-1.0 | speculative-work framing correct | conservative | triple absent | triggering condition required | REINVENT |

#### Defer 3 — Sister-crate disposition (parse-that, bbnf-regex)

`MASTER-PLAN.md:109` ("synthesizer adjudicates submodule + workspace-member is preferred"). The adjudication *resolves* in this very paragraph — "submodule + workspace-member is preferred because it preserves the sibling-repo provenance". The receiver is the master plan itself; the blocker is the precedent at `docs/precepts/CONSUMING.md`; the gate is tranche A.W2 ("parse-that disposition per Pass A Proposal 4 (submodule-as-workspace-member is the synthesizer's adjudication)" per line 793).

**Verdict**: honoured. The triple resolves inline at `MASTER-PLAN.md:109` with receiver = master plan; blocker = precedent; gate = tranche A.W2.

**Substantive concern**: the operational sequence at §7.2 (lines 1056-1093) lists 8 prelude commits but does NOT include `git submodule add` operations for parse-that or bbnf-regex. The synthesizer ratifies the disposition but doesn't replicate the operational step. Per Lane 1 Lock 11 surgery, punch-list item 11 already names this. Cross-referenced from Lane 8.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:109, 793` | sister-crate submodule disposition | resolves inline | adjudication ratified | operational step missing in §7.2 | already in punch-list 11 | KEEP (cross-ref) |

#### Defer 4 — Cutover decision (Option A vs Option B at §7.2)

`MASTER-PLAN.md:1087-1093` ("Cutover decision (USER):"). Per HARDENING-PASS-C.md §10.8:

| Surface | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Master plan §7.2 step 7 (line 1087-1093) | USER | hardening returns *ready* | post-hardening cutover |

**Verdict**: honoured. The triple is named — USER as receiver is per HARDENING.md prompt convention; blocker is hardening; gate is post-hardening master-plan-V2 + tranche-A-stub readiness.

**Substantive concern**: §7.2 names Option A (force-push) and Option B (keep both); §7.3 line 1101 expresses the synthesizer's preference (Option A). The user has the final adjudication. Honoured per HARDENING.md §Methodology — "user adjudicates" is acceptable when the receiver-blocker-gate triple is named.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1087-1093, 1101` | cutover Option A vs B | synthesizer prefers A | dual option named | none — receiver+blocker+gate triple complete | honoured | KEEP |

#### Defer 5 — Pass C silent on `docs/restart/` location

`MASTER-PLAN.md:1170` ("`docs/restart/` ... `docs/process/restart/` (or kept if user prefers; Pass C is silent on this)"). Per HARDENING-PASS-C surgery: the location is silent in Pass C; the master plan inherits the silence and OR-disposition.

| Surface | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Master plan §8.2.4 (line 1170) | OR ("user prefers") | "Pass C is silent" | unnamed |

**Fault**: OR-disposition without explicit adjudication. Per Lane 9 §11.1 the OR-disposition is a quick-solution-shaped offering ("rename OR merge" pattern repeats here as "relocate OR keep").

**Surgery**: §8.2.4 line 1170 surgery: "**`docs/restart/`**: relocates to `docs/process/restart/` per Lock 13 (cohesion with `docs/process/instructions/`, `docs/process/tranches/`). The OR-disposition retracts." Punch-list item 69.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1170` | docs/restart/ OR-disposition | non-adjudication | scope-limited | OR-disposition is quick-solution-shaped | relocate per Lock 13 | REINVENT |

#### Defer 6 — Future maintainers delete archive at 1.0

`MASTER-PLAN.md:1259, 1264` ("Disposition: keep-as-historical archive (Pass C Agent 4 §2.3); future maintainers may delete at 1.0 cut.").

| Surface | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Master plan §10.2 (line 1259) | "future maintainers" | "1.0 cut" | unnamed |

**Verdict**: borderline. The receiver is a cohort, the blocker is a release event, the gate is implicit ("at 1.0 cut" is a calendar, not a wave). Per HARDENING.md §Lane 8 standard, this fails the "real, drafted tranche" test (Pass C HARDENING-prompt §10 lane standard).

**Surgery**: §10.2 line 1264 surgery: "**Disposition**: keep-as-historical archive (Pass C Agent 4 §2.3); deletion permissible at post-J 1.0-release tranche W0 if `archive/{ser, gorgeous}/` content is fully reproducible from sister-repo provenance (`gorgeous-external`, `pprint-external`)." Punch-list item 70.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1259, 1264` | "future maintainers may delete" | calendar-anchored | archaeology preserved | gate implicit | post-J 1.0-release tranche W0 | REINVENT |

### §10.3 — Tranche-level carry verification

The §5.1 master tranche table (`MASTER-PLAN.md:760-771`) names FROM/TO
per tranche:

| Tranche | Carry FROM | Carry TO | FROM verified in Lane 2 | TO verified in Lane 2 |
|---|---|---|:-:|:-:|
| A | (none) | B, C | ✓ | B inherits errors+pipeline; C inherits skeletons |
| B | A | C, D | ✓ | C consumes bbnf-error; D consumes bbnf-pipeline |
| C | A, B | D, E | ✓ | D consumes bbnf-grammar+bbnf-parse+bbnf-ir; E consumes bbnf-passes |
| D | C | E, F | ✓ | E consumes bbnf-codegen-ir; F consumes Emitter trait |
| E | C, D | F, G, H | ✓ | F consumes runtime template; G consumes runtime; H consumes IR + runtime template |
| F | C, D, E | G, J | ✓ | G consumes optimiser-emitted runtime; J consumes SOTA-anchored bench |
| G | E, F | H, J | ✓ | H consumes user-facing API; J consumes parity matrix |
| H | D, E, G | I, J | ✗ (per Lane 2: H carry FROM should be D, E only; G is wrong) | I consumes published TS+WASM; J consumes parity |
| I | A, F | J | ✓ | J consumes published egraph+csp+regex |
| J | All | (close) | ✓ | n/a |

One tranche-level carry fault: H carries FROM G but Lane 2 §"Cross-tranche dependency-DAG" identifies the carry-FROM should be D + E only. Already in punch-list item 22.

### §10.4 — Carry-tag receivers under Amendment 01

Amendment 01 introduces new substrate (`bbnf-host-prims`,
`bbnf-runtime-template`, template-emitted subdirs) and retracts old
substrate (per-grammar declaration crates). Every carry-tag in the
master plan body that touches the retracted substrate must re-anchor.

| Carry source | Original receiver | Amendment-01 receiver | Punch-list |
|---|---|---|---|
| §5.2 line 785 ("9 declaration crates scaffold") | tranche E | tranche E (re-anchored to template-emitted subdirs) | already item 35 |
| §11 row 14 verification (line 1296) | A.W3 + E.W6 | A.W3 + E.W6 (re-anchored verification command) | already items 13-15 |
| §13 risk R9 (line 1371) | tranche E | tranche E (re-anchored: "Per-grammar metadata block missing fields") | already (re-frame) |
| §7.2 step 4 commit 8 (line 1080) | "Master Plan + per-pass restart audit synthesis" | adds Amendment 01 as commit 8 sub-artefact | punch-list item 71 |

The §7.2 prelude commit 8 names the master plan but is silent on
Amendment 01. The amendment is authoritative; the prelude commit must
include it. Surgery: §7.2 commit 8 verbiage extends to "Master Plan +
Amendment 01 + per-pass restart audit synthesis".

### §10.5 — Stage-2 hardening receiver

`MASTER-PLAN.md:7` ("After commit, the user invokes `docs/restart/HARDENING.md` against this master plan; if hardening returns *ready to execute*, ..."). Per `restart/prompts/HARDENING.md` §6 the path is `restart/prompts/HARDENING.md`, not `docs/restart/HARDENING.md`. The master plan body's path reference is stale.

Per the same prompt §7 ("After Stage 1 commits, a **Stage 2** hardening pass invoked from `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md`"), Stage 2 is the receiver of Stage 1 outputs. The master plan body is silent on Stage 2.

| Surface | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Master plan §1 line 7 | "user invokes HARDENING.md" | post-master-plan commit | "ready to execute" |
| Stage 2 hardening | per `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md` (not cited in master plan body) | Stage 1 commits | Stage 2 verdict |

**Fault**: master plan body cites stale path (`docs/restart/HARDENING.md` retired in favour of `restart/prompts/HARDENING.md`); silent on Stage 2 receiver.

**Surgery**: §1 line 7 + §15 line 1414 update path references; §15 closing posture extends to name Stage 2 ("Stage 1 hardening commits, Stage 2 fires per `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md`; both must pass before tranche drafting opens"). Punch-list items 72-73.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:7, 1414` | hardening path stale | hardening invoked | named | path retired | re-anchor | REINVENT |
| `MASTER-PLAN.md:1414` | Stage 2 silent | Stage 1 ratified-pending-hardening | scope correct | Stage 2 receiver missing | name in §15 | REINVENT |

### §10.6 — Lane 8 summary

| Defer | Verdict | Surgery |
|---|---|---|
| 1 — bbnf-cli to 1.0 | REINVENT | §3.1 row 19 + §3.2 item 6 + §5.1 footnote: post-J tranche K |
| 2 — bbnf-py post-1.0 | REINVENT | §3.2 item 7: triggering condition |
| 3 — sister-crate submodule | KEEP (cross-ref to Lane 1 punch 11) | n/a |
| 4 — cutover Option A vs B | KEEP | n/a |
| 5 — docs/restart/ OR-disposition | REINVENT | §8.2.4: relocate per Lock 13 |
| 6 — archive at 1.0 | REINVENT | §10.2: post-J 1.0-release tranche W0 gate |
| Tranche-level H carry-FROM | REINVENT (cross-ref Lane 2 punch 22) | n/a |
| Amendment 01 receivers | REINVENT (cross-ref Lane 5) | n/a |
| §7.2 commit 8 prelude | REINVENT | name Amendment 01 in commit |
| Stage 2 hardening receiver | REINVENT | §15 names Stage 2 |

**Lane 8 verdict: partial. KEEP=5 (defer 3, defer 4, tranche-level
carries verified by Lane 2 except H, Amendment 01 receivers tracked in
Lane 5, the prior-pass-ratifying deferrals), REINVENT=8, DISCARD=1**
(the bbnf-py "speculative no consumer" framing surface — the *speculative*
disposition survives, but the framing as a non-action retires; replaced
by "triggering condition").

---

## §11 — Lane 9: Greenfield Discipline

Standard: no quick solutions; no workarounds; no legacy survives
uncontested; idiomatic gestalt; architectural transpositions for
elegance / simplicity / performance.

The greenfield mandate is summarised at `MASTER-PLAN.md:19` ("The
greenfield mandate carries through: no quick solutions, no workarounds,
idiomatic gestalt approaches, architectural transpositions in the sake
of elegance / simplicity / performance, no legacy code surviving
uncontested.").

The substantive shape — 24-member workspace under Amendment 01;
template-emit + direct-projection + Emitter coarsening as the
convergent pivot at tranche E; Lock 12 archive ceremony as blocking
precondition; commit-chain Option 3 ratification; docs re-do six waves
— honours the discipline. The faults below are localised within
otherwise-greenfield-compliant substrate.

### §11.1 — No quick solutions

#### §11.1.1 — Per-grammar declaration crate proliferation (the largest)

`MASTER-PLAN.md:79-87` enumerates 9 per-grammar declaration crates as the Lock-14 redress. Per Amendment 01 §"Premise" (line 9) this is overfitting — Lock 14 names per-grammar declaration crates as an *optional* escape hatch, not a default. The master plan elevated the escape valve to a mandatory 9-crate footprint.

This is *the* greenfield-discipline fault on the plan. It is also already retracted by Amendment 01. The master plan body has not been updated; the V2 re-issue applies the retraction across all 28 sites enumerated in Lane 5.

**Verdict**: REINVENT (already enumerated in Lane 5 punch list). Cross-referenced.

#### §11.1.2 — `specialised/` cohort module retention

`MASTER-PLAN.md:737, 750, 785` ("`specialised/` for hand-written
extensions"). Under Amendment 01 the per-grammar specialised cohort
extends via *host-fn composition* (declarative metadata or `@host`
directive), not a `specialised/` module. The master plan body's
`specialised/` retention is a quick-solution shape: it admits
"hand-written extensions" as a fallback rather than insisting on
metadata-declared composition for every variant.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:737, 750, 785` | `specialised/` cohort module | hand-written extension surface | escape hatch present | quick-solution shape; Amendment 01 retracts | host-fn composition | REINVENT |

(Already in Lane 3 surgery — punch-list item 51.)

#### §11.1.3 — OR-disposition language

`MASTER-PLAN.md:1170` ("`docs/restart/` ... `docs/process/restart/`
(or kept if user prefers; Pass C is silent on this)"). Already audited
in Lane 8 §10.2 Defer 5. The OR-disposition is a quick-solution-shaped
offering — it leaves the path of least resistance ("keep") open. Per
HARDENING.md §Lane 9, no quick solutions — the master plan should
adjudicate.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:1170` | OR-disposition for `docs/restart/` | scope-limited non-decision | bounded | quick-solution-shaped | adjudicate per Lock 13 | REINVENT |

(Already in Lane 8 punch-list item 69.)

#### §11.1.4 — `pub use` glob re-exports

`MASTER-PLAN.md:54, 683` (`bbnf` aggregator's `pub use bbnf_parse::*; pub use bbnf_codegen::*; pub use bbnf_runtime::*; pub use bbnf_grammar::*;`). Glob re-exports are a quick-solution shape — they delegate API surface management to compiler resolution and leak unintended symbols. Lock 14's verbatim text (line 60) explicitly names "BBNF aggregator `pub use bbnf::*`" as a failure-mode pattern.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:54, 683` | aggregator glob re-exports | minimal aggregator footprint | terse | leaks unintended API; Lock 14 prior-art critique | explicit re-exports | REINVENT |

(Already in Lane 3 punch-list item 53.)

### §11.2 — No workarounds

#### §11.2.1 — `Box::leak` in grammar lifetime escape

`MASTER-PLAN.md` body does not directly cite `Box::leak`, but the inherited Pass A surgery (per `HARDENING-PASS-A.md:39, 405-407`) names `Box::leak` at `crates/core/src/grammar/mod.rs:57` as a workaround for the lifetime question. The Pass A hardening adjudication is `parse_grammar_in(input, &bump)` per Lock 9 escape hatch; deprecate the leaking entry to `parse_grammar_owned`. Master plan body must reflect this adjudication in tranche A.W4 deliverable list.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:777` (tranche A) | Box::leak retirement silent | Pass A adjudication exists | inherited from Pass A | not in master plan body | name in tranche A.W4 | REINVENT |

#### §11.2.2 — Wildcard `@debug` strip-prefix workaround

Per HARDENING-PASS-A surgery: `crates/core/src/grammar/host.rs:387` defensive wildcard strip-prefix. The Pass A hardening adjudication is "FAIL-EXPLICIT" (per HARDENING-PASS-A.md:794-795 W6 punch-list 34). Master plan body must reflect in tranche C.W6 deliverables.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:781` (tranche C) | wildcard @debug strip-prefix retirement silent | Pass A adjudication exists | inherited | not in master plan body | name in tranche C.W6 | REINVENT |

#### §11.2.3 — Defensive `unreachable!()` fallback

Per HARDENING-PASS-A: `crates/core/src/lower/value_expr/simple_kinds.rs:185` defensive fallback. Adjudication: `unreachable!()` or fix upstream (W6 punch-list 38). Master plan body must reflect.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:781` (tranche C) | defensive fallback retirement silent | Pass A adjudication exists | inherited | not in master plan body | name in tranche C.W6 | REINVENT |

Three workarounds inherited from Pass A; master plan body silent on each. Surgery: tranche A.W4 + tranche C.W6 deliverable lists extend to name Box::leak retiral, wildcard-prefix retiral, defensive-fallback retiral. Punch-list items 74-76.

### §11.3 — No legacy code survives uncontested

`MASTER-PLAN.md:19` claims "no legacy code surviving uncontested";
§2 verdict ledger (lines 27-32) tabulates ~570 files across five
buckets (KEEP-OUTRIGHT, KEEP-MODIFY, ABROGATE-DELETE, ABROGATE-MOVE,
ABROGATE-REPLACE). Every file has a verdict.

**Honoured.**

Substantive concern: the verdict ledger sums to ~570; the workspace
member relocations at §3.1 ledger sum to 33 (or 24 under Amendment 01)
crate-level dispositions. The file-level verdicts terminate at the
crate level for some entries (whole crates retire) but the per-file
disposition is in the per-pass synthesis (Pass A / B / C), not in the
master plan body. This is appropriate — the master plan composes
pass-level findings.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:27-32` | five-bucket ledger | every file contested | enumerated | per-file verdicts in pass syntheses, not master plan | appropriate composition | KEEP |

### §11.4 — Idiomatic gestalt approaches

`MASTER-PLAN.md:13-19` claims "the convergent pivot — Lock 1 (tape
dead), Lock 13 (no god directories), and Lock 14 (full grammar
generalisation) retire as one architectural movement". This is gestalt
— three locks retire through one substrate (template-emit +
direct-projection + Emitter coarsening), not three separate patches.

Per HARDENING.md §Lane 9 ("Rust-idiomatic; sonic-rs / lightning-css /
simdjson cohesion the standard"):

| Cohesion exemplar | Crates | Workspace shape | Master plan posture |
|---|---:|---|---|
| sonic-rs | 1 | `src/{parser, value, serde, util, lazyvalue, ...}` | bbnf has 24 (under Amendment 01); 33 in master plan body |
| lightning-css | 1 | `src/{rules, properties, selector, declaration, traits, ...}` | bbnf has 24 |
| simdjson | 1 (single repo) | `{dom, ondemand, generic, ...}` | bbnf has 24 |

The 24-member shape (under Amendment 01) is *farther* from one-crate cohesion than sonic-rs etc., but workspace decomposition is mandated by Lock 13 ("4-10 children at the next level; uniform sub-API"). The 24-member shape honours Lock 13's per-directory cohesion at the workspace level — every member names one concern; siblings are peer partitions; sub-APIs uniform.

The 33-member shape (master plan body, pre-Amendment-01) was *overfit* — per Amendment 01 §"Premise", the 9 per-grammar declaration crates are escape-valve overfitting. 33 → 24 brings the workspace closer to sonic-rs cohesion *while honouring* Lock 13.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:50-89` | 33 → 24 workspace shape | sonic-rs-class cohesion | pre-Amendment overfit | Amendment 01 retracts | already in Lane 5 surgery | REINVENT |

(Already in Lane 5 punch list.)

### §11.5 — Architectural transpositions

`MASTER-PLAN.md:13` claims "architectural transposition required, not
patch". The pivot at tranche E (Lock 1 + Lock 13 + Lock 14 retire as
one) is a transposition; the Emitter trait collapse (30 → 8-10 methods)
at tranche D is a transposition; the path-crate triplet (Lock 7) at
tranches A + C is a transposition; the sister-crate path-deps + publish
sequence (Lock 11) at tranches A + I is a transposition.

Six transpositions enumerated (Pass A's Proposals 1-6 per HARDENING-PASS-A §11.5; Pass B's per-grammar declaration crate proliferation per HARDENING-PASS-B §11.5 retracted by Amendment 01; Pass C's bbnf-language-server consolidation per HARDENING-PASS-C §11.5).

**Honoured.**

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `MASTER-PLAN.md:13-17` | architectural-transposition framing | pivot at tranche E | gestalt | substrate-name re-anchor under Amendment 01 | retraction is naming, not substance | KEEP |

### §11.6 — Greenfield-discipline summary

| Sub-lane | Substrate | Verdict | Surgery |
|---|---|---|---|
| §11.1.1 per-grammar declaration crate proliferation | 9-crate footprint | REINVENT (Lane 5 punch list) | retract per Amendment 01 |
| §11.1.2 specialised/ cohort module | hand-written extension | REINVENT (Lane 3 punch 51) | host-fn composition |
| §11.1.3 OR-disposition language | docs/restart/ ambiguity | REINVENT (Lane 8 punch 69) | adjudicate per Lock 13 |
| §11.1.4 pub use glob re-exports | aggregator footprint | REINVENT (Lane 3 punch 53) | explicit re-exports |
| §11.2.1 Box::leak retiral silent | grammar lifetime escape | REINVENT (punch 74) | name in tranche A.W4 |
| §11.2.2 wildcard @debug retiral silent | host fn defensive | REINVENT (punch 75) | name in tranche C.W6 |
| §11.2.3 defensive fallback retiral silent | lower simple_kinds | REINVENT (punch 76) | name in tranche C.W6 |
| §11.3 no legacy uncontested | five-bucket ledger | KEEP | n/a |
| §11.4 idiomatic gestalt | 24-member workspace | REINVENT (Lane 5 punch list) | re-anchor per Amendment 01 |
| §11.5 architectural transpositions | six transpositions enumerated | KEEP | n/a |

**Lane 9 verdict: partial. KEEP=2 (§11.3 + §11.5 honoured),
REINVENT=8 (every quick-solution surface re-anchors per cited
sub-lane), DISCARD=0.** All eight REINVENT surgeries cross-reference to
prior-lane punch-list entries; Lane 9 produces no net-new punch-list
items beyond cross-references plus the three Pass-A-inherited
workaround retirements (74-76).

---

## §12 — Punch list

Ordered surgical edits for the V2 re-issue. Per entry: item number,
target file:line, verbatim edit (or surgery description), source
verdict (REINVENT / DISCARD), owner, estimated surgery scope, lane(s)
producing.

The list draws from Lanes 1-9; entries cross-reference both the
master-plan body sites that re-anchor and the synthesizer's adjacent
audit artefacts (Pass A / B / C reports, Amendment 01). Owner is
"V2 re-issue agent" throughout — a single agent applies the punch
list to produce `MASTER-PLAN-V2.md`.

### §12.A — Lock-Adherence reconciliation (items 1-22)

| # | Target | Surgery | Verdict | Scope | Lane |
|---:|---|---|---|---|---|
| 1 | `MASTER-PLAN.md:1283` | Extend Lock-1 verification command to grep also `crates/bbnf-runtime/src/grammars/`, `crates/bbnf-runtime-template/src/`, `crates/bbnf-host-prims/src/` per Amendment 01 substrate | REINVENT | single-line | 1 |
| 2 | `MASTER-PLAN.md:1284` | Extend Lock-2 verification to: "trait LayoutSink in bbnf-ir/src/registry/sink.rs is greppable; consumer count ≥ 2" | REINVENT | single-line | 1 |
| 3 | `MASTER-PLAN.md:1285` | Reanchor Lock-3 verification: cookbook entry at `docs/howto/cookbook/path-macro.md` is a tranche-G deliverable, not C.W6; cite tranche G.W3 instead | REINVENT | single-line | 1 |
| 4 | `MASTER-PLAN.md:1286` | Extend Lock-4 verification with negative claim: "no `crates/bbnf-fused-solver/` workspace member exists; verified by `[workspace] members` enumeration" | REINVENT | single-line | 1 |
| 5 | `MASTER-PLAN.md:1287` | Extend Lock-5 verification: "post-tranche-H, trait method counts compare three-way (Rust + TS + WASM); pre-tranche-H is two-way" | REINVENT | single-line | 1 |
| 6 | `MASTER-PLAN.md:1288` | Reanchor Lock-6 verification: regen targets are `crates/bbnf-runtime/src/grammars/<name>/{generated.rs, runtime.rs}` per Amendment 01, not `crates/<grammar>/src/generated.rs` | REINVENT | single-line | 1 |
| 7 | `MASTER-PLAN.md:1290, 787` | Extend Lock-8 row + tranche F gestalt: F.W6 close gate cites "JSON twitter parse measured before + after Pratt + SIMD auto-detection lands; expected within ±5% of pre-F or improvement; no regression > 5%" | REINVENT | paragraph | 1, 4 |
| 8 | `MASTER-PLAN.md:1290, 789` | Extend Lock-8 row + tranche G gestalt: G.W5 close gate cites "twitter.json parsed via parse(input), parse_in(input, &bump), parse_owned(input); slice-borrow ≤ sonic-rs's 436 µs (M1 Pro Criterion); arena variant within ±10%; owned variant within +30%" | REINVENT | paragraph | 1, 4 |
| 9 | `MASTER-PLAN.md:1290, 791` | Extend Lock-8 row + tranche H gestalt: H.W4 close gate cites "TS-emitted JSON parser parses twitter.json; emits typed-tree byte-for-byte equivalent to Rust-emit; WASM-emit equivalent" | REINVENT | paragraph | 1, 4 |
| 10 | `MASTER-PLAN.md:1291` | Extend Lock-9 verification to also grep `bbnf-parse/src/lib.rs` for the underlying impl, not just the bbnf aggregator re-export | REINVENT | single-line | 1 |
| 11 | `MASTER-PLAN.md:1056-1093, 1293` | Extend §7.2 prelude commits to include `git submodule add` operations for parse-that and bbnf-regex; Lock-11 verification cites the submodule-add step | REINVENT | paragraph | 1 |
| 12 | `MASTER-PLAN.md:1295` | Reanchor Lock-13 verification: annotate that `crates/bbnf-runtime/src/grammars/` is exempt per Amendment 01 §"Lock 13 footnote" — the per-grammar subdirectory is permissible because every subdirectory is structurally identical (template-emitted) | REINVENT | single-line | 1 |
| 13 | `MASTER-PLAN.md:1296` | Strike "creating its declaration crate" step from Lock-14 verification command; future-grammar onboarding test is two-step ceremony per Amendment 01 | DISCARD | single-line | 1, 5 |
| 14 | `MASTER-PLAN.md:1296` | Extend Lock-14 generic-crate enumeration with `bbnf-host-prims/src/` per Amendment 01 | REINVENT | single-line | 1, 5 |
| 15 | `MASTER-PLAN.md:1296` | Reformat Lock-14 verification as 16-row per-crate table; each row shows expected verification command output (zero hits) | REINVENT | multi-section | 1, 5 |
| 16 | `MASTER-PLAN.md:785` | Extend tranche E gestalt: future-grammar onboarding test (yaml.bbnf) gates the tranche E close per Amendment 01 §item 5 | REINVENT | single-line | 1, 5 |
| 17 | `MASTER-PLAN.md:781, 1289` | Tighten tranche C path-core scope: C.W6 covers `path-core/` only; `path/` proc-macro shell lands in G.W3; `path-ts/` cdylib lands in H.W2 | REINVENT | paragraph | 2 |
| 18 | `MASTER-PLAN.md:766, 785` | Reanchor tranche E "9 declaration crates" to "9 template-emitted runtime subdirs under `bbnf-runtime/src/grammars/`" + bbnf-host-prims | DISCARD | paragraph | 1, 2, 5 |
| 19 | `MASTER-PLAN.md:785` | Tranche E gestalt names future-grammar onboarding test as a close gate | REINVENT | paragraph | 1, 2, 5 |
| 20 | `MASTER-PLAN.md:766, 785` | Tranche E 8-wave breakdown enumerates substrate-consumer sequencing: template arrives W1-W3; consumer exercises W4-W8 with per-grammar tests | REINVENT | paragraph | 2 |
| 21 | `MASTER-PLAN.md:768, 1230` | Tighten tranche-A.W2 path/ + path-ts/ shell registration: Cargo.toml entry + lib.rs stub citing "tranche G implements" / "tranche H implements"; substantive impl lands in tranche G + H with same-wave consumer test | REINVENT | paragraph | 2 |
| 22 | `MASTER-PLAN.md:769, 1230-1233` | Reconcile §9.2 dependency-DAG with §5.1 H carry-FROM: H carries FROM D + E only (G dependency is wrong; H's TS+WASM emit doesn't depend on G's slice-borrow API surface) | REINVENT | paragraph | 2 |

### §12.B — Cohesion + amendment-01 reconciliation (items 23-49)

| # | Target | Surgery | Verdict | Scope | Lane |
|---:|---|---|---|---|---|
| 23 | `MASTER-PLAN.md:785` | Tranche E (E.W6 close) gate adds samply gate per Pass B hardening surgery: post-direct-projection-emit, JsonStructBuilder::checkpoint share <1% on twitter.json | REINVENT | single-line | 3 |
| 24 | `MASTER-PLAN.md:79-87, 1306-1319` | Reconcile §3.1 LOC column sums (168 020) with §12.1 baseline (168 750); update §3.1 estimates to CENSUS-exact figures (preferred) or annotate the 730-LOC drift inline | REINVENT | paragraph | 3, 6 |
| 25 | `MASTER-PLAN.md:13-17` | Strike "per-grammar declaration crates carrying generated parser + template-emitted runtime + host functions"; replace with "metadata-driven grammar onboarding via two declarative surfaces" | REINVENT | paragraph | 3, 5 |
| 26 | `MASTER-PLAN.md:38` | Reaffirm wording: "9 per-grammar runtime directories that retire for template emission" — wording is correct under Amendment 01 (template-emitted subdirectories under `bbnf-runtime/src/grammars/`); no surgery beyond clarifying the substrate path | REINVENT | single-line | 5 |
| 27 | `MASTER-PLAN.md:79-87` | Strike rows for `bbnf-meta`, `json`, `css-l4`, `google-sheets`, `bnf`, `csv`, `ebnf`, `css-pretty`, `math` (9 rows); add `bbnf-host-prims` row + `bbnf-test-fixtures` clarification per Amendment 01 §"Corrected workspace shape" | DISCARD | multi-section | 5 |
| 28 | `MASTER-PLAN.md:93` | Reanchor "9 are per-grammar declaration crates" to "0 per-grammar declaration crates; 9 template-emitted subdirs under `bbnf-runtime/src/grammars/`; 1 `bbnf-host-prims`" | REINVENT | paragraph | 5 |
| 29 | `MASTER-PLAN.md:99` | Strike reconciliation item 3 ("`bbnf-host` extracted from `bbnf-grammar`") — the host-fn primitive library is `bbnf-host-prims` per Amendment 01; `bbnf-host` (dispatch + registry mechanism) survives | REINVENT | paragraph | 5 |
| 30 | `MASTER-PLAN.md:111` | Strike entire reconciliation item 9 ("Per-grammar declaration crate names") — moot under Amendment 01 | DISCARD | paragraph | 5 |
| 31 | `MASTER-PLAN.md:157-167` | Strike 9 per-grammar member entries from `[workspace] members`; add `bbnf-host-prims` entry; total drops from 33 to 24 | DISCARD | multi-section | 5 |
| 32 | `MASTER-PLAN.md:719` | Reanchor "per-grammar benches live in per-grammar declaration crates" to "per-grammar benches template-emitted from `bbnf-bench/`" | REINVENT | single-line | 5 |
| 33 | `MASTER-PLAN.md:722-750` | Strike subsection §4.20 "Per-grammar declaration crate skeleton (`crates/<grammar>/`)" entirely; replace with "Per-grammar runtime subdir (`crates/bbnf-runtime/src/grammars/<name>/`)" | DISCARD | multi-section | 5 |
| 34 | `MASTER-PLAN.md:766` | Reanchor tranche E (line 766) gestalt: "`bbnf-host-prims` + `bbnf-runtime-template` + template-emitted per-grammar runtime subdirs + direct-projection emit + Lock 14 metadata-driven host-fn composition" | REINVENT | paragraph | 5 |
| 35 | `MASTER-PLAN.md:785` | Reanchor "9 per-grammar declaration crates scaffold" to "9 template-emitted runtime subdirs under `bbnf-runtime/src/grammars/`" | REINVENT | paragraph | 5 |
| 36 | `MASTER-PLAN.md:876` | Reanchor `[workspace.metadata.bbnf.grammars.bbnf-meta] output_dir` to `crates/bbnf-runtime/src/grammars/bbnf-meta/` | REINVENT | single-line | 5 |
| 37 | `MASTER-PLAN.md:887` | Reanchor `[workspace.metadata.bbnf.grammars.json] output_dir` to `crates/bbnf-runtime/src/grammars/json/` | REINVENT | single-line | 5 |
| 38 | `MASTER-PLAN.md:898` | Reanchor `[workspace.metadata.bbnf.grammars.css-l4] output_dir` | REINVENT | single-line | 5 |
| 39 | `MASTER-PLAN.md:909` | Reanchor `[workspace.metadata.bbnf.grammars.google-sheets] output_dir` | REINVENT | single-line | 5 |
| 40 | `MASTER-PLAN.md:920` | Reanchor `[workspace.metadata.bbnf.grammars.bnf] output_dir` | REINVENT | single-line | 5 |
| 41 | `MASTER-PLAN.md:931` | Reanchor `[workspace.metadata.bbnf.grammars.csv] output_dir` | REINVENT | single-line | 5 |
| 42 | `MASTER-PLAN.md:942` | Reanchor `[workspace.metadata.bbnf.grammars.ebnf] output_dir` | REINVENT | single-line | 5 |
| 43 | `MASTER-PLAN.md:953` | Reanchor `[workspace.metadata.bbnf.grammars.css-pretty] output_dir` | REINVENT | single-line | 5 |
| 44 | `MASTER-PLAN.md:964` | Reanchor `[workspace.metadata.bbnf.grammars.math] output_dir` | REINVENT | single-line | 5 |
| 45 | `MASTER-PLAN.md:973-999` | Strike entire §6.2 "Per-crate Cargo.toml schema" — no per-grammar Cargo.toml exists under Amendment 01 | DISCARD | multi-section | 5 |
| 46 | `MASTER-PLAN.md:1215` | Reanchor §9.1 phase summary tranche E entry to match item 34 wording | REINVENT | single-line | 5 |
| 47 | `MASTER-PLAN.md:1288` | Reanchor Lock-6 verification per item 6 above (cross-ref) | REINVENT | single-line | 1, 5 |
| 48 | `MASTER-PLAN.md:1296` | Strike "creating its declaration crate" step per item 13 above (cross-ref) | DISCARD | single-line | 1, 5 |
| 49 | `MASTER-PLAN.md:1325` | Reanchor §12.2 trajectory entry: "per-grammar runtime subdirs under `bbnf-runtime/src/grammars/` receive `generated.rs` as moved-not-modified" | REINVENT | single-line | 5 |

### §12.C — Tranche-E close gate + future-grammar test (items 50-59)

| # | Target | Surgery | Verdict | Scope | Lane |
|---:|---|---|---|---|---|
| 50 | `MASTER-PLAN.md:766, 785` | Tranche E close gate (E.W8) names: (a) all 9 grammars template-emit; (b) one grammar passes future-grammar onboarding test (yaml.bbnf added with two-surface ceremony); (c) samply gate fires; (d) bbnf-runtime-template grammar-agnosticism gate fires | REINVENT | paragraph | 3, 4, 5, 6 |
| 51 | `MASTER-PLAN.md:785` | Strike "specialised cohort retain `specialised/` for extensions only"; replace with "specialised cohort retain extensions via host-fn composition (metadata or grammar-source @host directive)" per Amendment 01 | REINVENT | paragraph | 3, 9 |
| 52 | `MASTER-PLAN.md:111` | Strike §3.2 reconciliation item 9 entirely (cross-ref item 30) | DISCARD | paragraph | 3 |
| 53 | `MASTER-PLAN.md:54, 683` | Replace `pub use ... ::*;` glob re-exports with explicit re-exports of canonical 10-15 public symbols per crate; aggregator at `bbnf/src/lib.rs` carries enumerated `pub use` statements | REINVENT | multi-section | 3, 9 |
| 54 | `MASTER-PLAN.md:64, 785` | Tranche E close gate adds `bbnf-runtime-template` grammar-agnosticism verification: `rg 'JsonValue\|CssL4Value\|BbnfValue\|GoogleSheetsValue' crates/bbnf-runtime-template/src/` returns 0 | REINVENT | single-line | 3 |
| 55 | `MASTER-PLAN.md:787` | Tranche F (F.W6 close) gate adds: "JSON twitter parse measured before + after Pratt + SIMD auto-detection; expected within ±5% of pre-F or improvement; regression >5% blocks the wave" | REINVENT | single-line | 4 |
| 56 | `MASTER-PLAN.md:789` | Tranche G (G.W5 close) gate adds: "twitter.json parsed via three lifetime APIs; slice-borrow ≤ sonic-rs's 436 µs (M1 Pro); arena ±10%; owned within +30%" | REINVENT | single-line | 4 |
| 57 | `MASTER-PLAN.md:791` | Tranche H (H.W4 close) gate adds: "TS-emitted JSON parser typed-tree byte-for-byte equivalent to Rust-emit; WASM-emit equivalent" | REINVENT | single-line | 4 |
| 58 | `MASTER-PLAN.md:1296` | Reformat Lock-14 verification cell as 16-row per-crate table per HARDENING.md §Lane 5 "Per-X tables" standard (cross-ref item 15) | REINVENT | multi-section | 5 |
| 59 | `MASTER-PLAN.md:1296` | Future-grammar onboarding ceremony reduces from three steps to two surfaces per Amendment 01 §"Settled position" lines 13-25; strike crates/yaml/ creation step (cross-ref item 13) | DISCARD | single-line | 5 |

### §12.D — Generated-LOC budget + xtask wall (items 60-66)

| # | Target | Surgery | Verdict | Scope | Lane |
|---:|---|---|---|---|---|
| 60 | `MASTER-PLAN.md:789, 1183` | Tranche G.W3 deliverable list extends with `docs/howto/cookbook/path-macro.md` lands; verbatim error messages committed (rule-name typo, wildcard misuse, index/key disambiguation) | REINVENT | paragraph | 6, 7 |
| 61 | `MASTER-PLAN.md:789, 1291` | Tranche G.W2 deliverable list extends with `docs/howto/cookbook/lifetime-surfaces.md` lands; verbatim error messages committed (when-to-use decision tree, bump ergonomics, lifetime escape) | REINVENT | paragraph | 6, 7 |
| 62 | `MASTER-PLAN.md:781, 1284` | Tranche C.W2 deliverable list extends with `docs/errors/layout-lowering.md` lands; verbatim error messages committed (untyped rule, mismatched layout, missing host fn) | REINVENT | paragraph | 6, 7 |
| 63 | `MASTER-PLAN.md:787, 1292` | Tranche F.W3 deliverable list extends with `docs/optimizer/pratt-simd-detection.md` cookbook + `cargo xtask diag` subcommand; cost-model trace verbatim | REINVENT | paragraph | 6, 7 |
| 64 | `MASTER-PLAN.md:1183` | Tranche C.W4 `docs/howto/migration/2026-restart.md` extends to carry verbatim sed-recipes (import-path breakage, Cargo.toml dep update); `git log --diff-filter=R` rename-discovery hint | REINVENT | paragraph | 6, 7 |
| 65 | `MASTER-PLAN.md:1183` | New cookbook `docs/howto/cookbook/host-fn-composition.md` lands at C.W3 or E.W6 (per Amendment 01 §"Host-fn implementations") | REINVENT | single-line | 6, 7 |
| 66 | `MASTER-PLAN.md:1183` | New cookbook `docs/howto/cookbook/test-fixtures.md` lands at C.W3 or E.W6 | REINVENT | single-line | 6, 7 |

### §12.E — Carry & deferral fixes (items 67-73)

| # | Target | Surgery | Verdict | Scope | Lane |
|---:|---|---|---|---|---|
| 67 | `MASTER-PLAN.md:73, 105, 155` | `bbnf-cli` defer Notes column extends to "DEFERRED to post-tranche-J 1.0-release tranche; blocker = API freeze (parse, parse_in, parse_owned, Document::get<T>(path), pointer![…]); receiving gate = post-J tranche W0 declares CLI surface, W1 implements"; §5.1 receives footnote naming tranche K | REINVENT | paragraph | 8 |
| 68 | `MASTER-PLAN.md:107` | `bbnf-py` §3.2 item 7 reads "DEFERRED post-1.0. Triggering condition: a Python consumer materialises (csc411, OR a downstream Python user surfaces). Receiver: a yet-to-name post-1.0 tranche. Receiving gate: that tranche's W0 declares the Python binding API shape; W1 implements PyO3 cdylib OR subprocess-CLI binding" | REINVENT | paragraph | 8 |
| 69 | `MASTER-PLAN.md:1170` | Strike OR-disposition for `docs/restart/`; commit to relocation: "**`docs/restart/`** relocates to `docs/process/restart/` per Lock 13 (cohesion with `docs/process/instructions/`, `docs/process/tranches/`)" | REINVENT | single-line | 8, 9 |
| 70 | `MASTER-PLAN.md:1259, 1264` | `archive/{ser, gorgeous}/` disposition: "deletion permissible at post-J 1.0-release tranche W0 if `archive/` content is fully reproducible from sister-repo provenance (gorgeous-external, pprint-external)" | REINVENT | paragraph | 8 |
| 71 | `MASTER-PLAN.md:1080` | §7.2 prelude commit 8 verbiage extends to "Master Plan + Amendment 01 + per-pass restart audit synthesis" | REINVENT | single-line | 8 |
| 72 | `MASTER-PLAN.md:7` | Update path reference: `docs/restart/HARDENING.md` → `restart/prompts/HARDENING.md` | REINVENT | single-line | 8 |
| 73 | `MASTER-PLAN.md:1414` | §15 closing posture extends to name Stage 2: "Stage 1 hardening commits, Stage 2 fires per `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md`; both must pass before tranche drafting opens" | REINVENT | paragraph | 8 |

### §12.F — Greenfield-discipline workaround retirements (items 74-76)

| # | Target | Surgery | Verdict | Scope | Lane |
|---:|---|---|---|---|---|
| 74 | `MASTER-PLAN.md:777` | Tranche A.W4 deliverable list extends to name "Box::leak retiral at `crates/core/src/grammar/mod.rs:57`; replaced by `parse_grammar_in(input, &bump)` per Lock 9 escape hatch; entry point deprecated to `parse_grammar_owned`" per Pass A hardening adjudication | REINVENT | paragraph | 9 |
| 75 | `MASTER-PLAN.md:781` | Tranche C.W6 deliverable list extends to name "wildcard `@debug` strip-prefix at `crates/core/src/grammar/host.rs:387` retired; FAIL-EXPLICIT replacement" per Pass A hardening adjudication | REINVENT | paragraph | 9 |
| 76 | `MASTER-PLAN.md:781` | Tranche C.W6 deliverable list extends to name "defensive `unreachable!()` fallback at `crates/core/src/lower/value_expr/simple_kinds.rs:185` retired; either `unreachable!()` or upstream invariant fix" per Pass A hardening adjudication | REINVENT | paragraph | 9 |

### §12.G — Cumulative reconciliation with sister Stage-1 reports

The four Stage-1 hardening reports (PASS-A 25 entries, PASS-B 30
entries, PASS-C 30 entries, this MASTER-PLAN report 76 entries)
collectively gate the master-plan V2 re-issue. Cumulative surgical
edits:

| Report | Entries | Apex faults |
|---|---:|---|
| HARDENING-PASS-A.md | 25 | per-grammar declaration crate sites (10), residue gates (4), friction surfaces (5), cohesion + budget (4), future-grammar test (1), carry triple (1) |
| HARDENING-PASS-B.md | 30 | per-grammar declaration crate reanchorings (18), SOTA gates (5), LOC budget (4), friction surface (1), greenfield posture (2) |
| HARDENING-PASS-C.md | 30 | bbnf-language-server framing (4), cohesion paths (7), LOC budget (4), friction (3), carry triples (3), defer scope (9) |
| **HARDENING-MASTER-PLAN.md** (this) | 76 | Lock reconciliations (22), Amendment 01 reanchorings (27), tranche-E close + future-grammar (10), generated-LOC + cookbook (7), carry & deferral (7), workaround retirals (3) |
| **Cumulative** | **161** | |

The cumulative 161-edit punch list applies as follows:

1. **Pass-source surgeries** (PASS-A 25 + PASS-B 30 + PASS-C 30 = 85
   entries): apply via **amendment agents** to each pass synthesis
   document (or as `AMENDMENT-02-PASS-{A,B,C}-RECONCILIATION.md`
   sidecars per the orchestrator's preference).
2. **Master-plan surgeries** (this report's 76 entries): apply via
   **V2 re-issue agent** to produce `MASTER-PLAN-V2.md`.
3. **Amendment 01** is authoritative supersedence; V2 absorbs its body
   wholesale.
4. **Stage 2 hardening** re-evaluates the V2 plus reconciled passes;
   user signs off; tranche drafting opens.

The largest single surgery cohort is §12.B (Amendment 01
reconciliation: 27 entries 23-49). The cumulative punch list completes
in a single V2 re-issue agent's pass.

---

## §13 — Final readiness

> **Decision: amendment-required.**
>
> The master plan's substantive shape — the 10-tranche allocation
> across A through J, the convergent pivot at tranche E retiring Lock
> 1 + Lock 13 + Lock 14 as one architectural movement, the
> commit-chain disposition (Option 3 + branch reset) ratification,
> the docs re-do plan, the §11 14-lock honoured table, the §12
> generated-LOC trajectory, the §13 risk table — survives the lanes.
> Each pass synthesis (Pass A, B, C) was correctly composed; the
> reconciliation in §3.2 is correct; the §5 tranche set sequencing is
> Era-V-correct (modulo the H carry-FROM and tranche-C path-core scope
> faults). What requires amendment is the *body* — the master plan
> body has not absorbed Amendment 01 (which retracts the per-grammar
> declaration crate default), so 28 sites carry retracted language;
> the per-tranche-wave SOTA gates are silent at F + G + H mid-tranche
> (only J close gates the SOTA anchors); the friction surfaces (six
> per HARDENING.md §Lane 7) are uniformly silent on user-facing
> cookbooks and verbatim error messages; the carry-and-deferral
> triples for `bbnf-cli`, `bbnf-py`, `docs/restart/`, archive
> deletion, Stage 2 hardening receiver are incomplete; the three
> Pass-A-inherited workaround retirals (Box::leak, wildcard @debug
> strip, defensive fallback) are silent.
>
> The 76-entry master-plan-specific punch list at §12 plus the 85
> entries from sister Stage-1 reports (Pass A 25 + Pass B 30 + Pass C
> 30) totals 161 cumulative surgical edits gating the V2 re-issue.
> The largest single surgery cohort is §12.B (Amendment 01
> reconciliation: 27 entries) — every site naming "per-grammar
> declaration crates" re-anchors per Amendment 01's authoritative
> retraction. The 9-crate proliferation was overfitting on Lock 14's
> escape valve; the 24-member workspace under Amendment 01 brings
> bbnf closer to sonic-rs / lightning-css / simdjson cohesion. The
> Lock 14 verification command, the future-grammar onboarding test,
> the workspace member list, the per-grammar metadata `output_dir`
> entries, the §12.2 LOC trajectory destinations, and the §13 risk
> R9 framing all materialise the retraction.
>
> Hereupon the V2 re-issue agent applies the master-plan punch list
> (76 entries) to produce `MASTER-PLAN-V2.md`; amendment agents
> apply the per-pass punch lists (Pass A 25, Pass B 30, Pass C 30)
> to each pass synthesis (in-place patch or sidecar
> `AMENDMENT-02-PASS-{A,B,C}-RECONCILIATION.md`). With V2 + reconciled
> passes committed, Stage 2 hardening fires per
> `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md` — fresh adversary,
> no shared context, distinct system — to re-evaluate the cumulative
> Stage 1 outputs. Stage 2's verdict gates the user sign-off and the
> opening of tranche drafting (10 agents A through J), each
> consuming `MASTER-PLAN-V2.md` plus `AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md`
> plus their tranche-specific stubs at `restart/tranches/{X}/{X}.md`.
> The plan from here is the V2 re-issue agent's; the substrate is
> settled; the greenfield mandate carries through.
