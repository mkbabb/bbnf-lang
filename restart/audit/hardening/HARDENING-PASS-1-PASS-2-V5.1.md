# HARDENING-PASS-1-PASS-2-V5.1

## §1 Target and commits verified

Verification target:

| Field | Value |
|---|---|
| Worker route | Phase 0.5 V5.1 PASS-1/PASS-2 verification worker |
| Primary PASS surfaces | `restart/audit/pass-1-substrate/PASS-1.md`; `restart/audit/pass-2-codegen/PASS-2.md` |
| Output report | `restart/audit/hardening/HARDENING-PASS-1-PASS-2-V5.1.md` |
| Write rule | Verification report only; no PASS source edits |
| Current HEAD during verification | `91af4882` |
| Amendment commit verified | `b64a18a14cfb1bbdacf553824672e42b560f4109` |
| Amendment subject | `docs(restart/pass-2): wave-5.1 narrow amendment - recognizer diagnostics and provenance` |

Commit topology checked:

| Check | Evidence | Result |
|---|---|---|
| PASS-local amendment exists | `git show --stat --oneline b64a18a1` reports 2 files changed, 16 insertions, 6 deletions. | PASS |
| Later commits did not touch PASS-1/PASS-2 | `git diff --name-only b64a18a1..HEAD -- restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md` returned zero paths. | PASS |
| PASS-local last touching commit | `git log --oneline -- PASS-1.md PASS-2.md` shows `b64a18a1` as latest for the two PASS surfaces. | PASS |
| Initial worktree state | `git status --short` returned no rows. | PASS |

Required reading consumed:

| Authority | Verification use |
|---|---|
| `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md` | Bundle routing, V5.1 success criteria, F/G/H pathology frame. |
| `restart/audit/hardening/HARDENING-PASS-1-V5.md` | PASS-1 rare-fence, yaml onboarding, and recognizer diagnostic adjacent rows. |
| `restart/audit/hardening/HARDENING-PASS-2-V5.md` | PASS-2 `BBNF-OPT001` / `BBNF-OPT002`, stale citation, yaml, WASM, and carry rows. |
| `restart/prompts/AMENDMENT-DISPATCH.md` §1 | Verify-then-patch discipline; current state must be read before classifying closure. |
| `restart/README.md` §13 | Voice and citation discipline; no stale authority cues. |
| `restart/locks/14-LOCKS.md` | Lock 10 retired recognizer directives; Lock 11 incubation; Lock 14 two-surface onboarding. |
| Current `PASS-1.md` and `PASS-2.md` | Current path:line evidence for every assigned bundle. |

Scope note:

No PASS source file was amended. This report verifies whether the amendment closed
the assigned bundles and records the remaining residue where the amended text
still carries provenance drift.

## §2 Bundle closure table

| Bundle | Current evidence | Verdict | Rationale |
|---:|---|---|---|
| 2 - PASS-2 recognizer diagnostics remove retired `@pratt` / `@simd` | `PASS-2.md:545` says `PrattSpine was not auto-selected; add stable precedence metadata or restructure the rule`. | CLOSED | The prior user-facing `@pratt` remediation is gone. The surviving advice is automatic selection, metadata, or grammar restructuring. |
| 2 - PASS-2 recognizer diagnostics remove retired `@pratt` / `@simd` | `PASS-2.md:546` says SIMD fell back because cost evidence did not win and metadata may disable unsupported kernels but cannot force SIMD. | CLOSED | The prior user-forced `@simd` hint is gone. The current text forbids force semantics. |
| 2 - Lock 10 alignment | `restart/locks/14-LOCKS.md:52` states Pratt and SIMD are auto-detected and no `@pratt` / `@simd` directives exist. | CLOSED | PASS-2 now agrees with Lock 10 on author surface. |
| 2 - Wider directive scan | `rg -n "@pratt|@simd" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md` finds only `MASTER-PLAN.md:204`, a rejected-scope row. | CLOSED | The only wider hit is prohibition context outside the PASS files. |
| 3 - Stale `BBNF-SEM040` route repaired | `PASS-2.md:176` names the §8 diagnostic ledger instead of stale `line 478`; `PASS-2.md:544` is the current `BBNF-SEM040` ledger row. | CLOSED for assigned row | The V5 target line reference was removed and replaced with section/table evidence. |
| 3 - Layout stale line references repaired | `PASS-2.md:69` binds layout lowering to the §7 per-construct contribution plan and §7 runtime emission table, not old `line 459` / `line 475`. | CLOSED for assigned row | The amendment removed brittle local line references from the layout-canon paragraph. |
| 3 - Stale history/report references | `rg -n "line 478|line 459|line 475|HARDENING-CONSOLIDATED-V3|HARDENING-CONSOLIDATED-V4|exactly two added paths|@pratt|@simd" PASS-1.md PASS-2.md` returned zero. | CLOSED for the required gate | The specific V5 stale tokens are absent from the PASS surfaces. |
| 3 - New/local PASS-line citation check | `PASS-1.md:218` still says `PASS-1.md:90` and `PASS-1.md:96`, while the current error vocabulary and diagnostic table are at `PASS-1.md:95` and `PASS-1.md:97-101`. | PARTIAL | The required stale grep is clean, but the rare-fence insertion shifted nearby PASS-1 self-citations into H-class provenance drift. |
| 3 - PASS-2 cross-PASS line citation check | `PASS-2.md:92`, `PASS-2.md:112`, and `PASS-2.md:359` still cite old PASS-1 diagnostic line ranges after the rare-fence insertion. | PARTIAL | This does not reopen architecture, but it leaves actionable citation residue in the assigned PASS surfaces. |
| 4 - YAML onboarding alignment in PASS-1 | `PASS-1.md:228-230` allows only `grammars/yaml.bbnf`, workspace metadata, and generated output; `PASS-1.md:232` says generated runtime/path/visitor/host/diagnostic/budget files are not a third onboarding surface. | CLOSED | PASS-1 now states the author-input count directly and rejects manual Rust registry inflation. |
| 4 - YAML onboarding alignment in PASS-2 | `PASS-2.md:385-391` binds yaml to grammar source, workspace metadata, generated runtime, generated registry, a build/test gate, and a rejected onboarding path. | CLOSED | PASS-2 now states generated output is not author input and rejects manual registry edits, handwritten runtime files, fixture-only admission, and a yaml declaration crate. |
| 4 - Fixture overfit scan | `rg -n "fixtures/yaml" PASS-1.md PASS-2.md` returned zero. | CLOSED | No yaml fixture path is treated as source authority in the assigned PASS surfaces. |
| 7 - WASM host primitive route | `PASS-2.md:110` states host primitives are lowerer/runtime ABI concerns and lists exported function names, host-call shape, marshalling descriptors, and scalar/SIMD parity evidence. | CLOSED | The route is explicit and does not add a BBNF primitive annotation or force directive. |
| 7 - WASM carry ledger | `PASS-2.md:559` routes the WASM host primitive ABI descriptor to BD.W2 / H.W3 and BD.W3, with exported names, host-call shape rows, marshalling descriptors, scalar/SIMD parity evidence, and no runtime trait dispatch. | CLOSED | The receiver, blocker, and receiving gate shape is present. |
| 9 - Rare declaration-crate fence in PASS-1 | `PASS-1.md:89-93` adds deletion path, reviewer, extant grammar emptiness, verification, and canonical eight-field Architecture review form sync. | CLOSED | PASS-1 now carries the missing review fields and states that it cannot admit a declaration crate by itself. |
| 9 - Rare declaration-crate route in PASS-2 | `PASS-2.md:298-300` says host functions decompose through primitives, metadata, or `@host fn`; rare declaration crates stay behind the Architecture/Lock 11 incubation fence. | CLOSED | PASS-2 keeps generated adapters behind approved metadata or host-fn bodies and forbids generic codegen/runtime imports of declaration crates. |
| 9 - Diagnostic alias polish | `PASS-1.md:95`, `PASS-1.md:101`, `PASS-1.md:135`, `PASS-1.md:218`, `PASS-2.md:176`, and `PASS-2.md:544` bind `LookbehindWidth`, `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, and `BBNF-SEM040`. | CLOSED with citation residue | The alias chain is semantically correct. The only fault is stale local line numbers in the explanatory parenthetical at `PASS-1.md:218`. |

Bundle result:

| Bundle | Verdict |
|---:|---|
| 2 | CLOSED |
| 3 | PARTIAL |
| 4 | CLOSED |
| 7 | CLOSED |
| 9 | CLOSED with citation residue |

## §3 Pathology regression scan

| Lens | Site | Spot-check | Result | Classification |
|---|---|---|---|---|
| F - directive completion bias | `PASS-2.md:545-546` | Diagnostics no longer offer familiar `@pratt` / `@simd` directive escape hatches. | PASS | No new retired recognizer syntax. |
| F - force-control bias | `PASS-2.md:546` | Text says metadata can disable unsupported kernels but cannot force SIMD. | PASS | The replacement wording resists a renamed force directive. |
| F - generic compiler substrate bias | `PASS-1.md:57`, `PASS-1.md:287`, `PASS-2.md:460`, `PASS-2.md:566` | OpenFrame remains deletion archaeology; no public substrate role is preserved. | PASS | No OpenFrame relapse. |
| G - table-overfit onboarding | `PASS-1.md:232`, `PASS-2.md:390-391` | YAML proof now distinguishes author inputs from generated outputs and rejected onboarding paths. | PASS | The assigned PASS surfaces no longer rely only on a two-row assertion. |
| G - WASM parity overfit | `PASS-2.md:110`, `PASS-2.md:559` | WASM host route names ABI descriptors and parity evidence without benchmark numerology. | PASS | No invented measurements landed. |
| G - declaration-crate overfit | `PASS-1.md:89-93`, `PASS-2.md:300`, `PASS-2.md:391` | Rare declaration crates remain fenced and yaml declaration crate admission fails onboarding. | PASS | The escape valve did not become a default path. |
| H - stale target refs | Required stale grep against PASS-1/PASS-2 | `line 478`, `line 459`, `line 475`, V3/V4 hardening refs, exact two-path yaml wording, `@pratt`, and `@simd` returned zero matches. | PASS | The named V5 stale tokens are removed. |
| H - shifted local citations | `PASS-1.md:218` | The paragraph cites `PASS-1.md:90` and `PASS-1.md:96`; current target lines are `PASS-1.md:95` and `PASS-1.md:97-101`. | FAIL-NARROW | New or remaining provenance drift in the PASS-local text. |
| H - cross-PASS line fragility | `PASS-2.md:92`, `PASS-2.md:112`, `PASS-2.md:359` | PASS-2 still cites PASS-1 diagnostic line ranges that no longer point exactly after the PASS-1 rare-fence insertion. | FAIL-NARROW | Section citations or current line ranges are required before READY. |

Pathology conclusion:

The amendment closed the highest-risk F-class user-syntax drift. It did not
introduce new grammar surface, BIR surface, OpenFrame substrate, yaml third
surface, WASM annotation, or declaration-crate default path. The remaining risk
is H-class citation confidence inside PASS-1/PASS-2.

## §4 Gate rerun

Minimum commands:

```text
git status --short
```

Result: clean at the start and clean before report creation.

```text
git show --stat --oneline b64a18a1
```

Result:

```text
b64a18a1 docs(restart/pass-2): wave-5.1 narrow amendment - recognizer diagnostics and provenance
 restart/audit/pass-1-substrate/PASS-1.md |  5 +++++
 restart/audit/pass-2-codegen/PASS-2.md   | 17 +++++++++++------
 2 files changed, 16 insertions(+), 6 deletions(-)
```

```text
rg -n "path!|@pratt|@simd|OpenFrame|LayoutFacts|LayoutSink|passes::layout|pointer!|select!|LookbehindWidth|BBNF-LOOKBEHIND-WIDTH|BBNF1004|@host fn|waves-v4|wave-4|Wave 4|WASM|incubat|rare|yaml|diagnostic|BBNF-SEM040" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md
```

Result summary:

| Family | Classification |
|---|---|
| `@pratt` / `@simd` | Zero matches in PASS-1/PASS-2. |
| `OpenFrame` | Deletion archaeology only at `PASS-1.md:57`, `PASS-1.md:287`, `PASS-2.md:460`, `PASS-2.md:566`. |
| Layout vocabulary | `passes::layout`, `LayoutFacts`, and `LayoutSink` appear in the canonical BIR-boundary sentence at `PASS-2.md:69`. |
| Diagnostic alias chain | `LookbehindWidth`, `BBNF-LOOKBEHIND-WIDTH`, `BBNF1004`, and `BBNF-SEM040` appear in PASS-owned diagnostic contexts. |
| YAML | Only two-surface/generated-output proof rows and rejected onboarding path rows in PASS-1/PASS-2. |
| WASM | Lowerer/runtime ABI route and parity rows; no source annotation or force directive. |

```text
rg -n "line 478|line 459|line 475|HARDENING-CONSOLIDATED-V3|HARDENING-CONSOLIDATED-V4|exactly two added paths|@pratt|@simd" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md
```

Result: zero matches; command exited with no output.

```text
git diff --check
```

Result: clean before report creation.

Additional verification commands:

```text
rg -n "@pratt|@simd" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md restart/MASTER-PLAN.md
```

Result: one match at `MASTER-PLAN.md:204`, a rejected-scope row: `@pratt` or
`@simd` grammar directives. No PASS-2 diagnostic hit.

```text
rg -n "PASS-2.md:293-310|PASS-2.md:98-116|ARCHITECTURE.md:1273-1281|BBNF-SEM040.*line 478|line 478.*BBNF-SEM040|line 459|line 475|exactly two added paths" restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md
```

Result: zero matches.

```text
rg -n "PASS-1\\.md:[0-9]+|PASS-2\\.md:[0-9]+|line [0-9]+|lines [0-9]+" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md
```

Result: found residual PASS-local line citations. Most are stable enough or
historical, but `PASS-1.md:218`, `PASS-2.md:92`, `PASS-2.md:112`, and
`PASS-2.md:359` are actionable because they point to shifted PASS-1 diagnostic
lines.

```text
git diff --name-only b64a18a1..HEAD -- restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md
```

Result: zero paths; later V5.1 commits did not modify the assigned PASS files.

## §5 Residue ledger

| ID | Actionable residue | Receiver | Blocker | Receiving gate |
|---|---|---|---|---|
| R1 | `PASS-1.md:218` cites `PASS-1.md:90` and `PASS-1.md:96`; current evidence is `PASS-1.md:95` for `LookbehindWidth` and `PASS-1.md:97-101` for the diagnostic table. | PASS-1/PASS-2 narrow provenance amendment | H-class line-citation drift remains in the assigned PASS surface. | `rg -n "PASS-1.md:90|PASS-1.md:96" restart/audit/pass-1-substrate/PASS-1.md restart/audit/pass-2-codegen/PASS-2.md` returns zero or section-scoped replacement text. |
| R2 | `PASS-2.md:92`, `PASS-2.md:112`, and `PASS-2.md:359` cite PASS-1 diagnostic line ranges that no longer point exactly after the PASS-1 rare-fence insertion. | PASS-2 narrow provenance amendment | PASS-2 still carries shifted PASS-1 line anchors in diagnostic ownership and PASS-3 consumer gate prose. | Replace with `PASS-1.md:95-106` where exact lines are desired, or use PASS-1 §2 diagnostic table section citations. |

No residue requires re-drafting PASS-1 or PASS-2. No residue changes the
recognizer, YAML, WASM host primitive, or rare declaration-crate architecture.

## §6 Final verdict

Verdict: `AMENDMENT-REQUIRED`.

Reason:

The substantive V5.1 amendment work closed bundles 2, 4, 7, and 9, and closed
the named stale-reference gates in bundle 3. The PASS surfaces still carry
actionable H-class provenance drift through shifted PASS-1 line citations. Under
the V5 rule that citations are evidence and not decoration, that residue blocks
READY.

Re-draft threshold:

| Threshold | Present? | Evidence |
|---|---|---|
| Retired `@pratt` / `@simd` syntax revived | No | Zero PASS-1/PASS-2 matches; PASS-2 diagnostics use auto-selection and cost evidence. |
| YAML requires third author surface | No | `PASS-1.md:232`; `PASS-2.md:390-391`. |
| WASM host primitive becomes source annotation | No | `PASS-2.md:110`; `PASS-2.md:559`. |
| Rare declaration crate becomes default | No | `PASS-1.md:89-93`; `PASS-2.md:300`; `PASS-2.md:391`. |
| OpenFrame becomes preserved substrate | No | Deletion archaeology only in PASS-1/PASS-2. |

Therefore the verdict is not `RE-DRAFT`. It is a narrow provenance amendment.

## §7 Closing posture

The PASS-1/PASS-2 V5.1 route is architecturally sound but not citation-clean.

Closed:

| Closed surface | Evidence |
|---|---|
| Recognizer diagnostics | `PASS-2.md:545-546`; no `@pratt` / `@simd` in PASS-1/PASS-2. |
| YAML/onboarding alignment | `PASS-1.md:228-232`; `PASS-2.md:385-391`. |
| WASM host primitive route | `PASS-2.md:110`; `PASS-2.md:559`. |
| Rare declaration-crate fence | `PASS-1.md:89-93`; `PASS-2.md:300`. |
| Required stale-token grep | Zero matches for the required stale pattern. |

Not closed:

| Open surface | Evidence |
|---|---|
| PASS-local line citation hygiene | `PASS-1.md:218`; `PASS-2.md:92`; `PASS-2.md:112`; `PASS-2.md:359`. |

Closing instruction:

Patch only the PASS-local provenance residue, or section-scope those citations.
Do not reopen recognizer syntax, YAML onboarding surfaces, WASM host ABI shape,
rare declaration-crate policy, BIR ownership, OpenFrame deletion, locks, or
tranche routing.
