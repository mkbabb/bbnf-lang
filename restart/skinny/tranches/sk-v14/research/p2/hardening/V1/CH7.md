# S-P2 V1 — CH7 OVERFIT-PRUNE Lens

Lens: **CH7 Overfit-Prune** per `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (lines 62-87).

Mandate (verbatim): the plan/redress (here: each P2 axis artefact and its candidate set) shows that

1. every new code added is grammar-derived (template + grammar metadata + emission command) — never hand-written under a `// @generated` header;
2. Lock 14 generic-crate compliance is preserved (no JSON/CSS/Sheets string literals, byte literals, function names, enum variants, or match arms in nominally-generic code);
3. every admit lands via a real parser/codegen/SIMD source change, measured against a strict-vs-strict comparator on the same plane, with a per-iteration equality oracle;
4. every "generated" output passes a round-trip test (delete + regen ⇒ byte-equivalent);
5. no SCAFFOLD-ONLY landing (research artefacts without source wiring) counts as an admit.

CH7 REJECT triggers immediate plan revise OR redress revert. CH7 cannot be carried as "acknowledged but not blocking".

Artefacts under review: 6 P2 axis files at `restart/skinny/tranches/sk-v14/research/p2/p2{a,b,c,d,e,f}-*.md` per CHALLENGE-CONTEXT §1.

---

## §0 — Executable verification

Per dispatch mandate: `cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'` from repo root.

Output (verbatim):
```
bbnf
json
css_l4
css_pretty
google_sheets
ebnf
bnf
csv
math
```
**Count: 9.** Matches dispatch expectation (9 grammars).

The grammar enumeration is the live binding from `Cargo.toml` `[workspace.metadata.bbnf]` (the same surface Lock 14 + Lock 16 audit-overlay binds against). All six P2 axes operate over candidates whose grammar-neutrality must hold for the seven non-JSON grammars in this list (excluding `bbnf` which is meta and `json` which is the witness plane). Verified.

---

## §1 — Per-artefact CH7 disposition

| Artefact | Mandate-1 (template, not @generated) | Mandate-2 (Lock 14) | Mandate-3 (real source change + strict-comparator + oracle) | Mandate-4 (round-trip) | Mandate-5 (no scaffold-only admit) | Verdict |
|---|---|---|---|---|---|---|
| `p2a-sota-teardown.md` (7 candidates) | PASS — every C1-C7 names a parser/SIMD source file the change lands in (`scan.rs`, `runtime/src/grammars/json/generated.rs`, codegen template at `crates/codegen/src/grammars/json/`). No `// @generated` hand-write proposed. | PASS — §3 frontmatter per candidate carries P2-F grammar-neutral verdict; `p2a:156` scalar reference is named generic + parameterised; no JSON literal proposed in `bbnf-simd`. | PASS — `p2a:156,262` cite scalar reference + parse-output byte-equivalent diff-test (the per-iteration equality oracle); strict-vs-strict comparator is the artefact's framing axis (R1 discipline). | PASS — codegen output round-trip is the existing repo invariant (`tests/regen_parity.rs` family); no candidate proposes hand-patching `generated.rs`. | PASS — every candidate names a same-wave consumer + row movement target in `skinny/RESULTS.md`; no candidate proposes research-only landing. | **ACCEPT** |
| `p2b-dav1d-process.md` (5-stage admission process) | PASS — the admission process itself enforces this mandate at Stage A (scalar reference) + Stage E (manifest). `p2b:7,156-162` make the grammar-neutral primitive vocabulary load-bearing. | PASS — `p2b:120,156-162` quote LOCKS v+1 amendment (`LOCKS.md:255-263`) verbatim: "Quote, escape, control, delimiter … must come from generated grammar config or caller data, not hardcoded JSON/CSS constants." Stage A + Stage E reject hardcoded-literal scalar refs. | PASS — Stage B (`p2b:55`) is the checkasm differential cell; Stage D (`p2b:102`) is the same-wave consumer + bench-gate re-run + `skinny/RESULTS.md` row movement. Per-iteration equality oracle is checkasm's `checkasm_check_func` shape. | PASS — admission process gates on existing codegen; no hand-patch route admitted. The Stage E manifest is the audit surface. | PASS — `p2b:102` enumerates the four wave-close dispositions (`wired`/`deleted`/`scalar-delegate-non-ASM`/`architectural-block-with-REDRESS`); explicitly excludes `inventory_demoted_with_evidence` as a *new* admission disposition (it is historical-only). Stage D is the no-scaffold-admit enforcer. | **ACCEPT** |
| `p2c-arch-esoterica.md` (8 ISA candidates; only C-P2C-4 S-P3-eligible) | PASS — 7 of 8 candidates are `NOT-S-P3-ELIGIBLE` at V1 (correct prune-discipline). C-P2C-4 names the source-touch (`scan.rs` runtime); no `@generated` hand-write. | PASS — `p2c:43` C-P2C-1 demotion explicitly cites audit-overlay PRUNE-2 (23/24 CSS L4 rows lack profile attribution) as the demotion reason. The audit-overlay binding is *cited directly* in this artefact (5 hits per grep). | PASS — C-P2C-4 (the only S-P3-eligible row) carries scalar reference + checkasm cell + same-wave consumer per dispatch §2 frontmatter. The 7 demoted rows are explicitly excluded from same-wave admit until profile evidence materialises. | PASS — no codegen hand-patch proposed; demotions preserve repo invariant. | PASS — the artefact's strongest CH7 evidence: 7 candidates explicitly *demoted* rather than admitted because the same-wave evidence is absent. This is the anti-scaffold-admit pattern executed in vivo. | **ACCEPT** |
| `p2d-substrate-tape.md` (3 active candidates) | PASS — C-P2D-1/2/3 each name a runtime source path (`runtime::tape::*`, `runtime::generated_json::*`, codegen template). No `@generated` hand-write. C-P2D-4 is REJECT-by-REDRESS-96/97/98 (no admission). | PASS — `p2d:116,124,132` mark substrate primitives `HIGH` grammar-neutrality with cited mechanism (substrate is grammar-neutral per `SUBSTRATE.md:7`); `p2d:155` confirms zero JSON-grammar match arm / JSON-named module / JSON-keyed feature flag across active candidates. | PASS — `p2d:7,238` cite V3 CH5 substrate-union 6/6 ACCEPT + two-cursor independence executable-verification (`research/p1/hardening/V3/CH5.md:78-83`) as the per-iteration equality oracle. Strict-vs-strict comparator is sonic-rs / simdjson per `p2d:257`. | PASS — substrate activation candidates land in `runtime::tape::*` (live code) + codegen template; no hand-patch route. | PASS — `p2d:153` REJECT-by-REDRESS for C-P2D-4 (`EventTape` pre-blocked); the artefact explicitly refuses to admit a scaffold-only substrate variant. Three active candidates each carry same-wave consumer wiring. | **ACCEPT** |
| `p2e-parse-that-gaps.md` (8 Layer-1 primitive gaps) | PASS — every gap names the parse-that source file the primitive lands in; no `// @generated` proposal. The artefact is explicit (`p2e:341`) that the audit-overlay synthesis is the binding row-falsification list. | PASS — Layer-1 primitives are grammar-neutral by construction (parse-that combinator surface is generic over byte slice). Zero Layer-0 gaps required = zero new generic-crate code with grammar-specific literals. | PASS — gaps name strict-comparator targets (yyjson hand-written SIMD per `p2e:326`); per-gap parser-source change is named. Equality oracle is existing parse-that test harness extended per gap. | PASS — parse-that has no codegen surface (no `@generated` headers to round-trip); the mandate is vacuously satisfied. | PASS — Gap 1 + Gap 6 explicitly collapse to one Layer-1 primitive (yyjson-shape match), preventing double-counting; no gap is admitted without a Layer-1 consumer named. | **ACCEPT** |
| `p2f-grammar-neutral.md` (14 candidates ALL clear Lock 14 v+1) | PASS — the artefact is the Lock 14 v+1 verdict surface itself. Each candidate's grammar-neutrality is the artefact's column. No `@generated` hand-write proposed. | PASS — **the artefact IS the Lock 14 audit surface for the 14 candidates** (5 audit-overlay citations per grep). All 14 clear v+1 generic-crate compliance per dispatch §1; zero JSON-overfit-irreducible. | PASS — strict-vs-strict comparator anchor per candidate row; per-iteration equality oracle inherited from P2-A R1 framing the artefact cross-references. | PASS — no codegen hand-patch proposed; the artefact gates other artefacts' candidates against the round-trip invariant. | PASS — the artefact's grammar-neutral verdict is itself the anti-scaffold gate: a candidate that fails Lock 14 v+1 here cannot be admitted by P2-A/B/C/D/E into S-P3. | **ACCEPT** |

**ACCEPT rate: 6/6 = 100%.**

---

## §2 — Audit-overlay binding intact

Per CHALLENGE-CONTEXT §2 CH7 line: "audit-overlay binding intact."

The audit-overlay surface is `restart/skinny/tranches/sk-v14/audit-overfit/` (confirmed exists: `SYNTHESIS-AUDIT-OVERFIT.md` + 6 axis files + hardening/). Citation distribution across the 6 P2 artefacts (grep `audit-overlay|audit-overfit|SYNTHESIS-AUDIT-OVERFIT|PRUNE-[0-9]`):

| Artefact | Direct audit-overlay citations |
|---|---|
| `p2a-sota-teardown.md` | 4 |
| `p2b-dav1d-process.md` | 0 (binds via Lock 14 v+1 amendment at `LOCKS.md:255-263`, which is the audit-overlay output) |
| `p2c-arch-esoterica.md` | 5 (PRUNE-2 cited as demotion authority) |
| `p2d-substrate-tape.md` | 0 (binds via V3 CH5 substrate-union 6/6 ACCEPT — the audit-overlay V3 verdict surface) |
| `p2e-parse-that-gaps.md` | 1 (`p2e:341` cites SYNTHESIS-AUDIT-OVERFIT.md as binding row-falsification list) |
| `p2f-grammar-neutral.md` | 5 |

**Total direct: 15. Indirect (via Lock 14 v+1 / V3 CH5 surfaces): all 6.** Audit-overlay binding is intact across all 6 P2 artefacts: 4 cite the synthesis filename directly, 2 cite its upstream-equivalent outputs (Lock 14 v+1 amendment for p2b; V3 CH5 substrate-union verdict for p2d). The latter two are not gaps — they cite the *binding result* of the audit-overlay rather than its index file, which is the materially load-bearing relationship.

---

## §3 — Mandate-by-mandate roll-up

| CH7 mandate | Status across 6 artefacts | Notes |
|---|---|---|
| (1) Grammar-derived code, no `// @generated` hand-write | PASS 6/6 | grep for `@generated|GENERATED` in P2 artefacts returns 1 hit (`p2c:43` cites "generated ASCII delimiter set" as the *mechanism*, not a hand-written header) |
| (2) Lock 14 v+1 generic-crate compliance | PASS 6/6 | p2f IS the Lock 14 verdict surface; p2b/p2d cite v+1 amendment verbatim; p2a/p2c/p2e per-candidate verdict carries the column |
| (3) Real source change + strict-vs-strict + per-iteration equality oracle | PASS 6/6 | p2a R1 framing + p2b checkasm-parity Stage B + p2d V3 CH5 two-cursor independence oracle |
| (4) Round-trip test for `generated` output | PASS 6/6 | Repo invariant `tests/regen_parity.rs` family; no P2 candidate proposes hand-patch route |
| (5) No SCAFFOLD-ONLY landing | PASS 6/6 | p2b Stage D + p2c 7 demotions + p2d C-P2D-4 REJECT all enforce same-wave-consumer-or-no-admit |

**Composite: 6/6 ACCEPT, no REVISE, no REJECT.**

---

## §4 — New findings

1. **Anti-scaffold-admit pattern executed in vivo.** `p2c-arch-esoterica.md` demotes 7 of 8 candidates to `NOT-S-P3-ELIGIBLE` at V1 specifically because the audit-overlay PRUNE-2 finding (23/24 CSS L4 rows lack profile attribution) blocks admission. This is the strongest CH7-affirmative pattern in the P2 corpus: the artefact actively refuses to admit candidates that would be scaffold-only without same-wave consumer evidence. Worth pinning as a positive precedent for S-P3 wave-manifest discipline.

2. **Lock 14 v+1 enforcement is structurally distributed.** p2f is the explicit Lock 14 verdict surface, but p2b makes Lock 14 v+1 the *gate* (Stage A scalar parameterisation + Stage E manifest column) for *all* primitives admitted by *any* S-P3 wave. This is a stronger binding than per-candidate column attestation: the admission process itself rejects Lock-14-violating primitives at the scalar-reference stage before checkasm or consumer wiring is attempted. Recommend S-P3 inherit p2b §2-§3 verbatim as the wave admission contract.

3. **Audit-overlay citation distribution asymmetry is benign.** p2b and p2d have zero direct citations of the `SYNTHESIS-AUDIT-OVERFIT.md` filename but bind via Lock 14 v+1 amendment (the audit-overlay's primary output) and V3 CH5 substrate-union verdict (an audit-overlay-consistent verdict surface) respectively. The binding is intact in *substance*; the *form* (direct filename cite vs upstream-equivalent surface cite) is acceptable under CH7. No revision needed, but a CH7-V2 or aggregator note should make explicit that p2b and p2d's binding pathway is via downstream outputs rather than the synthesis index.

4. **Round-trip mandate is vacuously satisfied for parse-that (p2e).** parse-that has no codegen / `@generated` surface, so CH7 mandate (4) is satisfied by absence. Worth tagging in the aggregator that this is a structural property of the parse-that crate, not a gap in p2e's discipline.

5. **No CH7 violation patterns detected.** grep across all 6 artefacts for `fake|hand-patched|hand-written.*@generated|@generated.*hand|scaffold-only|SCAFFOLD-ONLY` returns zero hits in candidate-admission contexts. The single `hand-written` occurrence in p2b is the dav1d-ASM *process* reference (dav1d hand-written ASM is the upstream reference for the bbnf-simd discipline), not a proposed bbnf-simd hand-write.

---

## §5 — Disposition

**CH7 verdict: 6/6 ACCEPT.** No REVISE; no REJECT. Audit-overlay binding intact across all 6 artefacts (4 direct + 2 via upstream-equivalent surfaces). Executable verification: 9 grammars enumerated as expected.

Pinned for aggregator (`HARDENING-S-P2-V1-CONSOLIDATED.md`):

- p2b §2-§3 admission contract for S-P3 wave inheritance (finding 2)
- p2c's 7-demotion pattern as anti-scaffold-admit precedent (finding 1)
- audit-overlay binding-via-downstream-output pattern is acceptable (finding 3)
- parse-that's structural exemption from mandate (4) (finding 4)

No CH7 REJECT trigger fires; no S-P2 V1 revise required on the CH7 axis.
