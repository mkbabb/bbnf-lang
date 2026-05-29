# SK-V17 S-P3 CHALLENGE — CH7 OVERFIT-PRUNE (V1)

Lens: CH7 OVERFIT-PRUNE. Cycle: V1. Date: 2026-05-29.
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` + `restart/skinny/tranches/sk-v17/SPEC.md`.
Mandate (PASS-3 §3W / ORCHESTRATOR §3W): no contrivance in the plan — lightningcss the
fair bar; success = real >SOTA on a regular corpus, not a broadcast; tailwind handled
honestly; no fixture/FNV re-entry; grammar-derived projections.
Master HEAD verified: `f87ee713a` (`git rev-parse HEAD` = `f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §0 — Verdict summary

The S-P3 packet is materially CH7-clean. The fair bar is the same-run re-baselined
lightningcss **full-CSSOM** build (verified live in source); the tranche success
criterion is a genuine per-corpus median crossing on a real CDN-pinned regular corpus,
NOT a broadcast; tailwind is held to an honest-residual discipline with corpus-average
substitution explicitly forbidden; the FNV/per-corpus-fixture re-entry seams are
pre-blocked at every wave; the projections are derived from the `BackendRule` shape with
`W5C_REQUEST_FACT_PROFILES` retired (not relocated). One REVISE: a wave-numbering
divergence between `p3b` (5-wave map) and the SPEC/`p3c` (6-wave map) leaves the
load-bearing >SOTA gate attributed to different wave numbers across artefacts — a
coherence defect that can mask which wave bears the close. No REJECTs.

Counts: ACCEPT 6, REVISE 1, REJECT 0.

## §1 — CH7-axis findings (the six contrivance vectors)

### 1.1 — Fair bar (lightningcss full-CSSOM, not a token-scan) — ACCEPT

The comparator is wired as a materializing full-CSSOM build, verified in source:
`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:113-115` —
`StyleSheet::parse(input, ParserOptions::default())` returning `sheet.rules.0.len()`
(a materialized rule count, not a token scan). The SPEC §0.2 comparator table and the
strict `gate-json` rule (`SPEC.md:122-126`) pin `css_comparator_plane=full-cssom` as the
ONLY strict admission anchor and explicitly demote cssparser token-scan to a flaw probe
(`SPEC.md:118`, never a SOTA claim). `p3c:24` restates this and `p3d:172` binds the
schema column to the §0.6 strict comparator. The denominator is the W0-re-baselined
same-run median (`lcss(corpus)@W0`), and `p3a:179-182`/`p3c:28` correctly mark all
per-corpus endpoints UNMEASURED-PENDING — the prior 793/833/929/974 numbers are
explicitly forbidden as gate denominators. No inferred endpoint is load-bearing. The bar
is fair and self-defended against being weakened to the token-scan plane. ACCEPT.

### 1.2 — Real >SOTA on a regular corpus, not a broadcast — ACCEPT

The four corpora are sha256-pinned real published CSS
(`skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54`): animate.css 4.1.1 (71750 B),
bootstrap 5.3.3 (232803 B), material-components-web 14.0.0 (495454 B), tailwindcss
(179631 B) — genuine widely-deployed stylesheets fetched from jsDelivr, not synthetic
fixtures. The tranche success criterion (`SPEC.md:216-220`, `p3c:161`) is a per-corpus
median crossing — `max(typed@close(animate)/lcss(animate)@W0,
typed@close(bootstrap)/lcss(bootstrap)@W0) > 1.0` at N≥50 — on a structurally-regular
corpus. The harness asserts N≥50 in code (`css_canon_bench.rs:250`,
`assert!(n >= 50, ...)`, verified present) and reports the MEDIAN
(`css_canon_bench.rs:160-166`). The 24-row W8R broadcast (one timing tuple replicated
across rows, RESULTS 112-135 / 5316-5350) is the explicitly-tracked tripwire: W0 retires
it (`SPEC.md:372`, `p3e:86`, `p3d:51-66`) and `gate-json` rejects any
`css_sample_count==1` or one-tuple-across-multiple-corpus-rows row
(`SPEC.md:190-191`, `p3d:61-66`). Success is a real measurement on a real corpus, never
a broadcast. ACCEPT.

### 1.3 — Tailwind handled honestly — ACCEPT

tailwindcss is the designated hardest hold-out (S-P1 measured the recognizer beats
lightningcss 3.09× but the eager path WATCHDOG'd 10583× under AZ-IV). The plan does NOT
contrive a tailwind win: `SPEC.md:84-88` (close condition 7) and `SPEC.md:210` require
tailwind benched cold N≥50 with median; ADMIT only if it crosses, ELSE the residual gap
is REPORTED with hot-leaf attribution and recorded in REDRESS — "NOT paper-closed, NOT
hidden behind a corpus average." `p3c:139` and the W5 falsifiability note (`p3c:144`)
make a tailwind "admit" without a per-corpus median > lcss an explicit CH6 FAIL, and the
W5 exit gate (d) greps RESULTS for a corpus-average admit claim and rejects it
(`p3c:141`). The tranche does not block on tailwind provided ≥1 regular corpus crosses
(`SPEC.md:210,219`). Honest residual is a first-class disposition, not a failure to be
papered. ACCEPT.

### 1.4 — No fixture re-entry — ACCEPT

The CH7-critical risk: the bench crate already carries a per-corpus
`real_typed_struct`/`fixture_for_name` path (`real_typed_struct.rs:1349,1517`, consumed
by `direct_struct.rs:458-470`), exactly the "per-corpus hand-coded `real_typed.rs`
fixtures" the SPEC pre-blocks. I verified the canonical CSS harness does NOT route
through it: `css_canon_bench.rs` parses each real corpus directly via
`css_decl::parser::parse_full` / `parse` (`:103-109,:123-128`), with zero reference to
`real_typed_struct` or `fixture_for_name` (grep of `css_canon_bench.rs` for
`real_typed`/`fixture`/`direct_struct` = empty). The tape-activated typed plane W1 builds
must therefore route through the same `css_decl::parser` surface, not the fixture path.
The pre-block is explicit and correctly attributed: REDRESS-70 (first eager
`real_typed_struct` rejected) is named in `p3e:98,170`; the SPEC global block
(`SPEC.md:742-744`) bars per-corpus `real_typed.rs` fixtures and hand-tuned per-corpus
capacity constants; L7 one-shot reserve is bound to "no per-corpus capacity literal"
(`SPEC.md:430`, `p3a:131,136` — grep-clean requirement). No fixture re-entry seam is left
open. ACCEPT.

### 1.5 — No FNV re-entry — ACCEPT

FNV / `push_ascii_lower_hex` (the 8.98-9.11% leaf) is correctly classed as retiring
WHOLESALE with the fact-stream String, never re-admitted as a primitive: `p3a:196`,
`p3e:189-190,249-250` (any NEON hex/FNV kernel pre-emptively REJECTed), `SPEC.md:744`
(FNV stays bench-only; no production FNV selector/arbiter/correctness/migration), and the
W3 pre-block list (`SPEC.md:601`, `p3c:181`) bars "FNV/hex as a primitive." The barred-set
in `p3a:194-200` and `SPEC.md:791-793` excludes the orphan udot/i8mm digit kernels and
FNV/hex by the categorical no-CSS-antecedent rule. FNV cannot re-enter as a hot-path
construct. ACCEPT.

### 1.6 — Grammar-derived projections (no relocated W5C overfit) — ACCEPT

The projection generator walks the SAME `BackendRule` shape the parser emits
(`SPEC.md:54-67` close condition 3, `p3a:84-93` S3, `p3c:90-102` W2), isomorphic to
JSON's `value_from_ref` (`json/value.rs:143`). The hand-coded
`W5C_REQUEST_FACT_PROFILES` routing array (`codegen/src/lib.rs:336`) is RETIRED and
DERIVED from the grammar — and the plan explicitly forbids the overfit re-entry seam of
relocating it into projection DATA or flag form (`SPEC.md:62-64,231-234,738-739`;
binding condition 3 `p3a:209-212`, `SPEC.md:781-783`). The L8 sparse-flag side-table is
constrained to `BackendRule` branch-tag projections, NOT a hand-curated per-rule
catalogue (binding condition 2 `p3a:207-208`, `SPEC.md:779-780`, W2 pre-block
`SPEC.md:522`) — the relocated-W5C overfit is named as a CH2 REVISE trigger. The W2 exit
gate is greppable: `grep 'W5C_REQUEST_FACT_PROFILES'` empty + no CSS-keyed per-rule match
arms JSON does not need (`p3c:99`, `SPEC.md:64`). Projections are grammar-derived by
construction with the overfit relocation explicitly barred. ACCEPT.

## §2 — REVISE disposition

### R-CH7-1 — Wave-numbering divergence: p3b (5-wave) vs SPEC/p3c (6-wave) — REVISE

`p3b-wave-sequencing.md:54-59,73,79-85` sequences a **5-wave** plan: W0 infra,
**W1 = tape activation + lazy projection MERGED** (levers 1+2, L2/L3/L8), **W2 = NEON
structural index** (lever 3, L1/L4/L5/L6/L7) carrying "THE >SOTA gate"
(`p3b:302` "W2 exit (THE >SOTA gate)"), **W3 = commit-by-construction spine** (L9,
conditional), W4 = close; "Wave count = 5 (W0-W4)" (`p3b:85`).

The SPEC (`SPEC.md:257-267`) and `p3c` (`p3c:46-54,104-116,157`) use a **6-wave** plan:
W0 infra, **W1 = tape activation**, **W2 = layout-driven projection SPLIT OUT**,
**W3 = NEON structural index** carrying THE >SOTA gate (`p3c:111`, `SPEC.md:583-587`),
**W4 = commit-by-construction spine**, W5 = close; "Wave count = 6" (`SPEC.md:266`).

Path:line: `p3b:54-59,73,79-85,302` vs `SPEC.md:257-267,266,583-587` /
`p3c:46-54,104-116,157`.

Why this is a CH7 concern (not purely CH1): the load-bearing >SOTA falsifiability gate —
the single measurement that admits the tranche — is attributed to **W2 in p3b** and
**W3 in the SPEC/p3c**. When the one gate that prevents a paper-close is referenced by
two different wave numbers across the binding artefacts, a downstream wave triumvirate
can satisfy "the W2 gate" (p3b) without satisfying "the W3 gate" (SPEC) or vice-versa,
which is exactly the seam through which a non-crossing corpus could be reported as
closed against the wrong wave's exit. The contrivance vector is real even though no
single artefact contrives.

Concrete fix: the SPEC is the authoritative contract (`SPEC.md:817` "The SPEC is the
contract"; §10 dispatch scope keys on the SPEC's W0-W5). Re-author
`p3b-wave-sequencing.md` to the SPEC's 6-wave map — split the merged p3b-W1 into
SPEC-W1 (tape, L2/L3-minimal/L7-conservative/L8) and SPEC-W2 (projection generator, L3
full rider + L4 + L8-read), renumber NEON to W3, spine to W4, close to W5 — and update
the `p3b:302` ">SOTA gate" attribution to W3 to match `p3c:111` and `SPEC.md:583-587`.
No content change to the gate predicate itself (it is identical in substance); the fix is
a wave-index reconciliation so all four binding artefacts (p3a/p3b/p3c/SPEC) name the
same wave for the same measurable close. This is a V2 fold for the P3-B re-dispatch.

## §3 — Lens-scope notes (deferred to sibling lenses)

- The W1 **+40% over fact_stream** threshold (`p3c:83`, `SPEC.md`-implicit) was audited
  for contrivance: it is a *conservative lower-bound tripwire* proving the String floor
  fell, NOT a cherry-picked success target. The fact_stream plane is 214-365 i/B vs
  full_parse 46-58 i/B (4.4× gap, S-P1 §3.4 / HARDENING-S-P1-V4 `:115,:118,:187`);
  removing even half the String tax yields far more than +40% throughput, so +40% is a
  falsifiable floor, not an inflated bar. CH7-clean. (Threshold *measurability* per se is
  CH1's province.)
- The p3b "Wave count = 5" vs SPEC "= 6" ceiling check (both ≤12) is a CH4 cost-axis
  note; flagged here only as the surface of R-CH7-1.

## §4 — Dispositions

| # | Disposition | Section | Path:line | Fix |
|---|---|---|---|---|
| 1 | ACCEPT | Fair bar (lightningcss full-CSSOM) | `css_canon_bench.rs:113-115`; `SPEC.md:118,122-126`; `p3c:24` | none |
| 2 | ACCEPT | Real >SOTA on regular corpus, not broadcast | `css_l4_corpus.rs:22-54`; `css_canon_bench.rs:250,160-166`; `SPEC.md:216-220,372`; `p3d:51-66` | none |
| 3 | ACCEPT | Honest tailwind | `SPEC.md:84-88,210,219`; `p3c:139,141,144` | none |
| 4 | ACCEPT | No fixture re-entry | `css_canon_bench.rs:103-128` (no `real_typed`); `SPEC.md:430,742-744`; `p3e:98,170`; `p3a:131,136` | none |
| 5 | ACCEPT | No FNV re-entry | `p3a:196`; `p3e:189-190,249-250`; `SPEC.md:601,744,791-793` | none |
| 6 | ACCEPT | Grammar-derived projections (no relocated W5C) | `SPEC.md:54-67,231-234,738-739,779-783`; `p3a:207-212`; `p3c:99` | none |
| 7 | REVISE | Wave-numbering divergence p3b 5-wave vs SPEC/p3c 6-wave | `p3b:54-59,73,79-85,302` vs `SPEC.md:257-267,583-587`; `p3c:46-54,111,157` | Re-author p3b to SPEC's 6-wave map; move ">SOTA gate" attribution to W3; reconcile all four artefacts to the same close-wave index |

## §5 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 scope matrix, §3 CH lens registry.
- `restart/skinny/tranches/sk-v17/SPEC.md` §0.1-0.5, §1, §2, §3-8 wave gates, §9 ledger, §10.
- `restart/skinny/tranches/sk-v17/research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md`.
- `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:103-128,160-166,250` (harness:
  full-CSSOM comparator, N≥50 assert, median).
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54` (sha256-pinned real corpora).
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:1349,1517` +
  `direct_struct.rs:458-470` (the fixture path verified NOT in the canonical CSS bench).
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  `:88-106,:115,:118,:187` (corpora, ratio band, instr/byte gap).
- Master HEAD `f87ee713a`.
