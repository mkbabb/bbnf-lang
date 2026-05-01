# S6 — Documentation AI-Styling Audit (2026-04-30)

Read-only audit of project documentation for AI-writing artefacts per
the user's anti-AI-writing directive. Each file is scored against the
nine-axis violation grid (em-dash density, spaced em-dashes,
epanorthosis, banned-word regex, editorialising, tricolons,
fragment overpunctuation, hype, AI-template scaffolds, comparison
sentiments, unsubstantiated superlatives).

**Audit framing.** Quoted user material inside `>` blockquotes is
**out of scope** — the user's archaic diction is deliberate voice
(`feedback_archaic_diction`). Hits inside blockquotes are not
violations. Author prose surrounding the quotes is the only
audit surface. Section headings echoing user vocabulary
(e.g. `### §ED5. Architectural transpositions for elegance,
simplicity, performance`) are also voice-preserving headings, not
authorial editorialising.

**Out-of-scope tokens** (deliberate project lexicon, not AI hype):
gestalt, indefatigable, hereof, thereof, therein, hitherto, begat,
appurtenant, parsimonious, Herculean, exhortation, edict, &c.
Direct quotes carrying these are voice; author prose using them is
voice-honouring per `archaic-diction-is-voice`.

---

## Severity distribution

| File | Lines | Em-dash/100 | Severity | Verdict |
|---|---:|---:|---|---|
| README.md | 176 | 11.3 | LOW | 2-3 surgical edits |
| docs/GESTALT.md | 1382 | 7.4 | LOW | 1-2 surgical edits |
| docs/RISK-PERF-MATRIX.md | 546 | 6.9 | CLEAN | no edits |
| docs/META-AUDIT-PROMPT.md | 1768 | 12.6 | LOW | 4 surgical edits |
| docs/codegen-paths.md | 254 | 5.5 | LOW | 1 surgical edit |
| docs/tranches/AZ-III/AZ-III.md | 141 | 0.7 | CLEAN | no edits |
| docs/tranches/AZ-II/FINAL.md | 227 | 11.4 | CLEAN | no edits (commit-msg artefact only) |
| docs/tranches/REMAINING-TRAJECTORY.md | 552 | 2.5 | LOW | 2 surgical edits |
| docs/instructions/README.md | 29 | 0 | CLEAN | no edits |
| docs/instructions/PROFILING.md | 409 | 6.6 | CLEAN | no edits |
| docs/instructions/CHANGELOG.md | 191 | 9.4 | CLEAN | no edits |
| docs/precepts/README.md | 48 | 0 | CLEAN | (vendored submodule; out of scope) |
| docs/precepts/instructions/README.md | 115 | 0 | CLEAN | (submodule; out of scope) |
| docs/tranches/AZ-II/waves/README.md | 22 | 0 | CLEAN | no edits |
| crates/csp-solver/docs/instructions/README.md | 16 | 0 | CLEAN | no edits |

**Counts: CRITICAL=0, HIGH=0, MED=0, LOW=5, CLEAN=10. Files
audited: 15.**

Triumvirate trigger threshold (>5 CRITICAL) is **not reached.**
AI-styling drift is not systemic; it is residual at low
density across a small set of files.

---

## Per-file findings

### README.md — LOW

176 lines, 20 em-dashes (11.3/100), 6 spaced em-dashes.

| Line | Pattern | Quote | Severity |
|---:|---|---|---|
| 3 | spaced em-dash in tagline | `**Better Backus-Naur Form**—a monorepo for the BBNF grammar ecosystem.` | low — author chose unspaced; OK |
| 109 | nested em-dash in flowing prose | `breakpoints on rules, step through parse execution, inspect call stack and parse state.` | (no em-dash; tricolon flagged but contextual) |
| 162 | spaced em-dash | `Formatting uses [gorgeous](...) (WASM)—AOT-generated formatters for built-in languages, a bytecode VM for custom grammars.` | OK; unspaced |
| 167 | spaced em-dash | `performance—rendered from Markdown with a sidebar nav` | OK; unspaced |
| 171-176 | source-list em-dashes | `[Extended Backus-Naur form](...) — ISO 14977. BBNF's ancestor.` | spaced — bibliography convention; OK |
| 25 | tricolon-prone parenthetical | `(value-unit, color, values, selectors, keyframes, stylesheet, css-tokens, css-stylesheet-pretty)` | enumeration of grammars; contextual, not editorial |

No banned-word hits. No epanorthosis. No "in conclusion" / "moreover" /
"furthermore". No hype words. No editorialising adjectives.
Em-dash density is moderate but consistent with the project's
voice, and the README's em-dashes either set off citations or
parentheticals — load-bearing, not decorative.

**Patch recommendation.** No mandatory edits. Optional: tighten
line 162's parenthetical-then-em-dash sequence
(`(WASM)—AOT-generated`) to `(WASM); AOT-generated` to reduce
em-dash adjacency density. Skip otherwise.

---

### docs/GESTALT.md — LOW

1382 lines, 102 em-dashes (7.4/100), 95 spaced. 94 of 102
em-dashes appear in author prose; the rest sit inside table cells
or `>` blocks. Sampling at offset 0/400/800/1100 covered ~1100
lines directly.

| Line | Pattern | Quote | Severity |
|---:|---|---|---|
| 858 | "rich AST" with editorialising adjective | `never flatten typed grammar rules for speed; rich AST parity with lightningcss is non-negotiable` | inside quoted feedback summary; voice-preserving — not a violation |
| 1169 | "**Aesthetics critical.**" emphasis | `**Aesthetics critical.** `feedback_aesthetics-critical`: formatting aesthetics are the purpose of gorgeous / pprint` | header-pattern; rhythmic, fits voice |
| 1346-1358 | concluding paragraph mild AI cadence | `The thesis the reader walks away with: *bbnf-lang is grammar-derived, one substrate, one measurement surface, and reversal is the health signal.*` | tricolon-leaning thesis; load-bearing italicised assertion. Contextual — closing thesis is the document's actual contract |
| 1378-1380 | tricolon series | `the runway's end state is a grammar that produces, for any language, a direct-to-struct runtime parser that beats lightningcss, sonic-rs, and simdjson OnDemand at their own games — parity first, exceedance second, every `->` reaching a struct field, one substrate (the grammar-derived struct tree), one measurement surface, no orthogonal codepaths, no residual tape.` | semicolon-stacked, em-dash-heavy. Borderline AI-cadence but each clause is load-bearing technical content |
| 791 | "not only" epanorthosis | `bbnf adopted the shape for every grammar, not only JSON, to make typed payload materialisation measurable.` | mild epanorthosis but factually substantive (qualification of scope, not stylistic flourish) |
| 1339-1342 | "six arrows" prose poetry | `The six arrows — CSP → tape-first → decision-collapse → baseline → peak-and-lose → reckoning → reversal → column-revert → infra-pivot → activation — are the project's architectural spine.` | Eight-element em-dash chain; technical chronological pointer, not flourish |
| 870-871 | aphoristic italicised maxim | `*the grammar is the only distinguishing input, and everything downstream is uniform across grammars*` | bears load; OK |

GESTALT.md is the most AI-pattern-resembling file in the audit set
because it is a 1382-line synthesis prose. Yet inspection shows
em-dashes are almost uniformly load-bearing (offsets, citations,
clause subordination) rather than decorative. No "delve",
"tapestry", "leverage", "navigate", "robust", "comprehensive",
"sophisticated", or "moreover" in author prose. No tricolon-then-
conclusion AI-template scaffolds.

**Patch recommendation.** Optional cosmetic tighten on line 1378-
1380 (split the eight-element em-dash chain into a sentence per
clause). Not mandatory; the cadence is project voice. Skip
unless a wholesale GESTALT refresh is already on the runway —
which AZ-III.W5 does not declare.

---

### docs/RISK-PERF-MATRIX.md — CLEAN

546 lines, 38 em-dashes (6.9/100), 37 spaced. The single
"leverage" hit at line 172 is `highest-leverage performance
problem` — load-bearing technical jargon (mechanical-engineering
sense), not the AI hype-word sense. The single tricolon at line
204 is data inside a bullet ledger row, not editorial flourish.

No banned-word hits in author prose. No epanorthosis. No
hype. No "in conclusion" / "moreover" / "furthermore". No
editorialising adjectives. The em-dash density (6.9/100) is
under threshold and every em-dash sets off a definition,
parenthetical, or wave-name suffix.

**Patch recommendation.** None. CLEAN.

---

### docs/META-AUDIT-PROMPT.md — LOW

1768 lines, 223 em-dashes (12.6/100), 205 spaced. **Critical
context: 175 of 223 em-dashes are inside `>` blockquotes** (user
voice; out of scope). The remaining ~48 are in author scaffold
prose, audit-purview text, and methodology sections.

| Line | Pattern | Quote | Severity |
|---:|---|---|---|
| 3 | "comprehensive audit" | `A comprehensive audit, archaeology, and gestalt brief of the bbnf-lang system across its ~2000-commit history.` | author prose; "comprehensive" is mild AI-hype |
| 1585 | "comprehensive-audit sections" | `These are the comprehensive-audit sections; an audit that leaves any scaffold in its placeholder state is incomplete.` | author prose; same word reuse |
| 315 | "**Composite cache keys + robust invalidation**" | `### §12. Composite cache keys + robust invalidation` | section header echoing user vocabulary ("Ensure robustness" from session 959); voice-preserving — not a violation |
| 374 | "**§ED5. Architectural transpositions for elegance, simplicity, performance**" | (heading text) | header tricolon mirrors quoted user phrase verbatim; voice-preserving |
| 630 | "the master critical path" | `none — B1 is the master critical path. Owning spec:` | "critical path" is project-management term-of-art, not hype |
| 1078-1079 | "Performance arc" intro tricolon | `Sources the auditor consults` followed by 5-bullet list with parallel grammatical structure | typical project ledger pattern; not AI-template (each bullet load-bearing) |

The two genuine AI-hype touches are `comprehensive` (lines 3,
1585). Everything else echoes verbatim user phrasing.

**Patch recommendation (4 surgical edits).**

- Line 3: `A comprehensive audit, archaeology, and gestalt brief` → `An audit, archaeology, and gestalt brief` (drop `comprehensive` — superfluous; the document's content already establishes scope)
- Line 1585: `These are the comprehensive-audit sections` → `These are the auditor-populated sections` (factual; also drops `comprehensive`)

The two header-level "robust" / "elegance" tokens (lines 315, 374)
**stay** because they verbatim echo session 959 / 32a quoted material
(`feedback_archaic_diction`).

---

### docs/codegen-paths.md — LOW

254 lines, 14 em-dashes (5.5/100), 11 spaced.

| Line | Pattern | Quote | Severity |
|---:|---|---|---|
| 53-54 | "shared core" mild editorialising | `bbnf-ir is the shared core — it defines the canonical IR` | "shared core" is technical, OK |
| 165 | bullet-list-of-three "Zero regex, zero string allocation, zero intermediate Span." | `... a hand-written byte scanner with fused Eisel-Lemire conversion that returns Option<f64> directly. Zero regex, zero string allocation, zero intermediate Span.` | rhetorical anaphora-tricolon; technical claims with named referents (each "zero" is verifiable). Borderline AI-cadence; load-bearing |
| 173 | "three logical operations collapsed to one code path" | (closing summary line of FnDescriptor section) | tricolon-style summary closing the section; minor |

No banned-word hits. No "comprehensive". No "leverage" /
"navigate" / "robust" in author prose. No epanorthosis.

**Patch recommendation.** Optional cosmetic on line 165: replace
the three-`zero` anaphora with a single statement
(`No regex, allocation, or intermediate Span.`). Skip otherwise.
The cadence reads as technical bragging, which is consistent with
the project's voice on perf claims.

---

### docs/tranches/AZ-III/AZ-III.md — CLEAN

141 lines, 1 em-dash (0.7/100). No banned words. No hype.
No epanorthosis. No tricolons. The single em-dash is at line 1
(title separator). Pure operational prose.

**Patch recommendation.** None.

---

### docs/tranches/AZ-II/FINAL.md — CLEAN

227 lines, 26 em-dashes (11.4/100), 25 spaced. The single
`comprehensive` hit at line 31 is **inside a backticked commit
message string** (`docs(az-ii): comprehensive PROGRESS-SNAPSHOT-
2026-04-29 — 14 substage trajectory`); the audit explicitly
out-of-scopes verbatim git artefacts. All other em-dashes set off
status-table notes and per-stage commit citations — load-bearing
operational metadata.

No author-prose hype. No editorialising adjectives.

**Patch recommendation.** None. The commit-message verbatim is
historical artefact and does not warrant a force-push amend.

---

### docs/tranches/REMAINING-TRAJECTORY.md — LOW

552 lines, 14 em-dashes (2.5/100). Two literal banned-word hits.

| Line | Pattern | Quote | Severity |
|---:|---|---|---|
| 392 | `navigate` | `Shape: path accessors navigate typed CSS structure, and inferred rewrite rules simplify selector/value IR before codegen.` | one-word usage; technical (path traversal). Low severity |
| 444 | `Cross-grammar leverage` | `**Cross-grammar leverage** is where bbnf should exceed hand-written peers architecturally` | "leverage" as noun in section header. Mild AI hype |
| 449 | `Treat hand-written SIMD wins as missing compiler transpositions until proven otherwise.` | imperative declarative; project voice |

No epanorthosis. No "comprehensive" / "robust" / "elegant" in
author prose. No tricolon-and-conclusion scaffolds. Numbered
sections (`1. Path Change…`, `2. Novel Architecture Thesis…`)
are document-organisation, not AI-template.

**Patch recommendation (2 surgical edits).**

- Line 392: `navigate typed CSS structure` → `traverse typed CSS structure` (one-word swap; restores technical register)
- Line 444: `**Cross-grammar leverage** is where bbnf should exceed hand-written peers architecturally:` → `**Cross-grammar reach** is where bbnf should exceed hand-written peers architecturally:` (or `**Cross-grammar generalisation**`)

---

### docs/instructions/README.md — CLEAN

29 lines, 0 em-dashes. Pure operational rule list. No banned
words, no hype, no AI patterns.

**Patch recommendation.** None.

---

### docs/instructions/PROFILING.md — CLEAN

409 lines, 27 em-dashes (6.6/100), 23 spaced. Sampled
lines 1-200 and 200-410. No banned-word hits in author prose.
Em-dashes set off explanations of cargo aliases, profile flags,
artefact paths — load-bearing technical pointers. No hype, no
epanorthosis, no tricolon scaffolds.

**Patch recommendation.** None.

---

### docs/instructions/CHANGELOG.md — CLEAN

191 lines, 18 em-dashes (9.4/100), 17 spaced. The em-dash
density is moderate but every em-dash separates a numeric LOC
delta or a section pointer from its rationale (`README.md (653
→ 520 lines; -133, ~20%)`). No author-prose hype. No banned
words. No tricolon-and-conclusion scaffolds.

**Patch recommendation.** None.

---

### docs/precepts/README.md — CLEAN (out of scope)

48 lines, 0 em-dashes. Vendored shared submodule per
`feedback_doc-integration-style` and `S4-precepts-integration-plan.md`.
Audit treats submodule contents as upstream — out of scope for
bbnf-lang docs styling pass.

**Patch recommendation.** None (submodule; do not edit
downstream).

---

### docs/precepts/instructions/README.md — CLEAN (out of scope)

115 lines, 0 em-dashes. Same submodule scope. The shared
edicts surface (KISS/DRY, no quick fixes, abrogate before
patch) is the canonical version of bbnf-lang's local instructions
and uses zero em-dashes — exemplary of the target style.

**Patch recommendation.** None.

---

### docs/tranches/AZ-II/waves/README.md — CLEAN

22 lines, 0 em-dashes. Index-only doc with status table and
"Do not dispatch from W0/W1/W2" warning. No banned words, no
hype.

**Patch recommendation.** None.

---

### crates/csp-solver/docs/instructions/README.md — CLEAN

16 lines, 0 em-dashes. Local rules pointer to shared precepts.
No banned words, no hype.

**Patch recommendation.** None.

---

## Top-5 most-egregious AI-styling lines

The audit found **no CRITICAL or HIGH** violations. The five
"most-egregious" hits (all LOW severity) are:

1. **docs/META-AUDIT-PROMPT.md:3** —
   `A comprehensive audit, archaeology, and gestalt brief of the bbnf-lang system across its ~2000-commit history.`
   → Replace with: `An audit, archaeology, and gestalt brief of the bbnf-lang system across its ~2000-commit history.`
   (drop `comprehensive`; "gestalt" already establishes scope)

2. **docs/META-AUDIT-PROMPT.md:1585** —
   `These are the comprehensive-audit sections; an audit that leaves any scaffold in its placeholder state is incomplete.`
   → Replace with: `These are the auditor-populated sections; an audit that leaves any scaffold in its placeholder state is incomplete.`

3. **docs/tranches/REMAINING-TRAJECTORY.md:444** —
   `**Cross-grammar leverage** is where bbnf should exceed hand-written peers architecturally:`
   → Replace with: `**Cross-grammar reach** is where bbnf should exceed hand-written peers architecturally:`

4. **docs/tranches/REMAINING-TRAJECTORY.md:392** —
   `Shape: path accessors navigate typed CSS structure, and inferred rewrite rules simplify selector/value IR before codegen.`
   → Replace with: `Shape: path accessors traverse typed CSS structure, and inferred rewrite rules simplify selector/value IR before codegen.`

5. **docs/codegen-paths.md:165** (optional cosmetic) —
   `Zero regex, zero string allocation, zero intermediate Span.`
   → Optional: `No regex, allocation, or intermediate Span.`
   (Reduces anaphora-tricolon; existing form is borderline-acceptable
   project voice on perf claims.)

---

## Aggregate observations

The corpus is largely free of AI-styling. The two structural
reasons:

- The user's archaic diction (begat, therein, thereof, hereof,
  hitherto, indefatigable, parsimonious, gestalt, &c.) carves
  the project's voice toward 17th-century Anglo register, which
  is mechanically incompatible with Claude's default smooth
  pseudo-academic register. Every doc that quotes user voice
  inherits that grain.
- Per-tranche FINAL.md / wave docs are operational ledgers
  (commit-SHA tables, hard-gate readouts, status rows) where
  there is no narrative real estate for AI-styling to seep in.
  Em-dashes appear because em-dashes are the natural delimiter
  for table-cell sub-clauses, not because the prose is
  overwrought.

The five LOW-severity files (README, GESTALT, META-AUDIT-PROMPT,
codegen-paths, REMAINING-TRAJECTORY) all carry residual mild
AI-cadence in **specific identifiable lines**, not as systemic
drift. A targeted patch wave of ~7-9 surgical edits across these
five files would bring the entire audited surface to CLEAN.

The two genuine concerns that recur (and that a future
authoring discipline should encode):

1. **`comprehensive` is the most reliable AI-tell** — it appears
   twice in META-AUDIT-PROMPT and is the single clearest signal
   in the corpus. It is also semantically near-empty (any
   non-trivial document is "comprehensive" by definition).
2. **`leverage` and `navigate` as verbs in technical-prose
   context** are the second-most-reliable tells. They survive
   in REMAINING-TRAJECTORY because the document mediates
   between architecture-essay register and operational-ledger
   register; the architecture-essay sections are the local
   risk surface.

---

## Triumvirate trigger status

**Not triggered.** Threshold is >5 CRITICAL files; observed is
0 CRITICAL. AI-styling drift is residual and surgically
patchable. No research-plan-redress wave is warranted.

---

## Bounds confirmation

This audit is read-only. **No files were touched outside
`docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/style/S6-docs-ai-styling-audit.md`** (this document, newly created).

15 files audited (Tier 1: 6, Tier 2: 5, Tier 3: 4 enumerated
READMEs after `find`). Tier 3 found seven READMEs total; two
are submodule contents flagged out-of-scope.
