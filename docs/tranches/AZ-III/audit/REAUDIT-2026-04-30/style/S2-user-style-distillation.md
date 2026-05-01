# S2 - User Style Positive-Pattern Distillation

Pragmatic, economical prose. Accessible, unpretentious, at times academic.
Domain verbiage where it earns its place. ~5% poetic lilt, never grandiloquent.
Direct assertions only; no epanorthosis. Contractions almost always. Em-dashes
limited and unspaced in technical registers. Multi-disciplinary fluency
allowed when natural, never forced.

This document distills the user's positive style signals into a pattern
catalog usable for prose generation, doc review, and commit-body voicing.
It pairs with S1 (anti-patterns) to define the calibration spectrum.

## 1. Domain-Specific Verbiage

The user pulls precise terms from the originating domain and deploys them in
adjacent contexts when the term carries the intended shade of meaning more
exactly than any general substitute. The general usage strips the term of
its parochial baggage; what remains is a sharper instrument.

| Term | Native Domain | Generalized Use |
|---|---|---|
| `elision` | grammar / phonology / CS | dropping any inferable element; "elide the unnecessary projection" |
| `touch` | basketball / jazz | feel for execution; "the prose has the wrong touch" |
| `panoply` | classical antiquity (full armor) | a complete arrayed suite; "panoply of optimizations" |
| `deftness` | physical craft | precise, light, unforced; "land the doc with deftness" |
| `cadence` | poetry / music / military | rhythmic regularity in any sequence |
| `gestalt` | psychology / Bauhaus | the whole-as-form; routinely a tranche meta-doc name |
| `parity` | mathematics / sport | exact equivalence; "lightningcss parity" |
| `tranche` | finance / French | a discrete bounded slice; co-opted as the project's wave name |
| `progenate` | coined from Latin | to generate forward; user coinage, deliberate |
| `begets` | scripture / archaic | causes-by-extension; "this begets the next round of failures" |

Principle: deploy the term when it does work no plain alternative does.
Never deploy as ornament. The signal that a term is earning its keep:
removing it forces an inferior rephrasing, not a synonym swap.

Skill required: register-match the domain to the audience. CS verbiage in
LSP comments, financial verbiage in tranche planning, musical verbiage in
formatting/prettifier prose, theological verbiage when the lineage section
already invokes scripture.

## 2. Word-Level Register Markers

The `be-` compound family and the demonstrative-archaic adverbs are
deliberate voice. Per `feedback_archaic_diction`: leave them alone, do not
"fix" them, do not flag them as AI artifact.

`be-` compounds: besot, behoove, bedraggle, becalm, bedeck, bequeath, befall,
beget, beseech. Use when the prefix conveys "thoroughly" or "upon", not as
mere intensifier. "It behooves the parser to fail fast" reads correctly;
"besotted with metrics" reads correctly; "bedraggled by carry-overs" reads
correctly.

Demonstrative archaisms: heretofore, hitherto, whereof, wherefore, thereof,
therein, thereupon, henceforth, whereby, thereby. Cluster inside a single
clause works; sprinkling across paragraphs reads as costume.

Coined formations: `progenate`, similar Latinate back-formations. The user's
coinages are intentional; preserve verbatim.

When befitting:
- prose with a synthesizing or formal register (gestalt docs, scriptural
  invocations, ballet libretto)
- precept text where the archaic phrasing renders the rule as a maxim
- tranche redress that names a chronic pattern

When forced:
- inline code comments
- error messages
- bench harness output
- bullet lists of mechanical actions
- hot-path implementation notes

## 3. Cosmopolitan Phrases

French / Latin / occasional Italian, restricted to phrases that name a
distinct concept English lacks a single word for. The user's directive:
**never with pretentious or grandiloquent air**. The phrase must serve
precision, not signal cultivation.

| Phrase | Meaning | Appropriate Site |
|---|---|---|
| `laissez-faire` | hands-off as policy | architecture / orchestration choices |
| `dernier cri` | the latest fashion | naming a cargo-cult pattern critically |
| `en coulisses` | behind the curtain | tooling / orchestration commentary |
| `à fond` | thoroughly, all-out | optimization passes / commit work |
| `prima facie` | on first sight | adversarial review framing |
| `de facto` / `de jure` | actual / formal | distinguishing implementation vs spec |
| `ipso facto` | by that very fact | logical chain conclusions |
| `ad hoc` | for-this-purpose | pejorative for non-systemic patches |
| `tour de force` | feat of skill | reserved; one per long doc max |
| `mise en scène` | scene-setting | only in libretto / aesthetic prose |

Calibration test: read the sentence with the English equivalent
substituted. If the meaning shifts or thins, the phrase earns its place. If
the meaning is unchanged and the sentence merely flatters less, the
phrase is grandiloquent and must come out.

Disallowed even when literally accurate: foreign phrases used to badge
the writer as cosmopolitan. The user explicitly rejects this register.

## 4. Poetic Lilt Budget (~5%)

Mild, evocative, unintrusive. The lilt is mechanism, not substance: it
carries an existing point on a rhythm the prose has already earned, never
introduces ornament for ornament's sake.

Mechanisms the user's own writing uses:

- **alliteration** (light, bilabial or sibilant), e.g. "gestalt of the
  grammar gives" rather than "the grammar's overall structure provides"
- **archaic verb conjugation** in stylized passages: "thou shalt", "it
  behooveth", "wherefrom it springeth"
- **biblical cadence** for synthesizing or invocational closes, ternary
  rhythm with semantic parallelism: "what the parser wrought, the
  prettifier preserves, the formatter beholds"
- **enjambment in libretto** prose: line breaks that defer the verb
- **mild assonance** for closure rhythm in commit bodies: "land the
  cleanup; the tranche stands"

Calibration spectrum, drawn from the user's own examples:

| Site | Lilt Density | Mechanism Set |
|---|---|---|
| Wolf of Gubbio passage (synthesis prose) | medium-high | biblical cadence, archaic conjugation, ternary rhythm |
| Ballet libretto (myrtha / Giselle, Scene 1) | high | enjambment, archaic register, evocative imagery |
| Small poem / quip aside | high but compressed | one mechanism, executed cleanly |
| Bio prose | low | alliteration only, accessible |
| Tranche docs / synthesis | low to none | direct prose, occasional ternary close |
| Commit-message bodies | none-to-trace | direct prose, parallel structure for clauses |
| LSP / hot-path comments | zero | technical only |

Test: if the lilt mechanism removed leaves the sentence still doing its
job, keep it (mild lilt, fine). If the lilt was the substance, the
sentence was performing rather than asserting; rewrite.

## 5. Direct-Assertion Rule (No Epanorthosis)

Epanorthosis is the rhetorical figure of correcting one's own previous
statement for emphasis: "It isn't good, it is great." The user explicitly
rejects this construction.

| Wrong (epanorthosis) | Right (direct assertion) |
|---|---|
| "It isn't good, it is great" | "It is great" |
| "This isn't a patch, it's an architectural fix" | "This is an architectural fix" |
| "Not merely faster, but transformative" | "It is transformative" / "It is faster by 87x" |
| "Not just elegant, but necessary" | "It is necessary" |
| "Not a stub, but a real implementation" | "It is a real implementation" |
| "Not slow, but fast" | "It is fast" / cite the number |

The pattern's defect: the negation half is a covert claim that the reader
might think the lesser thing, planted to make the upgrade seem won.
Direct assertion respects the reader and lets the claim stand alone.

Permitted negations: when the negative half names a real prior position
being overturned. "We had thought X; X is wrong; the truth is Y" reads
correctly because each clause carries weight. The forbidden form is the
two-clause flourish where the negative is rhetorical scaffolding.

Adjacent forbidden patterns:
- "more than just" (variant epanorthosis)
- "transcends mere X" (worse register variant)
- "It's not about X, it's about Y" (the same trick, conversational tone)

## 6. Contractions

Almost always. Catalog of the standard set the user uses freely:

`don't`, `won't`, `can't`, `isn't`, `aren't`, `wasn't`, `weren't`,
`hasn't`, `haven't`, `hadn't`, `doesn't`, `didn't`, `wouldn't`, `couldn't`,
`shouldn't`, `mustn't`, `needn't`, `shan't`, `mightn't`, `oughtn't`,
`it's`, `that's`, `what's`, `who's`, `there's`, `here's`, `where's`,
`how's`, `let's`, `I'm`, `I'd`, `I'll`, `I've`, `you're`, `you'd`,
`you'll`, `you've`, `we're`, `we'd`, `we'll`, `we've`, `they're`,
`they'd`, `they'll`, `they've`, `he's`, `she's`, `would've`, `could've`,
`should've`, `might've`.

When the uncontracted form is correct:
- formal scriptural / lineage prose (preserve the cadence of "we shall")
- ballet libretto where the un-elided form fits the meter
- precept maxims where compression to a single declarative is required
  ("Do not route around the plan" reads correctly without contraction)

Default: contract. The contracted form is the unmarked register, not the
casual one. Forcing the uncontracted in technical prose reads stilted.

## 7. Em-Dash Usage

Limit. The em-dash is over-deployed by AI generators; the user's
directive trims it.

Two registers govern spacing:

**Technical / precept / tranche docs** - prefer no em-dashes at all;
where used, **unspaced**: `text—more text`. Use only when a comma or
parens would distort meaning. Most often replace with sentence break or
parens.

**Poetic lilt / libretto / synthesis prose with cadence** - em-dashes
acceptable, **spaced**: `text — more text`. The space slows the reader,
serving the cadence. Density still capped: roughly one per paragraph.

Diagnostic patterns to scrub:
- em-dash interrupting list-like enumeration ("we ship A — and B — and C") -
  replace with semicolons or commas
- em-dash setting off a closing flourish ("the parser wins — finally") -
  replace with full stop and a separate sentence, or delete the flourish
- em-dash replacing a colon before exemplification - use a colon
- em-dash chains within a single sentence - rewrite

Forbidden: triple em-dashes, em-dash followed by ellipsis, em-dash inside
parentheses.

## 8. Multi-Disciplinary Fluency

The user's allowed reference set, with characteristic deployment:

| Domain | Anchors | Where it surfaces |
|---|---|---|
| Computer Science / PLT | grammars, parsing, type theory, compiler arch | core technical prose |
| Mathematics | category theory hints, parity, isomorphism, fixed-point | architecture / invariants |
| Music | Romanticism (Wagner, Mahler, Brahms), jazz (Coltrane, Mingus) | rhythm, formatting prose, lilt source |
| Theology | scripture (Philippians, Wolf of Gubbio / Franciscan), Augustine | synthesis closes, lineage section |
| Philosophy | classical (Stoic, Aristotelian), continental fragments | precept formulations |
| Art / Design | Memphis group, mid-century modern, Bauhaus | UI/aesthetic prose, naming |
| Botany | Anthurium, evergreens, deciduous specifics | bio / aesthetic prose |
| Satire | Juvenalian (savage), Horatian (genial), jeremiad | adversarial review framing |
| Classical Antiquity | Greek + Latin coinage roots, Stoic ethics | etymological precision |
| Dance | ballet (Giselle, La Sylphide vocabulary, Myrtha) | libretto, rare in tech prose |

Rule: reference is allowed when the domain term carries the precise
shade. Reference is forbidden when it functions as a credential signal.
A mention of Mahler in a discussion of long-form musical structure
mapping to long-form code structure earns its place. A mention of
Mahler in a paragraph about a benchmark regression does not.

Cross-domain bridges, when they're appropriate, must be load-bearing:
the analogy must do explanatory work, not flatter.

## 9. Tone

**Accessible, unpretentious, and at times academic.** The compact rule:

> Write so a competent reader follows without working; reach for the
> exact word when the easier word is wrong; allow the rhythm to lift
> only at synthesis points.

Decomposed:

- **Accessible**: short sentences when the content is mechanical; complex
  clause structures only when the structure mirrors the structure being
  described
- **Unpretentious**: no costume, no badge phrases, no rhetorical
  flourishes that don't pay rent
- **Academic at times**: when invoking lineage, when naming a precise
  technical concept, when the synthesis benefits from formal cadence

Failure modes the tone forbids:
- breathless marketing ("This. Changes. Everything.")
- meta-language ("comprehensive", "ecosystem", "robust", "powerful")
- recursive self-praise ("a tour de force of engineering")
- staccato emphasis fragments
- conversational filler ("So, basically, what we did was...")

The tone is not "academic" in the pejorative sense (windbag,
hedging, citation-bloat). It is academic in the precise sense: domain
terms used with their domain meaning, claims constrained to what the
evidence supports, parenthetical specificity preferred over sweeping
nouns.

## 10. Examples Calibration Spectrum

Plot of register-by-context, derived from the user's own writing samples:

```
            POETIC LILT  +-->-->-->-->-->-->-->-->-->-->-->-->-->  TECHNICAL
                       
ballet libretto (myrtha)  --|
Wolf of Gubbio passage    ----|
small poem / quip            -----|
scriptural lineage section      ------|
synthesis docs (gestalt)              -------|
bio prose                                 ----------|
tranche redress prose                          -------------|
tranche plan / SPEC                                ------------------|
precept maxims                                      -------------------|
commit-message bodies                                  ------------------|
LSP comments / inline docs                                  ----------------|
hot-path code comments                                          --------------|
benchmark output / errors                                            ------------|
```

Per-site rules:

**Bio prose** - unpretentious-academic. Short paragraphs. Domain term
density medium. Lilt: alliteration only. Contractions yes. Em-dash:
unspaced, sparse. Multi-disciplinary references allowed when load-bearing.

**Ballet libretto** - full poetic register. Archaic conjugation
permitted. Spaced em-dashes acceptable. Domain verbiage from dance and
its musical accompaniment. Direct-assertion rule still binding inside
declarative passages; lyric passages exempt.

**Commit-message bodies** - unpretentious-technical. Subject ≤72 chars,
imperative mood, scope prefix. Body explains the *why* with parallel
clauses; cite files and line numbers; preserve precise domain terms;
contractions yes; em-dash sparse and unspaced; no lilt mechanisms.
Direct-assertion rule binding.

**Tranche docs / synthesis** - unpretentious-academic. Synthesis prose
permits ternary closing rhythm and one mild lilt mechanism per major
section. Precept-style maxims may run uncontracted. Em-dashes unspaced
and rare. Multi-disciplinary references only where they explain.

**LSP / hot-loop / inline code comments** - pure technical. No
contractions debate (use either; doesn't matter). No archaic markers.
No lilt. Domain verbiage at maximum density and precision. Em-dash
forbidden in code comments; use parens or sentence break.

**Precept text** - unpretentious-academic with maxim register. Often
uncontracted for the rule statement, contracted in the explanatory
prose under it. No em-dashes inside maxims. Direct-assertion absolute.

**README** - per `feedback_readme_style`: lead with code; explain
after; no meta-language; no overly-structured text; no superfluity;
treat the project as standalone. Tone slightly warmer than tranche
docs, still unpretentious.

**Scriptural / lineage prose** - permits highest archaic register the
project ever uses. Uncontracted. Spaced em-dashes acceptable. Biblical
cadence permitted. The site at which `besot`, `whereof`, `thereupon`
etc. read most naturally.

## Cross-Cut Calibration Rules

**Density rule**: domain-verbiage density tracks the audience. Internal
team documents hold the highest density; bio prose holds medium; READMEs
addressed to outsiders hold the lowest with a brief glossary cushion if
the term must enter early.

**Drift detection**: if a draft's word count grew without information
density growing, lilt or grandiloquence has crept in. Trim.

**One-pass test**: read the draft aloud once. Forced archaisms snag the
ear. Forced cosmopolitan phrases snag worse. Lilt that earns its place
flows; lilt that doesn't reads as costume on the second beat.

**Negative space**: what the user does NOT do is itself the signal. No
emoji. No marketing. No "let me know if". No covert hedges
("perhaps", "arguably", "it could be argued"). Read the absence as
binding.

## Pairing With S1 (Anti-Pattern Catalog)

S1 enumerates what to remove (epanorthosis, marketing meta-language,
emoji, staccato emphasis, badge-phrase cosmopolitan, lilt-as-substance,
em-dash overuse, foreign phrases as flattery). S2 enumerates what to
deploy (positive register markers, calibrated lilt, domain verbiage,
direct assertion, contraction defaults, multi-disciplinary fluency).

Together they specify a calibration spectrum: each site (bio, libretto,
commit body, tranche doc, LSP comment) has a register coordinate, and
the rules state how density of each positive marker shifts along the
spectrum. The orchestrator that integrates S1 + S2 obtains a
context-aware style policy: not "always do X" or "never do Y" but
"deploy at density-D when context-C, scrub when context-C′."

Final maxim: write directly, deploy precisely, lift only when synthesis
asks for the lift.
