# S8 - Calibration Corpus

A boundary set. Future readers consult these examples to learn the line between the user's voice and the AI register STYLE.md prohibits. The negative corpus comes from the project's own docs; the positive corpus pairs the user's verbatim style examples against the rare clean prose already on master.

Each negative entry names the violation by sign (banned word, em-dash overuse, epanorthosis, hype, editorialising) and supplies a one-line rewrite. Each positive entry names the pattern (mild lilt, archaic verb conjugation, biblical parataxis, evidence-bearing assertion) and indicates the register where it belongs.

---

## Negative corpus (project docs)

### N1 - editorialising "stands as / serves as"
- source: docs/RISK-PERF-MATRIX.md:6-7
- quote: "This is a planning artefact, not a forecast."
- sign: defensive epanorthosis "X, not Y" used as throat-clearing
- replace: "This is a planning artefact." (the negation adds nothing)

### N2 - hype-verb "validates"
- source: docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/SYNTHESIS.md:4-5
- quote: "Six read-only audit lanes inspected the workspace at master HEAD `d5179b8a` to validate or refute the prior 2026-04-30 SIX-AGENT-SYNTHESIS"
- sign: "validate or refute" is two-axis hedge; "to inspect against" carries the same fact with one verb
- replace: "Six read-only audit lanes inspected the workspace at master HEAD `d5179b8a` against the prior 2026-04-30 SIX-AGENT-SYNTHESIS."

### N3 - banned-word "robust" / "robustness"
- source: docs/GESTALT.md:88-89
- quote: "### Headline numbers at gestalt time"
  followed by "The numbers are measured, not estimated."
- sign: epanorthosis ("X, not Y") again; "measured" alone says it
- replace: "Each number cites the artefact it was measured from." (states what makes the claim hold)

### N4 - travel-prose "rich" / "modern"
- source: README.md:5-7
- quote: "BBNF extends EBNF for defining context-free grammars, used by the [`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library."
- sign: clean here; flagged only because "modern recursive-descent combinator layer" appears at GESTALT.md:828, which is the actual offence
- replace: at GESTALT.md:828, "A recursive-descent combinator layer with bespoke HIR…" (drop "modern", state the year if temporal context matters)

### N5 - em-dash overuse, multiple per sentence
- source: docs/RISK-PERF-MATRIX.md:62-65
- quote: "Both numbers are point estimates; actual confidence intervals are wide and asymmetric. A P(declared) = 0.55 reading means *if the project were to attempt this wave ten times under independent starting conditions, the declared gate would be met between four and six attempts*. The declared-vs-floor gap measures plan ambition — a wave with P(declared) = 0.50 and P(floor) = 0.95 has an aggressive headline target over a safe backstop"
- sign: em-dash carries a parenthetical that wants to be its own sentence
- replace: "The declared-vs-floor gap measures plan ambition. A wave with P(declared) = 0.50 and P(floor) = 0.95 has an aggressive headline target over a safe backstop."

### N6 - banned-word cluster: "tapestry / interplay / intricate"
- source: docs/GESTALT.md:200-215 (paragraph on cross-repo crates)
- quote: "the bespoke regex crate benefits from the same egraph-based rewriting that bbnf-lang uses at grammar level"
- sign: clean of the banned cluster, but "benefits from" is the soft AI verb; the sentence reads as marketing
- replace: "`bbnf-regex` runs the same egraph-based rewriting bbnf-lang runs over `IrNode`."

### N7 - mock-historical weight, "marks the inflection"
- source: docs/GESTALT.md:312-315
- quote: "The AQ.5 commit (`2f7c1bd4`) is the architectural inflection: `no-orthogonal-codepaths` is enforced in code."
- sign: "the architectural inflection" inflates a routine repository event
- replace: "AQ.5 (`2f7c1bd4`) is where `no-orthogonal-codepaths` first holds in code."

### N8 - editorialising "the discipline forbids it"
- source: docs/GESTALT.md:560-562
- quote: "`feedback_no-orthogonal-codepaths` binds: AZ-I exists because the tape was a stepping stone, and the discipline forbids it persisting alongside direct-to-struct on the grammars that have active struct targets."
- sign: "the discipline forbids" personifies a precept that already says itself; second clause restates the first
- replace: "AZ-I closes the tape codepath on the three grammars that gain active struct targets, per `feedback_no-orthogonal-codepaths`."

### N9 - tricolon (rule-of-three drift)
- source: docs/GESTALT.md:484-486
- quote: "Bounded prelude annex. Four waves."
- sign: clean; the actual tricolon offence is at the runway list "**B1** (dev-loop truth, 4 waves, one week)"; tricolon-of-three for every tranche
- replace: drop the parenthetical structure across the runway; cite each tranche as one prose clause with its hard gate

### N10 - banned-word "valuable"
- source: docs/RISK-PERF-MATRIX.md:303-304
- quote: "BB has a small declared-gate probability because discovery, ranking, and emission proof are stacked together; its floor remains valuable."
- sign: "valuable" is a hedge; replace with what it produces
- replace: "BB has a small declared-gate probability because discovery, ranking, and emission proof are stacked together; the floor still ships Class-1 auto-accept and Tranche H rediscovery."

### N11 - banned-word "leverage"
- source: docs/GESTALT.md:838-845
- quote: "yyjson observes that SIMD is not where the next 10% lives past a certain point; key dispatch and in-place payload placement are. bbnf already ships AP.4 key dispatch (the Tranche AP structural-dispatch substrate that survived AQ.5's rescope as a `PayloadKind → TypeDesc` projection) and AP.5 NibbleLut."
- sign: clean of "leverage" itself; flagged for the parenthetical-within-parenthetical that would be one straight sentence
- replace: "bbnf already ships AP.4 key dispatch and AP.5 NibbleLut. AP's structural-dispatch substrate survived AQ.5's rescope as a `PayloadKind → TypeDesc` projection."

### N12 - "in conclusion" register
- source: docs/RISK-PERF-MATRIX.md:520-521
- quote: "# Reading the matrix as a planning instrument\n\nThis document is a calibration tool. It answers two questions the tranche plan docs alone cannot:"
- sign: section heading "Reading the matrix as a planning instrument" plus "This document is a calibration tool" - the meta-paragraph that AI defaults to
- replace: drop the section; the matrix's calibration role is stated at top

### N13 - "intricate" / "intricacies" cluster
- source: docs/tranches/AZ-II/FINAL.md:53
- quote: "AZ-II's cutover wave decomposed into 14 sequential sub-stages (cutover.A through cutover.N) over multiple sessions."
- sign: clean of the banned word; flagged for "decomposed into 14 sequential sub-stages over multiple sessions" - "decomposed" is fine, "over multiple sessions" is a softener that says nothing
- replace: "AZ-II's cutover wave ran 14 sequential sub-stages, cutover.A through cutover.N."

### N14 - "framework" / "facilitates" register
- source: docs/RISK-PERF-MATRIX.md:524-528
- quote: "*How confident am I that the declared gates land?* The answer is \"moderately on any individual wave, small as a full-runway compound product, and materially stronger at the defensible-floor level\"."
- sign: rule-of-three "moderately / small / materially stronger" plus self-quoting
- replace: "Per-wave confidence is moderate; the full-runway compound is small; the defensible-floor compound is materially higher (see Cascade)."

### N15 - hype-verb "underscores"
- source: docs/GESTALT.md:130-134
- quote: "The phrase \"grammar-derived\" does real work. It is not decoration."
- sign: "does real work / not decoration" is the epanorthosis sign in disguise; the second sentence repeats the first
- replace: "The phrase \"grammar-derived\" carries semantic weight: the grammar mediates every projection downstream."

### N16 - inflated stakes "the cycle is the core of the architecture"
- source: docs/GESTALT.md:189-192
- quote: "Grammar-authoritative requires typed materialisation, because without `->` reaching the tape the grammar's authority ends at the parse boundary and the runtime re-asserts its own types. The cycle is the core of the architecture."
- sign: "the core of the architecture" is editorial framing; the four invariants already said it
- replace: "Grammar-authoritative requires typed materialisation, because without `->` reaching the tape the runtime re-asserts its own types. The four invariants close the loop."

### N17 - banned-word "delve"
- source: not found in the audited sample (the user's discipline holds on this word)
- replace: n/a; included as an explicit absence to confirm the lint set is observed

### N18 - "showcases / exemplifies"
- source: docs/GESTALT.md:444-446
- quote: "the AU bug-closure work that AV.V0 landed — AU Bug 1 typed materialisation of alt-lit payloads, AU Bug 2 `-> Span` threading, AU Bug 2b `-> i64` / `-> f64` scanner threading — is permanent."
- sign: clean; flagged for the em-dash sandwich that would be a separate sentence
- replace: "AV.V0 landed three AU bug closures: AU Bug 1 typed materialisation of alt-lit payloads, AU Bug 2 `-> Span` threading, AU Bug 2b `-> i64` / `-> f64` scanner threading. All three remain at HEAD."

### N19 - "navigate the landscape"
- source: docs/GESTALT.md:233-237
- quote: "Six eras, each with its own architectural thesis, each inheriting the substrate the prior era produced."
- sign: clean; "architectural thesis" verges on grandiose, retained because each era is in fact thesis-shaped and the term is load-bearing
- replace: keep as-is (this is the boundary case where "thesis" earns its place because the eras' theses are explicit and citable)

### N20 - "robust foundation"
- source: docs/RISK-PERF-MATRIX.md:74-80
- quote: "The 2026-04-24 four-agent preflight was executed and folded into the owning tranche docs. It found missing `cargo-nextest`, absent `rust-toolchain.toml`, pre-divan `bencher` state, absent `StructRegistry`, absent BB rewrite storage, and a generic `cargo expand -p bbnf --test projection_totality` probe that was killed after roughly two minutes because it pulled the full heavyweight graph."
- sign: clean of "robust"; flagged for the run-on listing; six absent items would read better as a bullet list
- replace: convert the list to a bullet-list under "Preflight findings:" and let each item carry its own line

### N21 - "in today's ever-evolving"
- source: not found; included as an absence-marker
- replace: n/a; the codebase's docs do not commit this

### N22 - "vibrant / thriving"
- source: not found in the sample; included as an absence-marker
- replace: n/a

### N23 - banned-word "cultivate"
- source: not found in the sample; included as an absence-marker
- replace: n/a

### N24 - banned-word "comprehensive"
- source: docs/GESTALT.md:7
- quote: "the implemented AZ-II progress snapshot at `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`, and the live hardening audit under `docs/tranches/AZ-II/audit/`"
- sign: clean of "comprehensive"; the file path "comprehensive PROGRESS-SNAPSHOT" appears at git log e.g. "comprehensive PROGRESS-SNAPSHOT-2026-04-29 — 14 substage trajectory"
- replace: in commit subjects, drop "comprehensive"; the trajectory count names the scope concretely

### N25 - banned-word "essential / crucial / pivotal"
- source: docs/RISK-PERF-MATRIX.md:482-485
- quote: "Four levers dominate the cascade probability"
  followed by "1. **Classifier unification intractability**…"
- sign: clean; "dominate" is concrete (probability-mass language); flagged adjacent because "the highest-impact lever" appears at line 502 and reads as inflated
- replace: "AZ-II cutover.O terminal hardening is the largest single probability mover; a miss here drops the cascade by ~25 % per re-plan."

### N26 - "passion / dedication / commitment"
- source: not found; absence-marker
- replace: n/a

### N27 - sycophantic "great question"
- source: not found in commits or docs; absence-marker for AI conversational debris
- replace: n/a

### N28 - "boasts a"
- source: not found in the sampled prose; absence-marker
- replace: n/a

### N29 - "renowned for"
- source: not found in the sampled prose; absence-marker
- replace: n/a

### N30 - filler "it is worth noting that"
- source: docs/GESTALT.md:243-244
- quote: "Era V's 114-per-day commit rate is a symptom visible in retrospect; at the time, the rate read as progress against a plan."
- sign: clean of "it is worth noting"; flagged for the soft-meta "in retrospect / at the time" pairing where one half-sentence carries the contrast
- replace: "Era V's 114 commits/day reads as plan-against-progress at the time; in retrospect, it is the symptom signature."

### N31 - "intricacies of the system"
- source: docs/GESTALT.md:39-43
- quote: "bbnf-lang is a grammar-derived compiler fleet. A BBNF grammar, typed by `->` annotations on rules, is lowered through an IR-pass substrate (`crates/ir`) into backend emitters that should project directly into grammar-derived structs and typed value APIs."
- sign: clean; this is the ideal opening - factual, citable, no AI register; included as a positive within the negative section to show the contrast
- replace: keep verbatim; this is a model opening

### N32 - banned-word "garnered"
- source: not found; absence-marker
- replace: n/a

### N33 - run-on epanorthosis chain
- source: docs/GESTALT.md:64-67
- quote: "JSON twitter sits at 688 MB/s — 35% of AU-baseline 1967 MB/s."
- sign: this is clean; the em-dash here is *load-bearing* because it states the relation 688/1967; included to mark the boundary between hype-em-dash and citation-em-dash
- replace: keep verbatim

---

## Positive corpus

### P1 - mild lilt with biblical cadence (full-poetic register)
- source: user style guideline §Wolf of Gubbio (verbatim from the orchestrator's invoking prompt)
- quote: "Brother wolf, thou hast sinned grievously for thou art wicked; and thou wert hungry. But lo, yea, we all have fallen."
- pattern: archaic verb conjugation (`thou hast`, `thou art`, `thou wert`); biblical parataxis (`but lo, yea`); deliberate lilt
- when to use: full-poetic register only - libretto, personal essay, dedications. Almost never in this codebase. Cited so the boundary is named: this is what *poetic* looks like; tranche docs are not this.

### P2 - quoted scripture as closing weight
- source: user style guideline §Philippians 2:12 quotation
- quote: "Therefore, my beloved, even as you have always obeyed, not as in my presence only, but now much more in my absence, work out your own salvation with fear and trembling."
- pattern: domain-specific verbiage (scripture); the cadence is canonical, not invented; user borrows it whole rather than imitating
- when to use: closing peroration; rare. Useful as evidence the user *can* sustain long sentences when the source warrants - the prohibition is on AI-pastiche cadence, not on long sentences per se.

### P3 - direct biographical assertion, contractions present
- source: user style guideline §bio paragraph
- quote: "Mike has perfervid passions: cooking and food, history, music, computer science (algorithms; type theory), reading, math, the violoncello, dance, languages — and is a recovering perfectionist with a propensity for getting frostbite during ballet class."
- pattern: domain-specific verbiage ("perfervid"); list with em-dash carrying coordinate elements (this is the *legitimate* em-dash); self-deprecating closing kicker
- when to use: prose where personality is the point; never in tranche docs. The em-dash carries real coordinate weight, not parenthetical hedging - this is the boundary the tranche docs cross when they em-dash for emphasis instead of for coordinate listing.

### P4 - the small poem
- source: user style guideline §the poem
- quote: "If thou wert here, my breath would falter / and the steady house would tilt — but I, like Saint Sebastian, do not yield."
- pattern: archaic conjugation; legitimate em-dash for clause-pivot; classical reference (Sebastian) without setup; line break carrying the metrical weight
- when to use: never in tranche docs; cited so the tranche-doc-em-dash can be recognised as imitation of this voice without earning it

### P5 - libretto excerpt (high register, unfamiliar diction)
- source: user style guideline §libretto excerpt
- quote: "Beloved chorus, this fairground holds two souls past mending; she of the spasm and he of the perfervid heart; let the lights drown them gently."
- pattern: high register, archaic ordering, unfamiliar diction ("perfervid" again, used precisely)
- when to use: never in tranche docs; cited as the *anti*-tranche-doc voice. Tranche docs achieve density through citation count and verb economy, not through register.

### P6 - Trader Joe's tote bag (mundane register, exact verb)
- source: user style guideline §the tote bag
- quote: (paraphrased: a small object carrying domestic affection without sentiment)
- pattern: mundane register; the noun does the work; no editorialising
- when to use: this is the register tranche-doc *prose* should sit in - say what it is, name the relation, stop. The user's archaic diction is reserved for poetic registers; the technical register is mundane and exact.

### P7 - Myrtha and Giselle (specific noun, no gloss)
- source: user style guideline §Myrtha-Giselle
- quote: (the user names two ballet figures and lets the cultural weight stand without explanation)
- pattern: cosmopolitan reference used without setup; the reader is trusted to recognise or look up
- when to use: technical docs gain density when they cite by exact name (commit hash, file path, fixture name) and trust the reader; same posture as the user's cultural references. Anti-pattern: glossing AU-baseline as "the AU baseline (the reference measurement from tranche AU)" - the bare term is the term.

### P8 - perfervid-passion bio (rare diction, exact use)
- source: user style guideline §perfervid-passion bio
- quote: (already cited above as P3)
- pattern: "perfervid" used precisely (intensely fervent); the rare word earns its place by carrying meaning the common word would lose
- when to use: tranche docs may use *rare technical terms* (epanorthosis, hyperoperation, e-class, equality saturation) where the rare term names a specific construct. The user's "perfervid" is the same move at register-of-emotion. Anti-pattern: rare adjective for register-puffery (`perfervid optimisation`).

### P9 - clean direct opening (project-internal)
- source: docs/GESTALT.md:39-41
- quote: "bbnf-lang is a grammar-derived compiler fleet. A BBNF grammar, typed by `->` annotations on rules, is lowered through an IR-pass substrate (`crates/ir`) into backend emitters that should project directly into grammar-derived structs and typed value APIs."
- pattern: bare copula ("is"); domain term ("grammar-derived compiler fleet") used without gloss; second sentence carries the mechanism, not editorial weight
- when to use: every tranche-doc opening. This is the model.

### P10 - precepts-edicts cadence (already-clean codebase prose)
- source: docs/precepts/instructions/README.md:7-13
- quote: "**KISS. DRY.** Use the simplest complete mechanism. Remove duplication before adding policy."
- pattern: imperative; two-word principle followed by two short imperatives; no editorialising
- when to use: every operational directive. The pattern is *principle, mechanism, prohibition*, three short sentences.

### P11 - precepts code-discipline cadence
- source: docs/precepts/instructions/README.md:52-56
- quote: "**No god modules.** `utils.rs`, `helpers.rs`, `common.rs`, and similar kitchen-sink namespaces are god modules in gestation. Split by concern, name by behaviour, never by namespace position."
- pattern: rule-name, examples, restatement-as-imperative; legitimate tricolon ("split by concern, name by behaviour, never by namespace position") because the three clauses name three distinct rules
- when to use: rule-stating prose. The tricolon earns its place because each clause is independent.

### P12 - commit body, evidence-bearing
- source: commit `7d4eaa53` ("feat(csp/dispatch): dispatch constraints with strategy consumer")
- quote: "Adds `dispatch::install` — pins Alt decision variables to `AltMode::KeyDispatch` whenever the upstream pass populated: 1. `ir.key_dispatch_configs[id]` (key-dispatch detector match), or 2. `ir.keyword_branches[id]` (keyword-statistics miner match)."
- pattern: subject names mechanism plus surface; body opens with verb of effect ("Adds", "Pins"); enumerated cases cited by exact field path
- when to use: every implementation commit. The body answers "why does this exist" by naming the upstream populator and the precise field.

### P13 - commit body, deletion with consumer-scan evidence
- source: commit `d316f40e` ("fix(backend/recognizer-plan): delete zero-consumer recognizer_plan")
- quote: "`crates/core/src/backend/recognizer_plan.rs` (159 LOC) was the X.8f \"unification bridge\" — a `ScannerPlanRecord` projection over IR sidecars that admitted in its own doc-comment: > The record is retained as the X.8f unification bridge for future"
- pattern: deletion proof carries the previous justification verbatim, then refutes it with the consumer-scan; quoted self-confession is evidence
- when to use: every deletion commit where the deleted code claimed a future role. Quote the claim, cite the scan that disproves it.

### P14 - commit body, plan-citation with hard data
- source: commit `954d166b` ("feat(grammar/bbnf-self-host): replace bootstrap_parser with canonical generated path")
- quote: "W2.4 close-path (a) — BBNF self-host parses canonically without `bootstrap_parser.rs` routing. The generated `BbnfBootstrap::parse` (emitted by the codegen alt_dispatch / Pratt / Flat shape pipeline) is the canonical entry point; the hand-written 1505-LOC `bootstrap_parser.rs` is deleted."
- pattern: plan-anchor (W2.4 close-path (a)); concrete LOC count; "is deleted" closes the claim
- when to use: every wave-close commit. The wave name, sub-path letter, and exact deletion count are the evidence.

### P15 - audit-finding cadence
- source: docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/SYNTHESIS.md:96-103
- quote: "### A6 - Substrate violations are exact lines\n\nThree findings have exact source coordinates and are immediately actionable:\n\n- `crates/ir/src/dta/` (~90 LOC) - zero `use bbnf_ir::dta` consumers; types appear only in test deny-strings. Delete in W1."
- pattern: heading-as-claim; one-sentence claim under it; bullet items carry path + LOC + consumer-count + action
- when to use: every audit lane finding. The discipline is path-LOC-consumer-action; nothing softer earns the claim.

---

## Suggested STYLE.md appendix structure

The corpus above is the appendix material. The recommended STYLE.md structure folds it into three sections:

### Appendix A - Negative reference (boundary set)

Twenty entries citing project file:line for each AI-writing sign present in the codebase, with the one-line rewrite for each. Future readers see the actual offence, not an abstract description.

### Appendix B - Positive reference (voice anchors)

Fifteen entries pairing the user's verbatim style guideline examples with the cleanest project-internal prose. Each entry names the register (poetic / mundane / commit-body / audit-finding) so the reader knows which positive applies to which writing context.

### Appendix C - Boundary cases

Three to five entries (drawn from the corpus markers above where the offence is conditional - em-dash for coordinate vs em-dash for hedging; "thesis" for citable theses vs "thesis" for grandiosity; long sentences for scriptural quotation vs long sentences for editorial weight). The boundary cases are where the lint cannot be mechanical; they require register-judgement.

### Cross-references

Each appendix entry should link back to the corresponding STYLE.md rule by section number. The negative corpus is the *test set* for STYLE.md; the positive corpus is the *training set*. A future audit checks new prose against the negative corpus first (regression catches), then against the positive corpus (register match).

### Maintenance posture

The corpus updates at every tranche close. New negative examples enter when an audit catches a new sign; new positive examples enter when the user produces a passage worth anchoring. The corpus does not delete entries - the absence-markers (N17, N21, N22, N23, N26, N27, N28, N29, N32) are deliberate, as evidence that the lint-set is observed, not just listed.
