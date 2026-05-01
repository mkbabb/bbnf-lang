# S1 — Wikipedia signs of AI writing (extraction)

Source: https://en.wikipedia.org/wiki/Wikipedia:Signs_of_AI_writing
Fetched 2026-04-30. Read-only audit. One doc, no other edits.

The article catalogues signals Wikipedia editors use to flag AI-generated prose. This extraction strips that catalogue down to patterns relevant to technical/tranche-style docs and pairs each sign with a counter-example in the user's voice.

---

## 1. Tone

| # | Pattern | Why flagged | Bad example | Counter |
|---|---------|-------------|-------------|---------|
| T1 | "marking a pivotal moment" / "marks a turning point" | Inflates routine facts with mock-historical weight | "The Statistical Institute of Catalonia was officially established in 1989, marking a pivotal moment in the evolution of regional statistics." | "Catalonia's statistical institute was founded in 1989." |
| T2 | "stands as a testament to" / "enduring testament" | Sentimentalises mundane data | "This etymology highlights the enduring legacy of the community's resistance." | State the etymology, drop the gloss. |
| T3 | "vibrant," "rich," "profound," "thriving" | Travel-brochure adjectives | "a vibrant town with a rich cultural heritage" | "a town of about 12,000 with three weekly markets" |
| T4 | "nestled," "in the heart of," "tucked away" | Romantic geography | "Nestled within the breathtaking region of Gonder..." | "In the Gonder region of Ethiopia..." |
| T5 | "showcasing," "exemplifies," "underscores" | Framing verbs that editorialise | "showcasing the state's rich history" | "displays" / "shows" or omit. |
| T6 | "commitment to," "dedication to" | Corporate copy register | "the brand's commitment to craftsmanship" | "the brand uses hand-stitched seams." |
| T7 | "stands as," "serves as," "marks" instead of "is" | Avoids basic copula | "Gallery 825 serves as LAAA's exhibition space." | "Gallery 825 is LAAA's exhibition space." |
| T8 | Hedged-significance constructions ("plays a key role," "is crucial") | Vague hype with no claim attached | "X plays a crucial role in the modern landscape." | Name the role concretely. |
| T9 | Rhapsodic openers ("In an era of...", "In today's...") | Generic time-frame puffery | "In today's ever-evolving landscape..." | Drop entirely. |
| T10 | Travel-guide pluralisation ("a diverse array of," "a tapestry of") | Vague enumeration | "offers a diverse array of experiences" | List the experiences or cut the sentence. |

---

## 2. Vocabulary

The article names these as the canonical AI lexicon, organised by era of model dominance.

### 2.1 GPT-4 cluster (2023–mid-2024)
additionally, boasts, bolstered, crucial, delve, emphasizing, enduring, garner, intricate / intricacies, interplay, key (as adjective), landscape, meticulous / meticulously, pivotal, tapestry, testament, underscore, valuable, vibrant.

### 2.2 GPT-4o cluster (mid-2024–mid-2025)
align with, bolstered, crucial, emphasizing, enhance, enduring, fostering, highlighting, pivotal, showcasing, underscore, vibrant.

### 2.3 GPT-5 cluster (mid-2025+)
emphasizing, enhance, highlighting, showcasing.

### 2.4 Promotional / travel-prose set
boasts a, commitment to, diverse array, enhancing, exemplifies, featuring, groundbreaking, in the heart of, natural beauty, nestled, profound, renowned, rich, showcasing, vibrant.

### 2.5 User-listed additions (carry these into the lint set)
delve, tapestry, ever-evolving, bustling, navigate, unleash, robust, leverage, in conclusion, in the realm of, it is worth noting, moreover, furthermore, nevertheless.

### 2.6 Hedge / filler set
contributing to, cultivating, encompassing, resonate with, valuable insights, reflects, symbolizing.

Counter rule: if the word adds no fact, cut it. If it replaces a simple word ("leverage" → "use"), use the simple word.

---

## 3. Syntax and structure

| # | Pattern | Why flagged | Bad example | Counter |
|---|---------|-------------|-------------|---------|
| S1 | "Not just X, but Y" | Defensive epanorthosis; pretends to argue | "not only a work of self-representation, but a visual document" | "a visual document" |
| S2 | "Not X — it's Y" / "Isn't X, it's Y" | Same move, dressed up | "isn't sourcing — it's framing" | "it frames the source" |
| S3 | Tricolons / rule of three ("passion, dedication, and excellence") | Forced parallel triads | "three key layers of meaning" | Name the layers if real, else drop. |
| S4 | Elegant variation (synonym chain for one referent) | Avoids repeat penalty; reads thesaural | "Soviet artistic constraints... non-conformist artists... their creativity..." for one group | Reuse the term. |
| S5 | Trailing present-participle clauses | Smuggles unattributed analysis | "..., contributing to the socio-economic development of the region." | Cut the participle clause. |
| S6 | Fragments for emphasis ("Period." / "Like. This.") | Fake cadence | "We tested every path. Period." | "We tested every path." |
| S7 | Anaphora ("Not a career, not a body of work, not sustained relevance") | Stylistic overcadence | repeat-noun chain | one direct sentence. |
| S8 | "Despite its [positive], X faces challenges. Future [thing] could enhance..." | Outline-template closer | "Despite its success, X faces challenges. Future investments could enhance..." | Stop at the facts; no future-prospects coda. |
| S9 | Section-summary sentence ("This section discusses X, which relates to Y") | Meta-prose padding | "In this section we examine the parser." | Delete; let the content carry. |
| S10 | "Hyperspecific source then generic claim" | Synthesis past what the source says | "One critic said Y; this demonstrates that society thinks Z." | Stop at what the source actually said. |

---

## 4. Punctuation

| # | Pattern | Why flagged | Bad example | Counter |
|---|---------|-------------|-------------|---------|
| P1 | Em-dash overuse | Excessive parenthetical interruption | "a bridge across divides — a line that often represents separation — and a marker of identity" | Use one period or one semicolon. |
| P2 | Spaced em-dashes ( — ) | Non-standard typography copy-pasted from word processors | "the parser — which we wrote in Rust — is fast" | Unspaced em-dash, used sparingly. User voice: prefer unspaced when used. |
| P3 | Curly / smart quotes in source | Word-processor artifact | "text" instead of "text" | Straight ASCII quotes. |
| P4 | Sentence chopping ("Like. This.") | Fragments masquerading as cadence | "Fast. Reliable. Tested." | Write the actual sentence. |
| P5 | Comma splice hidden by dash | Grammar error wearing a costume | "The build passes — the tests pass." | Two sentences or a semicolon. |
| P6 | Overpunctuation generally (multi-dash, ellipsis-as-suspense) | Visual noise | "It works... mostly... unless..." | Pick one mark; finish the thought. |

---

## 5. Formatting

| # | Pattern | Why flagged | Bad example | Counter |
|---|---------|-------------|-------------|---------|
| F1 | Title Case in headers | Capitalises every main word | "## Impact Of Technology And Digitalization" | "## Impact of technology and digitalisation" (sentence case) |
| F2 | Excessive bold | Mechanical emphasis on every recurring noun | every "private equity firms" bolded | Bold once on first definitional use, or never. |
| F3 | Bulleted vertical list with inline-bold headers | Slide-deck format | "- **SEO**: traditional methods..." | Prose, or a plain list. |
| F4 | Markdown syntax inside wiki source | Mixed mark-up | `**bold**` in wikitext | Native syntax of the host format only. |
| F5 | Emoji-as-bullet | Non-encyclopedic | "📌 Key point" | Standard `-` or `*`. |
| F6 | Skipped heading levels | `##` jumping to `####` | "## Section\n#### Subdetail" | Sequential `##` then `###`. |
| F7 | "Key Takeaways" / "TL;DR" boxes everywhere | Outline scaffolding | "Key Takeaways: 1. ... 2. ... 3. ..." | Integrate into prose; one-line summary at top is fine. |
| F8 | Horizontal rule (`----`) before every header | Decorative breaks | `----\n## Section` | Heading alone. |
| F9 | AI tool markup leak (`<oaicite>`, `turn0search0`, `contentReference`, `grok_card`) | Tool artifacts | "...as shown in `turn0search0`." | Strip before commit; never appears in human prose. |

---

## 6. Citations and claims

| # | Pattern | Why flagged | Bad example | Counter |
|---|---------|-------------|-------------|---------|
| C1 | Unsubstantiated superlative | "best," "leading," "most innovative" with no source | "the leading parser combinator framework" | Drop the superlative or cite a benchmark. |
| C2 | Vague attribution ("observers," "experts," "industry reports") | Weasel sourcing | "Industry reports have noted..." | Name the report and link. |
| C3 | Comparison sentiment without numbers | Asserts ranking without measurement | "X performs significantly better than Y." | "X is 3.2x faster on the JSON bench (cold, n=50)." |
| C4 | Over-attribution of trivial coverage | Lists every mention to manufacture notability | "profiled in Vogue, Wired, and Toronto Star" (no citations) | Cite once; let the citation stand. |
| C5 | Generic "broader implications" gloss | Statistical regression to mean | "reflects a broader movement toward..." | Cut, or anchor to a specific source. |
| C6 | Fabricated DOI / ISBN / URL | Hallucinated identifiers | DOI that resolves to an unrelated paper | Verify every identifier before merging. |
| C7 | utm_source= / tracking parameters in URLs | Copy-paste from search-engine results | `?utm_source=chatgpt.com` | Strip parameters. |
| C8 | Page-less book citation | Quote without page reference | "Knuth (1984)" with a direct quote | Add page numbers or remove the quote. |

---

## 7. Repetition and formulaic structure

| # | Pattern | Why flagged | Bad example | Counter |
|---|---------|-------------|-------------|---------|
| R1 | "In conclusion," / "In summary," / "To summarize," | Five-paragraph-essay closer | "In conclusion, the parser is fast." | End on the last fact; no closer. |
| R2 | "In the realm of" / "In the world of" / "In today's" | Vague abstraction opener | "In the realm of compiler design..." | "In compiler design..." or just start. |
| R3 | "It is worth noting that" / "It should be noted that" | Hedge that adds zero | "It is worth noting that the cache is cold." | "The cache is cold." |
| R4 | "Moreover," / "Furthermore," / "Nevertheless," / "Additionally," sentence-openers | Mechanical connectors | "Furthermore, the tests pass. Moreover, the build is green." | Drop; use period and a new sentence. |
| R5 | Parallel "Challenges and Future Prospects" closing block | Formulaic outline ending | "Despite progress, challenges remain. Future work could enhance..." | Stop at present-tense facts. |
| R6 | Section meta-summaries inside the section | "This section covered..." | "This section discussed the parser, which relates to the lexer." | Delete; the content already showed it. |
| R7 | "Key Takeaways" recap at end of every section | Slide-deck habit | bullet recap of three points just made | Don't recap inside the same doc. |
| R8 | Repeated tricolon openers ("Fast, simple, reliable.") | Rule-of-three habit at every header | three-adjective list per heading | Pick one adjective when one is enough. |

---

## 8. Content / synthesis (Wikipedia-specific, partly relevant)

These are Wikipedia-specific but the engineering-doc analogues are obvious.

| # | Pattern | Engineering-doc analogue |
|---|---------|--------------------------|
| X1 | Generic ecosystem padding ("preservation efforts crucial...") | "scalability is crucial" with no scale numbers |
| X2 | Conjecture about future without source | Speculative roadmap items framed as commitments |
| X3 | "Active social media presence" idiom | "robust ecosystem" / "vibrant community" with no metric |
| X4 | Title Case in lead | Title Case in section headers |
| X5 | Pre-placed `{{cite needed}}` templates | TODO markers strewn through prose without owner |

---

## 9. Talk-page / commentary tells (low relevance to this codebase, kept for completeness)

- "I have worked to ensure quality, good faith, and adherence to policies."
- "In the absence of concrete evidence, I propose..."
- Multi-sentence edit summaries describing every change.
- Submission preambles explaining why a draft should be accepted.

These rarely surface in tranche docs; flag them in PR descriptions if they do.

---

## Counts

Signs extracted: 51 (T1-T10, V five clusters listed wholesale, S1-S10, P1-P6, F1-F9, C1-C8, R1-R8, plus X1-X5 analogue list). Distinct named overused words: 60+ across clusters 2.1-2.6.

---

## Top-5 most-violated patterns in technical / tranche-style docs

Ranked by the orchestrator's prior judgement on lane-6 throughput artifacts and adjacent commit-body samples in this audit. These are the patterns most likely to need automated lint coverage in S3's STYLE.md:

1. **R4 — sentence-opening "Moreover," / "Furthermore," / "Additionally," / "Nevertheless,"**. Almost every AI-drafted tranche summary opens at least one paragraph this way. Cheap to lint, high signal.
2. **T7 — copula avoidance ("stands as," "serves as," "marks")**. Pervasive in plan docs that try to sound consequential. Replace with "is."
3. **S1 / S2 — epanorthosis ("not just X, but Y," "isn't X, it's Y")**. The most distinctive AI rhythm; appears in commit bodies, PR descriptions, and tranche openers.
4. **R1 / R5 — outline closers ("In conclusion," / "Despite challenges, future work...")**. Tranche docs habitually end with a summary block that adds nothing the body did not say.
5. **F1 — Title Case in headers**. Mechanical and visible; sentence case is the user's stated voice. Easiest single lint to ship and the most-violated formatting rule across tranche docs.

Honourable mentions: P2 (spaced em-dashes), R3 ("it is worth noting that"), V "leverage" / "robust" / "delve" used straight.

---

## Confirmation

No other files touched. One doc written at the prescribed path. Source docs, precepts, and adjacent audit docs untouched.

---

## How this informs S3

S3's refined STYLE.md draft can lift this catalogue wholesale into a lint table — sections 1-7 map one-to-one onto rule categories (tone / vocab / syntax / punctuation / formatting / citation / repetition), section 2 supplies the literal banned-word list, and the top-5 give S3 the priority order for which rules ship as automated checks first.
