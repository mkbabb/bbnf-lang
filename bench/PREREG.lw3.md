# Contest pre-registration, LW3 addendum

This file extends `bench/PREREG.md`. That file is sealed: `DECISION-PLAN.lw2.json` was sealed over its sha256 (`55acca83…`), so any LW3 addition goes here, under its own digest (`PREREG.lw3.md.sha256`). The sections continue PREREG.md's numbering. Each section is written before any LW3 plan decision is tuned on the spine and is hashed with the rest of this file; a later change to this file is a further addendum, never an edit.

## 12. Held-out grammars (LW3.COMP-5b), registered by S5 ORACLE

Ruling H-1 (delegate rulings 2026-11-11e) names three held-out languages: TOML 1.0.0, GraphQL October 2021 and the R7RS datum syntax. Each is written in BBNF under `grammar/heldout/`, from the language's own specification text, by S5 ORACLE. Each is read by at least one oracle that is independent of bbnf-lang, over a corpus that enters through the R-c custody (`corpus/MANIFEST.sha256`, release asset `corpus-v2.tar.gz`).

**Held out means held out.** No pass, rewrite, plan decision, kernel choice, cost model or rule list is tuned, chosen or debugged against these grammars or their corpora. They enter G1 only as readings. A pass that fails on one of them is cured on its own terms, and the failing held-out input is recorded with the cure; it is never added to a training corpus. `grammar/misc/csv.bbnf` is one of the 9 training grammars and is not held out (LW3-ORDER COMP-5b). No file outside `crates/oracle/`, `bench/`, `docs/` and `restart/` names `grammar/heldout`.

### 12.1 The grammars

| Name | Grammar file | sha256 | Entry rule | Language and edition | Written from |
| --- | --- | --- | --- | --- | --- |
| H-TOML | `grammar/heldout/toml.bbnf` | `6d3847b9c47c69a047555e9106941a45f9635ab87e3f8b122c2fb217ec52dd3e` | `toml` | TOML 1.0.0 | toml-lang/toml tag 1.0.0, `toml.abnf` and `toml.md` (the prose governs where the ABNF is looser) |
| H-GRAPHQL | `grammar/heldout/graphql.bbnf` | `621ac0f8fb071802927343f9d174b9c32a0aa9fee25457b5fedaca280217adb0` | `document` | GraphQL, October 2021 edition | the October 2021 specification, Appendix B (grammar summary) |
| H-R7RS | `grammar/heldout/r7rs-datum.bbnf` | `4c227d28f0d46fa4c4faac925d51f34d561efc776c854aca6dc65af4db070ef1` | `data` | R7RS-small datum syntax | R7RS-small §7.1.1 (lexical structure) and §7.1.2 (external representations) |

### 12.2 The oracles

| Grammar | Oracle | Pin | Content address | What it judges |
| --- | --- | --- | --- | --- |
| H-TOML | toml-test | v2.2.0, commit `ce08da1ddb075d1c7596d663c7fcba9a2ae02c5c` | the commit | `valid/` files must be accepted and decode to the tagged JSON beside them; `invalid/` files must be rejected or fail TOML 1.0.0's semantics (crates/oracle TOML decoder) |
| H-GRAPHQL | graphql-js | npm `graphql` 16.11.0 | sha256 `7e34f9274fdf64549b08d23ab7480afbe47add16974de698cd387a36b09db1f6` | `parse` verdict; where both accept, the token stream equals the arm's scalar events |
| H-R7RS | lexpr | crate 0.2.7 | checksum `6a84de6a9df442363b08f5dbf0cd5b92edc70097b89c4ce4bfea4679fe48bc67` | the data read, as values, on the `r7rs/cases/lexpr/` domain |

These pins are rows of `bench/W0-BENCH-PINS.tsv` (`pins.py` ORACLES) and of `crates/oracle/ts/pin.json`. graphql 16.11.0 departs from the newest-release rule because 16.12.0 onward read syntax added after the October 2021 edition (see `pin.json`). Two divergences are ruled in `crates/oracle/ruled.tsv` under H-1 at registration: graphql-js reads the later braced `\u{…}` escape (`graphql/cases/doc-0043.graphql`), and lexpr reads `#t#f` without the delimiter R7RS §7.1.1 requires (`r7rs/cases/lexpr/invalid-14.scm`). The R7RS forms lexpr cannot read (`r7rs/cases/spec/`) have no oracle yet; they are a gap, never a pass (ask H-1b).

### 12.3 The corpora

Each digest is the sha256 of the corpus's `corpus/MANIFEST.sha256` lines (`<sha256>  <path>`, manifest order, each ending in a newline) whose path starts with the prefix. A change to any file of a corpus, or to which files it holds, changes its digest.

| Grammar | Corpus prefix | Files | Digest | Source |
| --- | --- | --- | --- | --- |
| H-TOML | `toml-test/` | 886 | `aeae350c2243501c27c5ef34013e4f5babca04665786f8d721e1e57afe8a5b26` | toml-test v2.2.0: exactly the files `tests/files-toml-1.0.0` lists (205 valid with their `.json`, 474 invalid), the list itself and the licence |
| H-GRAPHQL | `graphql/` | 1726 | `6cc96e31d28a52fa37192c158271f22266e2107c1cbc2e8c11d0c6810c7824d1` | the 583 documents graphql-js v16.11.0 (`c18e9f6aada9ae086ddf836e4d822cf1426f3868`) writes in its tests, two seeded one-character mutations of each (`crates/oracle/heldout/graphql-corpus.mjs`, seed 20261111), and the licence |
| H-R7RS | `r7rs/` | 460 | `4e8227fbedce909f614e5ea93a7b0c0e77df11aac71ebd55d3ab6819283946ed` | `g1 corpus-gen r7rs` (seed 20261111): 400 generated data and 19 invalid texts in lexpr's domain; 30 valid and 11 invalid R7RS texts outside it |

### 12.4 The reading

G1 reads each held-out grammar like the 9 (LW3.COMP-G1): every arm that supports it, against its oracle, under `obs()` and the failure contract, at the MANIFEST digest of record. At registration the TS 0.3.0 package arm reads H-TOML 679 of 679 in agreement with toml-test, H-GRAPHQL 1,724 of 1,725 with graphql-js (the one ruled), and H-R7RS 418 of 419 with lexpr (the one ruled). The root `backend/rust` arm has no generated parser for them, and the spine has none yet; each is read when it has one. A held-out divergence on the spine is a defect row with an owner, and the grammar stays out of every tuning loop while it is open.
