# Axis 5 - GESTALT Coherence and Accuracy

Audit target: `docs/GESTALT.md` at `HEAD 5a260f94` on 2026-04-23.

Document provenance matters here. `GESTALT.md` was introduced at
`4d3ebceb` on 2026-04-22 and then materially expanded/edited through
`4d18b89c`, `f321ce99`, and `46d69a5b` on 2026-04-23. It is therefore a
mixed-time document: some numbers and tranche names are frozen at the
2026-04-22 archaeology snapshot, while other sections were updated after
the AZ split. The main audit question is not whether the document was
once useful; it is whether it is internally coherent and accurate as the
claimed canonical master narrative today.

## Verified

1. The Era V retrospective is substantially honest about the failure
   mode. `docs/GESTALT.md:341-379` names the three interacting decision
   surfaces, the substrate-first-consumer-later pattern, the 0/17 gate
   failure, the ledger-vs-runtime evidence substitution, and the need for
   AX invariant 13. That account matches `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:1-15`
   and `docs/tranches/AX/AX.md:1-18`.

2. The claim that AX has twenty-one invariants is real. `docs/GESTALT.md:168-171`
   points to `4177a18c`; `docs/tranches/AX/AX.md:22-44` does in fact
   enumerate 21 invariants. The existence claim checks out even though
   some downstream numbering references do not.

3. Section 5's primary runway order is largely aligned with the live
   tranche stack: `B1 -> AY-II -> AZ-I -> AZ-II -> BA`, with BB opening
   after `AZ-I + AY-II`, not after BA. This matches
   `docs/GESTALT.md:445-706`,
   `docs/tranches/BA/BA.md:57-80`,
   `docs/tranches/BB/BB.md:260-265`, and
   `docs/RISK-PERF-MATRIX.md:206-245`.

4. The literal `crates/ir-rewrites/` path has been redressed away. A
   repo grep returns zero hits for `crates/ir-rewrites` or
   `ir-rewrites crate`. `GESTALT.md` itself consistently uses the module
   path `crates/ir/src/rewrites/`.

5. The "eleven worktree branches consumed" statement is defensible as a
   historical count. The appendix table at `docs/GESTALT.md:1252-1272`
   references 11 unique `worktree-agent-*` branches once the wildcard
   line is excluded.

6. Spot-checked numerical claims that do verify against cited docs or the
   live tree:

   | Claim in `GESTALT.md` | Status | Evidence |
   |---|---|---|
   | JSON twitter AU baseline = 1967 MB/s (`:77`, `:957`) | Verified | `docs/tranches/AU/FINAL.md:449` |
   | JSON canada AU baseline = 1231 MB/s (`:955`) | Verified | `docs/tranches/AU/FINAL.md:445` |
   | JSON citm AU baseline = 2438 MB/s (`:956`) | Verified | `docs/tranches/AU/FINAL.md:446` |
   | CSS normalize AU baseline = 735 MB/s (`:958`) | Verified | `docs/tranches/AU/FINAL.md:451` |
   | CSS bootstrap AU baseline = 454 MB/s (`:959`) | Verified | `docs/tranches/AU/FINAL.md:450` |
   | CSS tailwind AU baseline = 496 MB/s (`:960`) | Verified | `docs/tranches/AU/FINAL.md:452` |
   | Sheets parse_simple AU baseline = 95 MB/s (`:961`) | Verified | `docs/tranches/AU/FINAL.md:453` |
   | AY-I twitter post-fix = 688 MB/s (`:37`, `:78`, `:963`) | Verified | `docs/tranches/meta-audit/06-commit-archaeology.md:156,468` |
   | 688 MB/s is ~35% of 1967 MB/s (`:37`, `:963`) | Verified | `688 / 1967 = 0.3497` |
   | 61 post-migration divan sites (`:85`, `:883`) | Verified | `docs/tranches/next-tranche-research/repo-modernization/INDEX.md:301-309` |
   | `scripts/` entries = 19 (`:83`) | Verified | live tree: `find scripts -maxdepth 1 -type f | wc -l` = 19 |
   | Abrogation catalog total = 63 (`:84`) | Verified | `docs/tranches/meta-audit/08-abrogation-catalog.md:32` |
   | AX.W1r.0 revert = -6,128 LOC (`:75`, `:368`, `:1301`) | Verified | `git show --shortstat 3429aaba` and `06-commit-archaeology.md:597` |
   | 93-ICE cluster / `on_disk_cache.rs:663:9` (`:79`, `:452-460`) | Verified | `docs/tranches/B1/TOOLCHAIN-SOTA.md:45` |
   | B1 pin = `nightly-2026-04-11` (`:81`, `:450`) | Verified | `docs/tranches/B1/B1.md:98-100` |

## Refined

1. The era-rate arithmetic is internally fine, but only as shorthand over
   the archaeology's approximate tranche-tagged counts. `docs/GESTALT.md:210-214`
   says Era II ~14/day, Era III ~11/day, Era IV ~31/day, Era V ~114/day,
   Era VI ~43/day. Those ratios are consistent with the cited counts in
   `docs/tranches/meta-audit/06-commit-archaeology.md:46-50`
   (`264/18`, `280/25`, `185/6`, `572/5`, `130/3`). The arithmetic is
   not the issue; the presentation is. Era VI has already moved past the
   archaeology snapshot, so these are no longer live rates and should be
   labeled as archaeology-snapshot figures, not current project tempo.

2. The "~78,000 LOC reclaimed" statement is plausible only at AX.W0b
   cluster scope, not as a property of commit `a206b962` alone.
   `docs/GESTALT.md:31`, `:370`, and `:1299` attribute the reclaim to
   AX.W0b, which matches `docs/tranches/AX/AX.md:12` and
   `docs/tranches/meta-audit/06-commit-archaeology.md:153`. But the
   single named commit `a206b962` itself is `5,302` deletions by shortstat.
   The doc should say "AX.W0b cluster" consistently whenever it names the
   ~78K figure.

3. The `ir-rewrites` redress is complete in path form but not yet fully
   complete in semantics across the owning tranche docs. `GESTALT.md`
   correctly says "module within the existing IR crate" at
   `docs/GESTALT.md:706-719` and `:1179-1189`. But
   `docs/tranches/BB/BB.md:375` still says
   `` `crates/ir/src/rewrites/` | create (new crate) `` and
   `docs/tranches/BB/BB.md:392` says
   "`crates/ir/src/rewrites/` crate landed". The master narrative is more
   correct than its owning implementation doc, which means the redress is
   directionally right but not repo-complete.

4. The appendix is still useful as archaeology, but it needs explicit
   "historical branch artefact" labeling. Rows such as
   `docs/GESTALT.md:1271-1272` refer to BA/BB/BC refinement outputs that
   were real at synthesis time. As a historical appendix that is fine. As
   a present-tense "dive directly into any document from here" index, it
   is no longer safe because some referenced paths no longer exist.

## Flawed

1. The document is internally inconsistent on the most important
   dependency question: when BB opens.

   - Section 1 and Section 5 say BB may open in parallel with AZ-II:
     `docs/GESTALT.md:52-59`, `:676-685`.
   - The live BB and risk docs agree with that:
     `docs/tranches/BB/BB.md:262-265`,
     `docs/RISK-PERF-MATRIX.md:206-245`.
   - The closing thesis contradicts them and says BA must close before BB
     opens, then reintroduces BC: `docs/GESTALT.md:1315-1319`.

   This is not cosmetic drift. It means the canonical narrative gives two
   incompatible dependency graphs.

2. The post-split tranche rename did not land cleanly inside `GESTALT`.
   There are live stale references to old tranche ownership:

   - `docs/GESTALT.md:776` assigns `path!` to BB, but BA owns `path!`
     (`docs/tranches/BA/BA.md:150-153`).
   - `docs/GESTALT.md:783` says "BA.W3 gates parity node-for-node", but
     lightningcss parity is AZ-I.W3 (`docs/tranches/AZ-I/AZ-I.md:157`).
   - `docs/GESTALT.md:790`, `:799`, `:1080-1081`, `:1317` still use BC as
     a live future owner even though BB absorbed that scope and BC was
     retired.

3. The AX invariant references are materially wrong, not just loosely
   phrased.

   - `docs/GESTALT.md:425-431` says invariant 7 = bench checkpoint and
     invariant 10 = wire-contract compile gate.
   - In `docs/tranches/AX/AX.md:24-44`, invariant 7 is wire-contract
     tests and invariant 10 is the mid-wave bench checkpoint.
   - `docs/GESTALT.md:428` says invariant 17 is the frozen-contract rule
     for gate predicates, but in AX invariant 17 is
     "architectural transposition complete; throughput in next wave" is
     not a closeable wave. The frozen gate-predicate rule is in AX's
     operational posture (`docs/tranches/AX/AX.md:48-52`), not invariant
     17.
   - `docs/GESTALT.md:431` says AZ-I and AZ-II cite invariants 7 and 13
     directly. They cite AX invariant 13; they do not cite AX invariant 7
     (`docs/tranches/AZ-I/AZ-I.md:84,298`,
     `docs/tranches/AZ-II/AZ-II.md:87`).

4. Multiple citations are now broken.

   - `docs/GESTALT.md:1157`, `:1169`, `:1211`, `:1226` cite
     `docs/tranches/AZ/AZ.md`, which does not exist.
   - `docs/GESTALT.md:1222` cites
     `docs/tranches/AZ/CLASSIFIER-UNIFICATION.md`, which does not exist.
   - `docs/GESTALT.md:1271` cites `docs/tranches/BC/BC.md`, which does
     not exist.

   A master overview can contain historical references, but then it must
   mark them as historical and cite the commit or branch object, not a
   dead working-tree path.

5. The headline-number surface is stale against the live repo and
   self-inconsistent even within the document.

   | Claim in `GESTALT.md` | Current evidence | Result |
   |---|---|---|
   | "1,859 master commits" at `:25` | live `git rev-list --count master` = 1888; at `f321ce99` it was 1861 | Flawed |
   | "945 unpushed" at `:26` and `:66` | live `git rev-list origin/master..HEAD --count` = 991 | Flawed |
   | "24 feature branches" at `:26` | live local heads = 55 | Flawed as current statement |
   | "Master commits 1,842" at `:65` | live master count = 1888 | Flawed |
   | "Total commits across all refs 1,923" at `:67` | live `git rev-list --all --count` = 1965 | Flawed |
   | "16 (4 sibling, 12 workspace)" at `:87`, `:850-860` | cited assay says `16 repos (2 sibling, 14 workspace-member)` at `07-appurtenant-assay.md:3-14` | Flawed |

   The table is explicitly presented as measured fact at `docs/GESTALT.md:89-90`;
   it is no longer safe in that role.

6. `docs/GESTALT.md:337` says "By `be4b22b1` four weeks later". The
   cited commit is `2026-04-16`, one day after the AV plan commit on
   `2026-04-15`, not four weeks later. This is a straight numerical
   error.

7. The closing target statement still preserves an old tape-first thesis
   that the split explicitly retired. `docs/GESTALT.md:1336-1341` says
   the end state is "a direct-to-struct tape-first runtime parser" with
   "every `->` reaching the tape". That contradicts the current AZ-II and
   BA docs:

   - AZ-II says the tape crate is deleted and the struct graph is the only
     materialized form: `docs/tranches/AZ-II/AZ-II.md:69-79`.
   - BA says the substrate is the struct tree, not a tape:
     `docs/tranches/BA/BA.md:57-74`.

   This is the deepest conceptual drift in the document. It leaves the
   reader with the wrong end-state architecture.

8. Section 7's fleet-shape paragraph double-counts the ecosystem.
   `docs/GESTALT.md:850-861` says "four sibling repos and twelve
   workspace members," then lists gorgeous and csp-solver both as sibling
   repos and as workspace-internal surfaces. The assay that GESTALT cites
   already resolved this correctly as `2 sibling + 14 workspace-member`
   (`docs/tranches/meta-audit/07-appurtenant-assay.md:3-14`).

## Open

1. I did not run a full compile to validate the present-tense claim at
   `docs/GESTALT.md:1281` that `crates/ir/src/vm/` "compiles at HEAD".
   The source directory exists, but this axis pass was documentation and
   git verification only.

2. I did not independently reconstruct the full ~78K AX.W0b cluster
   deletion from raw `git diff --stat` over the whole cluster; I relied on
   the archaeology and AX plan documents for that aggregate figure. The
   point that survives audit is narrower: the cluster framing is
   believable, while the single-commit shorthand is not.

3. Two future research artefacts named by `GESTALT` are not yet present in
   the working tree:
   `docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md` and
   `docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md`. This may simply mean those
   planned artefacts have not been authored yet. The current problem is
   citation posture: `GESTALT` points to them as anchors without clearly
   marking them as future W0 outputs.

4. Verdict: `GESTALT.md` is still valuable as synthesis, but it is not
   currently safe as the canonical master overview. The document needs one
   dedicated normalization pass after the AZ split:

   - refresh the live headline numbers,
   - repair the AX invariant numbering,
   - remove live BC references,
   - replace dead `docs/tranches/AZ/...` citations,
   - align the closing dependency thesis with BB's actual open gate, and
   - rewrite the final target-state paragraph around the struct-only end
     state rather than the obsolete tape-first language.

Until that lands, readers should trust the owning tranche docs over
`GESTALT.md` whenever the two disagree.
