**Six defects found; (c) is clean.** Read at `origin/sk-v27-lw3` = `d98914e34`; paths are under `restart/skinny/tranches/sk-v27/` unless given in full. I ran nothing beyond `git fetch`, `git show` and `git grep`.

**(c) `bounded_sum` custody: no defect.** `git ls-remote origin` lists all three refs at the reported heads (`e60d7dcff`, `68f16d0a9`, `aae676163`). The sha256 of every `bounded_sum.rs`, test and example on them equals the stopgap `SHA256SUMS`. `git log --remotes -- '**/bounded_sum.rs'` now lists three commits.

**(b) Passages that still let a verdict delete a kernel or organ, un-amended**

1. **Kernel deletions on a measured verdict that PLAN §7.1 does not reach.** DIG-G1 names "the kernels", so the "outside DIG-G1's four organs" reading at `PLAN.md:302` is wrong for x86 kernels.
   - `designs/programme.md:483`: "an `.S` kernel that does not beat its intrinsic twin … It is deleted. ASM earns its place or goes."
   - `designs/pprint.md:206-207`: "With no gain, the kernel is deleted" / "or deletion".
   - `designs/compiler.md:114` and `PLAN.md:59` (X-4): an x86 kernel or tier with no measured win is deleted.
   - Fix, PLAN §7.1 rows:
     - `| G1-P23 | designs/programme.md:483 | "It is deleted. ASM earns its place or goes." | DIG-G1: as G1-P16; an .S kernel that does not beat its intrinsic twin is not selected by default, its record stands and a redesign unit with a re-trial opens (DIG-K-C10); it is not deleted. |`
     - `| G1-P24 | designs/pprint.md:206-207 (PP-U6) | "With no gain, the kernel is deleted." / "or deletion" | DIG-G1: as G1-P12; with no gain the escape kernel is not selected by default; its slot, oracle and fuzz target stay. |`
     - `| G1-P25 | designs/compiler.md:114 (R2); PLAN.md:59 (X-4) | "a measured win on that runner. A kernel missing any of the four is deleted" / "no measured win … is deleted" | DIG-G1: the measured-win clause deletes no x86 kernel; a tier with no measured win is not selected by default. Deletion for a missing oracle, fuzz target or executing CI is a correctness rule and stands. Removing x86 is the owner's O-2 only. This corrects §7.1 "Read, not amended": x86 kernels are inside DIG-G1. |`

2. **`PLAN.md:56` (X-1) is un-amended.** It reads "The root pipeline is a quarry, re-admitted only as passes that win a trial (C2-5: a root trial can only *delete*)". G1-P04 amends only `designs/compiler.md:89`.
   - Fix: `| G1-P26 | PLAN.md:56 (X-1) | "re-admitted only as passes that win a trial (C2-5: a root trial can only *delete*)" | DIG-G1: as G1-P04; a root trial decides only whether the root implementation is ported. The CSP, the e-graph, shape mining and the kernels are built on the spine whatever it reads. |`

3. **Order text that still says an organ's deletion is merely later.**
   - `LW3-ORDER.md:107` must-not: "Delete an organ from master in LW3 (deletion is LW7)". G1-P22 quotes only the tag sentence.
   - `LW3-ORDER.md:299`: "by I6 a LOSS or UNDECIDED trial would delete the organ".
   - `LW3-ORDER.md:300`: "LW7.COMP-1 (losers deleted)".
   - `LW3-ORDER.md:290` (OWN-1): "CB-13 judges them there".
   - `LW2-ORDER.md:88`: "anything else is UNDECIDED and goes to the simpler organ".
   - Fix: `| G1-P27 | LW3-ORDER.md:107, :290, :299, :300; LW2-ORDER.md:88 | "(deletion is LW7)"; "CB-13 judges them there"; "would delete the organ"; "(losers deleted)"; "goes to the simpler organ" | DIG-G1: no organ is deleted at LW7 or at any wave; CB-13 judges defaults per named family, never existence; "the simpler organ" names the LW3 port default only (G1-P19, G1-P21). |`

4. **Consequence of 1–3: `LEDGER.md:293` reads DIG-G1 as GREEN while un-amended passages remain.** The dig's five-phrase grep cannot see "It is deleted", "earns its place or goes", "win a trial" or "deletion is LW7".
   - Fix, LEDGER addendum row: `| 2026-11-12 | **DIG-G1 status corrected: RED until PLAN §7.1 rows G1-P23..P27 land.** LG-g's reading grep widens to add 'It is deleted|earns its place or goes|win a trial|deletion is LW7|losers deleted|kernel is withdrawn|is retired'; `dig-g1-passages.tsv` and `r0_check.py` gain the five rows; RED ×2 → GREEN ×2 with a mutant. | RED |`

**(a) Dig rows narrowed, or admitted but not carried into the plan**

5. **DIG-CSP-E15 and DIG-EG-D17 are narrowed (`LEDGER.md:320`, `:333`).** The dig says "PREREG addendum, before any run". The fold homes them "before the LW5 re-trial", while each row claims it "does not narrow". Ask E15-W should be answered yes: registering is a document under its own digest (the PRG-1 pattern) and leaves the sealed JSON trial untouched.
   - Fix, RULINGS row: `| E15-W | **Register before S14** (delegate, owner may overturn). The discriminating trial (CSS L4, BBNF, Sheets, the three H-1 grammars; four arms; F-TRIAL-EG re-registered) is written as a PREREG addendum under its own digest before LW3 Phase 5 opens. The sealed LW3 JSON trial runs unchanged; its record is labelled non-discriminating (DIG-C18) and governs the LW3 port only. The discriminating run is taken at LW5. |`
   - Plus a LEDGER row re-homing both ids to "LW3, before S14; run at LW5".

6. **DIG-G2 is marked GREEN on the grep alone (`LEDGER.md:294`), but its purpose is unmet.** Its own title says the census checker still refuses LW5+ and suffixed ids. A substrate whose only consumer is LW5.COMP-1-MINE, -BRIDGE, -PAT or LW5.COMP-3 is therefore still fated DELETE by `crates/ir/tests/substrate_audit.rs:99`. That is a live deletion route for CSP, e-graph and mining substrate.
   - Fix, RULINGS row: `| Z32-W | **Yes** (delegate, owner may overturn). Z-32's ROUTE window widens to LW2–LW7 and the id pattern admits suffixed ids (LWn.XXX-n-SUFFIX) that PLAN §7.2 or the LEDGER admits. Cured in LW3 as unit LW3.COMP-1b-W (census checker), born-RED on a ROUTE to LW5.COMP-1-MINE, before any LW3 census seat writes a fate. |`
   - Plus a LEDGER row: `DIG-G2 status corrected: grep GREEN, effect RED until LW3.COMP-1b-W`.

7. **DIG-C16 is admitted (`LEDGER.md:383`) but `PLAN.md:206` still reads "a delete-unless-wins trial".** PLAN §7.1 leaves it as an ask at `:302`. The admitted ledger row and the plan text now contradict each other.
   - Fix, PLAN §7.1 row: `| C16-P01 | PLAN.md:206 (LW4.COMP-3); designs/compiler.md:64, :187 | "a delete-unless-wins trial" / "Wasm parse VM deleted unless it wins" | DIG-C16 (admitted 2026-11-12a): a LOSS switches the VM off by default; the wasm face is not deleted without an owner ruling. |`

**(d) Rulings in 2026-11-12a that narrow scope**

8. **ORC-1 (`RULINGS.md:166`) sends the TS-package defects to `DIVERGENCE.tsv` "each with an owner" and no cure unit or wave.** That is a carry with no home, against DIG-C21 ("divergence table = 0 or each row ruled") and DIG-G11.
   - Fix, RULINGS row: `| ORC-1a | Each TS-package R-b row filed under ORC-1 names its cure unit and wave (the TS emitter at LW4.COMP-4 / LW4.PP-3-TS, released at LW6) and carries its born-RED falsifier; a row with an owner and no unit blocks LG-3. |`

9. **PLAN §7.2 A-11 (`PLAN.md:320`) drops the dig's "HM and bidirectional inference" from LW5.COMP-4.** It reads only "with inference on the CSP substrate; scope is the owner's O-5". The delegate's own 2026-11-11e rule takes the fuller reading until the owner rules.
   - Fix, PLAN §7.2 row: `| A-26 | LW5.COMP-4 | LW5 | Amends A-11: until O-5 is ruled the fuller reading stands: HM and bidirectional inference with the CSP as substrate (DIG-CSP-B7, DIG-C9). First-order only is a narrowing the owner alone may make. | DIG-CSP-B7 |`

**No defect found:** all 116 dig ids have exactly one LEDGER row with a home. Every new unit the dig names is in PLAN §7.2. DIG-G1's 22 listed passages each exist at the cited line. Ask PREREG-G1 needs no sign-off, since the bytes and the decision rule are unchanged.