# SK-V23 — The Owner Adjudication Packet (three checkboxes)

Date: 2026-07-10. Prepared for: the campaign owner. Prepared by: SK-V23 pass 2.

This packet closes the one residual the convergence ledger flags as **not wave-ownable
execution** — three decisions that need the owner's word, not a prototype
(`CONVERGENCE-LEDGER.md:55-58`, the "owner-ratification set"). Each item below carries a
**charter-grounded RECOMMENDATION the orchestrator will proceed on by default**, a rationale,
and the exact consequence of a veto. Silence is consent: absent a veto, the orchestrator
executes the recommendation as written. The packet exists so that consent is *informed*, and
so the one veto that would actually move the critical path is surfaced before it can surprise
anyone.

Nothing here is an implementation ask. Two of the three items (1 and 2) are framing decisions
about a letter bbnf-lang never places itself; the third (3) is a platform-envelope policy call
the x86-abrogation census deliberately refused to presume
(`research/01-x86-abrogation.md:88-95`).

---

## Item 1 — CONTINGENT-vs-DELIVERED framing of the BBNF-PT-F letter rows

**The question.** PASS-ALPHA's axes 1 and 2 read "BOTH repos" — full x86 abrogation and SOTA
SIMD in bbnf-lang *and* parse-that (`PASS-ALPHA.md:9-18`). But the non-clobber law forbids this
campaign from modifying, staging, or committing anything in `~/Programming/parse-that` beyond a
single additive coordination letter (`PASS-ALPHA.md:35-44`). So: what does the campaign get to
*claim* as delivered for the parse-that half, and how is the letter's one time-sensitive row
(the PT-R.5 patch-table repoint) handled without an external SLA holding bbnf-lang's critical
path hostage?

- [ ] **RATIFY (recommended)**  — [ ] Veto

**RECOMMENDATION.** Adopt the CONTINGENT-DELIVERY framing exactly as drafted at
`SPEC-DRAFT.md:477-482`: for parse-that, the campaign delivers only the **proposal artifacts**
(the PT-R\*/PT-T\* tranche specs, the diffs, the evidence coordinates) and claims *those* as
delivered. The x86-abrogation and SIMD *outcomes* inside parse-that are CONTINGENT on adoption
at parse-that's own cadence and are **never reported as campaign deliveries**. The five PT
tranches are review-not-execute (`SPEC-DRAFT.md:462`), placed via one letter the owner or a
parse-that-side session copies across — never a bbnf agent (`SPEC-DRAFT.md:466-472`).

The **one exception** is the sole HIGH / TIME-SENSITIVE row: the PT-R.5 repoint of parse-that's
gitignored `rust/.cargo/config.toml`, which hard-gates TB1.W2's `crates/` removal
(`SPEC-DRAFT.md:883`; the ordering law `SPEC-DRAFT.md:674-688`). Deliver that row as a **dated
ask at TB1 opening** (not at close), with the exact old→new path diff riding the letter, and a
**bounded fallback**: if it is unanswered by the TB1.W2 archival date, TB1.W2 proceeds anyway,
retaining the legacy `crates/egraph[-derive]` paths under a dated tag (Item 2's NEIGHBOR-GRACE
duplicate) so the repoint can still land later **without loss**. No external human act sits on
the critical path.

**RATIONALE.** Review-not-execute is the charter's own posture: the non-clobber law names the
one letter as the *entire* permitted footprint in parse-that (`PASS-ALPHA.md:35-44`), and the
census confirms both sibling repos (value.js Tranche T just ratified, keyframes.js Tranche U
just opened) are acutely mid-dispatch with **no open ask on bbnf-lang** — so any framing that
implied a parse-that gate would fabricate a dependency the fleet does not have
(`research/06-coordination.md:143-177`). Claiming parse-that *outcomes* as campaign deliveries
would be dishonest under a law that structurally prevents the campaign from producing them. The
contingent framing is the only reading that is simultaneously charter-faithful and honest.

**VETO CONSEQUENCE.** If the owner rejects the contingent framing and insists the "BOTH repos"
axes be reported as *delivered* outcomes, the campaign cannot honor that without breaching the
non-clobber law — the two directives are in direct tension, and the orchestrator would halt for
a charter re-issue rather than either clobber parse-that or misreport. If instead the owner
vetoes only the **opening-time delivery / fallback** for the PT-R.5 row (preferring at-close
delivery with no fallback), the critical path re-acquires an un-SLA'd external dependency:
TB1.W2 would block indefinitely on a one-minute edit to a file in a repo bbnf-lang may not
touch. This is the failure mode the amendment was written to close; vetoing it re-opens it.

---

## Item 2 — The NEIGHBOR-GRACE duplicate sanction

**The question.** The Item-1 fallback retains the legacy `crates/egraph` and
`crates/egraph-derive` paths past their scheduled TB1.W2 removal, so that parse-that's patch
table keeps resolving until the repoint is confirmed. That transiently leaves the egraph crate
existing at **two** coordinates (the legacy `crates/` path and the graduated
`crates-infra/egraph[-derive]` home) — a duplicate the no-legacy precept would ordinarily
forbid. Is that duplicate sanctioned?

- [ ] **RATIFY (recommended)**  — [ ] Veto

**RECOMMENDATION.** Sanction it as the campaign's **single, dated, bounded NEIGHBOR-GRACE
duplicate** (`SPEC-DRAFT.md:685-688`): one named row, retained only for `crates/egraph[-derive]`,
deleted **the instant** the repoint confirmation lands (owner acknowledgement + a read-only grep
of parse-that's gitignored `config.toml` showing `crates-infra` paths, ledgered in the TB1.W2
opening commit — reading violates nothing under the non-clobber law, `SPEC-DRAFT.md:682-688`).
It is grace extended to a *neighbor* repo's local build, not slack in bbnf-lang's own tree.

**RATIONALE.** parse-that's `rust/.cargo/config.toml` already broke once when bbnf-lang's B2
retired a crate path, and had to be hand-patched to compensate — the coupling is real,
gitignored, and invisible to a parse-that PR diff (`research/06-coordination.md:83-100`). A
hard TB1.W2 removal with no grace window would **brick parse-that's local Rust build** the
moment the owner or a parse-that session is slow to apply a one-minute edit — precisely the
sibling sessions the non-clobber law exists to keep unblocked (value.js/keyframes, mid-dispatch
today, `research/06-coordination.md:172-177`). The grace framing keeps them unblocked at the
cost of one transient, dated, auto-expiring duplicate — a strictly better trade than a bricked
neighbor.

**VETO CONSEQUENCE.** If the owner vetoes the duplicate on no-legacy grounds, the fallback in
Item 1 evaporates and the two options collapse to: (a) TB1.W2 blocks until the external repoint
is confirmed (re-introducing the un-SLA'd critical-path dependency), or (b) TB1.W2 removes on
schedule and parse-that's local build breaks until someone applies the repoint. Both are worse
than one dated, self-deleting duplicate. A veto here effectively also vetoes the Item-1
fallback and should be weighed as such.

---

## Item 3 — The release.yml LSP x86-target matrix

**The question.** PASS-ALPHA's axis 1 is a **binding** directive: full x86 abrogation,
aarch64/Apple-M only, everywhere, with anti-regrowth enforcement (`PASS-ALPHA.md:9-13`). The CI
and bench workflows flip to arm runners mechanically. But `.github/workflows/release.yml:16-33`
ships the **LSP editor binary** to five targets, three of them x86:
`x86_64-unknown-linux-gnu`, `x86_64-apple-darwin`, `x86_64-pc-windows-msvc`
(`research/01-x86-abrogation.md:90-95`). The census deliberately did **not** presume this one —
a distributed editor plugin arguably keeps x86 targets even when the parser core is NEON-only,
because the audience is end-user editors, not the aarch64 build/bench plane
(`research/01-x86-abrogation.md:90-95, 210-212`; flagged for owner at `SPEC-DRAFT.md:129-131`).

- [ ] **RATIFY aarch64-only + named-exception clause (recommended)**  — [ ] Veto (retain x86 LSP targets)

**RECOMMENDATION.** Drop the three x86 (and the Windows-MSVC) LSP targets; retain
`aarch64-apple-darwin` + `aarch64-unknown-linux-gnu`, consistent with the binding abrogation
directive. Attach a **named-exception clause**: if x86 editor distribution is ever shown to
matter (a real editor-user cohort on x86 hardware), reinstating the x86 LSP targets requires an
**explicit owner grant** recorded against this clause — it is the *one* sanctioned place x86 may
re-enter, and only for the distributed-editor artifact, never the parser/kernel/emit plane the
anti-regrowth gate guards (`SPEC-DRAFT.md:144-186`).

**RATIONALE.** The charter's word is "full x86 abrogation … everywhere" and it is marked
binding; the default therefore *must* be the drop, or "everywhere" is not honored. But the
census flagged this as a genuine policy call rather than a mechanical flip precisely because the
LSP is a *distribution* artifact with a plausibly-x86 audience — so the honest posture is
aarch64-by-default with an explicit, owner-gated escape hatch, rather than a silent presumption
in either direction. The named-exception clause keeps the abrogation total for everything the
anti-regrowth enforcement covers while leaving a legible, owner-controlled door for the one
surface where end-user hardware, not build hardware, sets the requirement.

**VETO CONSEQUENCE.** If the owner vetoes the drop and retains the x86 LSP targets, TB1.W3
records the retention as an owner-granted exception to axis 1 (the exact clause the
recommendation pre-drafts), the release.yml matrix stays five-wide, and the anti-regrowth token
gate's allowlist gains a scoped, reasoned entry for the release-workflow target strings so the
structural gate does not fire on them (`SPEC-DRAFT.md:155-167`). No other x86 surface is
affected — the parser, kernels, emitters, generated tree, CI and bench planes remain
aarch64-only regardless. The veto's blast radius is exactly the editor-distribution artifact and
nothing else.

---

## Decision block

| # | Item | Recommendation | Ratify | Veto |
|---|---|---|---|---|
| 1 | CONTINGENT-vs-DELIVERED letter framing + PT-R.5 dated ask & fallback | Adopt contingent framing; deliver the one time-sensitive row at TB1 opening with the bounded fallback | ☐ | ☐ |
| 2 | NEIGHBOR-GRACE duplicate sanction | Sanction one dated, bounded, self-deleting `crates/egraph[-derive]` duplicate | ☐ | ☐ |
| 3 | release.yml LSP x86 matrix | aarch64-only + named-exception clause (explicit owner grant to reinstate x86 LSP) | ☐ | ☐ |

**Default-proceed posture.** Absent a veto, the orchestrator executes all three recommendations
as written. Only Item 1's dependent element is time-boxed: the PT-R.5 repoint is a **dated ask**
delivered at TB1 opening; if unanswered by the TB1.W2 archival date, TB1.W2 proceeds under the
Item-2 NEIGHBOR-GRACE retention and the repoint lands later without loss. Items 2 and 3 carry no
external clock. Ratifying all three clears the owner-ratification mass the ledger names as the
last non-wave-ownable blocker to freeze (`CONVERGENCE-LEDGER.md:43-58, 74-76`).
