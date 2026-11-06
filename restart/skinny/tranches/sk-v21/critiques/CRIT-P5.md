# CRIT-P5 — Hand-asm vs intrinsics on the 64-byte classifier: instrument sound, decision unsettled

Date: 2026-07-05. Subject: `prototypes/PROTO-P5.md` (worktree-isolated at
`.claude/worktrees/wf_be85d0ab-073-11`) and the spec sections it tests — `SPEC-DRAFT-P1.md`
§0:28-31, §5.4:198-201, §13:375. Verdict: the prototype is a well-built, unusually honest
instrument whose top-line answer over-reaches its own hedge; the decision it exists to inform
remains open pending a quiet re-capture and two unpriced cheaper doors. **Convergence 58%.**

## What P5 gets right (verified, load-bearing)

The evidence citations are accurate, not contrived. The kernel under test exists exactly as cited:
`nospace_bitmap_64_neon` at `crates/core/src/grammar/generated/json.rs:815`, its
`chunk_ns_mask16` helper at `:837`, and the `vshrn_n_u16::<4>` movemask P5 names as the cheaper
door already lives in the same file at `:928`. The shipping source already writes the minimal
`vorrq_u8` OR-tree (`json.rs:839-840`) — which corroborates P5's central claim that LLVM
De-Morgans it into an 8-op-surplus `mvn`/`bic`/`and` form in the binary rather than the author
choosing a bad spelling.

Method discipline is genuine: 200,005 byte-exact parity inputs across four bindings, a
callee-saved-clobber + canary guard, checkasm liveness (iterating every variant), and — decisively
— the report refuses to let the contaminated wall-clock carry the verdict, resting instead on the
`objdump` static op-count (40 vs 32 SIMD-ALU ops), which is machine- and load-independent. Every
figure is loadavg-labeled (17-21) and the report never launders it as cold. It also tests the
literature's *well-shaped* clause honestly via `nospace_intrin_ilp` rather than a naive-intrinsic
strawman. This is the correct shape for a P5 instrument.

## Where the verdict over-reaches its evidence

1. **The bolded "YES, the bound is exceeded — +9% to +12%" (P5:13) is stronger than the honest
   hedge at P5:139-141** ("do not open it until the quiet re-capture confirms > 5%"). The static
   op-count proves a code-size delta; the ~10% throughput figure is a *model prediction*
   (8 ops ÷ 4 pipes ≈ 2 cyc on ~20 cyc/64B), and the only measurement confirming it was taken at
   loadavg 17-21. The falsifiable question ("beat by more than 5%") is therefore answered
   "the model predicts ~10%, quiet confirmation pending" — not decisively YES.

2. **The op-count→throughput inference is unreconciled with the binding resource.** P5 attributes
   the win to classify ALU-op count, yet its own kernel description (P5:28-29) and report
   `25:341-343` name the 8 SIMD→GPR crossings per block as the dominant cost. If the kernel is
   crossing-bound, removing 8 classify-ALU ops partially hides behind crossing latency and the real
   quiet delta could fall under 5%. P5 does not establish which resource is the constraint.

3. **"Un-coercible from intrinsics" rests on n=1 spelling.** P5 tested exactly one alternate
   intrinsic (`vbicq_u8`, canonicalized away) and generalizes to "the emitter author has no
   intrinsic spelling that survives" (P5:96-97). Door (b) — an opaque barrier / reassociation
   forcing LLVM to keep the ORR-tree — is named untested (P5:130-131). The single most decisive
   alternative to an asm path is left unrun.

4. **Door (a) likely obviates the asm path entirely, making the asm question premature.** P5
   itself notes (P5:126-129) the crossing-reducing `vshrn` movemask algorithm is intrinsic-
   expressible, already in-file at `json.rs:928`, and "would beat all four variants." If so, the
   honest reading is *a suboptimal algorithm expressed in intrinsics loses to a good algorithm in
   asm* — precisely report `25:250-252`'s "not asm vs intrinsics, it is algorithm vs algorithm"
   caution. The asm door is not justified until the better intrinsic algorithm is benched and
   fails.

5. **No PMU, single uarch, off-pin toolchain.** Cycles are estimated from mach-timebase ns at an
   assumed 4.0/4.6 GHz (P5:147); one kernel, one M5 Max (the cost table also keys M1/M3);
   `rustc 1.96.0-nightly`, not the TW-S pinned stable 1.96.1 (§3.2) — the De-Morgan peephole is a
   one-LLVM-version fact.

## Where the spec over-claims against this evidence

The spec's *posture* is sound: intrinsics default, asm gated behind proof (§0:28-30), answer
explicitly deferred to pass 2 (§13:375). It does not over-commit the decision. But two wordings
are now self-contradictory against the prototype cited beside them:

- **§5.4:201 and §0:24 state "the honest bound is 0-5%" as fact while citing P5** — the very
  prototype that measured ~10% on bbnf's own kernel. Post-P5 the bound must read "0-5% for
  straight-line SIMD generally, with a measured instruction-selection exception (~10%, directional)
  on the classify kernel."
- **§5.4:200 enumerates the asm-gate causes as "mis-schedule, missing intrinsic, or spill
  failure"** — none of which is P5's cause. P5's finding is an instruction-selection peephole miss;
  a literal gate would *reject* P5's kernel. The gate must add "instruction-selection miss the
  intrinsic cannot survive" or the spec's own bar closes the door P5 says is warranted. §0:25's
  "except at register-pressure failures" is likewise too narrow.

Deeper: P5's strongest reading is evidence the asm path is probably *unneeded* (the door-(a)
algorithm fix is intrinsic-level), which the spec does not yet carry.

## Convergence: 58%

The instrument is settled and reusable (the checkasm harness is a real TW4 asset; source citations
verified). The **decision** it informs is not: the >5% claim is a model prediction under
loadavg 17-21, the two cheaper doors (algorithm swap; opaque-barrier coercion) are unpriced, the
binding-resource question is open, and two spec wordings need reconciliation. By P5's own verdict
the default stays intrinsics and the door stays shut pending a quiet re-capture — that is a full
additional loop pass, which places the item below the 70% plan-ready line.

**To reach convergence:** (1) quiet (loadavg < 2) PMU re-capture confirming > 5% with the same
op-count cause; (2) bench door (a), the `vshrn` movemask algorithm, which may beat all variants and
retire the question; (3) test door (b), whether an opaque barrier keeps the ORR-tree; (4) reconcile
§5.4/§0's "0-5%" statement and the gate-cause enumeration with P5's instruction-selection finding.
