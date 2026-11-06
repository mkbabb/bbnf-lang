# CRIT-PB-1 — critique of the NEON shufti/truffle DFA prefilter spike

Adversarial review of `prototypes/PROTO-PB-1.md` against the code it tests
(`parse-that/rust/regex`, read-only, verified 2026-07-10) and against the codex
sections it feeds (`SPEC-DRAFT.md` PT-R2.W2/W3 lines 476-485; PB-1 brief 689-698;
§5 profiling law 47-55). Convergence: **80%.**

## What is solid (credit where earned)

1. **The wired code is real, not mocked.** Every anchor checks out against the live
   tree: `detect_accel` is `pub use`d (`lib.rs:27`) yet consumed by NO find loop — a
   grep of `dfa.rs` for accel/StateAccel returns nothing; the only `find_at`/
   `find_at_shortest` bodies (`dfa.rs:116,143`) step one byte at a time
   (`dfa.rs:123-137`). The `.take(8)` shufti cap is exactly at `accel.rs:124`; the
   `9+ exit bytes → _ => AccelStrategy::None` fallthrough is at `accel.rs:102-105`.
   The proto extends precisely the dead arm it claims to.
2. **The tier assignments are correct and self-consistent.** For a `+` self-loop the
   exit set is the COMPLEMENT of the class: `[^"\\]+` exits on `{", \}` = 2 bytes →
   Memchr2 (`accel.rs:92`); `[^\n]+` → 1 byte → Memchr1; `[ \t\r\n]+` exits on 252
   bytes and `[a-zA-Z0-9_]+` on 193 → both fall in the `9+` truffle gap. The proto's
   table labels these exactly right, which is why truffle (not shufti) carries the
   whitespace/identifier rows — a genuine net-new coverage claim, validated.
3. **Parity method is the gold standard.** Exhaustive per-offset
   `find_at == accel == gated` over 256 KiB + 8 MiB whole-doc equivalence, 0
   mismatches, negative-record discipline. This EXCEEDS the codex method's "run
   regex/tests for parity" (line 694) — a deviation toward higher rigor, not less.
4. **The skip-heavy directional verdict survives any plausible noise.** 18-67x is
   orders above the ≥2x bar and above any loadavg-7-14 artefact. That the DFA-accel
   prefilter is a large cold win on genuinely long self-loop runs is established.

## Where the evidence is weaker than the verdict claims

5. **"Regression gone" is contradicted by the proto's own table.** The PB-1 success
   bar is "no regression on dense-match" (SPEC line 696). The proto's gated ident
   dense-6 row is **0.95x** (`PROTO-PB-1.md:75`) — a 5% regression — yet the prose
   asserts "0.95-1.15x (regression gone)" (line 88). One of four dense cases still
   regresses at the gate. The criterion is met in a RELAXED sense (≤5% residual),
   not the crisp sense the verdict states.
6. **The design-critical numbers sit below the noise floor.** The whole "fused gate
   is MANDATORY" conclusion rests on dense deltas of 5-20% (ungated 0.53-0.86x →
   gated 0.95-1.15x; "gate costs ~5-20% of peak", line 89). But these are measured
   at **host loadavg 7-14, 9 rounds, median** (lines 12, 57) — not the §5 law's
   N≥200 + mean/95%CI + ≥3 windows (SPEC 49-51). Paired interleaving protects the
   39x ratios from slow drift, but a 5-20% delta at loadavg 7-14 is exactly the
   regime where scheduler descheduling dominates. The big win is safe; the small
   deltas that JUSTIFY the gate design are not certification-grade. The proto invokes
   "the profiling standing law" as its certifying authority (line 13) while its
   method meets none of that law's N/window/per-block-loadavg requirements — a mild
   misappropriation. Per-block loadavg is given as one global range, not per table.
7. **Corpus realism — the strongest reservation.** skip-4096 is a single 4 KB
   unescaped run; skip-256/512 are 256-512-byte runs. Real BBNF/JSON/CSS tokens are
   short: JSON keys 5-20 B, pretty-print indentation 1-4 B, identifiers 3-15 B.
   Whitespace and identifier runs — two of the four measured patterns — are almost
   never skip-heavy in practice; they live in the DENSE regime (~1x, or a regression
   without the gate). The proto's own scope note (lines 101-103) concedes the win is
   "per self-loop run, not per call," but the headline table and the Implications
   ("strings, comments, whitespace runs, identifiers — the common lexer shapes",
   lines 94-95) conflate token TYPE with token LENGTH. The realistic win on real
   lexer input is UNMEASURED; research/02's own estimate is a more sober 4-10x
   (research/02:83-85), and the proto's 39-67x on synthetic 4 KB runs overshoots it,
   which itself signals the corpora are tuned to the hypothesis.
8. **SIMD boundary-adversarial parity is asserted, not proven.** "Exhaustive
   per-offset" covers only the offsets in ONE 256 KiB corpus; the proto does not
   state the corpus was constructed to hit 15/16/17-byte run lengths, exit-byte-at-
   block-boundary, or the truffle hi-nibble≥8 `vbslq` edge. Parity is strong for the
   tested corpus, not proven for SIMD-edge adversaries — correctly why the PT-R2
   close gate demands regex/tests + checkasm (SPEC 494-496); leave it wave-owned but
   name it.

## Where the CODEX over-claims beyond the proto

9. **PT-R2.W2/W3 ship the ungated form the proto proves is forbidden.** SPEC-DRAFT
   (2026-07-09, predates this proto by hours) describes W2 as a bare "skip-to-next-
   exit-byte" (lines 478-480) and W3 truffle as "consumed by the same W2 wiring"
   (line 483) — with NO fused-trigger gate anywhere. The proto's central design
   output is that the ungated form MUST NEVER ship ("consume via the fused-trigger
   loop only", line 98) because it regresses on dense/short-token input. As written,
   the codex W2 would regress on the realistic short-token regime. This is the
   expected convergence-loop feedback: **W2 must be amended to make the fused GATE a
   required design element, and the "4-10x classic algorithmic win" (line 479-480)
   must carry a run-length/realism caveat.** Until folded, it is an OPEN design item,
   currently unowned.
10. **"The win is largest" for truffle needs the inverse caveat.** SPEC line 483 and
    research/02:104-106 say truffle's win is largest because it rescues the states
    shufti refused. True on skip-heavy — but the proto shows truffle ALSO has the
    WORST dense regression (0.53-0.59x ungated) because its two `vqtbl` LUT loads are
    the heaviest per-run dispatch. On the short-token regime where whitespace/idents
    actually live, truffle without the gate is the biggest loser, not the biggest
    winner. The codex frames only the upside.

## Residuals and ownership

- Fused-trigger gate absent from codex W2/W3 → **BLOCKING** (design change, unowned).
- Realistic-corpus win magnitude unmeasured; 4-10x claim uncaveated → **BLOCKING**
  (needs a real-token corpus before W2 is decision-grade; assign to PT-R2.W2 proof).
- Dense "regression gone" overstated + sub-noise at loadavg 7-14 → **BLOCKING** for
  the gate-cost claim; re-measure at low load / N≥200 in the PT-R2 close-gate proof.
- GATE=16 unswept (proto line 99) → non-blocking, PT-R2.W2 tuning, wave-owned.
- SIMD-edge adversarial parity → non-blocking, PT-R2 close gate (regex/tests +
  checkasm), wave-owned.

## Verdict

The hypothesis "wire the orphaned accel + a truffle tier → large cold skip win with
exact parity" is PROVEN for long self-loop runs, and the code substrate is real. But
the GO's two qualifiers — "no dense regression" and the mandatory-gate design — rest
on 5-20% deltas measured below the noise floor, and the codex has not yet absorbed
the gate as a W2 requirement nor caveated the win for realistic (short) tokens. The
directional finding is bankable; the design and the realistic-magnitude claim are
not yet executable-tomorrow. **80%.**
