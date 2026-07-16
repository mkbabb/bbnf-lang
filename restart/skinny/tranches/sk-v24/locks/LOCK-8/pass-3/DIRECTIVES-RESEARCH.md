# LOCK-8 — THE TS BACKBONE PLANE (parse-that/typescript) — PASS-3 RESEARCH DIRECTIVES

*parse-that/typescript is READ-ONLY; any landing rides letter BBNF-PT-G (owner or parse-that-side session). Prototype ONLY on scratch copies (PROVE-11). Never conflate the two engines: V8 JIT vs TS checker. Every measurement NAMES its HEAD and base-pins worktree prototypes (XL-H).*

**Settled cross-lock facts — convey as given, do not re-derive:**
- The fast TS execution face is the BUILD-TIME EMITTER (XL-D / LOCK-4B-TS, measured); runtime switch-loop and graph decoders survive only as isomorphism/correctness oracles — do NOT re-benchmark them as the fast face.
- parse-that is the canonical regex-engine home (XL-C); the copy vendored in skinny is the measurement substrate.
- Consumer-owner confirmations are pending and NON-blocking (XL-G): carry both hypotheses until the owner answers.

**PLANE-WIDE (answer early):**
- CONSUMER LIVENESS. Which repos are live, forward-looking consumers of the parse-that combinator surface today? (P1) Is value.js 4.0.0's current import posture intended, or a mid-migration WIP-branch artifact? (P2) Is latex-paper a consumer that will migrate onto a new cut, or effectively frozen? EVIDENCE: owner confirmation; commit cadence; installed-vs-pinned API-version lag; a re-run per-tree import census.
- SPEC FIDELITY. Where does each family-spec text diverge from the latest measured prototype outcomes? EVIDENCE: a per-family divergence list feeding a spec re-cut.

**8A — V8 RUNTIME PLANE.**
- Under Node's DEFAULT production tier, does the arity-N build-time specializer actually inline at each arity? EVIDENCE: `--trace-maglev-inlining` per arity; structural and temporal legs reported SEPARATELY; HEAD named.
- What arity distribution does the live consumer corpus exhibit, and does it justify specialization? EVIDENCE: an arity histogram from the ACTUAL live corpus, not a synthetic one.
- What is the relationship between the arity-N build-time specializer and the build-time TS emitter face (XL-D)? Is it one mechanism, constructible/available on the TS side? EVIDENCE: a cross-lock read of the emitter's state plus a scratch construction attempt.

**8B — TYPE PLANE.**
- Audit the `as unknown as` erasure inside the typed `all` signature: lawful variance/readonly erasure (documentable) or a genuine soundness leak? EVIDENCE: precise signature analysis; either a written justification or a readonly-variant signature that removes it with no new TS2589.
- Does the tightened typed algebra emit a BYTE-IDENTICAL `dist/`? EVIDENCE: the two-step isolation on the REAL vite/dist toolchain (not an esbuild proxy) — (i) the version-migration delta, then (ii) the tightening delta, which must be byte-empty.
- Can the instantiation-budget figure be taken on a live consumer's NATIVE corpus? EVIDENCE (gated on P2): if the consumer will migrate, measure native; else record un-runnable and take the number on self-use + type-tests under the EXACT tightened signature, disclosing signature + tsc version + corpus.
- Can the `skip`/`sepBy` input-type leaks be tightened? EVIDENCE: signatures + no-new-TS2589 check.

**8C — CORRESPONDENCE PLANE.**
- BUILD the divergence ledger: one machine-checkable row per combinator/leaf/subsystem (MIRRORED / LAWFUL-NON-MIRROR(reason) / DRIFT) from an AST/grep census over `parse_that/src` and `typescript/src/parse`. EVIDENCE: the ledger file itself.
- How should the regex correspondence row be classified given the measured two-engine divergences, and which differential corpus discriminates it? Enumerate the FULL set of hazard classes. EVIDENCE: a paired-triple corpus exercising every hazard class found.
- NEWLY OPENED (shared with LOCK-5): the Rust engine builds priority-ordered NFA ε-edges but the subset construction discards priority. Would a PRIORITY-PRESERVING DFA construction realize leftmost-first and dissolve the divergence classes — versus a build-time constraint on the grammar surface? EVIDENCE: a scratch priority-carrying subset-construction prototype plus the corpus that discriminates the two routes. Route the engine question to LOCK-5 as a decision.
- Coordinate the ledger↔linter-whitelist handshake and the tape-crossing artifact home with LOCK-6. Any decode-speed number must FENCE its measured path (pure-JS-decode vs wasm-boundary) per SPEC §0.

**8D — API PLANE.**
- CONSUMER-SET ADJUDICATION: for combinators lacking an external consumer, is the first-party test/bench suite a legitimate first-party consumer, or should the public surface prune to the live external contract? EVIDENCE: the consumer census + owner posture; state which combinators each disposition keeps or drops.
- Run the migration CRATER: baseline the live consumer GREEN at its pinned version, reinstall the candidate tarball, run tests + full type-check. EVIDENCE: crater results; run the minor-version step ALONE for attribution isolation.
- What mechanism carries per-parse diagnostic enablement, and what is its cost on the DISABLED path? EVIDENCE: hidden-class/monomorphism check; per-parse allocation comparison across candidate mechanisms.
- Does the inference tightening redden the consumer's type-check at its `any()` sites (a break caught only by full type-check, not unit tests)? EVIDENCE: a `vue-tsc`/`tsup` run at those sites. Resolve the nested-version dual-instance hazard (dedupe or document).
