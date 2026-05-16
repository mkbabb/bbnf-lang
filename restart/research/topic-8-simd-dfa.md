# Topic 8 — SIMD scanning + DFA construction + bespoke regex HIR

## §1 — Settled position in the restart

This section quotes or near-quotes the restart claims this topic engages.

Each row is a settled restart claim, followed by the implication for this topic.

1. `restart/research/INDEX.md:5` requires each topic artefact to be `~500-1000 lines`.

2. `restart/research/INDEX.md:5` says the fold cycle absorbs §5 and §7 into the trio and PASS surfaces.

3. `restart/research/INDEX.md:5` says §6 may trigger escalation if SOTA contradicts a settled lock.

4. `restart/research/INDEX.md:22` requires §1 to cite path:line for every settled claim the topic engages.

5. `restart/research/INDEX.md:24` requires primary sources and a 5-to-15 citation count.

6. `restart/research/INDEX.md:26` requires convergence points to cite both restart claim and SOTA evidence.

7. `restart/research/INDEX.md:28` requires divergence points to name the divergence and reason.

8. `restart/research/INDEX.md:30` requires fold refinements with target file:line, current text, proposed text, and rationale.

9. `restart/research/INDEX.md:32` requires adversarial findings with contradicted lock, SOTA evidence, amendment, and receiving phase.

10. `restart/research/INDEX.md:34` requires surgery proposals with target file:line, directive, acceptance gate, and §5/§6 dependency.

11. `restart/research/INDEX.md:137` anchors Topic 8 on Lock 1 and the dedicated regex crate.

12. `restart/research/INDEX.md:138` anchors Topic 8 on README §6 and §8, Architecture §10 and regex sections, and PASS-2 SIMD/Pratt rows.

13. `restart/research/INDEX.md:139` asks what bespoke `parse-that` regex buys over `regex-automata`.

14. `restart/research/INDEX.md:139` asks what contract applies when SIMD matches something the DFA does not.

15. `restart/research/INDEX.md:139` asks whether the SIMD-first posture survives cases the SIMD path cannot accelerate.

16. `restart/research/INDEX.md:151` obligates every research agent to surface at least one adversarial finding.

17. `restart/research/INDEX.md:157` locks the research voice: calibrated, direct prose, path:line citations, no placeholders.

18. `restart/README.md:31` says internal generic substrate drops the `bbnf-` prefix.

19. `restart/README.md:31` lists `parse-that`, `regex` within `parse-that`, and `simd-scan` as generic substrate.

20. `restart/README.md:56` names `parse-that` as the combinator library.

21. `restart/README.md:57` names `simd-scan` as SIMD scanner kernels.

22. `restart/README.md:62` says the regex engine eventually folds into `parse-that`.

23. `restart/README.md:62` says the regex engine is published as a generic Rust regex library, not as `bbnf-regex`.

24. `restart/README.md:62` says the interim home is `parse-that/regex/`.

25. `restart/README.md:84` shows `bbnf` depending on `parse-that` for the regex engine.

26. `restart/README.md:89` says `simd-scan` is consumed by passes and runtime.

27. `restart/README.md:123` says Unicode is solved at the regex layer, not the grammar layer.

28. `restart/README.md:133` says grammar-level Unicode char-class algebra is not added.

29. `restart/README.md:133` says `parse-that/regex/` carries Unicode coverage.

30. `restart/README.md:135` claims latest Unicode standard coverage.

31. `restart/README.md:136` claims full property support such as `\p{XID_Start}`.

32. `restart/README.md:138` claims set algebra inside regex character classes.

33. `restart/README.md:139` claims grapheme cluster awareness.

34. `restart/README.md:140` claims normalization modifiers.

35. `restart/README.md:143` says regex literals are the rich-Unicode entry point.

36. `restart/README.md:180-182` says Pratt, SIMD scanner opportunities, and PHF keyword sets emerge from grammar shape.

37. `restart/README.md:182` says there are no `@pratt`, `@simd`, or `@phf` directives.

38. `restart/README.md:188` says the pipeline is fixed-point co-iteration with explicit IR input/output.

39. `restart/README.md:197-198` says shape mining identifies Pratt operators, SIMD scanners, PHF keywords, recovery boundaries, and lookbehind widths.

40. `restart/README.md:199-201` includes charclass merging and keyword-set detection in V1 e-graph rewrites.

41. `restart/README.md:202` says cost-model extraction follows e-graph saturation.

42. `restart/README.md:217` defines `Cost` and says both parser and regex cost models implement it.

43. `restart/README.md:217` says the parser can know a regex scan is cheap without knowing regex internals.

44. `restart/README.md:285` names the value substrate as tape + direct-to-struct union.

45. `restart/README.md:291` defines tape as a contiguous parsed-token stream, cache-locality-optimal and SIMD-friendly.

46. `restart/README.md:294` says tape carries token discriminant, source span, payload offset, and structural pointer.

47. `restart/README.md:314` says there is one representation, one materialisation surface, and one Visitor pattern.

48. `restart/README.md:338-340` says SIMD is first-class everywhere: NEON, AVX2, AVX-512, WASM-SIMD, and scalar fallback.

49. `restart/README.md:358` says simdjson contributes contiguous tape, two-pass parse, on-demand API, and SIMD escape-handling primitives.

50. `restart/README.md:364` says logos contributes fast lexer-generator codegen idioms and SIMD-aware lexer specialization.

51. `restart/README.md:365` says `regex-automata` contributes DFA, NFA, and hybrid regex engines.

52. `restart/README.md:365` routes that contribution to `parse-that/regex`.

53. `restart/README.md:392` says Lock 10 is honored by shape miners and no `@pratt` or `@simd` directives.

54. `restart/README.md:393` says `parse-that` path-deps honor Lock 11 until publication.

55. `restart/ARCHITECTURE.md:21` says tape is the substrate and is unioned with direct-to-struct.

56. `restart/ARCHITECTURE.md:25` says Unicode class algebra is deferred to `parse-that/regex`.

57. `restart/ARCHITECTURE.md:30` says CSP, egraph, miners, and cost model compose by output piping.

58. `restart/ARCHITECTURE.md:51` says `codegen` lowers Rust, WASM V1, SIMD patterns, template rendering, and regen equality.

59. `restart/ARCHITECTURE.md:61` says `parse-that` owns regex and parser substrate utilities.

60. `restart/ARCHITECTURE.md:62` says `simd-scan` owns AVX2, AVX512, NEON, WASM SIMD, and scalar fallback kernels.

61. `restart/ARCHITECTURE.md:127-128` says `passes` depends on `parse-that` and `simd-scan`.

62. `restart/ARCHITECTURE.md:159-160` says `parse-that` depends only on `error`.

63. `restart/ARCHITECTURE.md:174` says `parse-that` has no `bbnf` dependency.

64. `restart/ARCHITECTURE.md:322` says `parse-that` exposes regex parser/program APIs, DFA/literal helpers, and Unicode wrappers.

65. `restart/ARCHITECTURE.md:535-540` gives `parse-that/src/` children: `regex/`, `dfa/`, `unicode/`, `literal/`.

66. `restart/ARCHITECTURE.md:542-549` gives `simd-scan/src/` children: scalar, neon, avx2, avx512, wasm, dispatch.

67. `restart/ARCHITECTURE.md:588` says `parse-that` private internals include Unicode table generation scratch data and DFA builder state.

68. `restart/ARCHITECTURE.md:589` says `simd-scan` private internals include intrinsic loop bodies and dispatch probe cache.

69. `restart/ARCHITECTURE.md:668-672` puts `pratt`, `simd`, `literal_trie`, and `regex_prefilter` under recognizer metadata.

70. `restart/ARCHITECTURE.md:694-700` repeats per-grammar optimization metadata for recognizers, Pratt, SIMD, layout, and regex prefilter.

71. `restart/ARCHITECTURE.md:720` says `pratt`, `simd`, and recognizers default to `auto`.

72. `restart/ARCHITECTURE.md:834` says Grammar IR has a `Regex` variant owned by `parse-that/regex`.

73. `restart/ARCHITECTURE.md:853-854` says `Literal` and `Regex` may lower to `SimdScan`.

74. `restart/ARCHITECTURE.md:870` says regex Unicode classes are opaque regex data.

75. `restart/ARCHITECTURE.md:888-889` says Backend IR has `RegexProgram` and `SimdScan`.

76. `restart/ARCHITECTURE.md:915` says SIMD may feed `DispatchAlt` discriminators.

77. `restart/ARCHITECTURE.md:917` says SIMD may accelerate a `RepeatLoop` body prefix.

78. `restart/ARCHITECTURE.md:919` says SIMD may widen byte-literal compare.

79. `restart/ARCHITECTURE.md:920` says `RegexProgram` calls the regex engine and Unicode stays below BBNF.

80. `restart/ARCHITECTURE.md:921` says `SimdScan` has scanner kind, needle/class, and fallback; Rust dispatches to `simd-scan`; VM calls scalar reference.

81. `restart/ARCHITECTURE.md:949` says a long literal set or regex prefilter can become `SimdScan`.

82. `restart/ARCHITECTURE.md:972` says SIMD and Pratt are mined, not syntax-directed.

83. `restart/ARCHITECTURE.md:1022` says `BBNF-SIMD-NOT-SELECTED` means cost rejected SIMD.

84. `restart/ARCHITECTURE.md:1092-1093` says `RegexProgram` is parsed by `parse-that/regex`.

85. `restart/ARCHITECTURE.md:1110` says `parse-that/regex` HIR may model class algebra inside regex literals.

86. `restart/ARCHITECTURE.md:1111` says `parse-that/regex` retains regex-internal lookbehind.

87. `restart/ARCHITECTURE.md:1233` says the SIMD lowerer is fed by recognizer facts and `SimdScan` BIR.

88. `restart/ARCHITECTURE.md:1271` says scalar parity hash must match for structural scans.

89. `restart/ARCHITECTURE.md:1280` says WASM/SIMD target-specific output must be attributed by target and SOTA report.

90. `restart/locks/LOCKS.md:34` defines tape as the greenfield contiguous parsed-token-stream-with-payload-arena.

91. `restart/locks/LOCKS.md:34` forbids parallel substrates and orthogonal codepaths.

92. `restart/locks/LOCKS.md:52` says Pratt and SIMD are auto-detected.

93. `restart/locks/LOCKS.md:52` says the optimizer mines leaf-pattern shape, including charclass, keyword set, and regex, into SIMD scanner decisions.

94. `restart/locks/LOCKS.md:52` says cost model decides when SIMD overhead is worth dispatch cost.

95. `restart/locks/LOCKS.md:54` path-deps `bbnf-regex`, `parse-that`, and other sister crates until API stability.

96. `restart/locks/LOCKS.md:60` says generic crates carry zero grammar-specific code.

97. `restart/audit/pass-2-codegen/PASS-2.md:5` assigns SIMD scanner kernels and Pratt/SIMD auto-detection to PASS-2.

98. `restart/audit/pass-2-codegen/PASS-2.md:7` says codegen is reinvented around Backend IR and tape-backed runtime templates.

99. `restart/audit/pass-2-codegen/PASS-2.md:15` says grammar-level Unicode class algebra is not added; regex literals and `parse-that/regex` carry Unicode.

100. `restart/audit/pass-2-codegen/PASS-2.md:42` says `simd-scan` is generic and BIR emits data-only `StructuralAlphabet` constants.

101. `restart/audit/pass-2-codegen/PASS-2.md:64-65` includes `Scanner` and `RegexDfa` BIR variants.

102. `restart/audit/pass-2-codegen/PASS-2.md:76` includes `SimdScan` as a BIR variant.

103. `restart/audit/pass-2-codegen/PASS-2.md:104` requires scanner lowering tests for slice compare, regex, and `simd-scan` parity.

104. `restart/audit/pass-2-codegen/PASS-2.md:105` requires Pratt LUT and SIMD-vs-scalar selection tests.

105. `restart/audit/pass-2-codegen/PASS-2.md:155-163` defines the SIMD coverage matrix and scalar reference.

106. `restart/audit/pass-2-codegen/PASS-2.md:170` selects SIMD only when alphabet is non-empty, kernel shape is non-empty, and cost beats scalar.

107. `restart/audit/pass-2-codegen/PASS-2.md:170` rejects SIMD when alphabet is Unicode-semantic, tiny, or setup cost wins.

108. `restart/audit/pass-2-codegen/PASS-2.md:178` says `RegexDfa` and `Scanner` variants realize the unified cursor/byte-skip path.

109. `restart/audit/pass-2-codegen/PASS-2.md:178` says `PrattSpine` and `SimdScan` elide cursor consultation in inner loops.

110. `restart/audit/pass-2-codegen/PASS-2.md:546` says SIMD fallback is scalar when cost evidence does not win.

111. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:15` says SIMD structural scan can lose on tiny leaves.

112. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:16` says SIMD byte scanner must not pretend to be a Unicode parser.

113. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:22` says there are no recognizer force/skip knobs in V1.

114. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:24` says every auto-decision has an audit row.

115. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:28` says SIMD uses `simd-scan` kernel shapes.

116. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:30` says SIMD scanner returns offsets mapped into tape nodes and `sib_skip` links.

117. `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:80-83` names risks: Pratt semantic rewrite, sparse SIMD slowdown, Unicode leakage, and host-chain mixing.

118. `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:17` says WASM SIMD has scalar fallback and parity before speed.

119. `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:30` says WASM SIMD and scalar are byte-identical.

120. `restart/audit/pass-2-codegen/agent-3-wasm-lowerer-simd-architect.md:57` says SIMD kernel differences from scalar are incorrect parse offsets.

121. `restart/audit/hardening/HARDENING-CONSOLIDATED-V4.md:14` says the V4 hardening cohort verdict was READY.

122. `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:14` says PASS-2 diagnostic strings reintroduced `@pratt` and `@simd` user controls.

123. `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:22-28` says V5 found contract-expression drift, not broad architecture reversal.

124. `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:74-78` says V5 did not redesign locks, IRs, runtime substrate, or migration topology.

125. `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:159` required `BBNF-OPT001` and `BBNF-OPT002` to avoid user-forced directives.

126. `restart/audit/hardening/HARDENING-PASS-2-V5.md:139` marks forbidden `@pratt` and `@simd` diagnostics as FAIL-AMEND.

127. `restart/audit/hardening/HARDENING-PASS-2-V5.md:222-223` prescribes replacing both diagnostic strings without forced directives.

128. `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:136` says Pratt/SIMD do not become author directives.

129. `docs/precepts/instructions/LESSONS-LEARNED.md:74-80` requires producer and consumer gates for contracts.

130. `docs/precepts/instructions/STYLE.md:5-8` requires pragmatic, economical writing.

Implication for Topic 8:

131. The restart already has the right broad posture: regex below BBNF, SIMD as a mined fast path, cost selection, scalar fallback, and no user force directive.

132. The restart is still under-specified at the precise SIMD-to-DFA boundary.

133. A SIMD positive must be classified as exact, superset-prefilter, or invalid.

134. For regex prefilters, the DFA or equivalent scalar/VM verifier must own acceptance.

135. For exact structural scans, scalar parity over offset vectors must be the invariant.

136. For Unicode-semantic regex, SIMD byte scanning cannot be the semantic engine.

137. For expensive Unicode DFAs, full DFA codegen cannot be the only execution plan.

## §2 — SOTA literature deep-dive

Primary-source set: 8 sources.

No tertiary summaries are used as evidence.

Provenance gap: none for the required primary sources; every required source was verified through a paper, official documentation, or canonical repository.

[S1] Russ Cox, Regular Expression Matching Can Be Simple And Fast, 2007.

URL: https://swtch.com/~rsc/regexp/regexp1.html

Verified claim: Thompson NFA simulation avoids exponential backtracking and can be cached into DFA states on demand.

Load-bearing evidence: Cox explains that DFA states correspond to lists of NFA states and can be built lazily as those lists are encountered.

Design tradeoff: full DFA execution is faster once built, but startup time and memory can rise.

Topic use: this supports NFA-to-DFA construction, but it also supports lazy/on-demand DFA rather than mandatory ahead-of-time DFA for every regex.

Topic use: `parse-that/regex` should preserve the ability to choose Thompson VM, lazy DFA, or full DFA codegen under cost and size limits.

[S2] Russ Cox, Regular Expression Matching: the Virtual Machine Approach, 2007.

URL: https://swtch.com/~rsc/regexp/regexp2.html

Verified claim: VM-style regular expression matching can implement unanchored search in one linear pass.

Verified claim: character decoding can happen once per input character in the VM loop.

Design tradeoff: captures and submatch semantics complicate otherwise clean finite-automata execution.

Topic use: a bespoke regex HIR may earn its keep if it records grammar-owned capture needs, bounded lookbehind, and Unicode mode before selecting VM or DFA.

Topic use: DFA codegen alone is insufficient if BBNF needs capture offsets or regex-internal lookbehind semantics.

[S3] Geoff Langdale and Daniel Lemire, Parsing Gigabytes of JSON per Second, VLDB Journal 2019.

URL: https://arxiv.org/abs/1902.08318

HTML verification URL: https://ar5iv.labs.arxiv.org/html/1902.08318v7

Verified claim: simdjson makes extensive use of SIMD and reports a standard-compliant parser processing gigabytes per second on one core.

Verified claim: JSON has six structural characters and stage 1 writes their locations into an integer index array.

Verified claim: stage 1 uses SIMD over bytes or bitsets and can do fixed work even on inputs where no complex quoting appears.

Verified claim: stage 2 consumes structural indexes and writes a tape with structural jump annotations.

Design tradeoff: SIMD scanning is a structural-index accelerator, not the whole parser.

Topic use: bbnf's SIMD scanner should feed candidate offsets into the tape path and exact parser, not bypass the parser's semantic checks.

[S4] Owens, Reppy, and Turon, Regular-expression derivatives re-examined, Journal of Functional Programming, 2009.

URL: https://www.cambridge.org/core/journals/journal-of-functional-programming/article/regularexpression-derivatives-reexamined/E5734B86DEB96C61C69E5CF3C4FB0AFA

DOI: https://doi.org/10.1017/S0956796808007090

Verified claim: derivatives compile regular expressions to deterministic finite-state machines.

Verified claim: derivatives support boolean operations such as intersection and complement.

Verified claim: the implementation can be extended to large character sets such as Unicode.

Verified claim: derivative-built machines can be smaller than traditional construction in the reported implementations.

Design tradeoff: derivatives are attractive for class algebra and Unicode HIR, but the implementation must carry canonicalization and state-size policy.

Topic use: `parse-that/regex` may reasonably own bespoke HIR if it wants class algebra, complement, intersection, and Unicode policy below BBNF.

[S5] `regex-automata` official crate documentation and current source home.

Docs URL: https://docs.rs/regex-automata/latest/regex_automata/

Hybrid URL: https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html

Source URL: https://github.com/rust-lang/regex/tree/master/regex-automata

Verified claim: the crate exposes regex engines used by the Rust `regex` crate and guarantees worst-case `O(m * n)` search time.

Verified claim: full DFA construction can take worst-case exponential time, while search is direct once built.

Verified claim: lazy DFA can be close to full DFA speed in practice, while avoiding worst-case exponential compile time.

Verified claim: lazy/full DFAs do not support captures or general Unicode word boundaries.

Verified claim: the meta engine composes engines dynamically; a faster engine may find match bounds before a slower engine resolves captures.

Design tradeoff: `regex-automata` already solves much of the generic regex-engine problem.

Topic use: bespoke `parse-that/regex` must justify itself by grammar-owned HIR, BIR integration, generated verifier contracts, and cost visibility.

[S6] Intel Hyperscan official documentation.

Runtime URL: https://intel.github.io/hyperscan/dev-reference/runtime.html

Intro URL: https://www.intel.com/content/www/us/en/developer/articles/technical/introduction-to-hyperscan.html

Verified claim: Hyperscan compiles regexes into a pattern database and scans via block, streaming, or vectored APIs.

Verified claim: scanning uses a preallocated scratch space.

Verified claim: runtime engines include NFA, DFA, and other engines, accelerated with SIMD.

Verified claim: streaming mode maintains state across blocks and can delay zero-width matches.

Design tradeoff: production SIMD regex is multi-engine and mode-aware; it is not a single DFA-only hot loop.

Topic use: bbnf can borrow the compile-time database idea for generated parser artefacts, but it should avoid Hyperscan's C API shape and large-pattern-set goal unless needed.

[S7] Vectorscan official repository.

URL: https://github.com/VectorCamp/vectorscan

Verified claim: Vectorscan is a fork of Intel Hyperscan modified for more platforms.

Verified claim: ARM NEON/ASIMD and Power VSX support are functional, with SIMDe as a portability route.

Verified claim: Vectorscan/Hyperscan is a high-performance multiple-regex matching library using hybrid automata.

Design tradeoff: SIMD regex portability is an architecture matrix problem, not a single intrinsic file.

Topic use: bbnf's `simd-scan` matrix of NEON, AVX2, AVX512, WASM, and scalar is directionally sound.

[S8] Logos official docs and repository.

Docs URL: https://docs.rs/logos/latest/logos/

Repository URL: https://github.com/maciejhirsz/logos

Verified claim: Logos combines token definitions into a single deterministic state machine.

Verified claim: Logos optimizes branches into lookup tables or jump tables.

Verified claim: Logos prevents backtracking inside token definitions and does compile-time heavy lifting.

Design tradeoff: lexer codegen benefits from deterministic generated code, but Logos is a token lexer rather than a grammar-owned regex/tape substrate.

Topic use: Logos supports bbnf's generated scanner idiom, while showing that a bespoke engine must be narrowly scoped to grammar integration.

SOTA synthesis for the engagement question:

1. Bespoke `parse-that/regex` buys control over grammar-owned regex HIR, BIR payloads, Unicode class algebra under regex literals, code-generated verifier stubs, and cost-model introspection.

2. It does not automatically buy better automata algorithms than `regex-automata`.

3. `regex-automata` already has full DFA, lazy DFA, Thompson NFA, PikeVM-style capture support through the meta engine, prefilter composition, and size-limit policy.

4. The strongest reason to build bespoke is not raw regex matching speed.

5. The strongest reason is end-to-end compiler integration: parsed regex handles in Grammar IR, verifier contracts in BIR, generated code snapshots, and tape offset ownership.

6. When SIMD matches something the DFA does not, SOTA points to a prefilter contract.

7. A SIMD positive for regex means candidate, never accepted token, unless the scan has been proven exact for that terminal class.

8. The DFA, VM, or scalar reference verifier is the authority for acceptance.

9. A SIMD negative for an exact scan must imply no verifier match; otherwise it is a soundness bug.

10. A SIMD negative for a superset prefilter is not allowed to skip a real DFA match.

11. The SIMD-first posture survives only if read as "SIMD is first-class and tried when cost and exactness permit."

12. It fails if read as "every regex path starts with SIMD."

## §3 — Convergence points

1. Convergence: Regular regex execution should avoid backtracking pathologies.

Restart evidence: `restart/README.md:365` routes DFA/NFA/hybrid regex engines into `parse-that/regex`.

SOTA evidence: [S1] shows Thompson NFA simulation and lazy DFA construction as the finite-automata answer to exponential backtracking.

Assessment: The restart matches the Cox posture if `parse-that/regex` keeps VM and lazy/full DFA options.

2. Convergence: DFA construction belongs below BBNF, not in grammar syntax.

Restart evidence: `restart/README.md:133-143` routes Unicode and class algebra through regex literals.

SOTA evidence: [S4] treats derivatives and large character sets as regex implementation machinery.

Assessment: BBNF should not grow grammar-level Unicode class algebra.

3. Convergence: SIMD scan should feed a structural index or offset set.

Restart evidence: `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:30` says SIMD scanner returns offsets mapped into tape nodes and `sib_skip`.

SOTA evidence: [S3] stage 1 writes structural locations to an integer index array, and stage 2 builds the tape.

Assessment: The restart correctly treats scan output as data feeding the tape path.

4. Convergence: SIMD needs scalar parity.

Restart evidence: `restart/audit/pass-2-codegen/PASS-2.md:155-163` names scalar as reference for all parity.

SOTA evidence: [S3] validates documents after SIMD structural indexing; [S6] separates compiled database scanning from application match handling.

Assessment: Scalar/reference parity is the correct test floor.

5. Convergence: SIMD should be cost-selected.

Restart evidence: `restart/locks/LOCKS.md:52` says the cost model decides whether SIMD overhead is worth dispatch cost.

SOTA evidence: [S5] warns lazy DFA speed may be worse without a good prefilter and that full DFA construction can be costly.

Assessment: A cost model is mandatory, not decorative.

6. Convergence: Unicode semantic matching is not byte structural scanning.

Restart evidence: `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:16` says SIMD byte scanner must not pretend to be a Unicode parser.

SOTA evidence: [S4] calls out large character-set support inside the regex algorithm; [S5] notes Unicode word boundaries are a difficult DFA case.

Assessment: Unicode stays in `parse-that/regex`, with SIMD only as an accelerator for safe byte-level prefilters.

7. Convergence: Generated deterministic lexer/scanner code is a valid target.

Restart evidence: `restart/README.md:364` absorbs Logos codegen idioms into `simd-scan` and `codegen/rust`.

SOTA evidence: [S8] combines token definitions into one deterministic state machine and optimizes branch shape at compile time.

Assessment: bbnf can generate deterministic scanner code when the grammar gives token-like leaves.

8. Convergence: No user `@simd` or `@pratt` directive.

Restart evidence: `restart/README.md:180-182`, `restart/locks/LOCKS.md:52`, and `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:136` all forbid author-forced SIMD/Pratt.

SOTA evidence: [S5] and [S8] make engine choice a compiler/library decision, with config and builders rather than grammar syntax.

Assessment: The restart's no-directive rule survives SOTA.

9. Convergence: Multi-engine regex is normal.

Restart evidence: `restart/README.md:365` cites DFA/NFA/hybrid engines, and `restart/audit/pass-2-codegen/PASS-2.md:170` rejects SIMD for Unicode-semantic or tiny cases.

SOTA evidence: [S5] meta regex composes fast and capable engines dynamically.

Assessment: `parse-that/regex` should be multi-engine from the start.

10. Convergence: Architecture matrix matters for SIMD.

Restart evidence: `restart/README.md:338-340` and `restart/audit/pass-2-codegen/PASS-2.md:155-163` name NEON, AVX2, AVX512, WASM, and scalar.

SOTA evidence: [S7] exists because Hyperscan portability across ARM and other platforms needed a fork.

Assessment: Per-ISA parity and dispatch are not optional engineering details.

## §4 — Divergence points

1. Divergence: The restart names `RegexDfa` and DFA codegen more firmly than SOTA would for every regex.

Restart evidence: `restart/audit/pass-2-codegen/PASS-2.md:65` names `RegexDfa`; Topic 8 asks about NFA-to-DFA construction and DFA codegen at `restart/research/INDEX.md:139`.

SOTA evidence: [S5] says full DFA construction can be worst-case exponential and lazy DFA or NFA simulation may be better for large patterns.

Reason: This is likely an under-specified shorthand, not a settled demand to fully determinize every regex.

Disposition: Refine `RegexDfa` to `RegexProgram` or make `RegexDfa` one execution plan under `RegexProgram`.

2. Divergence: The SIMD-to-DFA mismatch contract is implied, not stated.

Restart evidence: `restart/ARCHITECTURE.md:921` says `SimdScan` has fallback and scalar reference, but it does not define false-positive/false-negative semantics.

SOTA evidence: [S5] meta regex uses fast engines to find bounds and slower engines to resolve captures; [S6] reports matches through a scan API and callback, with scratch and mode state.

Reason: SOTA engines treat fast paths as candidates or exact engines with explicit mode contracts.

Disposition: Add "exact scan" versus "prefilter scan" classifications to BIR.

3. Divergence: `SIMD: first-class everywhere` can be misread as `SIMD first for every regex`.

Restart evidence: `restart/README.md:338-340` uses the phrase "SIMD: first-class everywhere."

SOTA evidence: [S3] says SIMD stage 1 can be inefficient on some inputs; [S5] says engine and prefilter choice depends on pattern and haystack.

Reason: The settled Lock 10 cost-model language is narrower and better than the heading.

Disposition: Keep first-class SIMD, retire any "always first" reading.

4. Divergence: Rich Unicode ambitions exceed what a simple DFA codegen story can carry.

Restart evidence: `restart/README.md:135-140` claims full Unicode properties, set algebra, graphemes, and normalization modifiers.

SOTA evidence: [S4] supports large character sets via derivatives, but [S5] marks Unicode word boundaries and large Unicode classes as hard DFA cases.

Reason: The restart lists feature coverage without stating engine fallback and state-size limits.

Disposition: Add Unicode-state-budget and fallback policy to `parse-that/regex`.

5. Divergence: Vectorscan/Hyperscan targets massive multi-pattern scanning, while bbnf needs grammar-local recognizers.

Restart evidence: `restart/locks/LOCKS.md:60` forbids grammar-specific code in generic crates and `restart/ARCHITECTURE.md:720` defaults recognizers to auto.

SOTA evidence: [S6] and [S7] target large sets of regexes and streaming/block/vectored scans.

Reason: bbnf should borrow contract shape and SIMD discipline, not a DPI-style API.

Disposition: Do not imitate Hyperscan's database and callback API unless future benchmark evidence asks for it.

6. Divergence: Logos uses derive-time Rust codegen; bbnf uses committed generated parser/runtime source.

Restart evidence: `restart/locks/LOCKS.md:44` requires xtask-emitted committed source.

SOTA evidence: [S8] does heavy lifting at compile time in derive-generated lexer implementation.

Reason: bbnf has a stronger auditability and regen-equality constraint than Logos.

Disposition: Borrow deterministic scanner shape, keep committed generated artefacts.

## §5 — Refinements to fold

Refinement 1.

Target: `restart/README.md:62`.

Current text: "The `regex` engine eventually folds into `parse-that` (Q8) — published as a generic Rust regex library, not as `bbnf-regex`."

Proposed text: "The `regex` engine eventually folds into `parse-that` (Q8), published as a generic Rust regex library with grammar-owned HIR, NFA/VM, lazy/full DFA execution plans, Unicode tables, and explicit SIMD prefilter/verifier contracts."

Rationale: [S5] already supplies generic NFA/DFA/hybrid engines; [S1] supports lazy DFA; [S4] supports Unicode/class algebra. Bespoke work must name the bbnf-specific addition.

Refinement 2.

Target: `restart/README.md:217`.

Current text: "Bridging via `Cost` allows the parser to know \"this regex scan is X cheap\" without knowing regex internals."

Proposed text: "Bridging via `Cost` allows the parser to know whether a regex path is exact scan, SIMD prefilter plus verifier, lazy DFA, full DFA, or VM, without knowing regex internals."

Rationale: [S5] meta regex composition and [S3] two-stage scanning both need the cost model to distinguish candidate generation from acceptance.

Refinement 3.

Target: `restart/README.md:338-340`.

Current text: "SIMD: first-class everywhere ... portable scalar fallback."

Proposed text: "SIMD: first-class where cost and exactness permit ... portable scalar and DFA/VM verifier fallback."

Rationale: [S3] and [S5] both show fast paths are conditional; Lock 10 at `restart/locks/LOCKS.md:52` already says cost decides.

Refinement 4.

Target: `restart/ARCHITECTURE.md:535-540`.

Current text: `parse-that/src/` lists `regex/`, `dfa/`, `unicode/`, `literal/`.

Proposed text: expand the tree to `regex/hir/`, `regex/nfa/`, `regex/dfa/`, `regex/vm/`, `regex/prefilter/`, `unicode/`, and `literal/`.

Rationale: [S1], [S2], [S4], and [S5] require more than a single DFA builder surface.

Refinement 5.

Target: `restart/ARCHITECTURE.md:588`.

Current text: "`parse-that` | Unicode table generation scratch data and DFA builder state. | Regex program APIs are stable."

Proposed text: "`parse-that` | Unicode table generation scratch data, HIR simplification caches, NFA/DFA builder state, lazy-DFA cache policy, and SIMD prefilter plans. | Regex program APIs and verifier contracts are stable."

Rationale: [S5] exposes size limits and engine composition as core API concerns; [S4] needs HIR simplification for derivatives and boolean class operators.

Refinement 6.

Target: `restart/ARCHITECTURE.md:920-921`.

Current text: "`RegexProgram` calls regex engine; `SimdScan` has scanner kind, needle/class, fallback."

Proposed text: "`RegexProgram` is the semantic verifier for regex literals. `SimdScan` declares `mode = Exact | Prefilter`, emits candidate offsets for prefilter mode, and must route acceptance to `RegexProgram` or scalar reference before tape emission."

Rationale: [S5] meta regex and [S6] scan APIs separate fast matching components from match reporting semantics.

Refinement 7.

Target: `restart/ARCHITECTURE.md:949`.

Current text: "`SimdScan` | Long literal set or regex prefilter. | Mined by recognizer pass."

Proposed text: "`SimdScan` | Long literal set, exact structural alphabet, or regex prefilter. | Mined by recognizer pass; exact scans require scalar parity, prefilters require verifier acceptance."

Rationale: [S3] structural index work differs from [S5] regex prefilter work.

Refinement 8.

Target: `restart/ARCHITECTURE.md:1022`.

Current text: "`BBNF-SIMD-NOT-SELECTED` ... dispatch cost outweighs the win."

Proposed text: "`BBNF-SIMD-NOT-SELECTED` ... cost, unsupported Unicode semantics, or missing exact/prefilter verifier contract rejected the SIMD path."

Rationale: [S5] and [S4] make Unicode and engine capability part of selection, not only dispatch cost.

Refinement 9.

Target: `restart/audit/pass-2-codegen/PASS-2.md:104`.

Current text: "`scanner_lowering` — slice-compare + regex + `simd-scan` parity."

Proposed text: "`scanner_lowering` — slice-compare + regex + `simd-scan` parity, including SIMD false-positive discard, no false-negative proof, and scalar offset-vector equality for exact scans."

Rationale: `docs/precepts/instructions/LESSONS-LEARNED.md:74-80` requires producer and consumer gates; [S5] requires exact engine composition.

Refinement 10.

Target: `restart/audit/pass-2-codegen/PASS-2.md:170`.

Current text: "SIMD selected when structural byte alphabet is non-empty ... rejected when alphabet is Unicode-semantic, tiny, or scanner setup cost wins."

Proposed text: "SIMD selected when structural byte alphabet is non-empty, the scan mode is exact with scalar parity or prefilter with verifier route, and cost beats scalar; rejected when Unicode semantics, tiny inputs, setup cost, or missing verifier route wins."

Rationale: [S3] supports exact structural scanning; [S5] supports prefilter plus verifier routing.

Refinement 11.

Target: `restart/audit/pass-2-codegen/PASS-2.md:546`.

Current text: "falling back to scalar because SIMD cost evidence did not win; metadata may disable unsupported kernels but cannot force SIMD."

Proposed text: "falling back to scalar or regex verifier-first because SIMD cost or exactness evidence did not win; metadata may disable unsupported kernels but cannot force SIMD."

Rationale: [S5] says engine choice can shift dynamically; Lock 10 forbids user forcing at `restart/locks/LOCKS.md:52`.

Refinement 12.

Target: `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:30`.

Current text: "SIMD scanner returns offsets; Rust/WASM lowerers map those offsets into Tape leaf nodes and `sib_skip` links."

Proposed text: "SIMD scanner returns exact offsets or candidate offsets; exact offsets may emit tape after scalar parity tests, while candidate offsets must pass `RegexProgram` or scalar verifier before tape leaf nodes and `sib_skip` links are emitted."

Rationale: [S3] supports exact structural offset indexing; [S5] supports candidate fast path followed by exact engine.

## §6 — Adversarial findings

Adversarial finding 1: SIMD positive versus DFA negative is under-specified.

Contradicted or weakened lock: Lock 1's single substrate and Lock 10's auto-detected SIMD remain settled, but the acceptance contract is too weak.

Restart evidence: `restart/ARCHITECTURE.md:921` names fallback and scalar reference, while `restart/audit/pass-2-codegen/PASS-2.md:104` asks for scanner parity.

SOTA evidence: [S5] composes fast engines with slower exact engines for captures and capability gaps; [S6] reports matches through a scan API after compiled-engine execution.

Pressure point: If SIMD candidate offsets are allowed to emit tape nodes directly, a false positive becomes a parsed token.

Proposed amendment: Add `SimdScanMode = Exact | Prefilter` and require verifier acceptance before tape emission in prefilter mode.

Receiving phase: Phase 2 fold into Architecture §7.2 and PASS-2 scanner gates.

Adversarial finding 2: Full DFA codegen cannot be mandatory for rich Unicode regex.

Contradicted or weakened lock: The dedicated regex crate survives, but "NFA-to-DFA construction + DFA codegen" is too strong if it means every regex.

Restart evidence: `restart/README.md:135-140` claims Unicode properties, set algebra, graphemes, and normalization; `restart/audit/pass-2-codegen/PASS-2.md:65` names `RegexDfa`.

SOTA evidence: [S5] warns full DFA construction can be exponential and large Unicode classes are costly; [S4] supports large character sets via derivatives but still requires canonicalization and state policy.

Pressure point: A CSS or future language identifier regex could demand huge Unicode tables or state sets.

Proposed amendment: Rename or document `RegexDfa` as a `RegexProgram` execution plan, with VM/lazy-DFA/full-DFA selection and size-limit diagnostics.

Receiving phase: Phase 2 fold into Architecture §7.2, PASS-2 BIR naming, and parse-that crate tree.

Adversarial finding 3: Bespoke regex risks reimplementing `regex-automata` without a clear delta.

Contradicted or weakened lock: Lock 11 path-deps for `parse-that` and `bbnf-regex` do not prove bespoke implementation value.

Restart evidence: `restart/README.md:365` explicitly names `regex-automata` as the DFA/NFA/hybrid source absorbed into `parse-that/regex`.

SOTA evidence: [S5] already exposes engines, prefilters, captures through meta composition, size limits, and source boundaries.

Pressure point: A from-scratch regex engine can lose years to correctness, Unicode, and performance parity before bbnf receives any grammar-specific benefit.

Proposed amendment: Require a `regex-automata` oracle lane for `parse-that/regex` until bespoke code proves grammar-HIR deltas and parity.

Receiving phase: Phase 2 fold into PASS-2 and MASTER-PLAN gates.

Adversarial finding 4: "SIMD-first everywhere" can train implementers into over-selection.

Contradicted or weakened lock: Lock 10 is sound, but README heading language is too broad.

Restart evidence: `restart/README.md:338-340` says SIMD is first-class everywhere; `restart/audit/pass-2-codegen/PASS-2.md:170` correctly rejects tiny and Unicode-semantic cases.

SOTA evidence: [S3] says stage 1 can do fixed work that is inefficient on some inputs; [S5] says lazy DFA speed depends on prefilter choice and pattern/haystack behavior.

Pressure point: If "first-class" becomes "first chosen," small leaf rules and Unicode regexes pay overhead or become incorrect.

Proposed amendment: State that SIMD is available across targets and selected only when exactness and cost win.

Receiving phase: Phase 2 fold into README §9 and Architecture diagnostics.

Adversarial finding 5: Hyperscan/Vectorscan multi-pattern expectations may be a false friend.

Contradicted or weakened lock: No lock is contradicted; this is a scope pressure point against over-borrowing.

Restart evidence: `restart/locks/LOCKS.md:60` demands grammar-neutral generic crates; `restart/ARCHITECTURE.md:720` keeps recognizers automatic.

SOTA evidence: [S6] and [S7] optimize large regex sets, streaming state, vectored input, and scratch/database APIs.

Pressure point: bbnf needs grammar-local recognizer facts and tape offsets; a DPI-style database API may add machinery with no seed-grammar payoff.

Proposed amendment: Cite Hyperscan/Vectorscan only for contract and SIMD matrix lessons unless a future benchmark proves multi-pattern database value.

Receiving phase: Phase 2 fold into research footnotes and MASTER-PLAN perf gates.

Adversarial finding count: 5.

## §7 — Surgery proposals

Surgery 1.

Target: `restart/ARCHITECTURE.md:920-921`.

Directive: Replace the two `RegexProgram` / `SimdScan` rows with rows that name `RegexProgram` as semantic verifier and `SimdScan` as `Exact | Prefilter`.

Acceptance gate: `rg -n "SimdScanMode|Exact \\| Prefilter|candidate offsets|verifier" restart/ARCHITECTURE.md restart/audit/pass-2-codegen/PASS-2.md` finds the contract in both architecture and PASS-2.

Dependency: §6 adversarial finding 1.

Surgery 2.

Target: `restart/audit/pass-2-codegen/PASS-2.md:104-105`.

Directive: Add scanner lowering tests for SIMD false-positive discard, no false-negative proof, scalar offset-vector parity, and verifier-before-tape emission.

Acceptance gate: PASS-2 test table names producer output and consumer acceptance, matching `docs/precepts/instructions/LESSONS-LEARNED.md:74-80`.

Dependency: §5 refinements 9 and 10 plus §6 finding 1.

Surgery 3.

Target: `restart/README.md:62`.

Directive: Add a sentence that `parse-that/regex` is bespoke only where grammar-owned HIR, Unicode class algebra, BIR payloads, generated verifier contracts, and tape offsets require it; otherwise `regex-automata` remains the oracle.

Acceptance gate: `rg -n "regex-automata.*oracle|grammar-owned HIR|verifier contract" restart/README.md restart/MASTER-PLAN.md restart/audit/pass-2-codegen/PASS-2.md` returns at least one owner row.

Dependency: §6 finding 3.

Surgery 4.

Target: `restart/ARCHITECTURE.md:535-540`.

Directive: Expand the `parse-that/src/` tree to include `regex/hir`, `regex/nfa`, `regex/dfa`, `regex/vm`, and `regex/prefilter`.

Acceptance gate: The tree still obeys Lock 13's 4-to-10 child rule at `restart/locks/LOCKS.md:58`.

Dependency: §5 refinement 4 and §6 finding 2.

Surgery 5.

Target: `restart/audit/pass-2-codegen/PASS-2.md:65`.

Directive: Rename the BIR variant display from `RegexDfa` to `RegexProgram`, or add a note that `RegexDfa` is a plan under `RegexProgram`, not the full regex execution universe.

Acceptance gate: `rg -n "RegexDfa" restart/audit/pass-2-codegen/PASS-2.md restart/ARCHITECTURE.md` returns only compatibility notes or no hits.

Dependency: §6 finding 2.

Surgery 6.

Target: `restart/README.md:338-340`.

Directive: Change the heading or first sentence to "SIMD is first-class across supported targets and selected only when exactness and cost win."

Acceptance gate: README keeps NEON, AVX2, AVX-512, WASM-SIMD, and scalar fallback, and also names verifier fallback.

Dependency: §6 finding 4.

Surgery 7.

Target: `restart/ARCHITECTURE.md:1022`.

Directive: Expand `BBNF-SIMD-NOT-SELECTED` to mention cost, unsupported Unicode semantics, missing verifier route, and scalar fallback.

Acceptance gate: Diagnostic language does not mention `@simd` as a force control, honoring `restart/audit/hardening/HARDENING-SYNTHESIS-V5.1.md:136`.

Dependency: §5 refinement 8 and §6 finding 4.

Surgery 8.

Target: `restart/audit/pass-2-codegen/agent-5-pratt-simd-auto-detection.md:80-83`.

Directive: Add a risk row for "SIMD prefilter emits tape without verifier" with mitigation "candidate offsets must pass `RegexProgram` or scalar verifier."

Acceptance gate: Risk table distinguishes false positive from false negative, and false negative is classified as correctness failure.

Dependency: §6 finding 1.

Surgery 9.

Target: `restart/MASTER-PLAN.md` SOTA/perf gate rows.

Directive: Add a Phase 2 receiver row for `parse-that/regex` oracle parity against `regex-automata` on seed regex fixtures, including Unicode class algebra, captures if supported, no-capture DFA, and prefilter candidate tests.

Acceptance gate: The row names owner, blocker, fixture set, and evidence artefact; it does not demand a new grammar surface.

Dependency: §6 finding 3.

Surgery 10.

Target: `restart/ARCHITECTURE.md:1233`.

Directive: Amend the SIMD lowerer contract to say it consumes recognizer facts and `SimdScan` BIR only after exactness mode and verifier route have been validated.

Acceptance gate: Lowerer contract includes no Grammar IR import and no verifier-less prefilter path.

Dependency: §5 refinements 6 and 7.

Surgery 11.

Target: `restart/audit/pass-2-codegen/PASS-2.md:546`.

Directive: Replace the SIMD diagnostic string with wording that says metadata may disable unsupported kernels, cannot force SIMD, and verifier-first fallback remains exact.

Acceptance gate: `rg -n "@simd hint may force|force SIMD" restart/audit/pass-2-codegen/PASS-2.md` returns zero outside rejection context.

Dependency: §5 refinement 11.

Surgery 12.

Target: `restart/README.md:365`.

Directive: Expand the `regex-automata` SOTA row to say "oracle and design reference for DFA/NFA/hybrid composition; parse-that owns grammar HIR and generated verifier integration."

Acceptance gate: The row no longer reads as if `parse-that/regex` is a blind replacement of the Rust regex ecosystem.

Dependency: §6 finding 3.

Closing answer to the engagement question:

1. Bespoke `parse-that/regex` buys bbnf-owned HIR, Unicode routing below grammar syntax, BIR-visible execution plans, committed generated verifier artefacts, and tape-offset ownership.

2. It does not buy mature generic regex algorithms over `regex-automata`; those must be treated as oracle, reference, or dependency until parity is proven.

3. When SIMD matches and DFA does not, the SIMD result is either an allowed false positive from a prefilter or a bug in an exact scanner.

4. In prefilter mode, the candidate is discarded and no tape event is emitted unless `RegexProgram`, VM, DFA, or scalar verifier accepts it.

5. In exact mode, SIMD and scalar offset vectors must match; DFA disagreement is a test failure unless the DFA was never the semantic verifier for that scan.

6. The SIMD-first posture survives as a cross-target availability and cost-selection posture.

7. It does not survive as an always-on ordering rule.

8. The fold should therefore rename the posture in prose: SIMD is first-class, verifier-bound, and cost-selected.
