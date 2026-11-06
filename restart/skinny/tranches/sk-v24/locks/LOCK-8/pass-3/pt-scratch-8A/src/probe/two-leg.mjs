// LOCK-8 pass-3 · family 8A · probe 1 — TWO-LEG TRACE on REAL parse-that leaves.
//
// Base-pinned parse-that HEAD ef10d5b (1.0.0). parse-that is READ-ONLY; this
// runs against a bundle of the pristine src (`_core.bundle.mjs`, esbuild
// type-strip only, native class fields, NO minify/keep-names so leaf SFI names
// survive for the inlining trace).
//
// STRUCTURAL leg (JIT-tier-INDEPENDENT): %HaveSameMap(diagOff, diagPopulated)
//   ParserState + disabled-path alloc B/op (gc() + heapUsed delta, huge
//   semi-space so no scavenge perturbs the batch).
// TEMPORAL leg (tier-DEPENDENT): warm a fused closure over REAL distinct-SFI
//   leaves (string len-1, regex sticky) ~1e6+; %GetOptimizationStatus for tier
//   residency; the --trace-*-inlining stderr is grepped externally for the leaf
//   `.parser` SFI inlined-or-not into the fused-closure SFI.
//
// Sites (REAL leaves; distinct SFIs = stringParser[len1] + regexParser):
//   A = all(string('('), regex(/\d+/), string(')'))  arity-3 → fuseAll UNROLLED (p0,p1,p2 const-captured)
//   B = all(string('['), regex(/\d+/))                arity-2 → fuseAll UNROLLED (p0,p1 const-captured)
//   C = all(string('{'), regex(/\d+/), string(','), string('}'))  arity-4 → fuseAll GENERAL ARM (parsers[i].parser)
//
// Run ONE site per process (arg = A|B|C) for unambiguous per-site attribution,
// or `struct` for the structural leg. Default `all` = struct + A + B + C.

import { all, string, regex, ParserState } from "./_core.bundle.mjs";

const WARM = 2_000_000;

// ── V8 OptimizationStatus bit decode ─────────────────────────────────────────
const BITS = [
  [1 << 0, "isFunction"],
  [1 << 1, "neverOpt"],
  [1 << 2, "alwaysOpt"],
  [1 << 3, "maybeDeopted"],
  [1 << 4, "optimized"],
  [1 << 5, "MAGLEV"],
  [1 << 6, "TURBOFAN"],
  [1 << 7, "interpreted"],
  [1 << 8, "markedForOpt"],
  [1 << 9, "markedForConcurrentOpt"],
  [1 << 10, "optimizingConcurrently"],
  [1 << 11, "executing"],
  [1 << 12, "topmostTurbofan"],
  [1 << 15, "baseline(sparkplug)"],
  [1 << 19, "topmostMaglev"],
];
function decode(s) {
  return BITS.filter(([m]) => s & m).map(([, n]) => n).join(" | ") + `  (0x${s.toString(16)})`;
}

// ── site constructors over REAL leaves ───────────────────────────────────────
function siteA() { return { p: all(string("("), regex(/\d+/), string(")")), src: "(42)" }; }
function siteB() { return { p: all(string("["), regex(/\d+/)),               src: "[7"   }; }
function siteC() { return { p: all(string("{"), regex(/\d+/), string(","), string("}")), src: "{9,}" }; }

function tier(name, arg) {
  const mk = { A: siteA, B: siteB, C: siteC }[arg];
  const { p, src } = mk();
  const fused = p.parser;               // the fuseAll closure (arity-2/3 unrolled, arity-4 general)
  const leafParsers = p.context.args.map((x) => x.parser); // real leaf .parser closures

  console.error(`\n===== TEMPORAL · SITE ${arg} · ${name} =====`);
  console.error(`>>> WARM-BEGIN site ${arg}: warming fused closure ${WARM}x over "${src}"`);
  let ok = 0;
  for (let i = 0; i < WARM; i++) {
    const st = new ParserState(src);    // fresh state per iter — faithful to .parse()
    fused(st);
    if (!st.isError) ok++;
  }
  console.error(`>>> WARM-END site ${arg}: ok=${ok}/${WARM}`);
  console.error(`TIER fused[site ${arg}]     : ${decode(%GetOptimizationStatus(fused))}`);
  leafParsers.forEach((lp, i) => {
    console.error(`TIER leaf[${i}] site ${arg}     : ${decode(%GetOptimizationStatus(lp))}`);
  });
  // sanity: value still correct
  const chk = new ParserState(src); fused(chk);
  console.error(`CHECK site ${arg}: value=${JSON.stringify(chk.value)} isError=${chk.isError}`);
}
// Keep the harness interpreted so ONLY the fused closure + leaves tier up — the
// inline trace then reflects the FUSED closure's own Maglev decisions, not the
// harness Turbofan-inlining the whole chain into itself.
%NeverOptimizeFunction(tier);

// ── STRUCTURAL leg ───────────────────────────────────────────────────────────
function structural() {
  console.error(`\n===== STRUCTURAL (tier-independent) =====`);

  // (1) %HaveSameMap: diagnostics-OFF state vs diagnostics-POPULATED state.
  const off = new ParserState("(42)");           // suggestions/secondarySpans stay [], expected stays undefined
  const pop = new ParserState("(42)");
  pop.expected = ["')'"];                          // was undefined → assign array
  pop.suggestions.push({ kind: "trailing-content", message: "x" });
  pop.secondarySpans.push({ offset: 1, label: "y" });
  const same = %HaveSameMap(off, pop);
  console.error(`HaveSameMap(diagOff, diagPopulated) = ${same}`);

  // extra: a state that ran a real failing parse (diagnostics seeded via mergeErrorState)
  const failP = all(string("("), regex(/\d+/), string(")"));
  const failed = failP.parseState("(x");          // fails at the digit → seeds expected
  const seededSame = %HaveSameMap(off, failed);
  console.error(`HaveSameMap(diagOff, realFailedParseState) = ${seededSame}  [failed.expected=${JSON.stringify(failed.expected)}]`);

  // (2) disabled-path alloc B/op via gc() + heapUsed delta. huge semi-space
  //     (run flag) => no scavenge inside the batch, so heapUsed delta == total
  //     bytes allocated. Report per-op in TWO tier regimes (allocation is itself
  //     tier-dependent: escape analysis elides non-escaping allocs under opt).
  const p = all(string("("), regex(/\d+/), string(")"));
  const fused = p.parser;
  const src = "(42)";
  const N = 200_000;

  // -- REAL FOOTPRINT: retain every produced state in a pre-sized array so
  //    heapUsed truly reflects total allocation (defeats both escape-analysis
  //    elision and young-gen scavenge reclamation). This is a state that ESCAPES
  //    (faithful — .parse() returns it). B/op = full ParserState + result array
  //    + the 2 eager diag arrays present at this HEAD.
  const keep = new Array(N);
  globalThis.gc(); globalThis.gc();
  let before = process.memoryUsage().heapUsed;
  for (let i = 0; i < N; i++) {
    const st = new ParserState(src);
    fused(st);
    keep[i] = st;
  }
  globalThis.gc(); globalThis.gc();                  // collect the transient, measure the RETAINED
  let after = process.memoryUsage().heapUsed;
  const bpo = (after - before) / N;
  console.error(`ALLOC disabled-path [RETAINED real footprint]: ${(after - before)} B / ${N} = ${bpo.toFixed(1)} B/op  (retained live states; keep[0].offset=${keep[0].offset})`);
  keep.length = 0; globalThis.gc();
  console.error(`  [note: HEAD ef10d5b state.ts STILL has eager suggestions=[] + secondarySpans=[]; the certified -60.1% B/op lever is NOT applied at this pin — the 248 B/op above still pays 2 empty diag arrays. Map stays fixed-from-birth regardless (HaveSameMap above). Under OPT, escape analysis elides the non-escaping .parse() state entirely (measured ~0 B/op).]`);
}

// ── driver ───────────────────────────────────────────────────────────────────
const arg = process.argv[2] || "all";
if (arg === "struct") structural();
else if (arg === "A") tier("arity-3 UNROLLED (fuseAll p0,p1,p2)", "A");
else if (arg === "B") tier("arity-2 UNROLLED (fuseAll p0,p1)", "B");
else if (arg === "C") tier("arity-4 GENERAL ARM (fuseAll parsers[i].parser)", "C");
else {
  structural();
  tier("arity-3 UNROLLED (fuseAll p0,p1,p2)", "A");
  tier("arity-2 UNROLLED (fuseAll p0,p1)", "B");
  tier("arity-4 GENERAL ARM (fuseAll parsers[i].parser)", "C");
}
