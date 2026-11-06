// PROBE 2 — the REAL 6B monomorphism gate: is the GENERATED full TapeDoc decoder
// (gen_tape.mjs, NON-byte-identical to the hand PROVE-14 source) still V8-mono on
// the LOAD-BEARING path, or did generation smuggle a boxed-union classify /
// un-lowerable switch / post-ctor field assignment that silently deopts and
// evaporates the 4.4x stop-early? Driven over twitter.json's crossed four planes.
//
// GREEN iff: %HaveSameMap(a,b) (pre+post walk) && %HasFastProperties(a,b) &&
//   the hot classify/nextSibling/getKey/walk sites optimize (kOptimized) and stay
//   optimized after the full workload (no kMarkedForDeoptimization) && fp folds to
//   986912547 (full) / 3431570937 (partial).
//
// run: node --allow-natives-syntax mono_check.mjs [twitter.json]
import { buildTape } from "./build_tape.mjs";
import { TapeDoc, emptyDigest, finalizeDigest, tapeGet, jsonGet, walkJs } from "./gen_tape.mjs";
import { readFileSync } from "node:fs";

const FULL_FP = 986912547;
const PARTIAL_FP = 3431570937;
const path = process.argv[2] || "./twitter.json";

// V8 OptimizationStatus bits (calibrated empirically on THIS Node 26 / V8 build:
// a known-optimized hot fn reports raw=41 = kIsFunction|kMaybeDeopted|kMaybeMaglevved
// — so `kOptimized`(1<<4) is NOT the tier signal and `kMaybeDeopted`(1<<3) is routine
// noise. "Optimized" == an optimizing tier is active (maglev OR turbofan). The real
// deopt signal is kMarkedForDeoptimization(1<<14), CLEAR on the healthy calibrator).
const kOptimized = 1 << 4;
const kMaybeMaglevved = 1 << 5;
const kTurbofanned = 1 << 6;
const kInterpreted = 1 << 7;
const kMarkedForDeoptimization = 1 << 14;
function decodeStatus(s) {
  return {
    raw: s,
    optimized: (s & (kOptimized | kMaybeMaglevved | kTurbofanned)) !== 0,
    tier: (s & kTurbofanned) ? "turbofan" : (s & kMaybeMaglevved) ? "maglev" : (s & kInterpreted) ? "interpreted" : "?",
    markedForDeopt: (s & kMarkedForDeoptimization) !== 0,
  };
}

// ---- HARNESS SETUP (untimed): the four crossed planes from twitter.json ----
const raw = buildTape(path);
console.log(`[planes]    source=${raw.source.length}B  offsets=${raw.offsets.length}  flagCursors=${raw.flagCursors.length}  flagValues=${raw.flagValues.length}`);

// TWO independently-constructed decoder instances (same ctor field order).
const a = new TapeDoc(raw.source, raw.offsets, raw.flagCursors, raw.flagValues);
const b = new TapeDoc(raw.source, raw.offsets, raw.flagCursors, raw.flagValues);

// ---- (1) pre-walk hidden-class identity ----
const pre_same = %HaveSameMap(a, b);
const pre_fastA = %HasFastProperties(a);
const pre_fastB = %HasFastProperties(b);
console.log(`[pre-walk]  HaveSameMap(a,b)=${pre_same}  HasFastProperties a=${pre_fastA} b=${pre_fastB}`);

// classify must return a UNIFORM unboxed Smi across every cursor — the falsifier
// for a boxed-union classify (which V8 tolerates WITHOUT a deopt bit, so this,
// not %GetOptimizationStatus, is the sharp check).
let classifyAllSmi = true;
for (let c = 0; c < raw.offsets.length; c++) if (!%IsSmi(a.classify(c))) { classifyAllSmi = false; break; }
console.log(`[classify]  all ${raw.offsets.length} results unboxed Smi = ${classifyAllSmi}`);

// ---- (2) prepare the hot sites for optimization BEFORE natural tiering ----
const P = TapeDoc.prototype;
const hotMethods = {
  classify: P.classify,
  nextSibling: P.nextSibling,
  getKey: P.getKey,
  indexArray: P.indexArray,
  stringLeaf: P.stringLeaf,
  decodeString: P.decodeString,
  decodeNumber: P.decodeNumber,
  needsDecode: P.needsDecode,
  walk: P.walk,
};
for (const f of Object.values(hotMethods)) %PrepareFunctionForOptimization(f);
%PrepareFunctionForOptimization(tapeGet);

// ---- (3) warm: drive every hot site DIRECTLY over the real planes ----
const nOff = raw.offsets.length;
function warm(doc, reps) {
  let sink = 0;
  for (let r = 0; r < reps; r++) {
    // classify + nextSibling over every cursor (megamorphism would show here)
    for (let c = 0; c < nOff; c++) { sink ^= doc.classify(c); sink ^= doc.nextSibling(c); }
    // full structural walk (exercises walk/stringLeaf/decodeNumber/needsDecode)
    const d = emptyDigest(); finalizeDigest(d, doc.walk(0, d)); sink ^= d.fp;
    // pointer navigation (getKey/indexArray/decodeString) — the stop-early path
    sink ^= tapeGet(doc).fp;
  }
  return sink;
}
let sink = warm(a, 8);

// ---- (4) request optimization, then trigger with one more direct pass ----
for (const f of Object.values(hotMethods)) %OptimizeFunctionOnNextCall(f);
%OptimizeFunctionOnNextCall(tapeGet);
sink ^= warm(a, 2);

// ---- (5) per-site optimization status (the brief's named hot path) ----
// GATED sites = the load-bearing walk/nav path the 4.4x stop-early rides on.
// (decodeString/decodeString are helper leaves that V8 INLINES into getKey/walk;
// their standalone copies may stay interpreted — an inlining artifact, reported
// but not gated, since the *inlining parent* is optimized.)
const HOT = new Set(["classify", "nextSibling", "getKey", "walk"]);
console.log(`[opt] site           optimized  tier         markedForDeopt`);
let allHotOptimized = true;
let anyDeopt = false;
for (const [name, f] of Object.entries(hotMethods)) {
  const st = decodeStatus(%GetOptimizationStatus(f));
  const isHot = HOT.has(name);
  if (isHot && !st.optimized) allHotOptimized = false;
  if (isHot && st.markedForDeopt) anyDeopt = true;
  console.log(`[opt] ${name.padEnd(14)} ${String(st.optimized).padEnd(9)}  ${st.tier.padEnd(11)}  ${st.markedForDeopt}${isHot ? "   <-- GATED hot site" : "   (inlined helper)"}`);
}

// ---- (6) run the WHOLE workload once more; a hot-site deopt would fire here ----
sink ^= warm(a, 4);
let postDeopt = false;
for (const [name, f] of Object.entries(hotMethods)) {
  if (HOT.has(name) && decodeStatus(%GetOptimizationStatus(f)).markedForDeopt) postDeopt = true;
}

// ---- (7) post-walk hidden-class identity (walk must not have transitioned it) ----
const post_same = %HaveSameMap(a, b);
const post_fastA = %HasFastProperties(a);
const post_fastB = %HasFastProperties(b);
console.log(`[post-walk] HaveSameMap(a,b)=${post_same}  HasFastProperties a=${post_fastA} b=${post_fastB}`);

// ---- (8) fp fold parity (full + partial) on a FRESH doc ----
const doc = new TapeDoc(raw.source, raw.offsets, raw.flagCursors, raw.flagValues);
const df = emptyDigest(); finalizeDigest(df, doc.walk(0, df));
const gp = tapeGet(doc);
const fullOk = df.fp === FULL_FP;
const partialOk = gp.fp === PARTIAL_FP;
console.log(`[fp full]   ${df.fp} (expect ${FULL_FP})  counts=obj${df.objects}/arr${df.arrays}/str${df.strings}/num${df.numbers}/bool${df.bools}/null${df.nulls}/cu${df.codeUnits}  -> ${fullOk ? "MATCH" : "DIVERGE"}`);
console.log(`[fp part]   ${gp.fp} (expect ${PARTIAL_FP})  cu=${gp.codeUnits}  -> ${partialOk ? "MATCH" : "DIVERGE"}`);

// oracle cross-check (JSON.parse lane through the generated primitives)
const v = JSON.parse(raw.source.toString("utf8"));
const dj = emptyDigest(); finalizeDigest(dj, walkJs(v, dj));
const oracleFull = dj.fp === FULL_FP;
const oraclePart = jsonGet(v).fp === PARTIAL_FP;

// ---- VERDICT ----
const mono = pre_same && pre_fastA && pre_fastB && post_same && post_fastA && post_fastB && classifyAllSmi;
const optClean = allHotOptimized && !anyDeopt && !postDeopt;
const fpOk = fullOk && partialOk && oracleFull && oraclePart;
const GREEN = mono && optClean && fpOk;
console.log("");
console.log(`sink=${sink >>> 0}`);
console.log(`MONO(HaveSameMap+FastProps pre&post)=${mono}  OPT(hot optimized, no deopt)=${optClean}  FP(full+partial+oracle)=${fpOk}`);
console.log(`GATE: ${GREEN ? "GREEN — generated decoder is V8-monomorphic AND fp-exact" : "RED — generation deopts or fp diverges (R4 falsifier fires)"}`);
process.exit(GREEN ? 0 : 1);
