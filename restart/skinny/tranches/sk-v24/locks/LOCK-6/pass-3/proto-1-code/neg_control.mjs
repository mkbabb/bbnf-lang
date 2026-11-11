// TEETH CHECK: prove the gate is NOT vacuous. Inject the two hazards the brief
// names — (1) post-ctor field assignment (hidden-class transition), (2) a
// boxed-union classify (returns objects, not small ints) — and show the SAME
// gate goes RED. If these pass, the GREEN on the real generated decoder would be
// meaningless. run: node --allow-natives-syntax neg_control.mjs [twitter.json]
import { buildTape } from "./build_tape.mjs";
import { TapeDoc } from "./gen_tape.mjs";
const path = process.argv[2] || "./twitter.json";
const raw = buildTape(path);

// ---- Hazard 1: post-ctor field assignment on ONE instance ----
const a = new TapeDoc(raw.source, raw.offsets, raw.flagCursors, raw.flagValues);
const b = new TapeDoc(raw.source, raw.offsets, raw.flagCursors, raw.flagValues);
const beforeSame = %HaveSameMap(a, b);
a.decodeCache = new Map(); // <-- the forbidden post-ctor field assignment
const afterSame = %HaveSameMap(a, b);
const fastA = %HasFastProperties(a);
console.log(`[hazard-1 post-ctor field] HaveSameMap before=${beforeSame} after=${afterSame}  HasFastProperties(a)=${fastA}`);
const h1_red = beforeSame === true && afterSame === false; // the gate would catch it

// ---- Hazard 2: boxed-union classify (heterogeneous return representation) ----
// The allocation-free walk depends on classify returning a UNIFORM Smi (small
// int). A generator that emitted a boxed union (object/array/string per arm)
// would heap-allocate per node and megamorphize consumers. V8/maglev tolerates
// this WITHOUT a hard deopt bit — so the sharp falsifier is return-value
// uniformity, not %GetOptimizationStatus. We check: across ALL cursors, does
// classify return only Smis?  (%IsSmi confirms the unboxed integer path.)
class BoxedDoc {
  constructor(source, offsets) { this.source = source; this.offsets = offsets; }
  classify(cur) {
    const bb = this.source[this.offsets[cur]];
    switch (bb) {
      case 0x7b: return { kind: "obj-open" };
      case 0x5b: return [3];
      case 0x22: return "string";
      default: return bb;
    }
  }
}
const bd = new BoxedDoc(raw.source, raw.offsets);
const gd = new TapeDoc(raw.source, raw.offsets, raw.flagCursors, raw.flagValues);
let genAllSmi = true, boxedAllSmi = true, samples = 0;
for (let c = 0; c < raw.offsets.length; c++) {
  if (!%IsSmi(gd.classify(c))) genAllSmi = false;
  if (!%IsSmi(bd.classify(c))) boxedAllSmi = false;
  samples++;
}
console.log(`[hazard-2 boxed classify] generated classify all-Smi=${genAllSmi}  boxed classify all-Smi=${boxedAllSmi}  (${samples} cursors)`);
const h2_red = genAllSmi === true && boxedAllSmi === false; // gate greens generated, reds boxed

console.log("");
console.log(`TEETH: hazard-1 caught=${h1_red}  hazard-2 caught=${h2_red}  -> ${h1_red && h2_red ? "GATE HAS TEETH (both hazards go RED; the generated decoder does not)" : "gate did NOT distinguish (would be vacuous)"}`);
process.exit(h1_red && h2_red ? 0 : 1);
