// R3 refutation gate: does the GENERATED TS decoder preserve the hand
// monomorphism? %HaveSameMap(a,b) && %HasFastProperties across two decoder
// instances AND after one walk(). If %HaveSameMap goes false, generation lost
// the single hidden class the 4.4x stop-early depends on -> REFUTES the crown.
import { TapeDoc, emptyDigest, finalizeDigest } from "./gen_tape.mjs";

// synthetic SoA tape for {"a":[1,true,null,"x"]} (separators carry no offset).
const src = Buffer.from('{"a":[1,true,null,"x"]}', "utf8");
const offsets = new Uint32Array([0, 1, 5, 6, 8, 13, 18, 21, 22]);
const flagCursors = new Uint32Array([]);
const flagValues = new Uint8Array([]);

// TWO independently-constructed decoder instances (same field order in ctor).
const a = new TapeDoc(src, offsets, flagCursors, flagValues);
const b = new TapeDoc(src, offsets, flagCursors, flagValues);

const chk1_same = %HaveSameMap(a, b);
const chk1_fastA = %HasFastProperties(a);
const chk1_fastB = %HasFastProperties(b);
console.log(`[pre-walk]  HaveSameMap(a,b)=${chk1_same}  HasFastProperties(a)=${chk1_fastA}  (b)=${chk1_fastB}`);

// exercise classify + the allocation-free walk on `a`.
const d = emptyDigest();
finalizeDigest(d, a.walk(0, d));
console.log(`[walk fp]   ${d.fp >>> 0}  counts=obj${d.objects}/arr${d.arrays}/str${d.strings}/num${d.numbers}/bool${d.bools}/null${d.nulls}`);

const chk2_same = %HaveSameMap(a, b);
const chk2_fastA = %HasFastProperties(a);
const chk2_fastB = %HasFastProperties(b);
console.log(`[post-walk] HaveSameMap(a,b)=${chk2_same}  HasFastProperties(a)=${chk2_fastA}  (b)=${chk2_fastB}`);

const pass = chk1_same && chk1_fastA && chk1_fastB && chk2_same && chk2_fastA && chk2_fastB;
console.log(`MONO-GATE: ${pass ? "PASS (single hidden class, fast props, preserved across walk)" : "FAIL (hidden-class transition -> REFUTES crown R3)"}`);
process.exit(pass ? 0 : 1);
