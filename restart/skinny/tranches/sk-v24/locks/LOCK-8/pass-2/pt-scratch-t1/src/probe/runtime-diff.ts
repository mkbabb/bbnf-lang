// Runtime drop-undefined differential (gate 8B-#5): proves all() drops undefined
// VALUES at runtime, so the DropUndefined<Values<Ps>> TYPE (which keeps positions)
// is a DOCUMENTED WIDENING when an opt() arm is absent.
import { all, string, regex } from "../parse/index.js";

const a = string("a");
const b = regex(/b/);
const c = string("c");
const p = all(a, b.opt(), c); // TYPE: [string, string, string]  (undefined dropped from arm)

const present = p.parse("abc"); // b present
const absent  = p.parse("ac");  // b absent

console.log("opt PRESENT  -> value =", JSON.stringify(present), "len =", present.length);
console.log("opt ABSENT   -> value =", JSON.stringify(absent),  "len =", absent.length);
console.log("WIDENING?    ->", absent.length !== present.length
  ? `YES: runtime length ${absent.length} != type-claimed 3 (position shifts) -> documented widening confirmed`
  : "NO: lengths equal");
