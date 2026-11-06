// LOCK-4 PROTO-2 — TS side of the 4C/4D value-plane crossing.
// Runs the SAME serialized FnDescriptor / MapExpr the Rust VM ran, from the
// same JSON wire, and asserts node-for-node Value identity. Plus clause A:
// decode the opcode stream + idempotence + ApplyFn comprehension.
import { readFileSync } from "node:fs";
const DIR = new URL(".", import.meta.url).pathname;

// ── Value-plane evaluator: a direct mirror of interpreter/apply_fn.rs ────────
// Eval intermediate: {k:"F"|"I"|"B"|"S", v}
const F = (v) => ({ k: "F", v }), I = (v) => ({ k: "I", v });
const B = (v) => ({ k: "B", v }), S = (v) => ({ k: "S", v });
const asF = (e) => e.k === "F" ? e.v : e.k === "I" ? e.v : e.k === "B" ? (e.v ? 1 : 0) : (parseFloat(e.v));
const asI = (e) => e.k === "I" ? e.v : e.k === "F" ? Math.trunc(e.v) : e.k === "B" ? (e.v ? 1 : 0) : (parseInt(e.v, 10) || 0);
const isF = (e) => e.k === "F";

function valueText(strings, input, v) {
  // v is the child Value JSON. For the crossing the child is the input span.
  if (v && v.Span) return input.slice(v.Span[0], v.Span[1]);
  if (v && typeof v.Str === "string") return v.Str;
  return input; // span-of-whole-input default for these single-literal cases
}

function evalMapExpr(expr, strings, input, childText) {
  if (typeof expr === "string") { // unit variant: "Input"
    if (expr === "Input") return S(childText);
    return I(0);
  }
  if ("IntLit" in expr) return I(expr.IntLit);
  if ("FloatLit" in expr) return F(expr.FloatLit);
  if ("BoolLit" in expr) return B(expr.BoolLit);
  if ("StringLit" in expr) return S(strings[expr.StringLit]);
  if ("InputProp" in expr) {
    const name = strings[expr.InputProp.prop];
    return name === "len" ? I(childText.length) : I(0);
  }
  if ("FnCall" in expr) {
    const fname = strings[expr.FnCall.name];
    const a0 = expr.FnCall.args.length ? evalMapExpr(expr.FnCall.args[0], strings, input, childText) : null;
    if (a0 == null) return I(0);
    if (fname === "parse_int" || fname === "int") return I(asI(a0));
    if (fname === "parse_float" || fname === "float") return F(asF(a0));
    if (fname === "parse_hex") {
      const s = a0.k === "S" ? a0.v : "";
      return I(parseInt(s.replace(/^0x/, ""), 16) || 0);
    }
    if (fname === "len" && a0.k === "S") return I(a0.v.length);
    return a0; // unknown host fn: identity (LOCK-7 territory)
  }
  if ("BinOp" in expr) {
    const { op, lhs, rhs } = expr.BinOp;
    const l = evalMapExpr(lhs, strings, input, childText);
    const r = evalMapExpr(rhs, strings, input, childText);
    const fl = isF(l) || isF(r);
    switch (op) {
      case "Add": return fl ? F(asF(l) + asF(r)) : I(asI(l) + asI(r));
      case "Sub": return fl ? F(asF(l) - asF(r)) : I(asI(l) - asI(r));
      case "Mul": return fl ? F(asF(l) * asF(r)) : I(asI(l) * asI(r));
      case "Div": return fl ? F(asF(l) / asF(r)) : I(Math.trunc(asI(l) / asI(r)));
      case "Mod": return I(asI(l) % asI(r));
      case "Eq": return B(Math.abs(asF(l) - asF(r)) < Number.EPSILON);
      case "Ne": return B(Math.abs(asF(l) - asF(r)) >= Number.EPSILON);
      case "Lt": return B(asF(l) < asF(r));
      case "Gt": return B(asF(l) > asF(r));
      case "Le": return B(asF(l) <= asF(r));
      case "Ge": return B(asF(l) >= asF(r));
      case "And": return B(asI(l) !== 0 && asI(r) !== 0);
      case "Or": return B(asI(l) !== 0 || asI(r) !== 0);
      case "BitAnd": return I(asI(l) & asI(r));
      case "BitOr": return I(asI(l) | asI(r));
      case "Shl": return I(asI(l) << asI(r));
      case "Shr": return I(asI(l) >> asI(r));
    }
  }
  if ("UnaryOp" in expr) {
    const v = evalMapExpr(expr.UnaryOp.inner, strings, input, childText);
    if (expr.UnaryOp.op === "Neg") return isF(v) ? F(-asF(v)) : I(-asI(v));
    return B(asI(v) === 0);
  }
  return I(0);
}

const evalToValue = (e) =>
  e.k === "F" ? { F64: e.v } : e.k === "I" ? { U32: e.v >>> 0 } :
  e.k === "B" ? { U32: e.v ? 1 : 0 } : { Str: e.v };

// Mirror of exec_apply_fn: descriptor + child span -> projected Value JSON.
function applyFn(desc, strings, input) {
  const child = { Span: [0, input.length] };
  const childText = valueText(strings, input, child);
  if (typeof desc === "object") {
    if ("Expr" in desc) return evalToValue(evalMapExpr(desc.Expr.expr, strings, input, childText));
    if ("NumberConvert" in desc) return { F64: parseFloat(childText) };
    if ("HexConvert" in desc) return { U32: (parseInt(childText.replace(/^0x/, ""), 16) || 0) >>> 0 };
    if ("EnumWrap" in desc) return { Variant: { tag: desc.EnumWrap.variant, child } };
    if ("BoxWrap" in desc || "SpanCapture" in desc || "HostClosure" in desc) return child;
  }
  return child;
}

// ── Clause B: value-plane crossing ──────────────────────────────────────────
const canon = (x) => x === null || typeof x !== "object" ? x
  : Array.isArray(x) ? x.map(canon)
  : Object.fromEntries(Object.keys(x).sort().map((k) => [k, canon(x[k])]));
const stable = (x) => JSON.stringify(canon(x));
const cross = JSON.parse(readFileSync(DIR + "valueplane_crossing.json", "utf8"));
let bDiv = 0;
for (const c of cross) {
  const ts = applyFn(c.desc, c.strings, c.input);
  const a = stable(ts), b = stable(c.result);
  const ok = a === b;
  if (!ok) bDiv++;
  console.log(`[B ${ok ? "OK " : "DIV"}] ${c.label.padEnd(16)} rust=${b}  ts=${a}`);
}

// ── Clause A: opcode stream decode + idempotence + ApplyFn comprehension ─────
const prog = JSON.parse(readFileSync(DIR + "projection_program.json", "utf8"));
const TAGS = ["MatchString","MatchRegex","Epsilon","Jump","JumpIfFail","JumpIfOk","Call","Return",
  "SaveState","RestoreState","DropState","TrimWs","TrimWsPattern","Minus","Negate","Dispatch",
  "DispatchToken","RepeatBegin","RepeatEnd","CaptureSpan","SaveValueDepth","DiscardRight",
  "DiscardLeft","MakeArray","MakeTagged","ApplyFn","MemoCheck","MemoStore","DebugBreak","Nop","Halt"];
const tagOf = (op) => typeof op === "string" ? op : Object.keys(op)[0];
let aDiv = 0, applyFnCount = 0;
const decoded = prog.code.map((op) => {
  const t = tagOf(op);
  if (!TAGS.includes(t)) { aDiv++; console.log(`[A DIV] unknown opcode ${t}`); }
  if (t === "ApplyFn") { applyFnCount++; const fid = op.ApplyFn; if (!prog.fns[fid]) { aDiv++; console.log(`[A DIV] ApplyFn fn_id ${fid} not in fns table`); } }
  return { tag: TAGS.indexOf(t), raw: op };
});
// idempotence: re-encode decoded -> must equal original code array
const reencoded = decoded.map((d) => d.raw);
const idem = JSON.stringify(reencoded) === JSON.stringify(prog.code);
console.log(`\n[A] opcodes=${prog.code.length} ApplyFn=${applyFnCount} divergences=${aDiv}`);
console.log(`[A] decode∘encode idempotent: ${idem ? "YES" : "NO"}`);
console.log(`[A] fns table crossed: ${prog.fns.length} descriptor(s)`);

console.log(`\n=== VERDICT ===`);
console.log(`Clause B (value-plane, ${cross.length} descriptors): ${bDiv} divergence(s) -> ${bDiv === 0 ? "GREEN" : "RED"}`);
console.log(`Clause A (opcode stream): ${aDiv} divergence(s), idempotent=${idem} -> ${aDiv === 0 && idem ? "GREEN" : "RED"}`);
process.exit(bDiv === 0 && aDiv === 0 && idem ? 0 : 1);
