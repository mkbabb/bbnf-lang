// v8-shapes.runner.mjs — LOCK-8 pass-1 probe 1 (8A closure-core-monomorphic thesis)
// RUN: node --allow-natives-syntax [--trace-turbo-inlining --trace-opt --trace-deopt] test/v8-shapes.runner.mjs
//
// Probes:
//   (1) mapState map-identity: parser.ts:177 Object.create(state) view vs a
//       fixed-shape {src,value,offset,isError,furthest} record. %HaveSameMap.
//   (2) inlining: arity-3 unrolled arm (constant-capture p0/p1/p2, 3 monomorphic
//       sites) vs general arm (parsers[i].parser, 1 megamorphic loop site).
//   (3) deopt sweep: steady-state jsonParser + synthetic CSS-value triple.
//
// The real combinators/ParserState are imported from the shipped 1.0.0 dist.
// The two fuseAll arms are byte-identical named replicas of leaf.ts:183-272 so
// that --trace-turbo-inlining lines carry greppable names (the bundled dist
// chunk is minified/anonymous). The child call-site shapes (p0.parser(state)
// vs parsers[i].parser(state)) are preserved exactly.

const DIST = "/Users/mkbabb/Programming/parse-that/typescript/dist/parse.js";
const { ParserState, string, all, regex, jsonParser } = await import(DIST);

const say = (...a) => console.log(...a);
const line = () => say("─".repeat(64));

// ────────────────────────────────────────────────────────────────
// PROBE 1 — mapState map-identity
// ────────────────────────────────────────────────────────────────
say("### PROBE 1 — mapState map-identity (Object.create vs fixed record)");
line();

// (1a) AS-SHIPPED grammar path: capture the real oldView objects that
// mapState (parser.ts:168-181) passes as fn's 2nd arg. Two fresh .parse()
// runs → two distinct ParserState prototypes → Object.create yields two
// distinct hidden classes.
const captured = [];
const gShipped = string("a").mapState((newState, oldState) => {
    captured.push(oldState); // oldState === Object.create(realState) view
    return newState;
});
gShipped.parse("a");
gShipped.parse("a");
const grammar_shipped_same = %HaveSameMap(captured[0], captured[1]);
say(`(1a) real .mapState() grammar, 2 runs:`);
say(`     captured ${captured.length} views; %HaveSameMap(v0,v1) = ${grammar_shipped_same}   [expect FALSE]`);

// (1b) MECHANISM apples-to-apples: shipped Object.create(state) view vs the
// proposed fix (one constructor, fixed field order). Uses the real ParserState.
class StateView {
    constructor(src, value, offset, isError, furthest) {
        this.src = src;
        this.value = value;
        this.offset = offset;
        this.isError = isError;
        this.furthest = furthest;
    }
}
// shipped mechanism (parser.ts:177-179)
function makeViewShipped(state, oldOffset, oldValue) {
    const oldView = Object.create(state);
    oldView.offset = oldOffset;
    oldView.value = oldValue;
    return oldView;
}
// proposed fix
function makeViewFixed(state, oldOffset, oldValue) {
    return new StateView(state.src, oldValue, oldOffset, state.isError, state.furthest);
}
const s1 = new ParserState("aaa", "x", 1, false, -1);
const s2 = new ParserState("bbb", "y", 2, false, -1);
const sh1 = makeViewShipped(s1, 0, "x");
const sh2 = makeViewShipped(s2, 0, "y");
const fx1 = makeViewFixed(s1, 0, "x");
const fx2 = makeViewFixed(s2, 0, "y");
const mech_shipped_same = %HaveSameMap(sh1, sh2);
const mech_fixed_same = %HaveSameMap(fx1, fx2);
say(`(1b) mechanism (real ParserState, 2 distinct source states):`);
say(`     shipped  Object.create(state):  %HaveSameMap = ${mech_shipped_same}   [expect FALSE]`);
say(`     fixed    new StateView(...):     %HaveSameMap = ${mech_fixed_same}   [expect TRUE]`);
// also confirm the view is a genuine distinct class from ParserState itself
say(`     shipped view vs ParserState:     %HaveSameMap = ${%HaveSameMap(sh1, s1)}   (view != base class)`);

const PROBE1_PASS =
    grammar_shipped_same === false &&
    mech_shipped_same === false &&
    mech_fixed_same === true;
say(`PROBE1: ${PROBE1_PASS ? "PASS" : "FAIL"}`);
line();

// ────────────────────────────────────────────────────────────────
// PROBE 2 — inlining (arity-3 unrolled vs general)
// ────────────────────────────────────────────────────────────────
say("### PROBE 2 — inlining child .parser (arity-3 constant-capture vs general index)");
line();

// byte-identical replica of leaf.ts:183-251 (arity-3 arm)
function fuseArity3(p0, p1, p2) {
    return function fusedArity3(state) {
        const savedOffset = state.offset;
        let w = 0;
        const out = [undefined, undefined, undefined];
        p0.parser(state);
        if (state.isError) { state.offset = savedOffset; state.isError = true; return state; }
        if (state.value !== undefined) out[w++] = state.value;
        p1.parser(state);
        if (state.isError) { state.offset = savedOffset; state.isError = true; return state; }
        if (state.value !== undefined) out[w++] = state.value;
        p2.parser(state);
        if (state.isError) { state.offset = savedOffset; state.isError = true; return state; }
        if (state.value !== undefined) out[w++] = state.value;
        if (w !== 3) out.length = w;
        return state.ok(out);
    };
}
// byte-identical replica of leaf.ts:255-272 (general arm)
function fuseGeneral(parsers) {
    const n = parsers.length;
    return function fusedGeneral(state) {
        const savedOffset = state.offset;
        const out = new Array(n);
        let w = 0;
        for (let i = 0; i < n; i++) {
            parsers[i].parser(state);
            if (state.isError) { state.offset = savedOffset; state.isError = true; return state; }
            if (state.value !== undefined) out[w++] = state.value;
        }
        if (w !== n) out.length = w;
        return state.ok(out);
    };
}

// real leaf parsers → distinct stringParser closures (a..e)
const a = string("a"), b = string("b"), c = string("c"), d = string("d"), e = string("e");
const f3 = fuseArity3(a, b, c);
const fG = fuseGeneral([a, b, c, d, e]);
const SRC = "abcde";

function runOnce(fn, src) {
    const st = new ParserState(src, undefined, 0, false, -1);
    fn(st);
    return st;
}

// small warm — enough to populate call-site feedback (megamorphic general site
// sees all 5 distinct closures in ONE call), few enough to avoid concurrent
// background auto-tier noise. Run under --no-maglev
// --no-optimize-on-next-call-optimizes-to-maglev so the explicit optimize is a
// single synchronous TurboFan compile (the only optimizer available) and
// --trace-turbo-inlining fires cleanly.
const WARM = 50;
%PrepareFunctionForOptimization(f3);
%PrepareFunctionForOptimization(fG);
for (let i = 0; i < WARM; i++) { runOnce(f3, SRC); runOnce(fG, SRC); }
say(">>> OPTIMIZE fusedArity3");
%OptimizeFunctionOnNextCall(f3);
runOnce(f3, SRC);
say(">>> OPTIMIZE fusedGeneral");
%OptimizeFunctionOnNextCall(fG);
runOnce(fG, SRC);
say(`arity-3 f3  raw-status: 0x${%GetOptimizationStatus(f3).toString(16)}`);
say(`general fG  raw-status: 0x${%GetOptimizationStatus(fG).toString(16)}`);
say(`(inlining: grep --trace-turbo-inlining for 'Inlining' into fusedArity3 vs fusedGeneral)`);
line();

// ────────────────────────────────────────────────────────────────
// PROBE 3 — deopt sweep (steady state)
// ────────────────────────────────────────────────────────────────
say("### PROBE 3 — deopt sweep (steady-state jsonParser + synthetic CSS-value triple)");
line();
// synthetic CSS-value fragment: the arity-3 hot shape (rgb-like triple).
const num = regex(/[0-9]+/);
const cssTriple = all(num, string(","), num); // hits fused arity-3
const JSONFRAG = '{"a":1,"b":[2,3],"c":"x"}';
const CSSFRAG = "12,34";
for (let i = 0; i < 200_000; i++) {
    jsonParser.parse(JSONFRAG);
    cssTriple.parse(CSSFRAG);
}
say("steady-state loops complete (200k jsonParser + 200k cssTriple).");
say("(deopts, if any, printed to stderr by --trace-deopt; a clean run prints none in steady state.)");
line();

// ── machine-readable summary line for the harness ──
say(`RESULT grammar_shipped_same=${grammar_shipped_same} mech_shipped_same=${mech_shipped_same} mech_fixed_same=${mech_fixed_same} PROBE1_PASS=${PROBE1_PASS}`);
