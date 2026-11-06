// Decisive megamorphic test: 5 SFI-HETEROGENEOUS leaves (the real value.js
// calc/rgb/hsl mix), so the general arm's single loop call site
// parsers[i].parser(state) sees >4 distinct SharedFunctionInfos → megamorphic →
// TurboFan cannot inline. The arity-3 unrolled arm has 3 SEPARATE call sites,
// each monomorphic → each inlines. Run under:
//   node --allow-natives-syntax --no-maglev
//        --no-optimize-on-next-call-optimizes-to-maglev --trace-turbo-inlining
const DIST = "/Users/mkbabb/Programming/parse-that/typescript/dist/parse.js";
const { ParserState, string, regex, whitespace } = await import(DIST);
const say = (...a) => console.log(...a);

// leaf.ts general arm replica
function fuseGeneralHetero(parsers) {
    const n = parsers.length;
    return function fusedGeneralHetero(state) {
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
// leaf.ts arity-3 arm replica
function fuseArity3Hetero(p0, p1, p2) {
    return function fusedArity3Hetero(state) {
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

// 5 leaves with 5 DISTINCT SFIs, all succeeding in sequence on the crafted input.
const pA = string("a");            // stringParser (len===1 SFI)
const pXX = string("xx");          // stringParser (len>1 SFI — distinct closure)
const pNum = regex(/[0-9]/);       // regexParser SFI
const pMap = string("b").map((v) => v); // map closure SFI
const pWs = whitespace;            // whitespace SFI
const SRC = "axx5b ";              // a | xx | 5 | b | <space>

const general5 = fuseGeneralHetero([pA, pXX, pNum, pMap, pWs]);
const arity3 = fuseArity3Hetero(pA, pXX, pNum); // 3 distinct SFIs, 3 sites
const SRC3 = "axx5";

function runG() { const st = new ParserState(SRC, undefined, 0, false, -1); general5(st); return st; }
function run3() { const st = new ParserState(SRC3, undefined, 0, false, -1); arity3(st); return st; }

// sanity: both must succeed so every leaf's call feedback is recorded
const cg = runG(), c3 = run3();
say(`sanity general5 isError=${cg.isError} value=${JSON.stringify(cg.value)}`);
say(`sanity arity3  isError=${c3.isError} value=${JSON.stringify(c3.value)}`);

%PrepareFunctionForOptimization(general5);
%PrepareFunctionForOptimization(arity3);
for (let i = 0; i < 50; i++) { runG(); run3(); }
say(">>> OPTIMIZE fusedGeneralHetero");
%OptimizeFunctionOnNextCall(general5);
runG();
say(">>> OPTIMIZE fusedArity3Hetero");
%OptimizeFunctionOnNextCall(arity3);
run3();
say("DONE");
