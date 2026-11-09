// LOCK-6 pass-3 6C probe t1 — the wasm-RESIDENT-bytes lane, Node/V8 (server face).
// Question: when source bytes are already resident in wasm linear memory (no
// per-call TextEncoder marshal), does the v128 scanOnly beat a TS scalar loop,
// and does the win hold at EVERY size incl. small — the regime PROTO-1 left
// PREDICTED-unmeasured (FAMILY-SPECS §3 gate [1], "one surviving win").
//
// Method: raw-C-ABI wasm (input_ptr/scan/output_ptr). Bytes copied into linear
// memory ONCE (resident), then scan(len) called repeatedly with NO re-marshal.
// TS lane = monomorphic 256-LUT scalar loop over the same Uint8Array. Marshal
// column measured separately = the tax the resident lane AVOIDS. Cold single-
// shot re-instantiates the module. Byte-budgeted timed regions, median of reps.

import { readFileSync } from "node:fs";
import { loadavg } from "node:os";

const wasmBytes = readFileSync(new URL("./scan.wasm", import.meta.url));

function instantiate() {
    const mod = new WebAssembly.Module(wasmBytes);
    const inst = new WebAssembly.Instance(mod, {});
    return inst.exports;
}

// 256-entry structural LUT: { } [ ] : ,  — identical target set to the kernel.
const LUT = new Uint8Array(256);
for (const c of ["{", "}", "[", "]", ":", ","]) LUT[c.charCodeAt(0)] = 1;

// Monomorphic TS scalar scanner: EMIT each structural offset into `out`
// (apples-to-apples with the wasm kernel, which materializes OUTPUT[n]=pos).
// Returns the count. `out` is a persistent Int32Array reused across calls,
// mirroring the kernel's static OUTPUT plane.
const TS_OUT = new Int32Array(3_000_000);
function tsScan(buf, len) {
    const out = TS_OUT;
    let n = 0;
    for (let i = 0; i < len; i++) {
        if (LUT[buf[i]]) { out[n] = i; n++; }
    }
    return n;
}

function median(xs) {
    const s = [...xs].sort((a, b) => a - b);
    const m = s.length >> 1;
    return s.length & 1 ? s[m] : (s[m - 1] + s[m]) / 2;
}

// Time a resident-region: op() processes `len` bytes; run enough iters to cover
// ~budget bytes, repeat R times, return median ns/byte.
function timeRegion(op, len, budget, R) {
    const iters = Math.max(1, Math.round(budget / len));
    const perRep = [];
    for (let r = 0; r < R; r++) {
        let sink = 0;
        const t0 = process.hrtime.bigint();
        for (let k = 0; k < iters; k++) sink += op();
        const t1 = process.hrtime.bigint();
        if (sink === -1) throw new Error("no-dce"); // defeat DCE
        perRep.push(Number(t1 - t0) / (iters * len));
    }
    return median(perRep);
}

function measureDoc(name, bytes) {
    const len = bytes.length;
    const ex = instantiate();
    const heap = new Uint8Array(ex.memory.buffer);
    const inPtr = ex.input_ptr();
    const cap = ex.input_cap();
    if (len > cap) throw new Error(`${name} ${len} > cap ${cap}`);

    // Make bytes RESIDENT once (this copy is OUTSIDE the resident timed loop).
    heap.set(bytes, inPtr);

    // Parity: wasm resident scan vs TS scalar (HALT gate).
    const wasmCount = ex.scan(len);
    const tsCount = tsScan(bytes, len);
    const parity = wasmCount === tsCount;

    const budget = 24_000_000; // ~24 MB processed per region
    const R = 9;

    // Warm both lanes (V8 JIT) before timing hot/resident numbers.
    for (let w = 0; w < 5; w++) { ex.scan(len); tsScan(bytes, len); }

    // Resident wasm scanOnly: bytes already in linear memory, NO re-copy.
    const wasmScanOnly = timeRegion(() => ex.scan(len), len, budget, R);
    // TS scalar over the JS Uint8Array.
    const ts = timeRegion(() => tsScan(bytes, len), len, budget, R);
    // Marshal (reference): the per-call copy the resident lane AVOIDS.
    const marshal = timeRegion(() => { heap.set(bytes, inPtr); return heap[inPtr]; }, len, budget, R);

    return { name, len, wasmCount, tsCount, parity, wasmScanOnly, ts, marshal,
             speedup: ts / wasmScanOnly };
}

// Cold single-shot: fresh module instance, one resident copy, one scan/one ts.
function coldDoc(bytes) {
    const len = bytes.length;
    // wasm cold: instantiate + copy-once + first scan.
    const ex = instantiate();
    const heap = new Uint8Array(ex.memory.buffer);
    const inPtr = ex.input_ptr();
    heap.set(bytes, inPtr); // resident copy (counted as amortized setup, not per-scan)
    const w0 = process.hrtime.bigint();
    ex.scan(len);
    const w1 = process.hrtime.bigint();
    const coldWasm = Number(w1 - w0) / len;
    // ts cold: first-ever tsScan on a fresh function realm-ish (best effort).
    const t0 = process.hrtime.bigint();
    tsScan(bytes, len);
    const t1 = process.hrtime.bigint();
    const coldTs = Number(t1 - t0) / len;
    return { coldWasm, coldTs };
}

// ---- synthetic ASCII sweeps (resident, small→large) ----
// DENSE (~37% structural): stresses the per-hit CTZ-emit inner loop (both lanes).
function synthDense(n) {
    const b = Buffer.alloc(n);
    const pat = Buffer.from('{"k":[1,2,3],"m":{"a":0},"z":[]},');
    for (let i = 0; i < n; i++) b[i] = pat[i % pat.length];
    return new Uint8Array(b);
}
// REALISTIC (~5% structural — matches twitter/citm JSON density): isolates the
// SIZE axis. A structural byte every ~20 chars amid non-structural filler.
function synthReal(n) {
    const b = Buffer.alloc(n);
    // 20-byte cell: one structural delimiter + 19 filler letters.
    const pat = Buffer.from('abcdefghij,klmnopqrs{');
    for (let i = 0; i < n; i++) b[i] = pat[i % pat.length];
    return new Uint8Array(b);
}

const la0 = loadavg();
console.log("LOADAVG_START", la0.map(x => x.toFixed(2)).join(" "));
console.log("NODE", process.version);

const sizes = [64, 128, 256, 512, 1024, 2048, 4096, 16384, 65536];
console.log("\n=== SYNTHETIC RESIDENT SWEEP — REALISTIC ~5% structural (isolates SIZE axis) ===");
console.log("bytes\tdens%\tts\twasmScan\tmarshal\tspeedup\tWIN");
for (const n of sizes) {
    const r = measureDoc(`synthReal${n}`, synthReal(n));
    if (!r.parity) console.log(`  PARITY FAIL synthReal${n}: wasm=${r.wasmCount} ts=${r.tsCount}`);
    const win = r.wasmScanOnly < r.ts ? "WASM" : "TS";
    const dens = (100 * r.wasmCount / r.len).toFixed(1);
    console.log(`${n}\t${dens}\t${r.ts.toFixed(3)}\t${r.wasmScanOnly.toFixed(3)}\t${r.marshal.toFixed(3)}\t${r.speedup.toFixed(2)}x\t${win}`);
}
console.log("\n=== SYNTHETIC RESIDENT SWEEP — DENSE ~37% structural (stresses emit loop) ===");
console.log("bytes\tdens%\tts\twasmScan\tmarshal\tspeedup\tWIN");
for (const n of sizes) {
    const r = measureDoc(`synthDense${n}`, synthDense(n));
    if (!r.parity) console.log(`  PARITY FAIL synthDense${n}: wasm=${r.wasmCount} ts=${r.tsCount}`);
    const win = r.wasmScanOnly < r.ts ? "WASM" : "TS";
    const dens = (100 * r.wasmCount / r.len).toFixed(1);
    console.log(`${n}\t${dens}\t${r.ts.toFixed(3)}\t${r.wasmScanOnly.toFixed(3)}\t${r.marshal.toFixed(3)}\t${r.speedup.toFixed(2)}x\t${win}`);
}

console.log("\n=== REAL DOCS RESIDENT (twitter/citm/canada) ===");
console.log("doc\tbytes\tcount\tparity\tts\twasmScan\tmarshal\tspeedup\tWIN\tcoldTs\tcoldWasm");
const docs = [
    ["twitter", "./twitter.json"],
    ["citm", "./citm_catalog.json"],
    ["canada", "./canada.json"],
];
for (const [name, path] of docs) {
    const bytes = new Uint8Array(readFileSync(new URL(path, import.meta.url)));
    const r = measureDoc(name, bytes);
    const cold = coldDoc(bytes);
    const win = r.wasmScanOnly < r.ts ? "WASM" : "TS";
    console.log(`${name}\t${r.len}\t${r.wasmCount}\t${r.parity}\t${r.ts.toFixed(3)}\t${r.wasmScanOnly.toFixed(3)}\t${r.marshal.toFixed(3)}\t${r.speedup.toFixed(2)}x\t${win}\t${cold.coldTs.toFixed(3)}\t${cold.coldWasm.toFixed(3)}`);
}

const la1 = loadavg();
console.log("\nLOADAVG_END", la1.map(x => x.toFixed(2)).join(" "));
