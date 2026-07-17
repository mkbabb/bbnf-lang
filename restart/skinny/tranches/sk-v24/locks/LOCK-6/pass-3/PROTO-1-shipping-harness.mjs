// PROTO-6C resident-bytes harness (LOCK-6 pass-3, probe 1).
//
// Drives the SHIPPING simd-scan v128 kernel (via bbnf-wasm-6c cdylib) in the
// RESIDENT regime and pairs it against a marshalled arm and a TS-native
// scalar arm. Cold + paired: fresh wasm instance per trial, arms interleaved
// on identical bytes, median over trials. loadavg recorded at start/end.
//
//   node --expose-gc harness.mjs <path-to.wasm>
//
// Three arms, all counting the SAME structural set (singletons {}[]:, that
// fall OUTSIDE JSON string bodies, backslash-parity escaping) so parity is a
// HALT gate:
//   (a) resident   — bytes pre-written to arena ONCE; time scan_resident only
//   (b) marshalled — time TextEncoder.encode(str)+copy-to-arena+scan_resident
//   (c) scan.ts    — TS scalar 256-entry Int8Array LUT, emits into Int32Array

import fs from "node:fs";
import os from "node:os";
import process from "node:process";

const WASM_PATH = process.argv[2];
if (!WASM_PATH) {
  console.error("usage: node --expose-gc harness.mjs <path-to.wasm>");
  process.exit(2);
}

const SINGLETONS = [0x7b, 0x7d, 0x5b, 0x5d, 0x3a, 0x2c]; // { } [ ] : ,
const QUOTE = 0x22; // "
const BSLASH = 0x5c; // backslash

// ---- TS-native scalar reference (arm c) --------------------------------
// Monomorphic 256-entry LUT; emits each structural offset into a persistent
// Int32Array (apples-to-apples with the kernel materialising positions).
// Replicates the kernel tail epilogue EXACTLY (crates/simd-scan/src/wasm.rs
// scan_nibble tail): emit singleton iff NOT inside a string; toggle string
// state on unescaped quote; backslash parity per byte.
const LUT = new Int8Array(256);
for (const b of SINGLETONS) LUT[b] = 1;

function scanTs(bytes, out) {
  let n = 0;
  let inString = false;
  let prevWasBs = false;
  const len = bytes.length;
  for (let i = 0; i < len; i++) {
    const b = bytes[i];
    if (!inString && LUT[b] === 1) {
      out[n++] = i;
    }
    if (b === QUOTE && !prevWasBs) {
      inString = !inString;
    }
    prevWasBs = b === BSLASH && !prevWasBs;
  }
  return n;
}

// ---- wasm instance ------------------------------------------------------
function freshInstance() {
  const buf = fs.readFileSync(WASM_PATH);
  const mod = new WebAssembly.Module(buf);
  const inst = new WebAssembly.Instance(mod, {});
  return inst.exports;
}

function residentCopy(ex, off, bytes) {
  const heap = new Uint8Array(ex.memory.buffer, ex.input_ptr(), ex.input_cap());
  heap.set(bytes, off);
}

// ---- corpus -------------------------------------------------------------
const twitter = fs.readFileSync(
  new URL("../../skinny/test_data/twitter.json", import.meta.url)
);
const enc = new TextEncoder();

// Synthetic KB-scale "docs" = slices of the real corpus (realistic byte
// distribution + structural density). Batch = B such slices laid contiguous.
function makeSlice(nBytes) {
  // tile twitter to reach nBytes if needed (twitter is 631 KB, plenty)
  return twitter.subarray(0, nBytes);
}

const nowNs = () => process.hrtime.bigint();
const loadavg = () => os.loadavg().map((x) => x.toFixed(2)).join(" ");

// Median helper
const median = (a) => {
  const s = [...a].sort((x, y) => x - y);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};

// One cold-paired trial for a batch of `count` docs each `docBytes` big.
// Returns {res, mar, ts} in NANOSECONDS for the whole batch, plus counts.
function trial(docBytes, count) {
  const ex = freshInstance(); // fresh instance = cold
  const out = new Int32Array(Math.ceil(docBytes / 2) + 16);
  const slice = makeSlice(docBytes);
  // JS-string form of the same bytes for the marshalled arm (browser lane):
  // decode once here (this decode is NOT timed; the TextEncoder RE-encode is).
  const str = Buffer.from(slice).toString("latin1"); // 1:1 byte<->char

  // Pre-write all `count` docs into the arena (RESIDENT setup, untimed).
  const offs = [];
  let cursor = 0;
  for (let d = 0; d < count; d++) {
    offs.push(cursor);
    residentCopy(ex, cursor, slice);
    cursor += docBytes;
  }

  // Parity check (wasm vs ts) on one doc — HALT gate.
  const wc = ex.scan_resident(offs[0], docBytes);
  const tc = scanTs(slice, out);
  if (wc !== tc) {
    console.error(`PARITY FAIL @${docBytes}B: wasm=${wc} ts=${tc}`);
    process.exit(1);
  }

  // (a) RESIDENT — copy already done; time scan only.
  let t0 = nowNs();
  let acc = 0;
  for (let d = 0; d < count; d++) acc += ex.scan_resident(offs[d], docBytes);
  const res = Number(nowNs() - t0);

  // (b) MARSHALLED — time encode+copy+scan per doc (browser lane).
  const ex2 = freshInstance();
  t0 = nowNs();
  let acc2 = 0;
  for (let d = 0; d < count; d++) {
    const encoded = enc.encode(str); // UTF-16 -> UTF-8 transcode (the tax)
    const heap = new Uint8Array(ex2.memory.buffer, ex2.input_ptr(), ex2.input_cap());
    heap.set(encoded, 0);
    acc2 += ex2.scan_resident(0, encoded.length);
  }
  const mar = Number(nowNs() - t0);

  // (c) scan.ts — time scalar emit per doc.
  t0 = nowNs();
  let acc3 = 0;
  for (let d = 0; d < count; d++) acc3 += scanTs(slice, out);
  const ts = Number(nowNs() - t0);

  if (acc === 0 || acc2 === 0 || acc3 === 0) throw new Error("DCE guard");
  return { res, mar, ts, count: wc };
}

function sweepConfig(docBytes, count, trials) {
  const R = [], M = [], T = [];
  for (let k = 0; k < trials; k++) {
    if (global.gc) global.gc();
    const r = trial(docBytes, count);
    const totalBytes = docBytes * count;
    const mb = totalBytes / 1e6;
    R.push(r.res / 1e6 / mb); // ms per MB
    M.push(r.mar / 1e6 / mb);
    T.push(r.ts / 1e6 / mb);
  }
  return { res: median(R), mar: median(M), ts: median(T) };
}

// ---- run ----------------------------------------------------------------
console.log(`# PROTO-6C resident harness`);
console.log(`# node ${process.version}  wasm=${WASM_PATH}`);
console.log(`# loadavg(start) = ${loadavg()}`);
console.log(`# metric = ms/MB (lower=faster). speedup = ts/res (>1 => wasm wins)`);
console.log("");

const TRIALS = 15;

// Real-doc anchor: full twitter.json, single doc.
{
  const r = sweepConfig(twitter.length, 1, TRIALS);
  console.log(`## real: twitter.json (${twitter.length} B)`);
  console.log(
    `res=${r.res.toFixed(4)}  mar=${r.mar.toFixed(4)}  ts=${r.ts.toFixed(4)}  ` +
      `speedup(ts/res)=${(r.ts / r.res).toFixed(2)}x  ` +
      `marshal-tax(mar/res)=${(r.mar / r.res).toFixed(2)}x`
  );
  console.log("");
}

// Sweep: doc-size {1,8,64} KB  ×  batch-count {1,8,64}
const DOC_KB = [1, 8, 64];
const BATCH = [1, 8, 64];
console.log(`## sweep doc-size x batch-count (median ms/MB, ${TRIALS} cold trials)`);
console.log(`docKB | batch | res | mar | ts | speedup(ts/res) | marshal(mar/res)`);
const rows = [];
for (const kb of DOC_KB) {
  for (const b of BATCH) {
    const r = sweepConfig(kb * 1024, b, TRIALS);
    const sp = r.ts / r.res;
    rows.push({ kb, b, ...r, sp });
    console.log(
      `${kb}KB | ${b} | ${r.res.toFixed(4)} | ${r.mar.toFixed(4)} | ` +
        `${r.ts.toFixed(4)} | ${sp.toFixed(2)}x | ${(r.mar / r.res).toFixed(2)}x`
    );
  }
}

console.log("");
console.log(`# loadavg(end) = ${loadavg()}`);

// Crossover: smallest total-bytes where resident beats ts by >=2x, and the
// smallest where it wins at all.
const win2 = rows.filter((r) => r.sp >= 2.0).map((r) => r.kb * r.b * 1024);
const winAny = rows.filter((r) => r.sp > 1.0).map((r) => r.kb * r.b * 1024);
console.log(
  `# resident>=2x from total-bytes >= ${win2.length ? Math.min(...win2) : "NEVER"}`
);
console.log(
  `# resident>1x  from total-bytes >= ${winAny.length ? Math.min(...winAny) : "NEVER"}`
);
// Verdict helper: does resident beat ts by >=2x at KB-scale batch sizes?
const kbScale = rows.filter((r) => r.kb <= 8); // 1-8 KB docs
const green = kbScale.every((r) => r.sp >= 2.0);
const greenAny = kbScale.some((r) => r.sp >= 2.0);
console.log(
  `# GATE: all KB-scale(<=8KB) batches >=2x ? ${green}  (any>=2x ? ${greenAny})`
);
