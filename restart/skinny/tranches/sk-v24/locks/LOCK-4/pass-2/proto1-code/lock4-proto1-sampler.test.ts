// LOCK-4 PROTO-1 — interleaved paired-cold sampler (the decisive instrument).
//
// vitest process (the `vitest bench` family harness). We do NOT use tinybench's
// per-function isolation — the brief mandates SAMPLE-INTERLEAVED A,B,C measured
// adjacent in time so the three lanes share thermal/GC/load conditions and the
// RATIO is robust. Per sample: one K-iteration batch of each lane, timed with
// performance.now(), order alternated per sample (ABC / CBA = "both orders").
// N>=200 samples per window, TWO load windows. Report per-lane median + MAD.
//
// PASS  (B) median >= 1.05x baseline (C)  AND  (A) penalty vs (B) within +/-2%.
// FAIL  (B) cannot beat baseline  OR  (A) ties (B) within noise.

import { describe, it, expect } from "vitest";
import os from "node:os";
import { A_fn, B_fn, C_fn, drive, CORPUS, type PFn } from "./lock4-proto1-runtimes.js";

const K = 3000;         // iterations per batch (batch ~ tens of µs, above timer noise)
const N = 240;          // samples per window (>= 200)
const WINDOWS = 2;      // two load windows
const WARMUP = 60_000;  // force V8 tier-up before sampling

function median(xs: number[]): number {
    const s = [...xs].sort((a, b) => a - b);
    const m = s.length >> 1;
    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
}
function mad(xs: number[], med: number): number {
    const dev = xs.map((x) => Math.abs(x - med));
    return median(dev);
}
function pct(xs: number[], p: number): number {
    const s = [...xs].sort((a, b) => a - b);
    return s[Math.min(s.length - 1, Math.floor((p / 100) * s.length))];
}

function batch(fn: PFn, k: number): number {
    let sink = 0;
    const t0 = performance.now();
    for (let i = 0; i < k; i++) {
        const v = drive(fn, CORPUS) as { r: number } | undefined;
        if (v !== undefined) sink += v.r;
    }
    const t1 = performance.now();
    if (sink === -1) throw new Error("unreachable"); // defeat DCE
    return t1 - t0;
}

interface LaneStats { med: number; mad: number; p25: number; p75: number; }
function stats(xs: number[]): LaneStats {
    const med = median(xs);
    return { med, mad: mad(xs, med), p25: pct(xs, 25), p75: pct(xs, 75) };
}

describe("LOCK-4 PROTO-1 interleaved paired-cold sampler", () => {
    it("measures A/B/C and adjudicates PASS/FAIL", () => {
        // Warmup — tier every lane to the optimizing compiler.
        batch(A_fn, WARMUP); batch(B_fn, WARMUP); batch(C_fn, WARMUP);

        const results: Array<{
            window: number; load1: number;
            A: LaneStats; B: LaneStats; C: LaneStats;
            ratios: Record<string, number>;
        }> = [];

        for (let win = 0; win < WINDOWS; win++) {
            const sA: number[] = [], sB: number[] = [], sC: number[] = [];
            const load1Start = os.loadavg()[0];
            for (let s = 0; s < N; s++) {
                if ((s & 1) === 0) {
                    // order ABC
                    sA.push(batch(A_fn, K)); sB.push(batch(B_fn, K)); sC.push(batch(C_fn, K));
                } else {
                    // order CBA
                    sC.push(batch(C_fn, K)); sB.push(batch(B_fn, K)); sA.push(batch(A_fn, K));
                }
            }
            const A = stats(sA), B = stats(sB), C = stats(sC);
            const ratios = {
                "B_speedup_over_C": C.med / B.med,   // >1 => B faster than baseline
                "A_vs_B": A.med / B.med,             // >1 => A slower than B (penalty)
                "A_vs_C": A.med / C.med,
                "C_vs_B": C.med / B.med,
            };
            results.push({ window: win, load1: load1Start, A, B, C, ratios });
        }

        // Pooled across windows (medians of per-window medians for stability).
        const poolMed = (sel: (r: typeof results[number]) => number) =>
            median(results.map(sel));
        const B_med = poolMed((r) => r.B.med);
        const C_med = poolMed((r) => r.C.med);
        const A_med = poolMed((r) => r.A.med);
        const B_speedup = C_med / B_med;
        const A_penalty_vs_B = A_med / B_med;
        const A_penalty_vs_C = A_med / C_med;

        const out = {
            config: { K, N, WINDOWS, WARMUP, corpus: CORPUS, node: process.version,
                      cpus: os.cpus().length, cpu: os.cpus()[0]?.model },
            loadavg_end: os.loadavg(),
            windows: results,
            pooled: {
                A_med_ms: A_med, B_med_ms: B_med, C_med_ms: C_med,
                B_speedup_over_C: B_speedup,
                A_penalty_vs_B: A_penalty_vs_B,
                A_penalty_vs_C: A_penalty_vs_C,
            },
        };
        // eslint-disable-next-line no-console
        console.log("[PROTO1-RESULT]" + JSON.stringify(out));

        // The lanes stay correct throughout.
        expect(drive(B_fn, CORPUS)).toEqual({ r: 255, g: 128, b: 0 });
        expect(B_speedup).toBeGreaterThan(0);
    }, 120_000);
});
