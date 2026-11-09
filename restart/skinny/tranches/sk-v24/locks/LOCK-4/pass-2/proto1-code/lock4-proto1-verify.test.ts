import { describe, it, expect } from "vitest";
import {
    A_fn, B_fn, C_fn, drive, CORPUS,
    B_emittedSrc, A_programByteLen,
} from "./lock4-proto1-runtimes.js";

describe("LOCK-4 PROTO-1 equivalence + instrumentation", () => {
    it("all three lanes produce identical output on the corpus", () => {
        const c = drive(C_fn, CORPUS);
        const b = drive(B_fn, CORPUS);
        const a = drive(A_fn, CORPUS);
        expect(c).toEqual({ r: 255, g: 128, b: 0 });
        expect(b).toEqual(c);
        expect(a).toEqual(c);
    });

    it("emitted B source contains ZERO `.parser(` indirection", () => {
        const hits = (B_emittedSrc.match(/\.parser\(/g) || []).length;
        // eslint-disable-next-line no-console
        console.log("[INSTRUMENT] B `.parser(` count =", hits);
        // eslint-disable-next-line no-console
        console.log("[INSTRUMENT] A program byte length =", A_programByteLen);
        // eslint-disable-next-line no-console
        console.log("[INSTRUMENT] B emitted source:\n" + B_emittedSrc);
        expect(hits).toBe(0);
    });

    it("lanes reject a malformed input identically", () => {
        expect(drive(C_fn, "rgb(255,)")).toBeUndefined();
        expect(drive(B_fn, "rgb(255,)")).toBeUndefined();
        expect(drive(A_fn, "rgb(255,)")).toBeUndefined();
    });
});
