import { describe, it, expect } from "vitest";
import {
    inlineMath,
    displayMathDollar,
    displayMathBracket,
    inlineMathParen,
} from "../../src/grammar/math";

describe("math parsers", () => {
    it("parses inline math $...$", () => {
        const result = inlineMath.parse("$x^2$");
        expect(result).toEqual({ type: "math", value: "x^2", display: false });
    });

    it("parses display math $$...$$", () => {
        const result = displayMathDollar.parse("$$x^2 + y^2$$");
        expect(result).toEqual({
            type: "math",
            value: "x^2 + y^2",
            display: true,
            environment: "$$",
            numbered: false,
        });
    });

    it("parses display math \\[...\\]", () => {
        const result = displayMathBracket.parse("\\[x^2\\]");
        expect(result).toEqual({
            type: "math",
            value: "x^2",
            display: true,
            environment: "\\[\\]",
            numbered: false,
        });
    });

    it("parses inline math \\(...\\)", () => {
        const result = inlineMathParen.parse("\\(a + b\\)");
        expect(result).toEqual({
            type: "math",
            value: "a + b",
            display: false,
        });
    });
});
