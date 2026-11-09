import { describe, it, expect } from "vitest";
import {
    plainText,
    emDash,
    enDash,
    leftDoubleQuote,
    rightDoubleQuote,
    tilde,
    escapedSpecial,
    bracedAccent,
    unbracedAccent,
    comment,
    singleNewline,
} from "../../src/grammar/text";

describe("text parsers", () => {
    it("parses plain text", () => {
        const result = plainText.parse("Hello world");
        expect(result).toEqual({ type: "text", value: "Hello world" });
    });

    it("parses em dash", () => {
        const result = emDash.parse("---");
        expect(result).toEqual({ type: "text", value: "\u2014" });
    });

    it("parses en dash", () => {
        const result = enDash.parse("--");
        expect(result).toEqual({ type: "text", value: "\u2013" });
    });

    it("parses left double quote", () => {
        const result = leftDoubleQuote.parse("``");
        expect(result).toEqual({ type: "text", value: "\u201C" });
    });

    it("parses right double quote", () => {
        const result = rightDoubleQuote.parse("''");
        expect(result).toEqual({ type: "text", value: "\u201D" });
    });

    it("parses tilde as space", () => {
        const result = tilde.parse("~");
        expect(result).toEqual({ type: "text", value: " " });
    });

    it("parses escaped special characters", () => {
        const result = escapedSpecial.parse("\\#");
        expect(result).toEqual({ type: "text", value: "#" });
    });

    it("parses escaped ampersand", () => {
        const result = escapedSpecial.parse("\\&");
        expect(result).toEqual({ type: "text", value: "&amp;" });
    });

    it("parses braced accent", () => {
        const result = bracedAccent.parse('\\\"{a}');
        expect(result).toEqual({ type: "text", value: "\u00e4" });
    });

    it("parses unbraced accent", () => {
        const result = unbracedAccent.parse("\\'e");
        expect(result).toEqual({ type: "text", value: "\u00e9" });
    });

    it("strips comments", () => {
        const result = comment.parse("% this is a comment\n");
        expect(result).toBeNull();
    });
});
