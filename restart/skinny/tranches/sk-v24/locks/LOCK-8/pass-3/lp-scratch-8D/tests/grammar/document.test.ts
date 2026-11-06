import { describe, it, expect } from "vitest";
import {
    parseLatex,
    parseInlineString,
    astToText,
} from "../../src/grammar/document";
import type { LatexNode } from "../../src/types/ast";

describe("parseLatex", () => {
    it("parses plain text", () => {
        const nodes = parseLatex("Hello, world!");
        const text = astToText(nodes);
        expect(text).toContain("Hello");
        expect(text).toContain("world");
    });

    it("parses inline math", () => {
        const nodes = parseLatex("The equation $x^2$ is simple.");
        const mathNodes = nodes.filter((n) => n.type === "math");
        expect(mathNodes.length).toBe(1);
        expect(mathNodes[0].type === "math" && mathNodes[0].value).toBe("x^2");
    });

    it("parses section commands", () => {
        const nodes = parseLatex("\\section{Introduction}\nSome text.");
        const sections = nodes.filter((n) => n.type === "section");
        expect(sections.length).toBe(1);
    });

    it("parses multiple paragraphs", () => {
        const nodes = parseLatex("First paragraph.\n\nSecond paragraph.");
        const breaks = nodes.filter((n) => n.type === "paragraphBreak");
        expect(breaks.length).toBe(1);
    });

    it("handles em dashes", () => {
        const nodes = parseLatex("before---after");
        const text = astToText(nodes);
        expect(text).toContain("\u2014");
    });

    it("handles en dashes", () => {
        const nodes = parseLatex("pages 1--10");
        const text = astToText(nodes);
        expect(text).toContain("\u2013");
    });

    it("handles smart quotes", () => {
        const nodes = parseLatex("``hello''");
        const text = astToText(nodes);
        expect(text).toContain("\u201C");
        expect(text).toContain("\u201D");
    });

    it("handles accents", () => {
        const nodes = parseLatex("Schr\\\"odinger");
        const text = astToText(nodes);
        expect(text).toContain("\u00f6");
    });

    it("parses formatting commands", () => {
        const nodes = parseLatex("\\textbf{bold} and \\emph{italic}");
        const cmds = nodes.filter(
            (n) => n.type === "command" && ["textbf", "emph"].includes(n.name),
        );
        expect(cmds.length).toBe(2);
    });

    it("strips comments", () => {
        const nodes = parseLatex("visible % invisible\nnext line");
        const text = astToText(nodes);
        expect(text).toContain("visible");
        expect(text).not.toContain("invisible");
    });

    it("handles tilde as space", () => {
        const nodes = parseLatex("Figure~1");
        const text = astToText(nodes);
        expect(text).toBe("Figure 1");
    });

    it("handles escaped specials", () => {
        const nodes = parseLatex("100\\% complete");
        const text = astToText(nodes);
        expect(text).toContain("%");
    });

    it("parses environments", () => {
        const nodes = parseLatex(
            "\\begin{theorem}\nA theorem.\n\\end{theorem}",
        );
        const theorems = nodes.filter((n) => n.type === "theorem");
        expect(theorems.length).toBe(1);
    });

    it("parses display math environments", () => {
        const nodes = parseLatex(
            "\\begin{equation}\nx^2\n\\end{equation}",
        );
        const math = nodes.filter(
            (n) => n.type === "math" && n.display,
        );
        expect(math.length).toBe(1);
    });
});

describe("parseInlineString", () => {
    it("parses simple text", () => {
        const nodes = parseInlineString("hello");
        expect(astToText(nodes)).toBe("hello");
    });

    it("parses text with math", () => {
        const nodes = parseInlineString("$x^2$ norm");
        expect(nodes.some((n) => n.type === "math")).toBe(true);
    });
});

describe("astToText", () => {
    it("extracts text from text nodes", () => {
        const nodes: LatexNode[] = [
            { type: "text", value: "hello " },
            { type: "text", value: "world" },
        ];
        expect(astToText(nodes)).toBe("hello world");
    });

    it("includes inline math", () => {
        const nodes: LatexNode[] = [
            { type: "text", value: "the formula " },
            { type: "math", value: "x^2", display: false },
        ];
        expect(astToText(nodes)).toContain("$x^2$");
    });

    it("skips display math", () => {
        const nodes: LatexNode[] = [
            { type: "text", value: "before " },
            { type: "math", value: "x^2", display: true },
            { type: "text", value: " after" },
        ];
        expect(astToText(nodes)).toBe("before after");
    });
});
