import { describe, it, expect } from "vitest";
// Import document.ts first to register inlineNode (required by braceContent)
import "../../src/grammar/document";
import {
    sectionCommand,
    formattingCommand,
    labelCommand,
    citeCommand,
    hrefCommand,
    paragraphCommand,
    bibliographyCommand,
    skipCommand,
    footnoteCommand,
} from "../../src/grammar/commands";

describe("command parsers", () => {
    describe("section commands", () => {
        it("parses \\chapter{Title}", () => {
            const result = sectionCommand.parse("\\chapter{My Chapter}");
            expect(result.type).toBe("section");
            expect(result.level).toBe("chapter");
            expect(result.starred).toBe(false);
        });

        it("parses \\section*{Title}", () => {
            const result = sectionCommand.parse("\\section*{Starred}");
            expect(result.type).toBe("section");
            expect(result.level).toBe("section");
            expect(result.starred).toBe(true);
        });

        it("parses \\subsection{Title}", () => {
            const result = sectionCommand.parse("\\subsection{Sub}");
            expect(result.level).toBe("subsection");
        });
    });

    describe("formatting commands", () => {
        it("parses \\textbf{bold}", () => {
            const result = formattingCommand.parse("\\textbf{bold}");
            expect(result.name).toBe("textbf");
            expect(result.args[0][0]).toEqual({
                type: "text",
                value: "bold",
            });
        });

        it("parses \\emph{emphasis}", () => {
            const result = formattingCommand.parse("\\emph{emphasis}");
            expect(result.name).toBe("emph");
        });
    });

    describe("reference commands", () => {
        it("parses \\label{key}", () => {
            const result = labelCommand.parse("\\label{sec:intro}");
            expect(result).toEqual({ type: "label", key: "sec:intro" });
        });

        it("parses \\cite{key}", () => {
            const result = citeCommand.parse("\\cite{fourier1822}");
            expect(result.name).toBe("cite");
            expect(result.args[0][0]).toEqual({
                type: "text",
                value: "fourier1822",
            });
        });

        it("parses \\bibliography{file}", () => {
            const result = bibliographyCommand.parse("\\bibliography{fourier_paper}");
            expect(result.name).toBe("bibliography");
            expect(result.args[0][0]).toEqual({
                type: "text",
                value: "fourier_paper",
            });
        });
    });

    describe("other commands", () => {
        it("parses \\href{url}{text}", () => {
            const result = hrefCommand.parse(
                "\\href{https://example.com}{click here}",
            );
            expect(result.name).toBe("href");
            expect(result.args[0][0]).toEqual({
                type: "text",
                value: "https://example.com",
            });
            expect(result.args[1][0]).toEqual({
                type: "text",
                value: "click here",
            });
        });

        it("parses \\paragraph{title}", () => {
            const result = paragraphCommand.parse("\\paragraph{My Para}");
            expect(result.name).toBe("paragraph");
        });

        it("parses \\footnote{text}", () => {
            const result = footnoteCommand.parse("\\footnote{a note}");
            expect(result.name).toBe("footnote");
        });
    });
});
