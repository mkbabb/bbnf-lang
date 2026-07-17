import { describe, it, expect } from "vitest";
import { Transformer, transformDocument } from "../src/transform/html";
import { parseLatex } from "../src/grammar/document";
import { parseBibToMap } from "../src/bibtex/parser";
import { createCompiledPaperMetadata } from "../src/compiled/metadata";
import type { LatexNode } from "../src/types/ast";

describe("Transformer", () => {
    it("renders text nodes as-is", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({ type: "text", value: "hello" });
        expect(html).toBe("hello");
    });

    it("renders inline math with delimiters", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "math",
            value: "x^2",
            display: false,
        });
        expect(html).toBe("$x^2$");
    });

    it("renders textbf as strong", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "command",
            name: "textbf",
            args: [[{ type: "text", value: "bold" }]],
        });
        expect(html).toBe("<strong>bold</strong>");
    });

    it("renders emph as em", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "command",
            name: "emph",
            args: [[{ type: "text", value: "italic" }]],
        });
        expect(html).toBe("<em>italic</em>");
    });

    it("renders texttt as code", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "command",
            name: "texttt",
            args: [[{ type: "text", value: "code" }]],
        });
        expect(html).toContain("<code");
        expect(html).toContain("code");
    });

    it("renders href as link", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "command",
            name: "href",
            args: [
                [{ type: "text", value: "https://example.com" }],
                [{ type: "text", value: "click" }],
            ],
        });
        expect(html).toContain('href="https://example.com"');
        expect(html).toContain("click");
    });

    it("renders cite with bib entries", () => {
        const bibEntries = parseBibToMap(
            `@article{key1,
  author = {John Smith},
  title = {A Paper},
  year = {2020}
}`,
        );

        const t = new Transformer({ bibEntries });
        const html = t.nodeToHtml({
            type: "command",
            name: "cite",
            args: [[{ type: "text", value: "key1" }]],
        });
        expect(html).toContain("Smith");
        expect(html).toContain("2020");
    });

    it("renders list as HTML list", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "list",
            ordered: true,
            items: [
                [{ type: "text", value: "First" }],
                [{ type: "text", value: "Second" }],
            ],
        });
        expect(html).toContain("<ol");
        expect(html).toContain("<li>First</li>");
        expect(html).toContain("<li>Second</li>");
    });

    it("renders quote as blockquote", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "quote",
            body: [{ type: "text", value: "A wise saying." }],
        });
        expect(html).toContain("<blockquote");
        expect(html).toContain("A wise saying.");
    });

    it("renders paragraph command as strong", () => {
        const t = new Transformer();
        const html = t.nodeToHtml({
            type: "command",
            name: "paragraph",
            args: [[{ type: "text", value: "Title" }]],
        });
        expect(html).toContain("<strong>Title</strong>");
    });

    it("renders bibliography from cited entries", () => {
        const bibEntries = parseBibToMap(
            `@article{key1,
  author = {John Smith},
  title = {A Paper},
  year = {2020}
}
@book{key2,
  author = {Jane Doe},
  title = {Another Work},
  year = {2021}
}`,
        );

        const t = new Transformer({ bibEntries });
        t.nodeToHtml({
            type: "command",
            name: "cite",
            args: [[{ type: "text", value: "key1" }]],
        });

        const html = t.nodeToHtml({
            type: "command",
            name: "bibliography",
            args: [[{ type: "text", value: "refs" }]],
        });
        expect(html).toContain("paper-bibliography");
        expect(html).toContain("A Paper");
        expect(html).not.toContain("Another Work");
    });
});

describe("transformDocument", () => {
    it("transforms a simple document into sections", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Intro}\nHello world paragraph text here.\n\\end{document}",
        );
        const { sections } = transformDocument(ast);
        expect(sections.length).toBeGreaterThanOrEqual(1);
        expect(sections[0].title).toBe("Intro");
    });

    it("nests sections within chapters", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Ch1}\n\\section{Sec1}\nContent.\n\\end{document}",
        );
        const { sections } = transformDocument(ast);
        expect(sections.length).toBe(1);
        expect(sections[0].subsections).toBeDefined();
        expect(sections[0].subsections!.length).toBe(1);
    });

    it("extracts theorems from sections", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Ch}\n\\begin{theorem}\nA theorem statement here.\n\\end{theorem}\n\\end{document}",
        );
        const { sections } = transformDocument(ast);
        expect(sections[0].theorems).toBeDefined();
        expect(sections[0].theorems!.length).toBe(1);
    });

    it("interleaves display math as MathBlockData in content", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Ch}\nBefore the equation paragraph text.\n\\[\nx^2 + y^2 = z^2\n\\]\nAfter the equation paragraph text.\n\\end{document}",
        );
        const { sections } = transformDocument(ast);
        const content = sections[0].content;
        // Should have: paragraph, math block, paragraph
        expect(content.length).toBe(3);
        expect(typeof content[0]).toBe("string");
        expect(typeof content[1]).toBe("object");
        expect(typeof content[2]).toBe("string");
        if (typeof content[1] === "object") {
            expect(content[1].tex).toContain("x^2 + y^2 = z^2");
        }
    });

    it("assigns label IDs to display math blocks", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Ch}\nSome intro text for the section.\n\\begin{equation}\n\\label{eq:test}\nE = mc^2\n\\end{equation}\n\\end{document}",
        );
        const { sections } = transformDocument(ast);
        const mathBlock = sections[0].content.find(
            (b) => typeof b === "object",
        );
        expect(mathBlock).toBeDefined();
        if (typeof mathBlock === "object") {
            expect(mathBlock.id).toBe("eq-test");
        }
    });

    it("resolves eqref cross-references to labeled equations", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Ch}\n\\begin{equation}\n\\label{eq:euler}\ne^{i\\pi} + 1 = 0\n\\end{equation}\nSee \\eqref{eq:euler} for the identity.\n\\end{document}",
        );
        const { sections, labelMap } = transformDocument(ast);
        // Label should be resolved
        expect(labelMap["eq:euler"]).toBeDefined();
        // The paragraph referencing the equation should contain a paper-ref link
        const refParagraph = sections[0].content.find(
            (b) => typeof b === "string" && b.includes("paper-ref"),
        );
        expect(refParagraph).toBeDefined();
    });

    it("applies callouts", () => {
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Applications}\nSome text about apps.\n\\end{document}",
        );
        const { sections } = transformDocument(ast, {
            callouts: {
                applications: {
                    text: "Try it!",
                    link: "/app",
                },
            },
        });
        expect(sections[0].callout).toEqual({
            text: "Try it!",
            link: "/app",
        });
    });

    it("prefers compiled numbering, bibliography placement, and anchor metadata", () => {
        const source = `
\\begin{document}
\\section{Introduction}
\\label{sec:intro}
\\begin{equation}
x = 1
\\end{equation}
\\chapter{Linear Algebra}
\\section{Alternative Orthogonal Bases}
\\label{sec:orthogonal_polynomials}
\\begin{definition}[Kronecker Delta]
Body.
\\end{definition}
\\begin{equation}
\\label{eq:test}
E = mc^2
\\end{equation}
See Section~\\ref{sec:orthogonal_polynomials} and \\eqref{eq:test}.
\\bibliography{paper}
\\end{document}
        `;
        const compiledMetadata = createCompiledPaperMetadata({
            texSource: `
\\newtheorem{theorem}{Theorem}[section]
\\newtheorem{definition}{Definition}[section]
\\newtheorem{example}{Example}[section]
${source}
            `,
            tocSource: `
\\contentsline {section}{\\numberline {0.1}Introduction}{4}{section.0.1}%
\\contentsline {chapter}{\\numberline {1}Linear Algebra}{5}{chapter.1}%
\\contentsline {section}{\\numberline {1.1}Alternative Orthogonal Bases}{6}{section.1.1}%
            `,
            auxSource: `
\\newlabel{sec:intro}{{0.1}{4}{Introduction}{section.0.1}{}}
\\newlabel{sec:orthogonal_polynomials}{{1.1}{6}{Alternative Orthogonal Bases}{section.1.1}{}}
\\newlabel{eq:test}{{1.1}{6}{Alternative Orthogonal Bases}{equation.1.1}{}}
            `,
            bblSource: `
\\begin{thebibliography}{1}
\\bibitem{heat_fourier}
Jean-Baptiste~Joseph Fourier.
\\newblock {\\em Th\'{e}orie analytique de la chaleur}.
\\end{thebibliography}
            `,
            logSource: "Output written on paper.pdf (12 pages, 42 bytes).",
        });

        const { sections, labelMap } = transformDocument(parseLatex(source), {
            compiledMetadata,
        });

        expect(sections[0].number).toBe("0.1");
        expect(sections[1].number).toBe("1");
        expect(sections[1].subsections?.[0]?.number).toBe("1.1");
        expect(sections[2]).toMatchObject({
            id: "bibliography",
            title: "Bibliography",
            number: "2",
        });

        const introEquation = sections[0].content.find(
            (block) => typeof block === "object" && block !== null && "tex" in block,
        );
        const orthogonalSection = sections[1].subsections?.[0];
        const definitionBlock = orthogonalSection?.content.find(
            (block) => typeof block === "object" && block !== null && "theorem" in block,
        );
        const labeledEquation = orthogonalSection?.content.find(
            (block) =>
                typeof block === "object" &&
                block !== null &&
                "tex" in block &&
                block.anchorId === "eq-test",
        );
        const refParagraph = orthogonalSection?.content.find(
            (block) => typeof block === "string" && block.includes("paper-ref"),
        );

        expect(introEquation && typeof introEquation === "object" && "number" in introEquation && introEquation.number).toBe("0.1");
        expect(
            definitionBlock &&
                typeof definitionBlock === "object" &&
                "theorem" in definitionBlock &&
                definitionBlock.theorem.number,
        ).toBe("1.1.1");
        expect(
            labeledEquation &&
                typeof labeledEquation === "object" &&
                "number" in labeledEquation &&
                labeledEquation.number,
        ).toBe("1.1");
        expect(refParagraph).toContain("1.1");
        expect(refParagraph).toContain("(1.1)");
        expect(labelMap["sec:orthogonal_polynomials"]?.anchorId).toBe(
            "alternative-orthogonal-bases",
        );
        expect(labelMap["eq:test"]?.anchorId).toBe("eq-test");
    });

    it("preserves proof, code listings, and bibliography content blocks", () => {
        const bibEntries = parseBibToMap(
            `@article{sturm,
  author = {Jacques Sturm},
  title = {Memoire},
  year = {1836}
}`,
        );
        const ast = parseLatex(
            "\\begin{document}\n\\chapter{Appendix}\n\\section{Sturm-Liouville Completeness}\n\\begin{proof}[Proof of Theorem]\\begin{equation}x^2\\end{equation}Thus done.\\end{proof}\n\\paragraph{Chebyshev fitting.} Sample text.\n\\begin{lstlisting}[caption={Chebyshev coefficient computation (\\texttt{bases\\_fitting.py})}]\ncoeffs = fit(values)\n\\end{lstlisting}\nSee \\cite{sturm}.\n\\bibliography{paper}\n\\end{document}",
        );
        const { sections } = transformDocument(ast, { bibEntries });
        const appendix = sections[0].subsections?.[0];
        expect(appendix).toBeDefined();
        if (!appendix) return;

        const proofBlock = appendix.content.find(
            (block) => typeof block === "object" && block !== null && "proof" in block,
        );
        const codeBlock = appendix.content.find(
            (block) => typeof block === "object" && block !== null && "code" in block,
        );
        const bibliography = appendix.content.find(
            (block) => typeof block === "string" && block.includes("paper-bibliography"),
        );

        expect(proofBlock).toBeDefined();
        expect(codeBlock).toBeDefined();
        expect(bibliography).toBeDefined();
        if (typeof proofBlock === "object" && proofBlock && "proof" in proofBlock) {
            const proofText = proofBlock.proof.content.find(
                (block) => typeof block === "string",
            );
            const proofMath = proofBlock.proof.content.find(
                (block) => typeof block === "object" && "tex" in block,
            );
            expect(proofText).toContain("Thus done");
            expect(proofMath && "tex" in proofMath ? proofMath.tex : "").toContain("x^2");
            expect(proofMath && "number" in proofMath ? proofMath.number : undefined).toBe("1.1");
            expect(proofMath && "numbered" in proofMath ? proofMath.numbered : undefined).toBe(true);
        }
        if (typeof codeBlock === "object" && codeBlock && "code" in codeBlock) {
            expect(codeBlock.code.caption).toContain("bases_fitting.py");
            expect(codeBlock.code.code).toContain("coeffs = fit(values)");
        }
    });
});
