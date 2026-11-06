import { describe, expect, it } from "vitest";
import { parseLatex } from "../src/grammar/document";
import { makeDoc } from "./helpers";
import { transformDocument } from "../src/transform/html";
import {
    buildPageMapFromTocEntries,
    parseLatexTocPages,
} from "../src/vite";
import { flattenPaperSections } from "../src/paper/flattenPaperSections";

describe("TOC-based page mapping", () => {
    it("maps ordered page numbers onto math-heavy section titles", () => {
        const source = makeDoc(`
\\section{Introduction}
Intro text.
\\chapter{Lens I: Linear Algebra}
\\section{Alternative Orthogonal Bases}
Body text.
\\subsection{The Transform Pipeline: Fourier $\\leftrightarrow$ Chebyshev $\\leftrightarrow$ Legendre}
Body text.
\\subsection{Lagrange Interpolation}
Body text.
\\subsection{Gaussian Quadrature}
Body text.
\\section{Fourier Series; Real and Complex Forms}
Body text.
\\subsection*{Pointwise Convergence}
Body text.
\\subsection*{$\\mathbf{L}^2$ Convergence}
Body text.
\\subsection*{The Gibbs Phenomenon}
Body text.
\\subsection*{Almost Everywhere Convergence}
Body text.
\\subsection{Extending the Result to $\\mathbf{L^2}[-L, L]$}
Body text.
        `);

        const { sections } = transformDocument(parseLatex(source));
        const tocEntries = parseLatexTocPages(`
\\@writefile{toc}{\\contentsline {section}{\\numberline {0.1}Introduction}{4}{section.0.1}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {chapter}{\\numberline {1}Lens I: Linear Algebra}{17}{chapter.1}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {section}{\\numberline {1.1}Alternative Orthogonal Bases}{26}{section.1.1}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {subsection}{\\numberline {1.1.1}The Transform Pipeline: Fourier $\\leftrightarrow $ Chebyshev $\\leftrightarrow $ Legendre}{32}{subsection.1.1.1}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {subsection}{\\numberline {1.1.2}Lagrange Interpolation}{32}{subsection.1.1.2}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {subsection}{\\numberline {1.1.3}Gaussian Quadrature}{33}{subsection.1.1.3}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {section}{\\numberline {1.2}Fourier Series; Real and Complex Forms}{35}{section.1.2}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {subsection}{\\numberline {1.2.1}Extending the Result to $\\mathbf  {L^2}[-L, L]$}{38}{subsection.1.2.1}\\protected@file@percent }
        `);

        const warnings: string[] = [];
        const pageMap = buildPageMapFromTocEntries(
            sections,
            tocEntries,
            (message) => warnings.push(message),
        );
        const flat = flattenPaperSections(sections);

        const transformPipeline = flat.find((section) =>
            section.section.title.includes("Transform Pipeline"),
        );
        const introSection = flat.find((section) =>
            section.section.title === "Introduction",
        );
        const chapterSection = flat.find((section) =>
            section.section.title === "Lens I: Linear Algebra",
        );
        const orthogonalBases = flat.find((section) =>
            section.section.title === "Alternative Orthogonal Bases",
        );
        const carriedSections = flat.filter((section) => section.starred);
        const l2Section = flat.find((section) =>
            section.section.title.includes("Extending the Result"),
        );

        expect(warnings).toEqual([]);
        expect(introSection).toBeDefined();
        expect(chapterSection).toBeDefined();
        expect(orthogonalBases).toBeDefined();
        expect(transformPipeline).toBeDefined();
        expect(l2Section).toBeDefined();
        expect(carriedSections).toHaveLength(4);
        expect(flat).toHaveLength(tocEntries.length + carriedSections.length);
        expect(pageMap[introSection!.id]).toBe(4);
        expect(pageMap[chapterSection!.id]).toBe(17);
        expect(pageMap[orthogonalBases!.id]).toBe(26);
        expect(pageMap[transformPipeline!.id]).toBe(32);
        expect(carriedSections.map((section) => pageMap[section.id])).toEqual([
            35,
            35,
            35,
            35,
        ]);
        expect(pageMap[l2Section!.id]).toBe(38);
    });

    it("carries forward the last known page for sections absent from the compiled TOC", () => {
        const source = makeDoc(`
\\section{Introduction}
\\chapter{Chapter}
\\section{Section}
\\subsection{Subsection}
\\subsection{Missing TOC Entry}
        `);
        const { sections } = transformDocument(parseLatex(source));
        const warnings: string[] = [];
        const pageMap = buildPageMapFromTocEntries(
            sections,
            parseLatexTocPages(`
\\@writefile{toc}{\\contentsline {section}{\\numberline {0.1}Introduction}{4}{section.0.1}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {chapter}{\\numberline {1}Chapter}{5}{chapter.1}\\protected@file@percent }
\\@writefile{toc}{\\contentsline {section}{\\numberline {1.1}Section}{6}{section.1.1}\\protected@file@percent }
            `),
            (message) => warnings.push(message),
        );
        const flat = flattenPaperSections(sections);
        const trackedTrailing = flat.find((section) =>
            section.section.title === "Missing TOC Entry",
        );

        expect(warnings).toHaveLength(0);
        expect(trackedTrailing).toBeDefined();
        expect(pageMap[trackedTrailing!.id]).toBe(6);
    });
});
