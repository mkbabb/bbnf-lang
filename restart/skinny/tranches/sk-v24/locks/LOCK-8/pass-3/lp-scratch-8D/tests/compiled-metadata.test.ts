import { describe, expect, it } from "vitest";
import {
    createCompiledPaperMetadata,
    parseBibliographyItems,
    parseLatexAuxLabels,
    parseLatexLogTotalPages,
    parseLatexTocEntries,
    parseTheoremCounterConfigs,
} from "../src/compiled/metadata";

describe("compiled LaTeX metadata parsers", () => {
    it("parses TOC entries with intro numbering and appendix lettering", () => {
        const entries = parseLatexTocEntries(`
\\contentsline {section}{\\numberline {0.1}Introduction}{4}{section.0.1}%
\\contentsline {chapter}{\\numberline {A}Extended Proofs}{83}{chapter.A}%
\\contentsline {section}{\\numberline {A.1}Sturm-Liouville Completeness}{84}{section.A.1}%
        `);

        expect(entries).toEqual([
            {
                level: "section",
                number: "0.1",
                title: "Introduction",
                page: 4,
                anchor: "section.0.1",
            },
            {
                level: "chapter",
                number: "A",
                title: "Extended Proofs",
                page: 83,
                anchor: "chapter.A",
            },
            {
                level: "section",
                number: "A.1",
                title: "Sturm-Liouville Completeness",
                page: 84,
                anchor: "section.A.1",
            },
        ]);
    });

    it("parses aux labels, bibliography items, theorem counters, and log page totals", () => {
        const labels = parseLatexAuxLabels(`
\\newlabel{thm:sturm_completeness}{{1.2.2}{10}{Completeness of Sturm-Liouville Eigenfunctions}{equation.1.46}{}}
\\newlabel{sec:transform_pipeline}{{2.6.5}{32}{The Transform Pipeline}{subsection.2.6.5}{}}
        `);
        const bibliography = parseBibliographyItems(`
\\begin{thebibliography}{10}
\\bibitem{heat_fourier}
Jean-Baptiste~Joseph Fourier.
\\newblock {\\em Th\'{e}orie analytique de la chaleur}.
\\end{thebibliography}
        `);
        const theoremCounters = parseTheoremCounterConfigs(`
\\newtheorem{theorem}{Theorem}[section]
\\newtheorem{corollary}{Corollary}[theorem]
\\newtheorem{lemma}[theorem]{Lemma}
\\newtheorem{definition}{Definition}[section]
        `);
        const pages = parseLatexLogTotalPages("Output written on paper.pdf (97 pages, 12345 bytes).");

        expect(labels.get("thm:sturm_completeness")?.number).toBe("1.2.2");
        expect(labels.get("sec:transform_pipeline")?.anchor).toBe("subsection.2.6.5");
        expect(bibliography).toHaveLength(1);
        expect(bibliography[0]?.key).toBe("heat_fourier");
        expect(bibliography[0]?.body).toContain("Jean-Baptiste~Joseph Fourier.");
        expect(bibliography[0]?.body).toContain("\\newblock");
        expect(bibliography[0]?.body).toContain("analytique de la chaleur");
        expect(theoremCounters.get("theorem")).toEqual({
            envName: "theorem",
            counterName: "theorem",
            resetWithin: "section",
        });
        expect(theoremCounters.get("corollary")).toEqual({
            envName: "corollary",
            counterName: "theorem",
            resetWithin: "section",
        });
        expect(theoremCounters.get("lemma")).toEqual({
            envName: "lemma",
            counterName: "theorem",
            resetWithin: "section",
        });
        expect(pages).toBe(97);
    });

    it("builds a complete compiled metadata bundle", () => {
        const metadata = createCompiledPaperMetadata({
            texSource: "\\newtheorem{theorem}{Theorem}[section]",
            tocSource: "\\contentsline {section}{\\numberline {0.1}Introduction}{4}{section.0.1}%",
            auxSource: "\\newlabel{sec:intro}{{0.1}{4}{Introduction}{section.0.1}{}}",
            bblSource: "\\begin{thebibliography}{1}\\bibitem{foo}Foo.\\end{thebibliography}",
            logSource: "Output written on paper.pdf (12 pages, 42 bytes).",
        });

        expect(metadata.tocEntries[0]?.number).toBe("0.1");
        expect(metadata.labels.get("sec:intro")?.number).toBe("0.1");
        expect(metadata.bibliography[0]?.key).toBe("foo");
        expect(metadata.theoremCounters.get("theorem")?.resetWithin).toBe("section");
        expect(metadata.totalPages).toBe(12);
    });
});
