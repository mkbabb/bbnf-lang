import { describe, expect, it } from "vitest";
import {
    flattenPaperSections,
    estimatePaperSectionHeight,
} from "../src/paper/flattenPaperSections";
import type { PaperSectionData } from "../src/types/output";

function makeSection(
    overrides: Partial<PaperSectionData> & Pick<PaperSectionData, "id" | "number" | "title">,
): PaperSectionData {
    return {
        content: [],
        ...overrides,
    };
}

describe("flattenPaperSections", () => {
    it("preserves hierarchy metadata in depth-first order", () => {
        const sections: PaperSectionData[] = [
            makeSection({
                id: "intro",
                number: "0.1",
                title: "Introduction",
                content: ["Warm-up paragraph"],
                subsections: [
                    makeSection({
                        id: "intro-sub",
                        number: "0.1.1",
                        title: "Prelude",
                        sourceLevel: 2,
                        content: ["A smaller subsection"],
                    }),
                ],
            }),
            makeSection({
                id: "chapter",
                number: "1",
                title: "Main Chapter",
                sourceLevel: 0,
                content: ["Chapter text"],
                subsections: [
                    makeSection({
                        id: "section",
                        number: "1.1",
                        title: "Main Section",
                        sourceLevel: 1,
                        content: [{ tex: "x^2 + y^2 = 1" }],
                        subsections: [
                            makeSection({
                                id: "subsub",
                                number: "1.1.1",
                                title: "Deep Dive",
                                sourceLevel: 3,
                                content: [
                                    {
                                        figure: {
                                            filename: "deep.png",
                                            caption: "A deep figure",
                                        },
                                    },
                                ],
                                callout: {
                                    text: "Open the visualizer",
                                    link: "/visualize",
                                },
                            }),
                        ],
                    }),
                ],
            }),
        ];

        const flat = flattenPaperSections(sections);

        expect(
            flat.map((section) => ({
                id: section.id,
                index: section.index,
                depth: section.depth,
                sourceLevel: section.sourceLevel,
                parentId: section.parentId,
                rootId: section.rootId,
                rootIndex: section.rootIndex,
            })),
        ).toEqual([
            {
                id: "intro",
                index: 0,
                depth: 0,
                sourceLevel: 0,
                parentId: null,
                rootId: "intro",
                rootIndex: 0,
            },
            {
                id: "intro-sub",
                index: 1,
                depth: 1,
                sourceLevel: 2,
                parentId: "intro",
                rootId: "intro",
                rootIndex: 0,
            },
            {
                id: "chapter",
                index: 2,
                depth: 0,
                sourceLevel: 0,
                parentId: null,
                rootId: "chapter",
                rootIndex: 1,
            },
            {
                id: "section",
                index: 3,
                depth: 1,
                sourceLevel: 1,
                parentId: "chapter",
                rootId: "chapter",
                rootIndex: 1,
            },
            {
                id: "subsub",
                index: 4,
                depth: 2,
                sourceLevel: 3,
                parentId: "section",
                rootId: "chapter",
                rootIndex: 1,
            },
        ]);
    });

    it("produces stable non-zero section height estimates", () => {
        const section = makeSection({
            id: "estimated",
            number: "2.4",
            title: "Estimated Section",
            content: [
                "A short proof sketch with enough prose to create multiple estimate rows in the heuristic.",
                { tex: "\\int_0^1 f(x)\\,dx" },
                {
                    theorem: {
                        type: "theorem",
                        content: [
                            "Every compact operator has a spectral decomposition.",
                            { tex: "Tf = \\lambda f" },
                        ],
                    },
                },
                {
                    figure: {
                        filename: "spectrum.png",
                        caption: "A rendered spectral plot.",
                    },
                },
                {
                    proof: {
                        content: [
                            "A proof body with enough prose to matter.",
                            { tex: "A_n \\to A" },
                        ],
                    },
                },
                {
                    code: {
                        code: "def fit(signal):\n    return signal",
                        caption: "A compact fitting helper.",
                    },
                },
            ],
            callout: {
                text: "Inspect the numerical version",
                link: "/visualize",
            },
        });

        const estimate = estimatePaperSectionHeight(section, 1);
        expect(estimate).toBeGreaterThan(300);
        expect(estimate).toBeLessThan(1800);
    });
});
