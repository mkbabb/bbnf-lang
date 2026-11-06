import { describe, expect, it } from "vitest";
import type { FlatPaperSection } from "../src/paper/flattenPaperSections";
import {
    buildSectionLayout,
    findSectionOffset,
    resolveActiveSection,
    resolveSectionWindow,
} from "../src/vue/composables/virtualSectionLayout";
import type { PaperSectionData } from "../src/types/output";

function makeItem(
    id: string,
    index: number,
    estimatedHeight: number,
): FlatPaperSection {
    return {
        id,
        index,
        depth: index === 0 ? 0 : 1,
        sourceLevel: index === 0 ? 0 : 1,
        parentId: index === 0 ? null : "root",
        rootId: "root",
        rootIndex: 0,
        estimatedHeight,
        section: {
            id,
            number: `1.${index}`,
            title: id,
            sourceLevel: index === 0 ? 0 : 1,
            content: [],
        } satisfies PaperSectionData,
    };
}

describe("virtualSectionLayout", () => {
    it("calculates bounded windows and spacers from mixed heights", () => {
        const items = [
            makeItem("root", 0, 220),
            makeItem("alpha", 1, 260),
            makeItem("beta", 2, 180),
            makeItem("gamma", 3, 240),
        ];
        const measured = new Map<string, number>([
            ["alpha", 420],
            ["gamma", 320],
        ]);

        const layout = buildSectionLayout(
            items,
            (item) => measured.get(item.id) ?? item.estimatedHeight,
        );

        expect(layout.totalHeight).toBe(1140);
        expect(layout.entries.map((entry) => entry.height)).toEqual([
            220,
            420,
            180,
            320,
        ]);

        const window = resolveSectionWindow(layout, 500, 280, 80, 80);
        expect(window).toEqual({
            startIndex: 1,
            endIndex: 3,
            topSpacerPx: 220,
            bottomSpacerPx: 0,
        });

        expect(findSectionOffset(layout, "beta")).toBe(640);
    });

    it("keeps forced target neighborhoods mounted and resolves the active section", () => {
        const items = [
            makeItem("root", 0, 180),
            makeItem("a", 1, 260),
            makeItem("b", 2, 260),
            makeItem("c", 3, 260),
            makeItem("d", 4, 260),
        ];
        const layout = buildSectionLayout(items, (item) => item.estimatedHeight);

        const window = resolveSectionWindow(
            layout,
            650,
            220,
            40,
            40,
            { startIndex: 0, endIndex: 2 },
        );
        expect(window.startIndex).toBe(0);
        expect(window.endIndex).toBeGreaterThanOrEqual(2);

        expect(resolveActiveSection(layout, 40)?.id).toBe("root");
        expect(resolveActiveSection(layout, 700)?.id).toBe("c");
        expect(resolveActiveSection(layout, 1200)?.id).toBe("d");
    });
});
