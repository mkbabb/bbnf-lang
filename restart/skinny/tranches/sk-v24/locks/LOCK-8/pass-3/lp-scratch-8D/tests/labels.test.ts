import { describe, it, expect } from "vitest";
import { LabelRegistry } from "../src/transform/labels";
import type { LatexNode } from "../src/types/ast";

describe("LabelRegistry", () => {
    it("collects section labels", () => {
        const registry = new LabelRegistry();
        const nodes: LatexNode[] = [
            {
                type: "section",
                level: "chapter",
                starred: false,
                title: [
                    { type: "text", value: "Intro" },
                    { type: "label", key: "ch:intro" },
                ],
            },
        ];
        registry.collectLabels(nodes);
        const info = registry.resolve("ch:intro");
        expect(info).toBeDefined();
        expect(info!.type).toBe("section");
        expect(info!.number).toBe("1");
    });

    it("collects theorem labels", () => {
        const registry = new LabelRegistry();
        const nodes: LatexNode[] = [
            {
                type: "section",
                level: "chapter",
                starred: false,
                title: [{ type: "text", value: "Ch" }],
            },
            {
                type: "theorem",
                envType: "theorem",
                body: [
                    { type: "text", value: "Statement" },
                    { type: "label", key: "thm:main" },
                ],
            },
        ];
        registry.collectLabels(nodes);
        const info = registry.resolve("thm:main");
        expect(info).toBeDefined();
        expect(info!.type).toBe("theorem");
        expect(info!.number).toBe("1.1");
    });

    it("collects figure labels", () => {
        const registry = new LabelRegistry();
        const nodes: LatexNode[] = [
            {
                type: "section",
                level: "chapter",
                starred: false,
                title: [{ type: "text", value: "Ch" }],
            },
            {
                type: "figure",
                filename: "test.png",
                caption: [{ type: "text", value: "A figure" }],
                label: "fig:test",
            },
        ];
        registry.collectLabels(nodes);
        const info = registry.resolve("fig:test");
        expect(info).toBeDefined();
        expect(info!.type).toBe("figure");
    });

    it("resets counters on new chapter", () => {
        const registry = new LabelRegistry();
        const nodes: LatexNode[] = [
            {
                type: "section",
                level: "chapter",
                starred: false,
                title: [{ type: "text", value: "Ch1" }],
            },
            {
                type: "theorem",
                envType: "theorem",
                body: [
                    { type: "label", key: "thm:a" },
                ],
            },
            {
                type: "section",
                level: "chapter",
                starred: false,
                title: [{ type: "text", value: "Ch2" }],
            },
            {
                type: "theorem",
                envType: "theorem",
                body: [
                    { type: "label", key: "thm:b" },
                ],
            },
        ];
        registry.collectLabels(nodes);
        expect(registry.resolve("thm:a")!.number).toBe("1.1");
        expect(registry.resolve("thm:b")!.number).toBe("2.1");
    });
});
