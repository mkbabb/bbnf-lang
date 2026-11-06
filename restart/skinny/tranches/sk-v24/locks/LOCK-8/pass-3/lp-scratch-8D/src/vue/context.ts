import type { InjectionKey } from "vue";
import type { PaperSectionData, PaperLabelInfo } from "../types/output";

export interface PaperContext {
    sections: PaperSectionData[];
    labelMap: Record<string, PaperLabelInfo>;
    renderInline: (tex: string) => string;
    renderDisplay: (tex: string) => string;
    renderTitle: (text: string) => string;
    assetBase: string;
    scrollToId: (id: string) => void;
}

export const PAPER_CONTEXT: InjectionKey<PaperContext> = Symbol("paper-context");

/**
 * Creates a renderTitle function that replaces $...$ with KaTeX HTML.
 */
export function createRenderTitle(renderInline: (tex: string) => string) {
    return (text: string) =>
        text.replace(
            /\$([^$]+)\$/g,
            (_, tex) => `<span class="math-inline">${renderInline(tex)}</span>`,
        );
}
