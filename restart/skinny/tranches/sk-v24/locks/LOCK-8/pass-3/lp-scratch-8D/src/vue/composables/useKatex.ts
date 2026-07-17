import katex from "katex";
import { inject } from "vue";
import { PAPER_CONTEXT } from "../context";
import { createRenderTitle } from "../context";

// Module-level cache: persists across all component instances for the
// lifetime of the page, avoiding redundant katex.renderToString calls.
const cache = new Map<string, string>();

/**
 * KaTeX rendering with module-level cache and configurable macros.
 * If no macros are provided, attempts to use macros from injected PaperContext.
 */
export function useKatex(macros?: Record<string, string>) {
    const resolvedMacros = macros ?? {};

    function renderInline(tex: string): string {
        const key = `i:${tex}`;
        let html = cache.get(key);
        if (html === undefined) {
            html = katex.renderToString(tex, {
                throwOnError: false,
                displayMode: false,
                macros: resolvedMacros,
            });
            cache.set(key, html);
        }
        return html;
    }

    function renderDisplay(tex: string): string {
        const key = `d:${tex}`;
        let html = cache.get(key);
        if (html === undefined) {
            html = katex.renderToString(tex, {
                throwOnError: false,
                displayMode: true,
                macros: resolvedMacros,
            });
            cache.set(key, html);
        }
        return html;
    }

    const renderTitle = createRenderTitle(renderInline);

    return { renderInline, renderDisplay, renderTitle };
}
