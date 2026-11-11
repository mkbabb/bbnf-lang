/**
 * CommandRenderer — map-based dispatch for \command → HTML rendering.
 */

import type { CommandNode, LatexNode } from "../types/ast";
import type { BibEntry } from "../types/bibtex";
import type { CompiledBibliographyItem, CompiledPaperMetadata } from "../compiled/metadata";
import { parseInlineString } from "../grammar/document";

/** Dependencies that command renderers need without coupling to the full Transformer. */
export interface RenderContext {
    /** Resolve a label key to its info (number + type). */
    resolveLabel(key: string): { number: string; type?: string } | undefined;
    /** Convert an array of AST nodes to HTML string. */
    nodesToHtml(nodes: LatexNode[]): string;
    /** Record citation keys in order. */
    recordCitations(keys: string[]): void;
    /** The bibliography entries map. */
    bibEntries: Map<string, BibEntry>;
    /** Cited keys in order. */
    citedKeys: string[];
    /** Compiled metadata (optional). */
    compiledMetadata?: CompiledPaperMetadata;
}

type CommandRenderFn = (node: CommandNode, ctx: RenderContext) => string;

export class CommandRenderer {
    private renderers = new Map<string, CommandRenderFn>();
    private ctx: RenderContext;

    constructor(ctx: RenderContext) {
        this.ctx = ctx;
        this.registerDefaults();
    }

    /** Register a custom command renderer. */
    register(name: string, render: CommandRenderFn): void {
        this.renderers.set(name, render);
    }

    /** Render a command node to HTML. */
    render(node: CommandNode): string {
        const renderer = this.renderers.get(node.name);
        if (renderer) return renderer(node, this.ctx);
        // Unknown command: try to render first arg
        return node.args.length > 0 ? this.arg(node, 0) : "";
    }

    /** Update the render context (e.g. when citation state changes). */
    updateContext(ctx: RenderContext): void {
        this.ctx = ctx;
    }

    private arg(node: CommandNode, i: number): string {
        return node.args[i] ? this.ctx.nodesToHtml(node.args[i]) : "";
    }

    private registerDefaults(): void {
        const self = this;

        // ── Formatting ────────────────────────────────────────────
        const italicRender: CommandRenderFn = (node) =>
            `<em>${self.arg(node, 0)}</em>`;
        this.renderers.set("textit", italicRender);
        this.renderers.set("emph", italicRender);
        this.renderers.set("mathit", italicRender);

        const boldRender: CommandRenderFn = (node) =>
            `<strong>${self.arg(node, 0)}</strong>`;
        this.renderers.set("textbf", boldRender);
        this.renderers.set("mathbf", boldRender);

        this.renderers.set("texttt", (node) =>
            `<code class="paper-code">${self.arg(node, 0)}</code>`,
        );

        const plainRender: CommandRenderFn = (node) => self.arg(node, 0);
        this.renderers.set("text", plainRender);
        this.renderers.set("mathrm", plainRender);

        this.renderers.set("underline", (node) =>
            `<u>${self.arg(node, 0)}</u>`,
        );

        this.renderers.set("paragraph", (node) =>
            `<strong>${self.arg(node, 0)}</strong> `,
        );

        // ── Links ─────────────────────────────────────────────────
        this.renderers.set("url", (node) => {
            const url = self.arg(node, 0);
            return `<a href="${url}" target="_blank" rel="noopener" class="text-primary hover:underline">${url}</a>`;
        });

        this.renderers.set("href", (node) =>
            `<a href="${self.arg(node, 0)}" target="_blank" rel="noopener" class="text-primary hover:underline">${self.arg(node, 1)}</a>`,
        );

        // ── References ────────────────────────────────────────────
        this.renderers.set("ref", (node, ctx) => {
            const refKey = self.arg(node, 0).trim();
            const refInfo = ctx.resolveLabel(refKey);
            if (!refInfo) return "";
            return `<a class="paper-ref" data-ref="${refKey}">${refInfo.number}</a>`;
        });

        this.renderers.set("eqref", (node, ctx) => {
            const eqKey = self.arg(node, 0).trim();
            const eqInfo = ctx.resolveLabel(eqKey);
            if (!eqInfo) return "";
            return `<a class="paper-ref" data-ref="${eqKey}">(${eqInfo.number})</a>`;
        });

        const autorefRender: CommandRenderFn = (node, ctx) => {
            const autoKey = self.arg(node, 0).trim();
            const autoInfo = ctx.resolveLabel(autoKey);
            if (!autoInfo) return "";
            const LABEL_TYPE_PREFIXES: Record<string, string> = {
                theorem: "Theorem",
                definition: "Definition",
                figure: "Figure",
                equation: "Equation",
                section: "\u00a7",
                chapter: "Chapter",
                lemma: "Lemma",
                corollary: "Corollary",
                proposition: "Proposition",
                example: "Example",
                remark: "Remark",
                table: "Table",
            };
            const prefix = (autoInfo.type ? LABEL_TYPE_PREFIXES[autoInfo.type] : undefined) ?? "";
            const sep = prefix ? "\u00a0" : "";
            return `<a class="paper-ref" data-ref="${autoKey}">${prefix}${sep}${autoInfo.number}</a>`;
        };
        this.renderers.set("cref", autorefRender);
        this.renderers.set("autoref", autorefRender);

        this.renderers.set("hyperref", (node, ctx) => {
            const hKey = node.optArgs?.[0]
                ? ctx.nodesToHtml(node.optArgs[0]).trim()
                : "";
            const hText = self.arg(node, 0);
            if (hKey) {
                return `<a class="paper-ref" data-ref="${hKey}">${hText}</a>`;
            }
            return hText;
        });

        // ── Citations ─────────────────────────────────────────────
        this.renderers.set("cite", (node, ctx) => {
            const keys = self.arg(node, 0).trim().split(/\s*,\s*/);
            ctx.recordCitations(keys);
            const parts = keys
                .map((k) => {
                    const entry = ctx.bibEntries.get(k);
                    return entry ? `${entry.shortAuthor}, ${entry.year}` : null;
                })
                .filter(Boolean);
            return parts.length > 0
                ? `<cite class="paper-cite">[${parts.join("; ")}]</cite>`
                : "";
        });

        this.renderers.set("bibliography", (_node, ctx) =>
            renderBibliography(ctx),
        );

        // ── Misc ──────────────────────────────────────────────────
        this.renderers.set("footnote", (node) =>
            `<span class="paper-footnote">(${self.arg(node, 0)})</span>`,
        );

        this.renderers.set("item", () => ""); // Handled by list extraction
        this.renderers.set("includegraphics", () => ""); // Handled by figure extraction
        this.renderers.set("caption", () => ""); // Handled by figure extraction
    }
}

// ── Bibliography helpers (shared with Transformer) ────────────────────

function escapeHtml(value: string): string {
    return value
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;");
}

function formatBibliographyEntry(entry: BibEntry): string {
    const parts: string[] = [];
    if (entry.author) {
        parts.push(`<span class="paper-bib-authors">${escapeHtml(entry.author)}</span>`);
    }
    if (entry.title) {
        parts.push(`<span class="paper-bib-title">${escapeHtml(entry.title)}</span>`);
    }

    const venue = entry.fields.journal || entry.fields.booktitle || entry.fields.publisher;
    if (venue) {
        parts.push(`<span class="paper-bib-venue">${escapeHtml(venue)}</span>`);
    }
    if (entry.year) {
        parts.push(`<span class="paper-bib-year">${escapeHtml(entry.year)}</span>`);
    }
    if (entry.fields.url) {
        const url = escapeHtml(entry.fields.url);
        parts.push(
            `<a class="paper-bib-link" href="${url}" target="_blank" rel="noopener">${url}</a>`,
        );
    }

    return parts.join(". ");
}

/** Render bibliography from bib entries (non-compiled path). */
export function renderBibliography(ctx: RenderContext): string {
    if (ctx.compiledMetadata?.bibliography.length) {
        return "";
    }

    const orderedEntries =
        ctx.citedKeys.length > 0
            ? ctx.citedKeys
                  .map((key) => ctx.bibEntries.get(key))
                  .filter((entry): entry is BibEntry => Boolean(entry))
            : [...ctx.bibEntries.values()];

    if (orderedEntries.length === 0) return "";

    const items = orderedEntries
        .map((entry) => `<li>${formatBibliographyEntry(entry)}</li>`)
        .join("");

    return `<div class="paper-bibliography"><ol class="paper-bibliography-list">${items}</ol></div>`;
}

/** Render compiled bibliography from .bbl data. */
export function renderCompiledBibliography(
    compiledMetadata: CompiledPaperMetadata | undefined,
    nodesToHtml: (nodes: LatexNode[]) => string,
): string {
    const items = (compiledMetadata?.bibliography ?? [])
        .map((item) => `<li>${renderBibliographyItem(item, nodesToHtml)}</li>`)
        .join("");

    if (!items) return "";
    return `<div class="paper-bibliography"><ol class="paper-bibliography-list">${items}</ol></div>`;
}

function renderBibliographyItem(
    item: CompiledBibliographyItem,
    nodesToHtml: (nodes: LatexNode[]) => string,
): string {
    const normalized = item.body
        .replaceAll("\\newblock", " ")
        .replaceAll("\n", " ")
        .trim();
    return nodesToHtml(parseInlineString(normalized));
}
