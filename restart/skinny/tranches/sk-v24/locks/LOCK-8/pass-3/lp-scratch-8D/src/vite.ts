/**
 * @mkbabb/latex-paper/vite — Vite plugin entry point.
 *
 * Configurable Vite plugin that parses LaTeX at build time and exposes
 * structured paper content as a virtual module.
 */

import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import type { Plugin } from "vite";
import { parseLatex } from "./grammar/document";
import { parseBibToMap } from "./bibtex/parser";
import {
    createCompiledPaperMetadata,
    parseLatexLogTotalPages,
    parseLatexTocEntries,
    type CompiledTocEntry,
} from "./compiled/metadata";
import { flattenPaperSections } from "./paper/flattenPaperSections";
import { Transformer } from "./transform/html";
import type { TransformOptions } from "./transform/html";

export function buildPageMapFromTocEntries(
    sections: Parameters<typeof flattenPaperSections>[0],
    tocEntries: readonly CompiledTocEntry[],
    warn?: (message: string) => void,
): Record<string, number> {
    const flatSections = flattenPaperSections(sections);
    const pageMap: Record<string, number> = {};
    const pageByNumber = new Map<string, number>();

    for (const entry of tocEntries) {
        if (entry.number) pageByNumber.set(entry.number, entry.page);
    }

    const trackedSections = flatSections.filter(
        (section) =>
            !section.starred &&
            Boolean(section.section.number) &&
            pageByNumber.has(section.section.number),
    );

    if (trackedSections.length !== tocEntries.length) {
        warn?.(
            `latex-paper: TOC page entry mismatch (tracked=${trackedSections.length}, toc=${tocEntries.length}); carrying forward last known page where needed.`,
        );
    }

    let lastPage = tocEntries[0]?.page ?? 1;
    for (let index = 0; index < flatSections.length; index++) {
        const section = flatSections[index];
        const number = section.section.number;

        if (section.starred) {
            pageMap[section.id] = lastPage;
            continue;
        }

        if (!number) {
            const previousPage = lastPage;
            let nextPage = previousPage;
            for (let nextIndex = index + 1; nextIndex < flatSections.length; nextIndex++) {
                const nextNumber = flatSections[nextIndex].section.number;
                if (!nextNumber || flatSections[nextIndex].starred) continue;
                const mappedPage = pageByNumber.get(nextNumber);
                if (mappedPage != null) {
                    nextPage = mappedPage;
                    break;
                }
            }
            pageMap[section.id] =
                nextPage > previousPage ? Math.max(previousPage, nextPage - 1) : previousPage;
            continue;
        }

        const page = pageByNumber.get(number);
        if (page != null) {
            lastPage = page;
        }
        pageMap[section.id] = lastPage;
    }

    return pageMap;
}

export interface LatexPaperPluginOptions {
    /** Path to the .tex file (absolute or relative to project root). */
    texPath: string;
    /** Path to the .bib file. Defaults to texPath with .bib extension. */
    bibPath?: string;
    /** KaTeX macros. */
    macros?: Record<string, string>;
    /** Section callout mapping. */
    callouts?: Record<string, { text: string; link: string }>;
    /** Virtual module ID. Defaults to "virtual:paper-content". */
    virtualModuleId?: string;
}

/**
 * Extract \DeclareMathOperator{cmd}{text} and \newcommand{cmd}{body}
 * definitions from raw TeX source and return them as KaTeX-compatible macros.
 */
function extractTexMacros(texSource: string): Record<string, string> {
    const macros: Record<string, string> = {};
    // \DeclareMathOperator{\cmd}{text} → \operatorname{text}
    for (const m of texSource.matchAll(
        /\\DeclareMathOperator\s*\*?\s*\{\\(\w+)\}\s*\{([^}]*)\}/g,
    )) {
        macros[`\\${m[1]}`] = `\\operatorname{${m[2]}}`;
    }
    // \newcommand{\cmd}[nargs]{body} — only zero-arg definitions
    for (const m of texSource.matchAll(
        /\\(?:re)?newcommand\s*\{\\(\w+)\}\s*(?:\[\d+\]\s*)?\{([^}]*)\}/g,
    )) {
        // Skip if already defined or if it has arg placeholders (#1, #2, ...)
        if (macros[`\\${m[1]}`] || /#\d/.test(m[2])) continue;
        macros[`\\${m[1]}`] = m[2];
    }
    return macros;
}

export default function latexPaperPlugin(
    options: LatexPaperPluginOptions,
): Plugin {
    const virtualId = options.virtualModuleId ?? "virtual:paper-content";
    const resolvedVirtualId = "\0" + virtualId;
    let resolvedTexPath: string;
    let resolvedBibPath: string;

    return {
        name: "vite-plugin-latex-paper",

        configResolved(config) {
            resolvedTexPath = resolve(config.root, options.texPath);
            resolvedBibPath = options.bibPath
                ? resolve(config.root, options.bibPath)
                : resolvedTexPath.replace(/\.tex$/, ".bib");
        },

        resolveId(id) {
            if (id === virtualId) return resolvedVirtualId;
        },

        load(id) {
            if (id !== resolvedVirtualId) return;

            // Watch files for HMR
            this.addWatchFile(resolvedTexPath);
            this.addWatchFile(resolvedBibPath);

            // Also watch .log and .aux for page data
            const logPath = resolvedTexPath.replace(/\.tex$/, ".log");
            const auxPath = resolvedTexPath.replace(/\.tex$/, ".aux");
            const tocPath = resolvedTexPath.replace(/\.tex$/, ".toc");
            const bblPath = resolvedTexPath.replace(/\.tex$/, ".bbl");
            if (existsSync(logPath)) this.addWatchFile(logPath);
            if (existsSync(auxPath)) this.addWatchFile(auxPath);
            if (existsSync(tocPath)) this.addWatchFile(tocPath);
            if (existsSync(bblPath)) this.addWatchFile(bblPath);

            // Parse bibliography
            let bibSource: string;
            try {
                bibSource = readFileSync(resolvedBibPath, "utf-8");
            } catch {
                bibSource = "";
            }
            const bibEntries = parseBibToMap(bibSource);

            // Parse LaTeX
            const texSource = readFileSync(resolvedTexPath, "utf-8");
            const texMacros = extractTexMacros(texSource);
            const allMacros = { ...texMacros, ...options.macros };
            const ast = parseLatex(texSource);

            const requiredArtifacts = [logPath, auxPath, tocPath, bblPath].filter(
                (path) => !existsSync(path),
            );
            if (requiredArtifacts.length > 0) {
                throw new Error(
                    `latex-paper: missing required compiled artifacts for ${resolvedTexPath}: ${requiredArtifacts.join(", ")}`,
                );
            }

            const compiledMetadata = createCompiledPaperMetadata({
                texSource,
                logSource: readFileSync(logPath, "utf-8"),
                auxSource: readFileSync(auxPath, "utf-8"),
                tocSource: readFileSync(tocPath, "utf-8"),
                bblSource: readFileSync(bblPath, "utf-8"),
            });

            if (compiledMetadata.tocEntries.length === 0) {
                throw new Error(`latex-paper: ${tocPath} did not contain any TOC entries.`);
            }

            if (compiledMetadata.totalPages <= 0) {
                throw new Error(`latex-paper: ${logPath} did not yield a valid page count.`);
            }

            // Transform to sections
            const transformOpts: TransformOptions = {
                macros: options.macros,
                callouts: options.callouts,
                bibEntries,
                compiledMetadata,
            };
            const transformer = new Transformer(transformOpts);
            const sections = transformer.transform(ast);
            const labelMap = transformer.labelMap;

            const pageWarnings: string[] = [];
            const pageMap = buildPageMapFromTocEntries(
                sections,
                compiledMetadata.tocEntries,
                (message) => pageWarnings.push(message),
            );
            if (pageWarnings.length > 0) {
                throw new Error(pageWarnings.join(" "));
            }

            return [
                `// Auto-generated from ${resolvedTexPath} — do not edit manually`,
                `export const paperSections = ${JSON.stringify(sections, null, 2)};`,
                `export const labelMap = ${JSON.stringify(labelMap, null, 2)};`,
                `export const totalPages = ${compiledMetadata.totalPages};`,
                `export const pageMap = ${JSON.stringify(pageMap)};`,
                `export const extractedMacros = ${JSON.stringify(allMacros)};`,
            ].join("\n");
        },

        handleHotUpdate({ file, server }) {
            const logPath = resolvedTexPath.replace(/\.tex$/, ".log");
            const auxPath = resolvedTexPath.replace(/\.tex$/, ".aux");
            const tocPath = resolvedTexPath.replace(/\.tex$/, ".toc");
            const bblPath = resolvedTexPath.replace(/\.tex$/, ".bbl");
            if (
                file === resolvedTexPath ||
                file === resolvedBibPath ||
                file === logPath ||
                file === auxPath ||
                file === tocPath ||
                file === bblPath
            ) {
                const mod =
                    server.moduleGraph.getModuleById(resolvedVirtualId);
                if (mod) {
                    server.moduleGraph.invalidateModule(mod);
                    return [mod];
                }
            }
        },
    };
}

// Re-export types that consumers of the plugin might need
export type {
    PaperSectionData,
    PaperTheoremData,
    PaperFigureData,
} from "./types/output";
export {
    parseLatexLogTotalPages as parseTotalPages,
    parseLatexTocEntries as parseLatexTocPages,
} from "./compiled/metadata";
