/**
 * CounterManager — centralized counter state for equations, theorems, sections,
 * and bibliography numbering during the AST→HTML transform.
 */

import type { TheoremNode } from "../types/ast";
import type { PaperLabelInfo, PaperSectionData } from "../types/output";
import type { CompiledPaperMetadata, TheoremCounterConfig } from "../compiled/metadata";

export class CounterManager {
    private compiledMetadata?: CompiledPaperMetadata;

    constructor(compiledMetadata?: CompiledPaperMetadata) {
        this.compiledMetadata = compiledMetadata;
    }

    /** Extract the chapter-level scope from a section number. */
    getEquationScope(number: string, level: number): string {
        if (!number) return "";
        if (level === 0) return number;
        const dot = number.indexOf(".");
        return dot === -1 ? number : number.slice(0, dot);
    }

    /** Compute the next bibliography section number (max chapter + 1). */
    nextBibliographyNumber(topLevel: PaperSectionData[]): string {
        let maxChapter = 0;
        for (const section of topLevel) {
            if (!/^\d+$/.test(section.number)) continue;
            maxChapter = Math.max(maxChapter, Number.parseInt(section.number, 10));
        }
        return maxChapter > 0 ? String(maxChapter + 1) : "";
    }

    /**
     * Synchronize a named counter from a resolved number string.
     * If `number` is provided, parse its last dot-segment and fast-forward the counter.
     * Otherwise, simply increment.
     */
    syncCounterFromNumber(counterMap: Map<string, number>, key: string, number?: string): number {
        const current = counterMap.get(key) ?? 0;
        if (!number) {
            const next = current + 1;
            counterMap.set(key, next);
            return next;
        }

        const dot = number.lastIndexOf(".");
        const suffix = dot === -1 ? number : number.slice(dot + 1);
        const parsed = Number.parseInt(suffix, 10);
        if (Number.isFinite(parsed)) {
            counterMap.set(key, Math.max(current + 1, parsed));
            return Math.max(current + 1, parsed);
        }

        const next = current + 1;
        counterMap.set(key, next);
        return next;
    }

    /** Resolve the next equation number within a scope. */
    resolveEquationNumber(
        counterMap: Map<string, number>,
        scope: string,
        resolvedNumber?: string,
    ): string | undefined {
        if (!scope) return resolvedNumber;
        const next = this.syncCounterFromNumber(counterMap, scope, resolvedNumber);
        return resolvedNumber ?? `${scope}.${next}`;
    }

    /** Resolve the next theorem number, respecting compiled counter config. */
    resolveTheoremNumber(
        node: TheoremNode,
        sectionNumber: string,
        theoremCounters: Map<string, number>,
        resolvedNumber?: string,
    ): string | undefined {
        const config =
            this.compiledMetadata?.theoremCounters.get(node.envType) ??
            ({
                envName: node.envType,
                counterName: node.envType,
                resetWithin: "section",
            } satisfies TheoremCounterConfig);
        const next = this.syncCounterFromNumber(
            theoremCounters,
            config.counterName,
            resolvedNumber,
        );

        if (resolvedNumber) return resolvedNumber;
        if (config.resetWithin === "chapter") {
            const scope = this.getEquationScope(sectionNumber, 0);
            return scope ? `${scope}.${next}` : String(next);
        }
        if (config.resetWithin === "none") return String(next);
        return sectionNumber ? `${sectionNumber}.${next}` : String(next);
    }

    /** Generate a fallback section number when compiled metadata is unavailable. */
    fallbackSectionNumber(
        level: number,
        counters: {
            chapter: number;
            section: number;
            subsection: number;
            subsubsection: number;
        },
    ): string {
        if (level === 0) {
            counters.chapter += 1;
            counters.section = 0;
            counters.subsection = 0;
            counters.subsubsection = 0;
            return String(counters.chapter);
        }
        if (level === 1) {
            if (counters.chapter === 0) {
                counters.section += 1;
                counters.subsection = 0;
                counters.subsubsection = 0;
                return `0.${counters.section}`;
            }
            counters.section += 1;
            counters.subsection = 0;
            counters.subsubsection = 0;
            return `${counters.chapter}.${counters.section}`;
        }
        if (level === 2) {
            counters.subsection += 1;
            counters.subsubsection = 0;
            return `${counters.chapter}.${counters.section}.${counters.subsection}`;
        }
        counters.subsubsection += 1;
        return `${counters.chapter}.${counters.section}.${counters.subsection}.${counters.subsubsection}`;
    }
}
