import type { FlatPaperSection } from "../../paper/flattenPaperSections";

export interface SectionLayoutEntry<T extends FlatPaperSection = FlatPaperSection> {
    item: T;
    height: number;
    top: number;
    bottom: number;
}

export interface SectionLayout<T extends FlatPaperSection = FlatPaperSection> {
    entries: SectionLayoutEntry<T>[];
    totalHeight: number;
}

export interface SectionWindowRange {
    startIndex: number;
    endIndex: number;
    topSpacerPx: number;
    bottomSpacerPx: number;
}

export interface ForcedSectionWindowRange {
    startIndex: number;
    endIndex: number;
}

export function buildSectionLayout<T extends FlatPaperSection>(
    items: readonly T[],
    getHeight: (item: T) => number,
): SectionLayout<T> {
    let offset = 0;
    const entries = items.map((item) => {
        const height = Math.max(1, Math.round(getHeight(item)));
        const top = offset;
        offset += height;
        return {
            item,
            height,
            top,
            bottom: offset,
        };
    });

    return {
        entries,
        totalHeight: offset,
    };
}

function findStartIndex<T extends FlatPaperSection>(
    entries: readonly SectionLayoutEntry<T>[],
    startOffset: number,
): number {
    if (entries.length === 0) return 0;
    let low = 0;
    let high = entries.length - 1;
    while (low < high) {
        const mid = Math.floor((low + high) / 2);
        if (entries[mid].bottom <= startOffset) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    return low;
}

function findEndIndex<T extends FlatPaperSection>(
    entries: readonly SectionLayoutEntry<T>[],
    endOffset: number,
): number {
    if (entries.length === 0) return 0;
    let low = 0;
    let high = entries.length - 1;
    while (low < high) {
        const mid = Math.ceil((low + high) / 2);
        if (entries[mid].top < endOffset) {
            low = mid;
        } else {
            high = mid - 1;
        }
    }
    return low;
}

export function resolveSectionWindow<T extends FlatPaperSection>(
    layout: SectionLayout<T>,
    scrollTopPx: number,
    viewportHeightPx: number,
    overscanBeforePx: number,
    overscanAfterPx: number,
    forcedRange?: ForcedSectionWindowRange | null,
): SectionWindowRange {
    if (layout.entries.length === 0) {
        return {
            startIndex: 0,
            endIndex: -1,
            topSpacerPx: 0,
            bottomSpacerPx: 0,
        };
    }

    const startOffset = Math.max(0, scrollTopPx - overscanBeforePx);
    const endOffset = Math.max(
        0,
        scrollTopPx + viewportHeightPx + overscanAfterPx,
    );

    let startIndex = findStartIndex(layout.entries, startOffset);
    let endIndex = findEndIndex(layout.entries, endOffset);

    if (forcedRange) {
        startIndex = Math.min(startIndex, forcedRange.startIndex);
        endIndex = Math.max(endIndex, forcedRange.endIndex);
    }

    startIndex = Math.max(0, Math.min(startIndex, layout.entries.length - 1));
    endIndex = Math.max(startIndex, Math.min(endIndex, layout.entries.length - 1));

    return {
        startIndex,
        endIndex,
        topSpacerPx: layout.entries[startIndex]?.top ?? 0,
        bottomSpacerPx: Math.max(
            0,
            layout.totalHeight - (layout.entries[endIndex]?.bottom ?? 0),
        ),
    };
}

export function resolveActiveSection<T extends FlatPaperSection>(
    layout: SectionLayout<T>,
    activeOffsetPx: number,
): T | null {
    if (layout.entries.length === 0) return null;

    let low = 0;
    let high = layout.entries.length - 1;
    while (low < high) {
        const mid = Math.ceil((low + high) / 2);
        if (layout.entries[mid].top <= activeOffsetPx) {
            low = mid;
        } else {
            high = mid - 1;
        }
    }

    return layout.entries[low]?.item ?? null;
}

export function findSectionOffset<T extends FlatPaperSection>(
    layout: SectionLayout<T>,
    id: string,
): number | null {
    for (const entry of layout.entries) {
        if (entry.item.id === id) {
            return entry.top;
        }
    }
    return null;
}
