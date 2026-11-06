import { inject } from "vue";
import type { Ref } from "vue";
import type { PaperSectionData } from "../../types/output";
import type { TreeNode } from "../tracking/types";
import { PAPER_CONTEXT, type PaperContext } from "../context";
import { useLazyLoader } from "../tracking/useLazyLoader";
import { useTreeIndex } from "../tracking/useTreeIndex";
import { useScrollTracker } from "../tracking/useScrollTracker";
import { useScrollTo } from "../tracking/useScrollTo";
import { useClickDelegate } from "../tracking/useClickDelegate";

/** Recursively adapt PaperSectionData → TreeNode. */
function paperToTree(section: PaperSectionData): TreeNode {
    return {
        id: section.id,
        children: section.subsections?.map(paperToTree),
    };
}

/**
 * Convenience composable that wires generic tracking primitives to PaperContext.
 * Accepts PaperContext directly, or injects it from an ancestor provider.
 */
export function usePaperReader(options?: {
    context?: PaperContext;
    batchSize?: number;
    scrollContainer?: Ref<HTMLElement | null>;
    sidebarEl?: Ref<HTMLElement | null>;
}) {
    const ctx = options?.context ?? inject(PAPER_CONTEXT);
    if (!ctx) {
        throw new Error(
            "usePaperReader requires PaperContext. Pass it via options.context or provide(PAPER_CONTEXT, ...).",
        );
    }

    const { sections, labelMap, renderInline, renderDisplay, renderTitle } = ctx;

    // Lazy loading
    const scrollContainer = options?.scrollContainer ?? { value: null } as Ref<HTMLElement | null>;
    const { visibleCount, loadSentinel } = useLazyLoader(sections.length, {
        batchSize: options?.batchSize,
        scrollContainer,
    });

    // Tree index (adapted from paper sections)
    const treeNodes = sections.map(paperToTree);
    const { index: treeIndex, isActive, isInActiveChain } = useTreeIndex(
        treeNodes,
    );

    // Scroll tracking
    const { activeId, activeRootId, forceRecalculate, lockTracking, unlockTracking } =
        useScrollTracker(treeNodes, treeIndex, visibleCount, {
            sidebarEl: options?.sidebarEl,
            scrollContainer,
        });
    const { scrollTo } = useScrollTo({
        scrollContainer,
        totalCount: sections.length,
        visibleCount,
        treeIndex,
    });

    // Cross-reference click delegation
    useClickDelegate({
        container: scrollContainer,
        selector: ".paper-ref",
        attribute: "data-ref",
        resolve: (refKey) => {
            const info = labelMap[refKey];
            if (!info) return null;
            return info.anchorId ?? info.elementId ?? info.sectionId;
        },
        scrollTo,
    });

    return {
        sections,
        visibleCount,
        loadSentinel,
        treeIndex,
        isActive,
        isInActiveChain,
        activeId,
        activeRootId,
        scrollTo,
        forceRecalculate,
        lockTracking,
        unlockTracking,
        renderInline,
        renderDisplay,
        renderTitle,
    };
}
