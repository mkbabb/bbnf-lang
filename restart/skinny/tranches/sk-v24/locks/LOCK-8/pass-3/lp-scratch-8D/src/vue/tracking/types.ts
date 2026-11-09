import type { Ref } from "vue";

/** Minimal interface for tree-structured content with scroll targets. */
export interface TreeNode {
    id: string;
    children?: TreeNode[];
}

/** Flat index entry for a tree node. */
export interface TreeIndexEntry<T extends TreeNode = TreeNode> {
    node: T;
    depth: number;
    /** ID of the root-level ancestor (self.id when depth === 0). */
    rootId: string;
    /** Direct parent ID (null for root nodes). */
    parentId: string | null;
    /** Index within root-level nodes. */
    rootIndex: number;
}

/** Options for scroll tracking. */
export interface ScrollTrackerOptions {
    /** IntersectionObserver rootMargin. Default: "-20% 0px -60% 0px" */
    rootMargin?: string;
    threshold?: number;
}

/** Options for lazy loading. */
export interface LazyLoaderOptions {
    /** Sections per batch. Default: 2 */
    batchSize?: number;
    /** IntersectionObserver rootMargin. Default: "0px 0px 600px 0px" */
    rootMargin?: string;
}

/** Options for scroll-to. */
export interface ScrollToOptions {
    scrollContainer: Ref<HTMLElement | null>;
    totalCount: number;
    visibleCount: Ref<number>;
    /** Pixel offset from top. Default: 16 */
    scrollOffset?: number;
    /** Max rAF retry attempts. Default: 60 */
    maxAttempts?: number;
    /** Tree index for resolving target IDs to root section indices.
     *  When provided, scrollTo loads only up to the target section instead of all content. */
    treeIndex?: Map<string, TreeIndexEntry>;
}

/** Options for click delegation. */
export interface ClickDelegateOptions {
    /** Container element to attach the delegated listener to. */
    container: Ref<HTMLElement | null>;
    /** CSS selector for clickable elements. Default: "[data-scroll-target]" */
    selector?: string;
    /** Attribute to read target ID from. Default: "data-scroll-target" */
    attribute?: string;
    /** Maps the attribute value to a scroll target ID. Return null to skip. */
    resolve: (value: string) => string | null;
    /** Called with the resolved ID. */
    scrollTo: (id: string) => void;
}
