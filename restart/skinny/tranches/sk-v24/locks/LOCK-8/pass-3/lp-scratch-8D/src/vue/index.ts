// Context
export { PAPER_CONTEXT, createRenderTitle } from "./context";
export type { PaperContext } from "./context";

// Generic tracking primitives
export {
    useLazyLoader,
    useTreeIndex,
    useScrollTracker,
    useScrollTo,
    useClickDelegate,
} from "./tracking";
export type {
    TreeNode,
    TreeIndexEntry,
    ScrollTrackerOptions,
    LazyLoaderOptions,
    ScrollToOptions,
    ClickDelegateOptions,
} from "./tracking";

// Paper composables
export { useKatex } from "./composables/useKatex";
export { usePaperReader } from "./composables/usePaperReader";
export { useSidebarFollow } from "./composables/useSidebarFollow";
export { useVirtualSectionWindow } from "./composables/useVirtualSectionWindow";
export { flattenPaperSections } from "../paper/flattenPaperSections";
export type { FlatPaperSection } from "../paper/flattenPaperSections";
export type { SidebarFollowOptions } from "./composables/useSidebarFollow";
export type { VirtualSectionWindowOptions } from "./composables/useVirtualSectionWindow";

// Components
export { default as MathBlock } from "./components/MathBlock.vue";
export { default as MathInline } from "./components/MathInline.vue";
export { default as Theorem } from "./components/Theorem.vue";
export { default as CodeBlock } from "./components/CodeBlock.vue";
export { default as PaperSection } from "./components/PaperSection.vue";
export { default as PaperSectionBlocks } from "./components/PaperSectionBlocks.vue";
export { default as PaperSectionContent } from "./components/PaperSectionContent.vue";
