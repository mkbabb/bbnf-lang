import type { TreeNode, TreeIndexEntry } from "./types";

/**
 * Builds a flat index of all nodes in a tree for O(1) lookup with hierarchy metadata.
 */
export function useTreeIndex<T extends TreeNode>(
    roots: T[],
    options?: { getChildren?: (node: T) => T[] | undefined },
) {
    const getChildren = options?.getChildren ?? ((n: T) => n.children as T[] | undefined);
    const index = new Map<string, TreeIndexEntry<T>>();

    function walk(
        list: T[],
        depth: number,
        parentId: string | null,
        rootId: string,
        rootIndex: number,
    ) {
        for (const node of list) {
            const ri = depth === 0 ? roots.indexOf(node) : rootIndex;
            const rid = depth === 0 ? node.id : rootId;
            index.set(node.id, {
                node,
                depth,
                rootId: rid,
                parentId: depth === 0 ? node.id : parentId,
                rootIndex: ri,
            });
            const children = getChildren(node);
            if (children) {
                walk(children, depth + 1, depth === 0 ? node.id : parentId, rid, ri);
            }
        }
    }
    walk(roots, 0, null, "", 0);

    function isActive(id: string, activeId: string | null): boolean {
        return id === activeId;
    }

    function isInActiveChain(id: string, activeId: string | null): boolean {
        if (!activeId) return false;
        const entry = index.get(activeId);
        if (!entry) return false;
        if (id === activeId) return true;
        if (id === entry.parentId) return true;
        const target = index.get(id);
        if (!target) return false;
        return isDescendant(activeId, id);
    }

    function isDescendant(childId: string, ancestorId: string): boolean {
        const ancestor = index.get(ancestorId);
        if (!ancestor) return false;
        const children = getChildren(ancestor.node);
        if (!children) return false;
        for (const child of children) {
            if (child.id === childId) return true;
            if (isDescendant(childId, child.id)) return true;
        }
        return false;
    }

    return { index, isActive, isInActiveChain, isDescendant };
}
