import { computed, type Ref } from "vue";
import type { UseDebugSessionReturn } from "@/components/debug/useDebugSession";

export function usePlaygroundDebug(
    grammarText: Ref<string>,
    debugSession: UseDebugSessionReturn,
) {
    // Single-pass rule definition map: line (1-based) <-> rule name.
    // Computed once from grammar text; all breakpoint logic derives from this.
    const ruleLineMap = computed(() => {
        const lineToName = new Map<number, string>();
        const nameToLine = new Map<string, number>();
        const lineArr = grammarText.value.split("\n");
        for (let i = 0; i < lineArr.length; i++) {
            const m = lineArr[i]!.match(/^\s*([a-zA-Z_][\w-]*)\s*=/);
            if (m) {
                const line = i + 1;
                lineToName.set(line, m[1]!);
                nameToLine.set(m[1]!, line);
            }
        }
        return { lineToName, nameToLine };
    });

    const ruleDefinitionLines = computed(() => new Set(ruleLineMap.value.lineToName.keys()));

    const breakpointLines = computed(() => {
        const lines = new Set<number>();
        for (const name of debugSession.breakpoints.value) {
            const line = ruleLineMap.value.nameToLine.get(name);
            if (line) lines.add(line);
        }
        return lines;
    });

    function onToggleBreakpointLine(line: number) {
        const name = ruleLineMap.value.lineToName.get(line);
        if (name) debugSession.toggleBreakpoint(name);
    }

    // Consumed offset for the input editor decoration.
    const debugConsumedOffset = computed(() => {
        if (!debugSession.active.value || !debugSession.snapshot.value) return 0;
        return debugSession.snapshot.value.offset;
    });

    return {
        ruleLineMap,
        ruleDefinitionLines,
        breakpointLines,
        onToggleBreakpointLine,
        debugConsumedOffset,
    };
}
