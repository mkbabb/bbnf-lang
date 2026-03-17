<script setup lang="ts">
import { ref, computed, watch, inject } from "vue";
import { Select, SelectTrigger, SelectValue, SelectContent, SelectItem } from "@/components/ui/select";
import { Slider } from "@/components/ui/slider";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";
import { Dialog, DialogContent, DialogTitle, DialogDescription, DialogTrigger } from "@/components/ui/dialog";
import { Settings2 } from "lucide-vue-next";

const props = defineProps<{
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
}>();

const settingsOpen = ref(false);

const dockKeepOpen = inject<(() => void) | null>("dockKeepOpen", null);
const dockRelease = inject<(() => void) | null>("dockRelease", null);

watch(settingsOpen, (open) => {
    if (open) dockKeepOpen?.();
    else dockRelease?.();
});

const widthModel = computed({
    get: () => [props.printerConfig.maxWidth],
    set: (val: number[]) => { props.printerConfig.maxWidth = val[0]; },
});

const indentLabel = computed(() => {
    if (props.printerConfig.useTabs) return "Tab";
    return String(props.printerConfig.indent);
});

const indentModel = computed({
    get: () => props.printerConfig.useTabs ? "tab" : String(props.printerConfig.indent),
    set: (val: string) => {
        if (val === "tab") {
            props.printerConfig.useTabs = true;
            props.printerConfig.indent = 1;
        } else {
            props.printerConfig.useTabs = false;
            props.printerConfig.indent = parseInt(val, 10);
        }
    },
});
</script>

<template>
    <Dialog v-model:open="settingsOpen">
        <Tooltip>
            <TooltipTrigger as-child>
                <DialogTrigger as-child>
                    <button
                        class="flex h-9 items-center gap-1.5 rounded-xl border px-2.5 transition-all active:scale-[0.97] sm:h-10"
                        :class="settingsOpen
                            ? 'border-border/55 bg-background/45 text-foreground'
                            : 'border-border/35 bg-background/30 text-muted-foreground hover:border-border/55 hover:bg-background/45 hover:text-foreground'"
                    >
                        <Settings2 class="h-4 w-4 shrink-0" />
                        <span class="hidden font-mono text-sm sm:inline">{{ printerConfig.maxWidth }}w · {{ indentLabel }}</span>
                    </button>
                </DialogTrigger>
            </TooltipTrigger>
            <TooltipContent side="top" :side-offset="8" class="max-w-xs">
                <p class="text-sm sm:text-base">Tune print width and indentation for the formatter output.</p>
            </TooltipContent>
        </Tooltip>
        <DialogContent class="max-w-sm">
            <DialogTitle class="instrument-serif text-xl">Formatter Settings</DialogTitle>
            <DialogDescription class="sr-only">Configure formatter width and indentation</DialogDescription>
            <div class="mt-4 space-y-5">
                <div class="space-y-2">
                    <div class="flex items-center justify-between">
                        <label class="instrument-serif text-base text-muted-foreground">Print Width</label>
                        <span class="tabular-nums text-sm text-foreground font-mono">{{ printerConfig.maxWidth }}</span>
                    </div>
                    <Slider v-model="widthModel" :min="40" :max="120" :step="1" />
                </div>

                <div class="space-y-2">
                    <label class="instrument-serif text-base text-muted-foreground">Indentation</label>
                    <Select v-model="indentModel">
                        <SelectTrigger class="h-9 w-full">
                            <span class="tabular-nums">{{ indentLabel === "Tab" ? "Tab" : `${indentLabel} spaces` }}</span>
                        </SelectTrigger>
                        <SelectContent>
                            <SelectItem value="2">2 spaces</SelectItem>
                            <SelectItem value="4">4 spaces</SelectItem>
                            <SelectItem value="tab">Tab</SelectItem>
                        </SelectContent>
                    </Select>
                </div>
            </div>
        </DialogContent>
    </Dialog>
</template>
