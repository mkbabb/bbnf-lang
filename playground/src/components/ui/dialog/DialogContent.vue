<script setup lang="ts">
import { type HTMLAttributes, computed } from "vue";
import {
    DialogClose,
    DialogContent,
    DialogOverlay,
    DialogPortal,
    type DialogContentEmits,
    type DialogContentProps,
    useForwardPropsEmits,
} from "reka-ui";
import { X } from "lucide-vue-next";
import { cn } from "@/lib/utils";

const props = defineProps<DialogContentProps & { class?: HTMLAttributes["class"] }>();
const emits = defineEmits<DialogContentEmits>();

const delegatedProps = computed(() => {
    const { class: _, ...delegated } = props;
    return delegated;
});

const forwarded = useForwardPropsEmits(delegatedProps, emits);
</script>

<template>
    <DialogPortal>
        <DialogOverlay class="fixed inset-0 z-50 bg-black/60 backdrop-blur-sm duration-200 data-[state=open]:animate-in data-[state=closed]:animate-out data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0" />
        <DialogContent
            v-bind="forwarded"
            :class="cn(
                'fixed left-1/2 top-1/2 z-50 w-full max-w-lg -translate-x-1/2 -translate-y-1/2 rounded-xl border border-border/60 bg-card/95 backdrop-blur-xl p-6 shadow-lg duration-200 data-[state=open]:animate-in data-[state=closed]:animate-out data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0',
                props.class,
            )"
        >
            <slot />
            <DialogClose class="absolute top-4 right-4 rounded-sm opacity-70 transition-opacity hover:opacity-100 focus:outline-none">
                <X class="h-4 w-4" />
            </DialogClose>
        </DialogContent>
    </DialogPortal>
</template>
