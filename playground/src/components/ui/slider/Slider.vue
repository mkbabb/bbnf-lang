<script setup lang="ts">
import { type HTMLAttributes, computed } from "vue";
import type { SliderRootEmits, SliderRootProps } from "reka-ui";
import { SliderRange, SliderRoot, SliderThumb, SliderTrack, useForwardPropsEmits } from "reka-ui";
import { cn } from "@/lib/utils";

const props = defineProps<SliderRootProps & { class?: HTMLAttributes["class"] }>();
const emits = defineEmits<SliderRootEmits>();

const delegatedProps = computed(() => {
    const { class: _, ...delegated } = props;

    return delegated;
});

const forwarded = useForwardPropsEmits(delegatedProps, emits);
</script>

<template>
    <SliderRoot
        :class="cn('relative flex w-full touch-none select-none items-center', props.class)"
        v-bind="forwarded"
    >
        <SliderTrack class="relative h-2 w-full grow overflow-hidden rounded-full bg-secondary">
            <SliderRange class="absolute h-full bg-primary" />
        </SliderTrack>
        <SliderThumb
            v-for="(_, key) in modelValue"
            :key="key"
            class="block h-3.5 w-3.5 rounded-full border-2 border-primary bg-background transition-colors focus-visible:outline-none disabled:pointer-events-none disabled:opacity-50"
        />
    </SliderRoot>
</template>
