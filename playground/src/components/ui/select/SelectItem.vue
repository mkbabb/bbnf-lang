<script setup lang="ts">
import { type HTMLAttributes, computed } from "vue";
import {
    SelectItem,
    type SelectItemProps,
    SelectItemText,
    useForwardProps,
} from "reka-ui";
import { cn } from "@/lib/utils";

const props = defineProps<SelectItemProps & { class?: HTMLAttributes["class"] }>();

const delegatedProps = computed(() => {
    const { class: _, ...delegated } = props;

    return delegated;
});

const forwardedProps = useForwardProps(delegatedProps);
</script>

<template>
    <SelectItem
        v-bind="forwardedProps"
        :class="
            cn(
                'relative flex w-full cursor-default select-none items-center rounded-sm py-1.5 pl-2 pr-2 text-sm outline-none focus:bg-accent focus:text-accent-foreground data-[state=checked]:bg-accent/15 data-[state=checked]:border-accent/30 data-[disabled]:pointer-events-none data-[disabled]:opacity-50',
                props.class,
            )
        "
    >
        <SelectItemText>
            <slot />
        </SelectItemText>
        <slot name="extra" />
    </SelectItem>
</template>
