import React from "react";
import { Slot, SlotSelection, Slots } from "./types";

const useSlotSelection = <S extends Slot>(
  getSlotsInSelection: (start: S, end: S) => Slots
): { slotSelection: SlotSelection<S> } => {
  const [selectedSlots, setSelectedSlots] = React.useState<Slots>(new Set());
  const [isSelecting, setIsSelecting] = React.useState(false);
  const [isDeselecting, setIsDeselecting] = React.useState(false);
  const [startSlot, setStartSlot] = React.useState<S | undefined>(undefined);

  const slotSelection = {
    start(slot: S): void {
      setIsSelecting(true);
      setStartSlot(slot);
      setIsDeselecting(selectedSlots.has(slot.id));
      setSelectedSlots(prev => {
        const next = new Set(prev);
        selectedSlots.has(slot.id) ? next.delete(slot.id) : next.add(slot.id);
        return next;
      });
    },

    move(currentSlot: S): void {
      if (!isSelecting || !startSlot) return;

      const newSelectedSlots = getSlotsInSelection(startSlot, currentSlot);
      setSelectedSlots((prev) => {
        const next = new Set(prev);
        if (isDeselecting) {
          newSelectedSlots.forEach(cell => next.delete(cell));
        } else {
          newSelectedSlots.forEach(cell => next.add(cell));
        }
        return next;
      });
    },

    end(): void {
      setIsSelecting(false);
      setStartSlot(undefined);
    },

    cancel(): void {
      setIsSelecting(false);
      setSelectedSlots(new Set());
      setStartSlot(undefined);
    },

    contains(slot: S): boolean { return selectedSlots.has(slot.id); },
    getSlots(): Slots { return selectedSlots; },
    setSlots(slots: Slots): void { setSelectedSlots(slots); },
    isSelecting(): boolean { return isSelecting; },
  }

  return { slotSelection };
};

export default useSlotSelection;

