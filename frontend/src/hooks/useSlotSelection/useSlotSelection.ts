import React from "react";
import { Slot, Slots } from "./types";

interface UseSlotSelectionReturn {
  selectedSlots: Slots;
  setSelectedSlots: React.Dispatch<React.SetStateAction<Slots>>;
  isSelecting: boolean;
  handleSelectionStart: (startSlot: Slot) => void;
  handleSelectionMove: (currentSlot: Slot) => void;
  handleSelectionEnd: () => void;
  handleCancelSelection: () => void;
}

const useSlotSelection = (
  getSlotsInSelection: (start: Slot, end: Slot) => Slots
): UseSlotSelectionReturn => {
  const [selectedSlots, setSelectedSlots] = React.useState<Slots>(new Set());
  const [isSelecting, setIsSelecting] = React.useState(false);
  const [isDeselecting, setIsDeselecting] = React.useState(false);
  const [startSlot, setStartSlot] = React.useState<Slot | undefined>(undefined);

  const handleSelectionStart = (slot: Slot): void => {
    setIsSelecting(true);
    setStartSlot(slot);
    setIsDeselecting(selectedSlots.has(slot.id));
    setSelectedSlots(prev => {
      const next = new Set(prev);
      selectedSlots.has(slot.id) ? next.delete(slot.id) : next.add(slot.id);
      return next;
    });
  };

  const handleSelectionMove = (currentSlot: Slot): void => {
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
  };

  const handleSelectionEnd = (): void => {
    setIsSelecting(false);
    setStartSlot(undefined);
  };

  const handleCancelSelection = (): void => {
    setIsSelecting(false);
    setSelectedSlots(new Set());
    setStartSlot(undefined);
  };

  return {
    selectedSlots,
    setSelectedSlots,
    isSelecting,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    handleCancelSelection,
  };
};

export default useSlotSelection;

