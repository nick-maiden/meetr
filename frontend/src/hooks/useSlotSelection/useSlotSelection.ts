import React from "react";
import { Position, SlotId, Slot, Slots } from "../../types/calendar";

interface UseSlotSelectionReturn {
  selectedSlots: Slots;
  setSelectedSlots: React.Dispatch<React.SetStateAction<Slots>>;
  isSelecting: boolean;
  createSlotId: (pos: Position) => SlotId;
  handleSelectionStart: (startSlotId: SlotId) => void;
  handleSelectionMove: (currentSlotId: SlotId) => void;
  handleSelectionEnd: () => void;
  handleCancelSelection: () => void;
}

const useSlotSelection = (
  slotIsSelectable: (slot: Position) => boolean = () => true,
): UseSlotSelectionReturn => {
  const initialStartPosition = { row: 0, col: 0 };
  const [selectedSlots, setSelectedSlots] = React.useState<Slots>(new Set());
  const [isSelecting, setIsSelecting] = React.useState(false);
  const [isDeselecting, setIsDeselecting] = React.useState(false);
  const [startPosition, setStartPosition] = React.useState<Position>(initialStartPosition);


  const createSlotId = (pos: Position): SlotId => {
    return `${pos.row}-${pos.col}`;
  };

  const parseSlotId = (slotId: SlotId): Position => {
    const [row, col] = slotId.split("-").map(Number);
    return { row, col };
  };

  const handleSelectionStart = (startSlotId: SlotId): void => {
    setIsSelecting(true);
    setIsDeselecting(selectedSlots.has(startSlotId));
    setStartPosition(parseSlotId(startSlotId));

    //setSelectedSlots(prev => {
    //  const next = new Set(prev);
    //  isDeselecting ? next.delete(slotId) : next.add(slotId);
    //  return next;
    //});
  };

  const getSlotsInRectangle = (start: Position, end: Position): Slots => {
    const minRow = Math.min(start.row, end.row);
    const maxRow = Math.max(start.row, end.row);
    const minCol = Math.min(start.col, end.col);
    const maxCol = Math.max(start.col, end.col);

    const slots = new Set<Slot>();

    for (let row = minRow; row <= maxRow; row++) {
      for (let col = minCol; col <= maxCol; col++) {
        const slot = { row, col };
        if (slotIsSelectable(slot)) {
          const slotId = createSlotId(slot);
          slots.add(slotId);
        }
      }
    }

    return slots;
  }

  const handleSelectionMove = (currentSlotId: SlotId): void => {
    if (!isSelecting) return;

    const currentPosition = parseSlotId(currentSlotId);
    const newSelectedSlots = getSlotsInRectangle(startPosition, currentPosition);

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
    setStartPosition(initialStartPosition);
  };

  const handleCancelSelection = (): void => {
    setIsSelecting(false);
    setSelectedSlots(new Set());
    setStartPosition(initialStartPosition);
  };

  return {
    selectedSlots,
    setSelectedSlots,
    isSelecting,
    createSlotId,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    handleCancelSelection,
  };
};

export default useSlotSelection;

