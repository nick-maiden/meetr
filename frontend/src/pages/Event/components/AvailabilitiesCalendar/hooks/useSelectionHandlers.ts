import { SlotSelection } from "src/hooks/useSlotSelection/types";
import { AvailabilitySlot } from "../types";
import React from "react";
import { getNumAvailableUsers, getNumUsers } from "../utils";
import { Event } from "src/global/types";

export interface SelectionHandler {
  getTimeSlotBackgroundColor: (slot: AvailabilitySlot) => string;
  startSelection: (slot: AvailabilitySlot) => void;
  moveSelection: (slot: AvailabilitySlot) => void;
  endSelection: () => void;
  leaveSelectionArea: () => void;
  cancelSelection: () => void;
}

interface UseSelectionHandlersReturn {
  hoveredSlot: AvailabilitySlot | null;
  selectionHandler: SelectionHandler;
}

const useSelectionHandlers = (
  event: Event,
  slotSelection: SlotSelection<AvailabilitySlot>,
  isSelectionMode: boolean,
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>
): UseSelectionHandlersReturn => {
  const [hoveredSlot, setHoveredSlot] = React.useState<AvailabilitySlot | null>(null);

  const getTimeSlotBackgroundColor = (slot: AvailabilitySlot): string => {
    const slotIsSelected = slotSelection.contains(slot);
    if (isSelectionMode) {
      return slotIsSelected ? 'rgb(34, 197, 94)' : 'rgb(254, 202, 202)';
    }

    const availableUsers = getNumAvailableUsers(event, slot);
    if (availableUsers === 0) return "transparent";

    const proportion = availableUsers / getNumUsers(event);
    const red = Math.round(255 - (255 - 34) * proportion);
    const green = Math.round(255 - (255 - 197) * proportion);
    const blue = Math.round(255 - (255 - 94) * proportion);

    return `rgb(${red}, ${green}, ${blue})`;
  };

  const startSelection = (slot: AvailabilitySlot) => isSelectionMode && slotSelection.start(slot);
  const moveSelection = (slot: AvailabilitySlot) => isSelectionMode ? slotSelection.move(slot) : setHoveredSlot(slot);
  const endSelection = () => slotSelection.end();
  const leaveSelectionArea = () => !isSelectionMode && setHoveredSlot(null);
  const cancelSelection = () => {
    setIsSelectionMode(false);
    slotSelection.cancel();
  };

  return {
    hoveredSlot,
    selectionHandler: {
      getTimeSlotBackgroundColor,
      startSelection,
      moveSelection,
      endSelection,
      leaveSelectionArea,
      cancelSelection,
    },
  };
};

export default useSelectionHandlers;

