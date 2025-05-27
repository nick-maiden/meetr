import { SlotSelection, Slots } from "../../../../../hooks/useSlotSelection/types";
import useSlotSelection from "../../../../../hooks/useSlotSelection/useSlotSelection";
import { AvailabilitySlot } from "../types";

const useSelectTimes = (
  displayDates: string[],
  timeSlots: string[]
): { slotSelection : SlotSelection<AvailabilitySlot> } => {
  const getSlotsInSelection = (start: AvailabilitySlot, end: AvailabilitySlot): Slots => {
    const minCol = Math.min(start.col, end.col);
    const maxCol = Math.max(start.col, end.col);
    const minRow = Math.min(start.row, end.row);
    const maxRow = Math.max(start.row, end.row);

    const slots = new Set<string>;

    for (let col = minCol; col <= maxCol; col++) {
      for (let row = minRow; row <= maxRow; row++) {
        const date = displayDates[col];
        const time = timeSlots[row];
        const slotId = AvailabilitySlot.createId(date, time);
        slots.add(slotId);
      }
    }

    return slots;
  };

  const { slotSelection } = useSlotSelection<AvailabilitySlot>(getSlotsInSelection);
  return { slotSelection };
};

export default useSelectTimes;

