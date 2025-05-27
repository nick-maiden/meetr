import React from "react";
import { DateSlot } from "../DateSlot";
import { SlotSelection, Slots } from "../../../hooks/useSlotSelection/types";
import { NUM_COLS } from "../const";
import useSlotSelection from "../../../hooks/useSlotSelection/useSlotSelection";

interface useSelectDatesReturn {
    currentDate: Date;
    prevMonth: () => void;
    nextMonth: () => void;
    slotSelection: SlotSelection<DateSlot>;
}

const useSelectDates = (): useSelectDatesReturn => {
  const [currentDate, setCurrentDate] = React.useState<Date>(new Date());

  const nextMonth = (): void => {
    setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() + 1));
  };

  const prevMonth = (): void => {
    setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() - 1));
  };

  const getSlotsInSelection = (start: DateSlot, end: DateSlot): Slots => {
    const startPos = start.getPosition();
    const endPos = end.getPosition();
    const minCol = Math.min(startPos.col, endPos.col);
    const maxCol = Math.max(startPos.col, endPos.col);
    const minRow = Math.min(startPos.row, endPos.row);
    const maxRow = Math.max(startPos.row, endPos.row);

    const slots = new Set<string>;
    const firstDayOfMonth = new Date(currentDate.getFullYear(), currentDate.getMonth(), 1).getDay();
    const daysInMonth = new Date(currentDate.getFullYear(), currentDate.getMonth() + 1, 0).getDate();

    for (let col = minCol; col <= maxCol; col++) {
      for (let row = minRow; row <= maxRow; row++) {
        const adjustedDay = row * NUM_COLS + col + 1 - firstDayOfMonth;
        if (adjustedDay > 0 && adjustedDay <= daysInMonth) {
          const slotId = DateSlot.createId(
            currentDate.getFullYear(),
            currentDate.getMonth(),
            adjustedDay,
          );
          slots.add(slotId);
        }
      }
    }

    return slots;
  };

  const { slotSelection } = useSlotSelection<DateSlot>(getSlotsInSelection);

  return {
    currentDate,
    prevMonth,
    nextMonth,
    slotSelection,
  }
};

export default useSelectDates;

