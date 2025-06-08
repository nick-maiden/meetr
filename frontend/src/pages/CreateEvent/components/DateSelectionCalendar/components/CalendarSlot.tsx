import React from "react";
import { DateSlot } from "../DateSlot";
import { SlotSelection } from "src/hooks/useSlotSelection/types";

interface Props {
  slot: DateSlot;
  slotSelection: SlotSelection<DateSlot>;
}

const CalendarSlot: React.FC<Props> = ({ slot, slotSelection }) => {
  return (
    <td key={slot.day} className="p-0">
      <button
        className={`btn btn-circle md:w-12 md:h-12 w-9 h-9 min-h-0 relative mb-1 font-normal ${
            slotSelection.contains(slot)
            ? 'btn-secondary text-secondary-content'
            : 'btn-ghost'
        } ${slot.isInPast()
            ? 'line-through btn-disabled !bg-transparent hover:!bg-transparent'
            : ''
        }`}
        onPointerDown={(e) => {
          e.preventDefault();
          if (!slot.isInPast()) slotSelection.start(slot);
          e.currentTarget.releasePointerCapture(e.pointerId)
        }}
        onPointerEnter={() => slotSelection.move(slot)}
        onPointerUp={() => slotSelection.end()}
        disabled={slot.isInPast()}
      >
        {slot.day}
      </button>
    </td>
  );
};

export default CalendarSlot;

