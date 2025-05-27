import React from "react";
import { AvailabilitySlot } from "../types";
import DayHeadings from "./DayHeadings";
import { getTimeSlotBackgroundColor } from "../utils";
import { SelectionContext } from "../contexts";
import { Event } from "../../../../../types";

interface Props {
  event: Event;
  hours: string[];
  displayDates: string[];
  timeSlots: string[];
  setHoveredSlot: React.Dispatch<React.SetStateAction<AvailabilitySlot | null>>;
}

const CalendarGrid: React.FC<Props> = ({
  event,
  hours,
  displayDates,
  timeSlots,
  setHoveredSlot
}) => {
  const {
    isSelectionMode,
    slotSelection
  } = React.useContext(SelectionContext);

  return (
    <div className="overflow-x-auto">
      <table className="table-auto table-compact w-full min-w-[190px]">
        <DayHeadings dates={displayDates.map(d => new Date(d))} />
        <tbody>
          {hours.map((hour, hourIndex) => (
            <React.Fragment key={`${hour}`}>
              <tr>
                <td className="font-bold text-right p-0 sm:pr-2 pr-1 text-sm select-none">
                  {hour}
                </td>
                {displayDates.map((date, col) => (
                  <td
                    key={`${date}-${hour}`}
                    className="p-0 border-b border-r border-l border-neutral"
                  >
                    <div className="grid grid-rows-4">
                      {[0, 1, 2, 3].map((quarter) => {
                        const row = hourIndex * 4 + quarter;
                        const time = timeSlots[row];
                        const slot = new AvailabilitySlot(date, time, row, col);
                        const border = quarter === 1 ? "border-b border-dotted border-neutral" : "";
                        const backgroundColor = getTimeSlotBackgroundColor(
                          isSelectionMode,
                          slotSelection.contains(slot),
                          (event.availabilities[slot.id] ?? []).length,
                          Object.keys(event.users).length
                        );
                        return (
                          <div
                            key={`${date}-${hour}-${quarter}`}
                            className={`h-3 ${border} cursor-pointer`}
                            style={{ backgroundColor }}
                            onPointerDown={(e) => {
                              e.preventDefault();
                              if (isSelectionMode) slotSelection.start(slot);
                              e.currentTarget.releasePointerCapture(e.pointerId)
                            }}
                            onPointerEnter={() => {
                              isSelectionMode ? slotSelection.move(slot) : setHoveredSlot(slot);
                            }}
                            onPointerUp={() => slotSelection.end()}
                            onPointerLeave={() => !isSelectionMode && setHoveredSlot(null)}
                          />
                        );
                      })}
                    </div>
                  </td>
                ))}
              </tr>
              {hourIndex < hours.length - 1 && (
                <tr className="h-0">
                  <td className="p-0"></td>
                  {displayDates.map((_, i) => (
                    <td key={i} className="p-0"></td>
                  ))}
                </tr>
              )}
            </React.Fragment>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default CalendarGrid;

