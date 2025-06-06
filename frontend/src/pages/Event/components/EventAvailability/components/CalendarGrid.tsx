import React from "react";
import { AvailabilitySlot, SelectionHandler } from "../types";
import DayHeadings from "./DayHeadings";

interface Props {
  hours: string[];
  displayDates: string[];
  timeSlots: string[];
  selectionHandler: SelectionHandler;
  isSelectionMode: boolean;
}

const CalendarGrid: React.FC<Props> = ({
  hours,
  displayDates,
  timeSlots,
  selectionHandler,
  isSelectionMode
}) => {
  const scrollMode = isSelectionMode ? "touch-none" : "";
  return (
    <div className="overflow-x-auto">
      <table className={`table-auto table-compact w-full min-w-[220px] ${scrollMode}`}>
        <DayHeadings dates={displayDates.map(d => new Date(d))} />
        <tbody>
          {hours.map((hour, hourIndex) => (
            <React.Fragment key={`${hour}`}>
              <tr>
                <td className="text-right p-0 pr-1 align-top relative">
                  <div className="relative -translate-y-1/2 sm:text-sm text-xs">
                    {hour}
                  </div>
                </td>
                {hourIndex !== hours.length - 1 &&
                  displayDates.map((date, col) => (
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
                          const backgroundColor = selectionHandler.getTimeSlotBackgroundColor(slot);
                          return (
                            <div
                              key={`${date}-${hour}-${quarter}`}
                              className={`h-3 ${border} cursor-pointer`}
                              style={{ backgroundColor }}
                              onPointerDown={(e) => {
                                e.preventDefault();
                                selectionHandler.startSelection(slot);
                                e.currentTarget.releasePointerCapture(e.pointerId)
                              }}
                              onPointerEnter={() => selectionHandler.moveSelection(slot)}
                              onPointerUp={selectionHandler.endSelection}
                              onPointerLeave={selectionHandler.leaveSelectionArea}
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

