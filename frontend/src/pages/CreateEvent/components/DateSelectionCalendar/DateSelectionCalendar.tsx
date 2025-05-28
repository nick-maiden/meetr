import React from 'react';
import { ChevronLeft, ChevronRight } from "lucide-react";
import { generateCalendarDays, generateCalendarWeeks } from './helpers';
import useSelectDates from './hooks/useSelectDates';
import { DAYS, MONTHS } from './const';
import { DateSlot } from './DateSlot';

interface Props {
  className?: string;
  setDates: React.Dispatch<React.SetStateAction<string[]>>
}

const DateSelectionCalendar: React.FC<Props> = ({ className, setDates }) => {
  const {
    currentDate,
    prevMonth,
    nextMonth,
    slotSelection
  } = useSelectDates();

  React.useEffect(() => {
    setDates(Array.from(slotSelection.getSlots())
      .sort((a, b) => { return new Date(a).getTime() - new Date(b).getTime(); })
      .map((slotId) => DateSlot.fromString(slotId).asFormattedString()));
  }, [slotSelection.getSlots()]);

  const days = generateCalendarDays(currentDate, slotSelection);
  const weeks = generateCalendarWeeks(days);

  return (
    <div className={`card bg-base-100 w-full max-w-md p-4 border border-base-content ${className || ''}`}>
      <div className="flex items-center justify-between mb-4">
        <button
          onClick={prevMonth}
          className="btn btn-square btn-ghost btn-sm"
        >
          <ChevronLeft className="h-4 w-4" />
        </button>
        <div className="flex items-center gap-4">
          <h2 className="text-lg font-semibold">
            {MONTHS[currentDate.getMonth()]} {currentDate.getFullYear()}
          </h2>
        </div>
        <button
          onClick={nextMonth}
          className="btn btn-square btn-ghost btn-sm"
        >
          <ChevronRight className="h-4 w-4" />
        </button>
      </div>

      <div className="p-0">
        <table className="w-full touch-none">
          <thead>
            <tr>
              {DAYS.map(day => (
                <th
                  key={day}
                  className="text-center p-2 text-sm font-normal opacity-60"
                >
                  {day}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>{weeks}</tbody>
        </table>
      </div>
    </div>
  );
};

export default DateSelectionCalendar;

