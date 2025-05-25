import React, { useState, useEffect, Dispatch, SetStateAction} from 'react';
import { ChevronLeft, ChevronRight } from "lucide-react";
import { Slot, Slots } from '../hooks/useSlotSelection/types';
import useSlotSelection from '../hooks/useSlotSelection/useSlotSelection';

const DAYS: readonly string[] = ['sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat'];
const MONTHS: readonly string[] = [
  'january', 'february', 'march', 'april', 'may', 'june',
  'july', 'august', 'september', 'october', 'november', 'december'
];
const COLS = 7;

interface Position {
  row: number;
  col: number;
}

interface IDateSlot extends Slot {
  year: number;
  month: number;
  day: number
}

class DateSlot implements IDateSlot {
  id: string;
  year: number;
  month: number;
  day: number

  constructor(year: number, month: number, day: number) {
    this.id = DateSlot.createId(year, month, day);
    this.year = year;
    this.month = month;
    this.day = day;
  }

  static fromString(dateStr: string): DateSlot {
    const [year, month, day] = dateStr.split("-").map(Number);
    return new DateSlot(year, month, day);
  }

  static createId(year: number, month: number, day: number) {
    return `${year}-${month}-${day}`;
  }

  getPosition(): Position {
    const firstDayOfMonth = new Date(this.year, this.month, 1).getDay();
    const adjustedDay = this.day + firstDayOfMonth - 1;
    const row = Math.floor(adjustedDay / COLS);
    const col = adjustedDay % COLS;
    return { row, col };
  }

  isInPast(): boolean {
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const dateToCheck = new Date(this.year, this.month, this.day);
    return dateToCheck < today;
  }

  asFormattedString(): string {
    return `${this.year}-${String(this.month + 1).padStart(2, '0')}-${this.day}`;
  }
}

interface Props {
  className?: string;
  setSelectedDates: Dispatch<SetStateAction<string[]>>
}

const DateSelectionCalendar: React.FC<Props> = ({ className, setSelectedDates }) => {
  const [currentDate, setCurrentDate] = useState<Date>(new Date());

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
        const adjustedDay = row * COLS + col + 1 - firstDayOfMonth;
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

  const {
    selectedSlots,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
  } = useSlotSelection<DateSlot>(getSlotsInSelection);

  useEffect(() => {
    setSelectedDates(Array.from(selectedSlots)
      .sort((a, b) => { return new Date(a).getTime() - new Date(b).getTime(); })
      .map((slotId) => DateSlot.fromString(slotId).asFormattedString()));
  }, [selectedSlots]);

  const getDaysInMonth = (date: Date): number => {
    return new Date(date.getFullYear(), date.getMonth() + 1, 0).getDate();
  };

  const getFirstDayOfMonth = (date: Date): number => {
    return new Date(date.getFullYear(), date.getMonth(), 1).getDay();
  };

  const nextMonth = (): void => {
    setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() + 1));
  };

  const prevMonth = (): void => {
    setCurrentDate(new Date(currentDate.getFullYear(), currentDate.getMonth() - 1));
  };

  const generateCalendarDays = (): JSX.Element[] => {
    const daysInMonth = getDaysInMonth(currentDate);
    const firstDayOfMonth = getFirstDayOfMonth(currentDate);
    const days: JSX.Element[] = [];

    // Empty slots for days before the first day of the month
    for (let i = 0; i < firstDayOfMonth; i++) {
      days.push(<td key={`empty-${i}`} className="p-0" />);
    }

    // Actual days of the month
    for (let day = 1; day <= daysInMonth; day++) {
      const year = currentDate.getFullYear();
      const month = currentDate.getMonth();
      const slot = new DateSlot(year, month, day);
      //const slotId = createSlotId(year, month, day);
      //const pos = getSlotPosition(year, month, day);
      //const slot: Slot = { id: slotId, ...pos };

      days.push(
        <td key={day} className="p-0">
          <button
            className={`btn btn-circle md:w-12 md:h-12 w-9 h-9 min-h-0 relative mb-1 ${
              selectedSlots.has(slot.id)
                ? 'btn-secondary text-secondary-content'
                : 'btn-ghost'
            } ${slot.isInPast()
                ? 'line-through btn-disabled !bg-transparent hover:!bg-transparent'
                : ''
            }`}
            onPointerDown={(e) => {
              e.preventDefault();
              if (!slot.isInPast()) handleSelectionStart(slot);
              e.currentTarget.releasePointerCapture(e.pointerId)
            }}
            onPointerEnter={() => handleSelectionMove(slot)}
            onPointerUp={handleSelectionEnd}
            disabled={slot.isInPast()}
          >
            {day}
          </button>
        </td>
      );
    }

    return days;
  };

  const weeks: JSX.Element[] = [];
  const days = generateCalendarDays();
  for (let i = 0; i < days.length; i += 7) {
    weeks.push(
      <tr key={i} className="h-12">
        {days.slice(i, i + 7)}
      </tr>
    );
  }

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
                <th key={day} className="text-center p-2 text-sm font-normal opacity-60">
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

