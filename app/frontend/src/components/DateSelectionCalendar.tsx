import React, { useState, useEffect, useCallback, useRef, Dispatch, SetStateAction} from 'react';
import { ChevronLeft, ChevronRight } from "lucide-react";

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

interface CalendarSelectionHookResult {
  selectedCells: Set<string>;
  isSelecting: boolean;
  handleSelectionStart: (cellId: string) => void;
  handleSelectionMove: (cellId: string) => void;
  handleSelectionEnd: () => void;
}

interface CellInfo {
  year: number;
  month: number;
  day: number;
}

const createCellId = (year: number, month: number, day: number): string => {
  return `${year}-${month}-${day}`;
};

const parseCellId = (cellId: string): CellInfo => {
  const [year, month, day] = cellId.split('-').map(Number);
  return { year, month, day };
};

const formatSelectedDate = (cellId: string): string => {
  const { year, month, day } = parseCellId(cellId);
  return `${year}-${String(month + 1).padStart(2, '0')}-${day}`;
};

const useCalendarSelection = (
  setSelectedDates: Dispatch<SetStateAction<string[]>>,
  currentDate: Date
): CalendarSelectionHookResult => {
  const [selectedCells, setSelectedCells] = useState<Set<string>>(new Set());
  const [isSelecting, setIsSelecting] = useState<boolean>(false);
  const [isDeselecting, setIsDeselecting] = useState<boolean>(false);

  const startPosRef = useRef<Position>({ row: 0, col: 0 });
  const previousSelectionsRef = useRef<Set<string>>(new Set());

  useEffect(() => {
    setSelectedDates(Array.from(selectedCells)
      .sort((a, b) => {
        const dateA = parseCellId(a);
        const dateB = parseCellId(b);
        return new Date(dateA.year, dateA.month, dateA.day).getTime() -
               new Date(dateB.year, dateB.month, dateB.day).getTime();
      })
      .map(formatSelectedDate));
  }, [selectedCells]);

  const getCellPosition = useCallback((cellId: string): Position => {
    const { day } = parseCellId(cellId);
    const firstDayOfMonth = new Date(currentDate.getFullYear(), currentDate.getMonth(), 1).getDay();
    const adjustedDay = day + firstDayOfMonth - 1;
    const row = Math.floor(adjustedDay / COLS);
    const col = adjustedDay % COLS;
    return { row, col };
  }, [currentDate]);

  const getCellsInRectangle = useCallback((startPos: Position, endPos: Position): Set<string> => {
    const minRow = Math.min(startPos.row, endPos.row);
    const maxRow = Math.max(startPos.row, endPos.row);
    const minCol = Math.min(startPos.col, endPos.col);
    const maxCol = Math.max(startPos.col, endPos.col);
    const cells = new Set<string>();
    const firstDayOfMonth = new Date(currentDate.getFullYear(), currentDate.getMonth(), 1).getDay();
    const daysInMonth = new Date(currentDate.getFullYear(), currentDate.getMonth() + 1, 0).getDate();

    for (let row = minRow; row <= maxRow; row++) {
      for (let col = minCol; col <= maxCol; col++) {
        const adjustedDay = row * COLS + col + 1 - firstDayOfMonth;
        if (adjustedDay > 0 && adjustedDay <= daysInMonth) {
          const cellId = createCellId(
            currentDate.getFullYear(),
            currentDate.getMonth(),
            adjustedDay
          );
          cells.add(cellId);
        }
      }
    }
    return cells;
  }, [currentDate]);

  const handleSelectionStart = useCallback((cellId: string): void => {
    const pos = getCellPosition(cellId);
    setIsSelecting(true);
    setIsDeselecting(selectedCells.has(cellId));
    startPosRef.current = pos;
    previousSelectionsRef.current = new Set(selectedCells);

    setSelectedCells(prev => {
      const next = new Set(prev);
      if (selectedCells.has(cellId)) {
        next.delete(cellId);
      } else {
        next.add(cellId);
      }
      return next;
    });
  }, [selectedCells, getCellPosition]);

  const handleSelectionMove = useCallback((cellId: string): void => {
    if (!isSelecting) return;

    const currentPos = getCellPosition(cellId);
    const currentSelectionCells = getCellsInRectangle(startPosRef.current, currentPos);

    setSelectedCells(() => {
      const next = new Set(previousSelectionsRef.current);
      if (isDeselecting) {
        currentSelectionCells.forEach(cell => next.delete(cell));
      } else {
        currentSelectionCells.forEach(cell => next.add(cell));
      }
      return next;
    });
  }, [isSelecting, isDeselecting, getCellPosition, getCellsInRectangle]);

  const handleSelectionEnd = useCallback((): void => {
    setIsSelecting(false);
    previousSelectionsRef.current = new Set();
  }, []);

  return {
    selectedCells,
    isSelecting,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
  };
};

interface CalendarProps {
  className?: string;
  setSelectedDates: Dispatch<SetStateAction<string[]>>
}

const DateSelectionCalendar: React.FC<CalendarProps> = ({ className, setSelectedDates }) => {
  const [currentDate, setCurrentDate] = useState<Date>(new Date());
  const {
    selectedCells,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
  } = useCalendarSelection(setSelectedDates, currentDate);

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

    // Empty cells for days before the first day of the month
    for (let i = 0; i < firstDayOfMonth; i++) {
      days.push(<td key={`empty-${i}`} className="p-0" />);
    }

    // Actual days of the month
    for (let day = 1; day <= daysInMonth; day++) {
      const cellId = createCellId(
        currentDate.getFullYear(),
        currentDate.getMonth(),
        day
      );

      days.push(
        <td key={day} className="p-0">
          <button
            className={`btn btn-circle md:w-12 md:h-12 w-9 h-9 min-h-0 relative mb-1 ${
              selectedCells.has(cellId)
                ? 'btn-primary text-primary-content'
                : 'btn-ghost'
            }`}
            onMouseDown={() => handleSelectionStart(cellId)}
            onMouseEnter={() => handleSelectionMove(cellId)}
            onMouseUp={handleSelectionEnd}
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
    <div className={`card bg-base-100 w-full max-w-md p-4 border ${className || ''}`}>
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
        <table className="w-full" onMouseLeave={handleSelectionEnd}>
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
