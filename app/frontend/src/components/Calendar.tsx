import React from 'react';
import { useCallback, useState, useRef } from 'react';

const DAYS: readonly string[] = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
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
  clearSelections: () => void;
}

const useCalendarSelection = (): CalendarSelectionHookResult => {
  const [selectedCells, setSelectedCells] = useState<Set<string>>(new Set());
  const [isSelecting, setIsSelecting] = useState<boolean>(false);
  const [isDeselecting, setIsDeselecting] = useState<boolean>(false);

  const startPosRef = useRef<Position>({ row: 0, col: 0 });
  const previousSelectionsRef = useRef<Set<string>>(new Set());

  const getCellPosition = useCallback((cellId: string): Position => {
    const day = parseInt(cellId);
    const row = Math.floor((day - 1) / COLS);
    const col = (day - 1) % COLS;
    return { row, col };
  }, []);

  const getCellFromPosition = useCallback((row: number, col: number): string => {
    return (row * COLS + col + 1).toString();
  }, []);

  const getCellsInRectangle = useCallback((startPos: Position, endPos: Position): Set<string> => {
    const minRow = Math.min(startPos.row, endPos.row);
    const maxRow = Math.max(startPos.row, endPos.row);
    const minCol = Math.min(startPos.col, endPos.col);
    const maxCol = Math.max(startPos.col, endPos.col);

    const cells = new Set<string>();
    for (let row = minRow; row <= maxRow; row++) {
      for (let col = minCol; col <= maxCol; col++) {
        const cellId = getCellFromPosition(row, col);
        if (parseInt(cellId) > 0 && parseInt(cellId) <= 31) {
          cells.add(cellId);
        }
      }
    }
    return cells;
  }, [getCellFromPosition]);

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

  const clearSelections = useCallback((): void => {
    setSelectedCells(new Set());
  }, []);

  return {
    selectedCells,
    isSelecting,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    clearSelections
  };
};

interface CalendarProps {}

const Calendar: React.FC<CalendarProps> = () => {
  const {
    selectedCells,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    clearSelections
  } = useCalendarSelection();

  const generateCalendarDays = (): number[] => {
    const days: number[] = [];
    for (let i = 1; i <= 31; i++) {
      days.push(i);
    }
    return days;
  };

  return (
    <div className="w-full max-w-3xl mx-auto p-4">
      <div className="mb-4 flex justify-between items-center">
        <h2 className="text-xl font-bold">December 2024</h2>
        <button 
          onClick={clearSelections}
          className="px-4 py-2 bg-gray-200 rounded hover:bg-gray-300"
        >
          Clear Selections
        </button>
      </div>

      <div 
        className="grid grid-cols-7 gap-1"
        onMouseLeave={handleSelectionEnd}
      >
        {DAYS.map((day) => (
          <div 
            key={day} 
            className="p-2 text-center font-semibold bg-gray-100"
          >
            {day}
          </div>
        ))}

        {generateCalendarDays().map((day) => (
          <div
            key={day}
            className={`
              p-4 border rounded cursor-pointer select-none
              ${selectedCells.has(day.toString()) 
                ? 'bg-blue-200 hover:bg-blue-300' 
                : 'hover:bg-gray-100'
              }
            `}
            onMouseDown={() => handleSelectionStart(day.toString())}
            onMouseEnter={() => handleSelectionMove(day.toString())}
            onMouseUp={handleSelectionEnd}
          >
            {day}
          </div>
        ))}
      </div>

      <div className="mt-4 p-4 bg-gray-100 rounded">
        <h3 className="font-semibold">Selected Dates:</h3>
        <div className="mt-2">
          {Array.from(selectedCells).sort((a, b) => Number(a) - Number(b)).join(', ')}
        </div>
      </div>
    </div>
  );
};

export default Calendar;
