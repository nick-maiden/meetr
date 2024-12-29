// temp storage for calendar solution

import React, { useCallback, useState, useRef } from 'react';

const DAYS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
const COLS = 7;

const useCalendarSelection = () => {
  const [selectedCells, setSelectedCells] = useState(new Set());
  const [isSelecting, setIsSelecting] = useState(false);
  const [isDeselecting, setIsDeselecting] = useState(false);

  // Track the start position of the current selection
  const startPosRef = useRef({ row: 0, col: 0 });
  // Track cells that were selected before this drag operation
  const previousSelectionsRef = useRef(new Set());

  const getCellPosition = useCallback((cellId) => {
    const day = parseInt(cellId);
    const row = Math.floor((day - 1) / COLS);
    const col = (day - 1) % COLS;
    return { row, col };
  }, []);

  const getCellFromPosition = useCallback((row, col) => {
    return (row * COLS + col + 1).toString();
  }, []);

  const getCellsInRectangle = useCallback((startPos, endPos) => {
    const minRow = Math.min(startPos.row, endPos.row);
    const maxRow = Math.max(startPos.row, endPos.row);
    const minCol = Math.min(startPos.col, endPos.col);
    const maxCol = Math.max(startPos.col, endPos.col);

    const cells = new Set();
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

  const handleSelectionStart = useCallback((cellId) => {
    const pos = getCellPosition(cellId);
    setIsSelecting(true);
    setIsDeselecting(selectedCells.has(cellId));
    startPosRef.current = pos;

    // Store current selections minus the cells we're about to modify
    previousSelectionsRef.current = new Set(selectedCells);

    // Handle initial cell
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

  const handleSelectionMove = useCallback((cellId) => {
    if (!isSelecting) return;

    const currentPos = getCellPosition(cellId);
    const currentSelectionCells = getCellsInRectangle(startPosRef.current, currentPos);

    setSelectedCells(prev => {
      const next = new Set(previousSelectionsRef.current);

      if (isDeselecting) {
        currentSelectionCells.forEach(cell => next.delete(cell));
      } else {
        currentSelectionCells.forEach(cell => next.add(cell));
      }

      return next;
    });
  }, [isSelecting, isDeselecting, getCellPosition, getCellsInRectangle]);

  const handleSelectionEnd = useCallback(() => {
    setIsSelecting(false);
    previousSelectionsRef.current = new Set();
  }, []);

  return {
    selectedCells,
    isSelecting,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    clearSelections: () => setSelectedCells(new Set())
  };
};

const Calendar = () => {
  const {
    selectedCells,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    clearSelections
  } = useCalendarSelection();

  const generateCalendarDays = () => {
    const days = [];
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
        {/* Header row */}
        {DAYS.map(day => (
          <div 
            key={day} 
            className="p-2 text-center font-semibold bg-gray-100"
          >
            {day}
          </div>
        ))}

        {/* Calendar days */}
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

      {/* Selection info */}
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
