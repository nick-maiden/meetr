import React from "react";
import { Position } from "../types";

const useSelectTemp = () => {
  const [selectedSlots, setSelectedSlots] = React.useState<Set<string>>(new Set());
  const [isSelecting, setIsSelecting] = React.useState(false);
  const [isDeselecting, setIsDeselecting] = React.useState(false);
  const [startPosition, setStartPosition] = React.useState<Position | null>(null);

  const handleSelectionStart = (hourIndex: number, quarter: number, dateIndex: number, slotId: string) => {
    setIsSelecting(true);
    setStartPosition({ hour: hourIndex, quarter, date: dateIndex });
    setIsDeselecting(selectedSlots.has(slotId));

    setSelectedSlots(prev => {
      const next = new Set(prev);
      isDeselecting ? next.delete(slotId) : next.add(slotId);
      return next;
    });
  };

  //const currentSlot = hourIndex * 4 + quarter;

  const handleSelectionMove = (currentSlot: number, dateIndex: number) => {
    if (!isSelecting || !startPosition) return;

    const minDate = Math.min(startPosition.date, dateIndex);
    const maxDate = Math.max(startPosition.date, dateIndex);

    const startSlot = startPosition.hour * 4 + startPosition.quarter;
    const minSlot = Math.min(startSlot, currentSlot);
    const maxSlot = Math.max(startSlot, currentSlot);

    setSelectedSlots(prev => {
      const next = new Set(prev);

      for (let date = minDate; date <= maxDate; date++) {
        for (let slot = minSlot; slot <= maxSlot; slot++) {
          const timeSlot = timeSlots[slot];
          const currentDate = displayDates[date];
          const slotId = `${currentDate.toISOString().split('T')[0]}-${timeSlot}`;
          isDeselecting ? next.delete(slotId) : next.add(slotId);
        }
      }

      return next;
    });
  };

  const handleSelectionEnd = () => {
    setIsSelecting(false);
    setStartPosition(null);
  };
};

export default useSelectTemp;

