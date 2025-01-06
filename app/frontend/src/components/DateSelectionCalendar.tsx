import React, { useState } from 'react';
import { ChevronLeft, ChevronRight } from "lucide-react";

interface Props {
  className?: string
  selectedDates: [Date] | [Date, Date] | [];
  setSelectedDates: React.Dispatch<React.SetStateAction<[Date] | [Date, Date] | []>>;
}

const DateSelectionCalendar: React.FC<Props> = ({ className, selectedDates, setSelectedDates }) => {
  const [currentDate, setCurrentDate] = useState<Date>(new Date());

  const daysOfWeek: string[] = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
  const months: string[] = [
    'January', 'February', 'March', 'April', 'May', 'June', 
    'July', 'August', 'September', 'October', 'November', 'December'
  ];

  const getDaysInMonth = (date: Date): number => {
    return new Date(date.getFullYear(), date.getMonth() + 1, 0).getDate();
  };

  const getFirstDayOfMonth = (date: Date): number => {
    return new Date(date.getFullYear(), date.getMonth(), 1).getDay();
  };

  const handleDateClick = (day: number): void => {
    const clickedDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
    const clickedTime = clickedDate.getTime();
    
    if (selectedDates.length === 0) {
      setSelectedDates([clickedDate]);
    } else if (selectedDates.length === 1) {
      const [startDate] = selectedDates;
      if (clickedTime < startDate.getTime()) {
        setSelectedDates([clickedDate, startDate]);
      } else {
        setSelectedDates([startDate, clickedDate]);
      }
    } else {
      setSelectedDates([clickedDate]);
    }
  };

  const isDateSelected = (day: number): boolean => {
    if (selectedDates.length === 0) return false;
    
    const currentDayDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
    const currentDayTime = currentDayDate.getTime();
    
    if (selectedDates.length === 1) {
      return currentDayTime === selectedDates[0].getTime();
    }
    
    const [startDate, endDate] = selectedDates;
    return currentDayTime >= startDate.getTime() && currentDayTime <= endDate.getTime();
  };

  const isStartDate = (day: number): boolean => {
    if (selectedDates.length === 0) return false;
    const currentDayDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
    return currentDayDate.getTime() === selectedDates[0].getTime();
  };

  const isEndDate = (day: number): boolean => {
    if (selectedDates.length !== 2) return false;
    const currentDayDate = new Date(currentDate.getFullYear(), currentDate.getMonth(), day);
    return currentDayDate.getTime() === selectedDates[1].getTime();
  };

  const getDateStyle = (day: number): string => {
    const isSelected = isDateSelected(day);
    const isStart = isStartDate(day);
    const isEnd = isEndDate(day);

    let className = 'btn btn-circle w-12 h-12 min-h-0 relative mb-1 ';

    if (isStart || isEnd) {
      className += 'btn-primary text-primary-content ';
    } else if (isSelected) {
      className += 'bg-primary/20 text-base-content hover:bg-primary/30 ';
    } else {
      className += 'btn-ghost ';
    }

    return className.trim();
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

    for (let i = 0; i < firstDayOfMonth; i++) {
      days.push(<td key={`empty-${i}`} className="p-0" />);
    }

    for (let day = 1; day <= daysInMonth; day++) {
      days.push(
        <td key={day} className="p-0">
          <button
            onClick={() => handleDateClick(day)}
            className={getDateStyle(day)}
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
        <h2 className="text-lg font-semibold">
          {months[currentDate.getMonth()]} {currentDate.getFullYear()}
        </h2>
        <button
          onClick={nextMonth}
          className="btn btn-square btn-ghost btn-sm"
        >
          <ChevronRight className="h-4 w-4" />
        </button>
      </div>
      <div className="p-0">
        <table className="w-full">
          <thead>
            <tr>
              {daysOfWeek.map(day => (
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
