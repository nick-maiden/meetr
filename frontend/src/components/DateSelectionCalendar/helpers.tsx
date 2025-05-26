import { SlotSelection } from "../../hooks/useSlotSelection/types";
import CalendarSlot from "./components/CalendarSlot";
import { DateSlot } from "./DateSlot";

const getDaysInMonth = (date: Date): number => {
  return new Date(date.getFullYear(), date.getMonth() + 1, 0).getDate();
};

const getFirstDayOfMonth = (date: Date): number => {
  return new Date(date.getFullYear(), date.getMonth(), 1).getDay();
};

export const generateCalendarDays = (
  date: Date,
  slotSelection: SlotSelection<DateSlot>
): JSX.Element[] => {
  const daysInMonth = getDaysInMonth(date);
  const firstDayOfMonth = getFirstDayOfMonth(date);
  const days: JSX.Element[] = [];

  // Empty slots for days before the first day of the month
  for (let i = 0; i < firstDayOfMonth; i++) {
    days.push(<td key={`empty-${i}`} className="p-0" />);
  }

  // Actual days of the month
  for (let day = 1; day <= daysInMonth; day++) {
    const year = date.getFullYear();
    const month = date.getMonth();
    const slot = new DateSlot(year, month, day);

    days.push(<CalendarSlot {...{ slot, slotSelection }}/>);
  }

  return days;
};

export const generateCalendarWeeks = (
  days: JSX.Element[]
): JSX.Element[] => {
  const weeks = [];
  for (let i = 0; i < days.length; i += 7) {
    weeks.push(
      <tr key={i} className="h-12">
        {days.slice(i, i + 7)}
      </tr>
    );
  }
  return weeks;
};

