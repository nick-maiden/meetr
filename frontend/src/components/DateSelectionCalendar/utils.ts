import CalendarSlot from "./components/CalendarSlot";

const getDaysInMonth = (date: Date): number => {
  return new Date(date.getFullYear(), date.getMonth() + 1, 0).getDate();
};

const getFirstDayOfMonth = (date: Date): number => {
  return new Date(date.getFullYear(), date.getMonth(), 1).getDay();
};

const generateCalendarDays = (date: Date): JSX.Element[] => {
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

    days.push(<CalendarSlot />);
  }

  return days;
};

