import { TimeData } from "./types";

export const generateTimeData = (earliestTime: string, latestTime: string): TimeData => {
  const hours: string[] = [];
  const timeSlots: string[] = [];

  const startHour = parseInt(earliestTime.split(':')[0]);
  const endHour = parseInt(latestTime.split(':')[0]);

  for (let hour = startHour; hour < endHour; hour++) {
    const hour12 = hour % 12 || 12;
    const ampm = hour < 12 ? 'am' : 'pm';
    hours.push(`${hour12}${ampm}`);

    for (let minute = 0; minute < 60; minute += 15) {
      timeSlots.push(
        `${hour.toString().padStart(2, '0')}:${minute.toString().padStart(2, '0')}`
      );
    }
  }

  return { hours, timeSlots };
};

export const getTimeSlotBackgroundColor = (
  isSelectionMode: boolean,
  slotIsSelected: boolean,
  availableUsers: number,
  totalUsers: number,
): string => {
  if (isSelectionMode) {
    return slotIsSelected ? 'rgb(34, 197, 94)' : 'rgb(254, 202, 202)';
  }

  if (availableUsers === 0) return "transparent";

  const proportion = availableUsers / totalUsers;
  const red = Math.round(255 - (255 - 34) * proportion);
  const green = Math.round(255 - (255 - 197) * proportion);
  const blue = Math.round(255 - (255 - 94) * proportion);

  return `rgb(${red}, ${green}, ${blue})`;
};

