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

