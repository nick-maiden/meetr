import { TimeData } from "./types";

export const generateTimeData = (earliestTime: string, latestTime: string): TimeData => {
  const displaySlots: string[] = [];
  const dataSlots: string[] = [];

  const startHour = parseInt(earliestTime.split(':')[0]);
  const endHour = parseInt(latestTime.split(':')[0]);

  for (let hour = startHour; hour < endHour; hour++) {
    const hour12 = hour % 12 || 12;
    const ampm = hour < 12 ? 'am' : 'pm';
    displaySlots.push(`${hour12}${ampm}`);

    for (let minute = 0; minute < 60; minute += 15) {
      dataSlots.push(
        `${hour.toString().padStart(2, '0')}:${minute.toString().padStart(2, '0')}`
      );
    }
  }

  return { displaySlots, dataSlots };
};

