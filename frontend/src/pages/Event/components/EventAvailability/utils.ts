import { Event, UserId } from "global/types";
import { AvailabilitySlot, TimeData } from "./types";

export const isUserAvailable = (userId: UserId, slot: AvailabilitySlot, event: Event): boolean => {
  return event.availabilities[slot.id]?.includes(userId) ?? false;
};

export const getNumAvailableUsers = (event: Event, slot: AvailabilitySlot): number => {
  return (event.availabilities[slot.id] ?? []).length;
};

export const getNumUsers = (event: Event): number => {
  return Object.keys(event.users).length
};

export const getUserAvailability = (userId: UserId, event: Event) => {
  return new Set(
    Object.entries(event.availabilities)
      .filter(([_, userIds]) => userIds.includes(userId))
      .map(([timeSlot]) => timeSlot)
      .sort()
  );
};

export const generateTimeData = (earliestTime: string, latestTime: string): TimeData => {
  const hours: string[] = [];
  const timeSlots: string[] = [];

  const startHour = parseInt(earliestTime.split(':')[0]);
  const endHour = parseInt(latestTime.split(':')[0]);

  for (let hour = startHour; hour <= endHour; hour++) {
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

