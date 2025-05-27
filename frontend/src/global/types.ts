/* Domain Types */

export type UserId = string;

export interface User {
  id: UserId;
  name: string;
}

export type EventId = string;

export interface Event {
  id: EventId;
  name: string;
  users: {
    [userId: UserId]: User
  };
  dates: string[];
  earliestTime: string;
  latestTime: string;
  availabilities: {
    [timeSlot: string]: UserId[];
  }
}

