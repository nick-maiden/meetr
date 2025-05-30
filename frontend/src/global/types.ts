/* domain types */
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

/* api types */
export interface EventInput {
  name: string;
  earliestTime: string;
  latestTime: string;
  dates: string[];
}

export interface NewAvailabilityInput {
  name: string;
  availability: string[];
}

export interface UpdateAvailabilityInput {
  availability: string[];
}

