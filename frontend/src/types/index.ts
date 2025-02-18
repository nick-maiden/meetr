/* Domain Types */

export interface User {
  id: number;
  name: string;
}

export interface Event {
  id: number;
  name: string;
  users: {
    [userId: number]: User
  };
  dates: [string];
  earliestTime: string;
  latestTime: string;
  availabilities: {
    [timeSlot: string]: number[];
  }
}
