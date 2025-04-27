/* Domain Types */

export interface User {
  id: string;
  name: string;
}

export interface Event {
  id: string;
  name: string;
  users: {
    [userId: number]: User
  };
  dates: [string];
  earliestTime: string;
  latestTime: string;
  availabilities: {
    [timeSlot: string]: string[];
  }
}
