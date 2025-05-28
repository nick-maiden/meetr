import { Slot } from "src/hooks/useSlotSelection/types";

export interface TimeData {
  hours: string[];
  timeSlots: string[];
}

interface IAvailabilitySlot extends Slot {
  row: number;
  col: number;
}

export class AvailabilitySlot implements IAvailabilitySlot {
  id: string;
  row: number;
  col: number;

  constructor(date: string, time: string, row: number, col: number) {
    this.id = AvailabilitySlot.createId(date, time);
    this.row = row;
    this.col = col;
  }

  static createId(date: string, time: string) {
    return date + '-' + time;
  }
}

