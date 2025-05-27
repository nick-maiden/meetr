import { Slot } from "src/hooks/useSlotSelection/types";
import { NUM_COLS } from "./const";

interface Position {
  row: number;
  col: number;
}

interface IDateSlot extends Slot {
  year: number;
  month: number;
  day: number
}

export class DateSlot implements IDateSlot {
  id: string;
  year: number;
  month: number;
  day: number

  constructor(year: number, month: number, day: number) {
    this.id = DateSlot.createId(year, month, day);
    this.year = year;
    this.month = month;
    this.day = day;
  }

  static fromString(dateStr: string): DateSlot {
    const [year, month, day] = dateStr.split("-").map(Number);
    return new DateSlot(year, month, day);
  }

  static createId(year: number, month: number, day: number) {
    return `${year}-${month}-${day}`;
  }

  getPosition(): Position {
    const firstDayOfMonth = new Date(this.year, this.month, 1).getDay();
    const adjustedDay = this.day + firstDayOfMonth - 1;
    const row = Math.floor(adjustedDay / NUM_COLS);
    const col = adjustedDay % NUM_COLS;
    return { row, col };
  }

  isInPast(): boolean {
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const dateToCheck = new Date(this.year, this.month, this.day);
    return dateToCheck < today;
  }

  asFormattedString(): string {
    return `${this.year}-${String(this.month + 1).padStart(2, '0')}-${this.day}`;
  }
}

