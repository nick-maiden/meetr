import { Slot } from "../../../../hooks/useSlotSelection/types";

export interface TimeData {
  hours: string[];
  timeSlots: string[];
}

export interface AvailabilitySlot extends Slot {
  row: number;
  col: number;
}
