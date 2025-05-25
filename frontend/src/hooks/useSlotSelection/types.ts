import { Position } from "../../types/calendar";

export interface UseSlotSelectionParams {
  slotIsSelectable?: (slot: Position) => boolean;
}

export interface Slot {
  id: string;
  row: number;
  col: number;
}

export type Slots = Set<Slot["id"]>;

