import { Position } from "../../types/calendar";

export interface UseSlotSelectionParams {
  slotIsSelectable?: (slot: Position) => boolean;
}

export interface Slot {
  id: string;
}

export type Slots = Set<Slot["id"]>;

