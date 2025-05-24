import { Position } from "../../types/calendar";

export interface UseSlotSelectionParams {
  slotIsSelectable?: (slot: Position) => boolean;
}

