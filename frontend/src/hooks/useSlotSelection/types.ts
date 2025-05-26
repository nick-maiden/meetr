export interface Slot {
  id: string;
}

export type Slots = Set<Slot["id"]>;

export interface SlotSelection <S extends Slot>{
  start:    (startSlot: S) => void;
  move:     (currentSlot: S) => void;
  end:      () => void;
  cancel:   () => void;
  contains: (slot: S) => boolean;
}

