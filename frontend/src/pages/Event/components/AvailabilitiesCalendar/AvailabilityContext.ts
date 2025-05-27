import React from 'react';
import { AvailabilitySlot } from './types';
import { SlotSelection } from '../../../../hooks/useSlotSelection/types';
import { Event } from '../../../../types';

interface AvailabilityContextType {
  isSelectionMode: boolean;
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>;
  hoveredSlot: AvailabilitySlot | null;
  setHoveredSlot: React.Dispatch<React.SetStateAction<AvailabilitySlot | null>>;
  isSaving: boolean;
  setIsSaving: React.Dispatch<React.SetStateAction<boolean>>;
  userName: string;
  setUserName: React.Dispatch<React.SetStateAction<string>>;
  userId: string | null;
  setUserId: React.Dispatch<React.SetStateAction<string | null>>;
  hasConfirmedName: boolean;
  setHasConfirmedName: React.Dispatch<React.SetStateAction<boolean>>;
  event: Event;
  hours: string[];
  timeSlots: string[];
  displayDates: string[];
  totalPages: number;
  slotSelection: SlotSelection<AvailabilitySlot>;
  cancelSetUserAvailability(): void;
}

const AvailabilityContext = React.createContext({} as AvailabilityContextType);

export { AvailabilityContext };

