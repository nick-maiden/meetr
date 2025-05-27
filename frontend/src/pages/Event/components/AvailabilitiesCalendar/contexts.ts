import React from 'react';
import { AvailabilitySlot } from './types';
import { SlotSelection } from '../../../../hooks/useSlotSelection/types';

interface SelectionContextType {
  isSelectionMode: boolean;
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>;
  slotSelection: SlotSelection<AvailabilitySlot>;
  cancelSetUserAvailability(): void;
}

interface UserContextType {
  userName: string;
  setUserName: React.Dispatch<React.SetStateAction<string>>;
  userId: string | null;
  setUserId: React.Dispatch<React.SetStateAction<string | null>>;
  hasConfirmedName: boolean;
  setHasConfirmedName: React.Dispatch<React.SetStateAction<boolean>>;
  isSaving: boolean;
  setIsSaving: React.Dispatch<React.SetStateAction<boolean>>;
}

const SelectionContext = React.createContext({} as SelectionContextType);
const UserContext = React.createContext({} as UserContextType);

export { SelectionContext, UserContext };

