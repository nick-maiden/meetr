import React from 'react';
import { AvailabilitySlot } from './types';
import { SlotSelection } from 'src/hooks/useSlotSelection/types';
import { UserId } from 'global/types';

interface SelectionContextType {
  isSelectionMode: boolean;
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>;
  slotSelection: SlotSelection<AvailabilitySlot>;
}

interface UserContextType {
  userName: string;
  setUserName: React.Dispatch<React.SetStateAction<string>>;
  userId: UserId | null;
  setUserId: React.Dispatch<React.SetStateAction<string | null>>;
  isSaving: boolean;
  setIsSaving: React.Dispatch<React.SetStateAction<boolean>>;
}

const SelectionContext = React.createContext({} as SelectionContextType);
const UserContext = React.createContext({} as UserContextType);

export { SelectionContext, UserContext };

