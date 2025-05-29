import React from "react";
import { Event, UserId } from "src/global/types";
import { SlotSelection } from "src/hooks/useSlotSelection/types";
import { addAvailability, updateAvailability } from "src/global/api";
import { errorCodeMap } from "src/global/err";
import { getUserAvailability } from "../utils";
import { AvailabilitySlot } from "../types";
import { AppContext } from "src/global/contexts";

interface UseAvailabilityEditorReturn {
  isSaving: boolean;
  onConfirmAvailability: () => void;
  onConfirmName: (name: string) => void;
  onCancelAddAvailability: () => void;
  onEditAvailability: (userId: UserId) => void;
}

const useAvailabilityEditor = (
  event: Event,
  slotSelection: SlotSelection<AvailabilitySlot>,
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>,
  cancelSelection: () => void
): UseAvailabilityEditorReturn => {
  const [userId, setUserId] = React.useState<UserId | null>(null);
  const [isSaving, setIsSaving] = React.useState(false);
  const { setErrorMessage } = React.useContext(AppContext);

  const onConfirmAvailability = (): void => {
    setIsSaving(true);
    if (!userId) {
      const modal = document.getElementById('name_input_modal') as HTMLDialogElement;
      modal?.showModal();
    } else {
      updateAvailability(
        event.id,
        userId,
        {availability: Array.from(slotSelection.getSlots())}
      );
    }
  };

  const onConfirmName = (name: string): void => {
    const availability = {
      name,
      availability: Array.from(slotSelection.getSlots())
    };
    addAvailability(event.id, availability)
      .then(cancelSelection)
      .catch((err) => {
        setErrorMessage(errorCodeMap[err.response?.data] ?? "unexpected error, please try again later");
      })
      .finally(() => {
        (document.getElementById('name_input_modal') as HTMLDialogElement)?.close();
        setIsSaving(false);
      });
  };

  const onCancelAddAvailability = (): void => setIsSaving(false);

  const onEditAvailability = (userId: string): void => {
    setUserId(userId);
    slotSelection.setSlots(getUserAvailability(userId, event));
    setIsSelectionMode(true);
  };

  return {
    isSaving,
    onConfirmAvailability,
    onConfirmName,
    onCancelAddAvailability,
    onEditAvailability
  };
};

export default useAvailabilityEditor;

