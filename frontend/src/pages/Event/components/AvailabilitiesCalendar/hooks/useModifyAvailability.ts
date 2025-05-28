import React from "react";
import { addAvailability, updateAvailability } from "global/api";
import { AppContext } from "global/contexts";
import { errorCodeMap } from "global/err";
import { SelectionContext, UserContext } from "../contexts";
import { EventId, UserId } from "global/types";
import { Slots } from "src/hooks/useSlotSelection/types";

interface UseModifyAvailabilityReturn {
  addNewAvailability: (userName: string, availability: Slots) => void;
  editAvailability: (userId: UserId, availability: Slots) => void;
  cancelSetAvailability: () => void;
}

const useModifyAvailability = (eventId: EventId): UseModifyAvailabilityReturn => {
  const { setErrorMessage } = React.useContext(AppContext);
  const { setIsSelectionMode, slotSelection } = React.useContext(SelectionContext);
  const { setUserName, setIsSaving } = React.useContext(UserContext);

  const cancelSetAvailability = () => {
    setUserName("");
    setIsSelectionMode(false);
    slotSelection.cancel();
  };

  const addNewAvailability = (userName: string, availability: Slots): void => {
    const userAvailability = { name: userName, availability: Array.from(availability) };
    addAvailability(eventId, userAvailability)
      .then(cancelSetAvailability)
      .catch((err) => {
        setUserName("");
        setErrorMessage(errorCodeMap[err.response?.data] ?? "unexpected error, please try again later");
      })
      .finally(() => {
        (document.getElementById('name_input_modal') as HTMLDialogElement)?.close();
        setIsSaving(false);
      });
  };

  const editAvailability = (userId: UserId, availability: Slots): void => {
    updateAvailability(eventId, userId, { availability: Array.from(availability) })
      .catch(() => {
        setErrorMessage('unable to edit availability, please try again later');
      })
      .finally(() => {
        cancelSetAvailability();
        setIsSaving(false);
      });
  };

  return { addNewAvailability, editAvailability, cancelSetAvailability };
};

export default useModifyAvailability;

