import React from "react";
import { postRequest, putRequest } from "global/api";
import { AppContext } from "global/contexts";
import { errorCodeMap } from "global/err";
import { SelectionContext, UserContext } from "../contexts";
import { Event } from "global/types";

interface UseModifyAvailabilityReturn {
  saveNewUserAvailability: () => void;
  updateUserAvailability: () => void;
}

const useModifyAvailability = (event: Event): UseModifyAvailabilityReturn => {
  const { setErrorMessage } = React.useContext(AppContext);
  const {
    cancelSetUserAvailability,
    slotSelection
  } = React.useContext(SelectionContext);
  const {
    setUserName,
    userName,
    userId,
    setIsSaving,
    setHasConfirmedName,
  } = React.useContext(UserContext);

  const saveNewUserAvailability = (): void => {
    const userAvailability = { name: userName, availability: Array.from(slotSelection.getSlots()) };
    postRequest(`/events/${event.id}/availability`, userAvailability)
      .then(cancelSetUserAvailability)
      .catch((err) => {
        setUserName("");
        setErrorMessage(errorCodeMap[err.response?.data] ?? "unexpected error, please try again later");
      })
      .finally(() => {
        (document.getElementById('name_input_modal') as HTMLDialogElement)?.close();
        setIsSaving(false);
        setHasConfirmedName(false);
      });
  };

  const updateUserAvailability = (): void => {
    const availability = Array.from(slotSelection.getSlots());
    putRequest(`/events/${event.id}/availability/${userId}`, {availability})
      .catch(() => {
        setErrorMessage('unable to edit availability, please try again later');
      })
      .finally(() => {
        cancelSetUserAvailability();
        setIsSaving(false);
      });
  };

  return { saveNewUserAvailability, updateUserAvailability };
};

export default useModifyAvailability;

