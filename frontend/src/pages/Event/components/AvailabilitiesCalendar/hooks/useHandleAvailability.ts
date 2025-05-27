import React from "react";
import { postRequest, putRequest } from "../../../../../util/api";
import { Context } from "../../../../../util/context";
import { errorCodeMap } from "../../../../../err";
import { AvailabilityContext } from "../AvailabilityContext";

interface UseModifyAvailabilityReturn {
  saveNewUserAvailability: () => void;
  updateUserAvailability: () => void;
}

const useModifyAvailability = (): UseModifyAvailabilityReturn => {
  const { setErrorMessage } = React.useContext(Context);
  const {
    event,
    cancelSetUserAvailability,
    setUserName,
    userName,
    userId,
    setIsSaving,
    setHasConfirmedName,
    slotSelection
  } = React.useContext(AvailabilityContext);

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

