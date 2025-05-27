import React from "react";
import { Event } from "../../../../../types";
import { postRequest, putRequest } from "../../../../../util/api";
import { Context } from "../../../../../util/context";
import { errorCodeMap } from "../../../../../err";

interface UseModifyAvailabilityReturn {
  saveNewUserAvailability: (availability: Array<string>, name: string) => void;
  updateUserAvailability: (userId: string, availability: Array<string>) => void;
}

const useModifyAvailability = (
  event: Event,
  cancelSetUserAvailability: () => void,
  setIsSaving: React.Dispatch<React.SetStateAction<boolean>>,
  setHasConfirmedName: React.Dispatch<React.SetStateAction<boolean>>,
  setUserName: React.Dispatch<React.SetStateAction<string>>,
): UseModifyAvailabilityReturn => {
  const { setErrorMessage } = React.useContext(Context);

  const saveNewUserAvailability = (availability: Array<string>, name: string) => {
    const userAvailability = { name, availability };
    postRequest(`/events/${event.id}/availability`, userAvailability)
      .then(() => {
        cancelSetUserAvailability();
      })
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

  const updateUserAvailability = (userId: string, availability: Array<string>) => {
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

