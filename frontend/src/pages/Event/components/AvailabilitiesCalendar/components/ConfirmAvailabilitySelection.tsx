import React from "react";
import useModifyAvailability from "../hooks/useModifyAvailability";
import { SelectionContext, UserContext } from "../contexts";
import { Event } from "../../../../../types";

const ConfirmAvailabilitySelection = ({ event }: { event: Event} ) => {
  const { updateUserAvailability } = useModifyAvailability(event);
  const { cancelSetUserAvailability } = React.useContext(SelectionContext);
  const { userId, isSaving, setIsSaving } = React.useContext(UserContext);

  return (
    <div className="bg-base-200 p-4 rounded-lg space-y-4">
      <button
        className="btn sm:btn-md btn-sm btn-outline btn-block sm:text-lg text-md"
        onClick={cancelSetUserAvailability}
      >
        cancel
      </button>
      <button
        className="btn sm:btn-md btn-sm btn-secondary btn-block sm:text-lg text-md"
        onClick={() => {
          setIsSaving(true);
          if (!userId) {
            (document.getElementById('name_input_modal') as HTMLDialogElement)?.showModal();
          } else {
            updateUserAvailability();
          }
        }}
      >
        {isSaving ?
          <span className="loading loading-spinner"></span> :
          <>save</>
        }
      </button>
    </div>
  );
};

export default ConfirmAvailabilitySelection;

