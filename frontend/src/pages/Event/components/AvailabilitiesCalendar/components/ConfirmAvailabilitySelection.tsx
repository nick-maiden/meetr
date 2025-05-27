import React from "react";
import { AvailabilityContext } from "../AvailabilityContext";
import useModifyAvailability from "../hooks/useHandleAvailability";

const ConfirmAvailabilitySelection = () => {
  const { updateUserAvailability } = useModifyAvailability();
  const {
    isSaving,
    setIsSaving,
    cancelSetUserAvailability,
    userId,
  } = React.useContext(AvailabilityContext);

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

