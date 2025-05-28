import React from "react";
import useModifyAvailability from "../hooks/useModifyAvailability";
import { SelectionContext, UserContext } from "../contexts";
import { Event } from "global/types";

const ConfirmAvailabilitySelection = ({ event }: { event: Event} ) => {
  const { editAvailability, cancelSetAvailability } = useModifyAvailability(event.id);
  const { slotSelection } = React.useContext(SelectionContext);
  const { userId, isSaving, setIsSaving } = React.useContext(UserContext);

  const handleConfirm = () => {
    setIsSaving(true);
    if (!userId) {
      const modal = document.getElementById('name_input_modal') as HTMLDialogElement;
      modal?.showModal();
    } else {
      editAvailability(userId, slotSelection.getSlots());
    }
  };

  return (
    <div className="bg-base-200 p-4 rounded-lg space-y-4">
      <button
        className="btn sm:btn-md btn-sm btn-outline btn-block sm:text-lg text-md"
        onClick={cancelSetAvailability}
      >
        cancel
      </button>
      <button
        className="btn sm:btn-md btn-sm btn-secondary btn-block sm:text-lg text-md"
        onClick={handleConfirm}
      >
        {isSaving ? <span className="loading loading-spinner"></span> : <>save</> }
      </button>
    </div>
  );
};

export default ConfirmAvailabilitySelection;

