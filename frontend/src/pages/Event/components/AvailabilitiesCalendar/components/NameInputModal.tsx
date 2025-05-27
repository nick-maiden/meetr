import React from "react";
import { AvailabilityContext } from "../AvailabilityContext";
import useModifyAvailability from "../hooks/useHandleAvailability";

const NameInputModal = () => {
  const { saveNewUserAvailability } = useModifyAvailability();
  const {
    userName,
    setUserName,
    setIsSaving,
    hasConfirmedName,
    setHasConfirmedName,
    slotSelection
  } = React.useContext(AvailabilityContext);
  return (
    <dialog id="name_input_modal" className="modal">
      <div className="modal-box">
        <article className="prose">
          <h2 className="font-bold text-3xl">save availability</h2>
          <div className="flex flex-col gap-y-4">
            <input
              type="text"
              placeholder="enter your name..."
              className="input input-bordered w-full"
              autoFocus
              value={userName}
              onChange={(e) => setUserName(e.target.value)}
            />
            <button
              className="btn btn-secondary self-end w-[30%] text-lg"
              disabled={userName.length === 0}
              onClick={() => {
                setHasConfirmedName(true);
                saveNewUserAvailability(Array.from(slotSelection.getSlots()), userName);
              }}
            >
              {hasConfirmedName ?
                <span className="loading loading-spinner"></span> :
                <>continue</>
              }
            </button>
          </div>
        </article>
      </div>
      <form method="dialog" className="modal-backdrop">
        <button
          onClick={() => {
            setIsSaving(false);
            setUserName("");
          }}
        >
          close
        </button>
      </form>
    </dialog>
  );
};

export default NameInputModal;

