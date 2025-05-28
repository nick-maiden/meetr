import React from "react";
import { SelectionContext, UserContext } from "../contexts";
import { Event } from "../../../../../global/types";
import { addAvailability } from "src/global/api";
import { errorCodeMap } from "src/global/err";
import { AppContext } from "global/contexts";

const NameInputModal = ({ event }: { event: Event} ) => {
  const {
    userName,
    setUserName,
    setIsSaving,
  } = React.useContext(UserContext);
  const { slotSelection, cancelSetUserAvailability } = React.useContext(SelectionContext);
  const { setErrorMessage } = React.useContext(AppContext);
  const [hasConfirmedName, setHasConfirmedName] = React.useState(false);

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
                const availability = {
                  name: userName,
                  availability: Array.from(slotSelection.getSlots())
                };
                addAvailability(event.id, availability)
                  .then(cancelSetUserAvailability)
                  .catch((err) => {
                    setUserName("");
                    setErrorMessage(errorCodeMap[err.response?.data] ?? "unexpected error, please try again later");
                  })
                  .finally(() => {
                    (document.getElementById('name_input_modal') as HTMLDialogElement)?.close();
                    setIsSaving(false);
                  });
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

