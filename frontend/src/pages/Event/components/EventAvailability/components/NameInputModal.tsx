import React from "react";
import Heading from "src/components/Heading";

interface Props {
  onConfirm: (name: string) => void;
  onCancel: () => void;
}

const NameInputModal: React.FC<Props> = ({ onConfirm, onCancel }) => {
  const [userName, setUserName] = React.useState("");
  const [awaitingConfirmation, setAwaitingConfirmation] = React.useState(false);

  return (
    <dialog id="name_input_modal" className="modal">
      <div className="modal-box">
        <article className="prose">
          <Heading size="lg">save availability</Heading>
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
              className="btn btn-secondary self-end w-[120px] text-base"
              disabled={userName.length === 0}
              onClick={() => {
                setAwaitingConfirmation(true);
                onConfirm(userName);
                setAwaitingConfirmation(false);
                setUserName("");
              }}
            >
              {awaitingConfirmation ?
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
            onCancel();
            setUserName("");
            setAwaitingConfirmation(false);
          }}
        >
          close
        </button>
      </form>
    </dialog>
  );
};

export default NameInputModal;

