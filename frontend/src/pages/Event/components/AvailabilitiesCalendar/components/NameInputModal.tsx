import React from "react";

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

