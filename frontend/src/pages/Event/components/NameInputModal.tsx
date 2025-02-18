interface NameInputModalProps {
  userName: string;
  onNameChange: (name: string) => void;
  onSave: () => void;
  onClose: () => void;
}

export const NameInputModal: React.FC<NameInputModalProps> = ({
  userName,
  onNameChange,
  onSave,
  onClose
}) => {
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
              onChange={(e) => onNameChange(e.target.value)}
            />
            <button
              className="btn btn-secondary self-end w-[30%] text-lg"
              disabled={userName.length === 0}
              onClick={onSave}
            >
              continue
            </button>
          </div>
        </article>
      </div>
      <form method="dialog" className="modal-backdrop">
        <button onClick={onClose}>close</button>
      </form>
    </dialog>
  );
};

