import React from "react";

interface Props {
  onConfirm: () => void;
  onCancel: () => void;
  isSaving: boolean;
}

const ConfirmAvailabilitySelection: React.FC<Props> = ({
  onConfirm,
  onCancel,
  isSaving
}) => {

  return (
    <div className="bg-base-200 p-4 rounded-lg space-y-4">
      <button
        className="btn sm:btn-md btn-sm btn-outline btn-block sm:text-lg text-md"
        onClick={onCancel}
      >
        cancel
      </button>
      <button
        className="btn sm:btn-md btn-sm btn-secondary btn-block sm:text-lg text-md"
        onClick={onConfirm}
      >
        {isSaving ? <span className="loading loading-spinner"></span> : <>save</> }
      </button>
    </div>
  );
};

export default ConfirmAvailabilitySelection;

