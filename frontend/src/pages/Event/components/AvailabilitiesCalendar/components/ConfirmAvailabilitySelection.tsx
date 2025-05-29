import React from "react";

interface Props {
  handleConfirm: () => void;
  handleCancel: () => void;
  isSaving: boolean;
}

const ConfirmAvailabilitySelection: React.FC<Props> = ({
  handleConfirm,
  handleCancel,
  isSaving
}) => {

  return (
    <div className="bg-base-200 p-4 rounded-lg space-y-4">
      <button
        className="btn sm:btn-md btn-sm btn-outline btn-block sm:text-lg text-md"
        onClick={handleCancel}
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

