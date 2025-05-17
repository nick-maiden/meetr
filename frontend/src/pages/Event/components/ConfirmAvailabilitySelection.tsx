import React from "react";

interface ConfirmAvailabilitySelectionProps {
  onCancel: () => void;
  onSave: () => void;
  isSaving: boolean;
  setIsSaving: React.Dispatch<React.SetStateAction<boolean>>;
}

export const ConfirmAvailabilitySelection: React.FC<ConfirmAvailabilitySelectionProps> = ({
  onCancel,
  onSave,
  isSaving,
  setIsSaving
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
        onClick={() => {
          setIsSaving(true);
          onSave();
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
