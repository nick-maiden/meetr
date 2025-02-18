interface ConfirmAvailabilitySelectionProps {
  onCancel: () => void;
  onSave: () => void;
}

export const ConfirmAvailabilitySelection: React.FC<ConfirmAvailabilitySelectionProps> = ({
  onCancel,
  onSave
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
        onClick={onSave}
      >
        save
      </button>
    </div>
  );
};
