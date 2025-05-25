const CalendarSlot = () => {
  return (
    <td key={day} className="p-0">
      <button
        className={`btn btn-circle md:w-12 md:h-12 w-9 h-9 min-h-0 relative mb-1 ${
          selectedSlots.has(slot.id)
            ? 'btn-secondary text-secondary-content'
            : 'btn-ghost'
        } ${slot.isInPast()
            ? 'line-through btn-disabled !bg-transparent hover:!bg-transparent'
            : ''
        }`}
        onPointerDown={(e) => {
          e.preventDefault();
          if (!slot.isInPast()) handleSelectionStart(slot);
          e.currentTarget.releasePointerCapture(e.pointerId)
        }}
        onPointerEnter={() => handleSelectionMove(slot)}
        onPointerUp={handleSelectionEnd}
        disabled={slot.isInPast()}
      >
        {day}
      </button>
    </td>
  );
};

export default CalendarSlot;

