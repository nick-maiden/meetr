import React from "react";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import useSelectTimes from "./hooks/useSelectTimes";
import Paginator from "./components/Paginator";
import CalendarGrid from "./components/CalendarGrid";
import { Event } from "global/types";
import useDisplayData from "./hooks/useDisplayData";
import useSelectionHandlers from "./hooks/useSelectionHandlers";
import useAvailabilityEditor from "./hooks/useAvailabilityEditor";

interface Props {
  isSelectionMode: boolean;
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>;
  event: Event;
}

const AvailabilitiesCalendar: React.FC<Props> = ({
  isSelectionMode,
  setIsSelectionMode,
  event
}) => {
  const {
    currentPage,
    setCurrentPage,
    hours,
    timeSlots,
    totalPages,
    displayDates
  } = useDisplayData(event);

  const { slotSelection } = useSelectTimes(displayDates, timeSlots);
  const { hoveredSlot, selectionHandler } = useSelectionHandlers(event, slotSelection);
  const {
    isSaving,
    onConfirmAvailability,
    onConfirmName,
    onCancelAddAvailability,
    onEditAvailability
  } = useAvailabilityEditor(
    event,
    slotSelection,
    setIsSelectionMode,
    selectionHandler.cancelSelection
  );

  return (
    <>
      <NameInputModal
        onConfirm={onConfirmName}
        onCancel={onCancelAddAvailability}
      />
      <div className="space-y-4">
        <div className="flex gap-6">

          <div className="flex-grow">
            {totalPages > 1 && <Paginator {...{ currentPage, setCurrentPage, totalPages }}/>}
            <CalendarGrid {...{
              event,
              hours,
              displayDates,
              timeSlots,
              selectionHandler
            }}/>
          </div>

          <div className="sm:w-48 w-32 sticky top-0 self-start">
            {isSelectionMode ? (
              <ConfirmAvailabilitySelection
                handleConfirm={onConfirmAvailability}
                handleCancel={selectionHandler.cancelSelection}
                isSaving={isSaving}
              />
            ) : (
              <RespondentsList {...{ event, hoveredSlot, onEditAvailability }}/>
            )}
          </div>

        </div>
      </div>
    </>
  );
};

export default AvailabilitiesCalendar;

