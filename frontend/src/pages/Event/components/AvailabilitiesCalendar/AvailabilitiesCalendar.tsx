import React from "react";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import useSelectTimes from "./hooks/useSelectTimes";
import Paginator from "./components/Paginator";
import CalendarGrid from "./components/CalendarGrid";
import { Event } from "global/types";
import useSelectionHandlers from "./hooks/useSelectionHandlers";
import useAvailabilityEditor from "./hooks/useAvailabilityEditor";
import useResponsiveDatesPerPage from "./hooks/useResponsiveDatesPerPage";
import { generateTimeData } from "./utils";

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
  const [currentPage, setCurrentPage] = React.useState(0);
  const datesPerPage = useResponsiveDatesPerPage();
  const { hours, timeSlots } = generateTimeData(event.earliestTime, event.latestTime);
  const totalPages = Math.ceil(event.dates.length / datesPerPage);
  const displayDates = event.dates.slice(currentPage * datesPerPage, (currentPage + 1) * datesPerPage);
  const { slotSelection } = useSelectTimes(displayDates, timeSlots);
  const { hoveredSlot, selectionHandler } = useSelectionHandlers(
    event,
    slotSelection,
    isSelectionMode,
    setIsSelectionMode
  );
  const {
    isSaving,
    onConfirmAvailability,
    onConfirmName,
    onCancelConfirmName,
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
        onCancel={onCancelConfirmName}
      />
      <div className="space-y-4">
        <div className="flex gap-6">

          <div className="flex-grow">
            {totalPages > 1 && <Paginator {...{ currentPage, setCurrentPage, totalPages }}/>}
            <CalendarGrid {...{
              hours,
              displayDates,
              timeSlots,
              selectionHandler,
              isSelectionMode
            }}/>
          </div>

          <div className="md:w-48 sm:w-40 w-32 sticky top-0 self-start">
            {isSelectionMode ? (
              <ConfirmAvailabilitySelection
                onConfirm={onConfirmAvailability}
                onCancel={selectionHandler.cancelSelection}
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

