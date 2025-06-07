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

const EventAvailability: React.FC<Props> = ({
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
        <div className="flex sm:gap-6 gap-4">
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
          <div className="flex flex-col gap-2 lg:w-48 md:w-40 sm:w-32 w-24 sticky top-0 self-start">
            <button
              className="btn btn-secondary btn-md md:text-base text-xs w-full"
              onClick={() => setIsSelectionMode(true)}
              disabled={isSelectionMode}
            >
              add availability
            </button>

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

export default EventAvailability;

