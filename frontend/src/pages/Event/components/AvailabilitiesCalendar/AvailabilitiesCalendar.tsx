import React from "react";
import { generateTimeData } from "./util";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import useSelectTimes from "./hooks/useSelectTimes";
import Paginator from "./components/Paginator";
import CalendarGrid from "./components/CalendarGrid";
import useResponsiveDatesPerPage from "./hooks/useResponsiveDatesPerPage";
import { AvailabilitySlot } from "./types";
import { AvailabilityContext } from "./AvailabilityContext";
import { Event } from "../../../../types";

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
  const [hoveredSlot, setHoveredSlot] = React.useState<AvailabilitySlot | null>(null);
  const [currentPage, setCurrentPage] = React.useState(0);
  const [userId, setUserId] = React.useState<string | null>(null);
  const [userName, setUserName] = React.useState<string>("");
  const [isSaving, setIsSaving] = React.useState(false);
  const [hasConfirmedName, setHasConfirmedName] = React.useState(false);
  const datesPerPage = useResponsiveDatesPerPage();
  const { hours, timeSlots } = generateTimeData(event.earliestTime, event.latestTime);
  const totalPages = Math.ceil(event.dates.length / datesPerPage);
  const displayDates = event.dates.slice(
    currentPage * datesPerPage,
    (currentPage + 1) * datesPerPage
  );
  const { slotSelection } = useSelectTimes(displayDates, timeSlots);

  const cancelSetUserAvailability = () => {
    setUserName("");
    setIsSelectionMode(false);
    slotSelection.cancel();
  };

  return (
    <AvailabilityContext.Provider value={{
      isSelectionMode,
      setIsSelectionMode,
      hoveredSlot,
      setHoveredSlot,
      isSaving,
      setIsSaving,
      userName,
      setUserName,
      userId,
      setUserId,
      hasConfirmedName,
      setHasConfirmedName,
      event,
      hours,
      timeSlots,
      displayDates,
      totalPages,
      slotSelection,
      cancelSetUserAvailability,
    }}>
      <NameInputModal />
      <div className="space-y-4">
        <div className="flex gap-6">
          <div className="flex-grow">
            {totalPages > 1 && <Paginator {...{ currentPage, setCurrentPage, totalPages }}/>}

            <CalendarGrid />
          </div>

          <div className="sm:w-48 w-32 sticky top-0 self-start">
            {isSelectionMode
              ? <ConfirmAvailabilitySelection />
              : <RespondentsList />
            }
          </div>
        </div>
      </div>
    </AvailabilityContext.Provider>
  );
};

export default AvailabilitiesCalendar;

