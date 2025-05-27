import React from "react";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import useSelectTimes from "./hooks/useSelectTimes";
import Paginator from "./components/Paginator";
import CalendarGrid from "./components/CalendarGrid";
import { AvailabilitySlot } from "./types";
import { SelectionContext, UserContext } from "./contexts";
import { Event } from "../../../../types";
import useUserContext from "./hooks/useUserContext";
import useDisplayData from "./hooks/useDisplayData";

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
  const {
    currentPage,
    setCurrentPage,
    hours,
    timeSlots,
    totalPages,
    displayDates
  } = useDisplayData(event);
  const { slotSelection } = useSelectTimes(displayDates, timeSlots);
  const userContext = useUserContext();

  const cancelSetUserAvailability = () => {
    userContext.setUserName("");
    setIsSelectionMode(false);
    slotSelection.cancel();
  };

  return (
    <SelectionContext.Provider value={{
      isSelectionMode,
      setIsSelectionMode,
      slotSelection,
      cancelSetUserAvailability,
    }}>
      <UserContext.Provider value={userContext}>
        <NameInputModal event={event}/>
        <div className="space-y-4">
          <div className="flex gap-6">
            <div className="flex-grow">
              {totalPages > 1 && <Paginator {...{ currentPage, setCurrentPage, totalPages }}/>}

              <CalendarGrid {...{
                event,
                hours,
                displayDates,
                timeSlots,
                setHoveredSlot
              }}/>
            </div>

            <div className="sm:w-48 w-32 sticky top-0 self-start">
              {isSelectionMode
                ? <ConfirmAvailabilitySelection event={event}/>
                : <RespondentsList {...{ event, hoveredSlot }}/>
              }
            </div>
          </div>
        </div>
      </UserContext.Provider>
    </SelectionContext.Provider>
  );
};

export default AvailabilitiesCalendar;

