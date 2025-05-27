import React from "react";
import { generateTimeData } from "./util";
import { Event } from "../../../../types";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import useSelectTimes from "./hooks/useSelectTimes";
import Paginator from "./components/Paginator";
import CalendarGrid from "./components/CalendarGrid";
import useModifyAvailability from "./hooks/useHandleAvailability";
import useResponsiveDatesPerPage from "./hooks/useResponsiveDatesPerPage";
import { AvailabilitySlot } from "./types";

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

  const {
    selectedSlots,
    setSelectedSlots,
    slotSelection
  } = useSelectTimes(displayDates, timeSlots);

  const cancelSetUserAvailability = () => {
    setUserName("");
    setIsSelectionMode(false);
    slotSelection.cancel();
  };

  const {
    saveNewUserAvailability,
    updateUserAvailability
  } = useModifyAvailability(
    event,
    cancelSetUserAvailability,
    setIsSaving,
    setHasConfirmedName,
    setUserName
  );

  const isUserAvailable = (userId: string, slot: AvailabilitySlot): boolean => {
    return event.availabilities[slot.id]?.includes(userId) ?? false;
  };

  const getUserAvailability = (userId: string) => {
    return new Set(
      Object.entries(event.availabilities)
        .filter(([_, userIds]) => userIds.includes(userId))
        .map(([timeSlot]) => timeSlot)
        .sort()
    );
  }

  const checkUser = () => {
    if (!userId) {
      (document.getElementById('name_input_modal') as HTMLDialogElement)?.showModal();
    } else {
      updateUserAvailability(userId, Array.from(selectedSlots));
    }
  };

  const editUserAvailability = (userId: string) => {
    setUserId(userId);
    setSelectedSlots(getUserAvailability(userId));
    setIsSelectionMode(true);
  };

  return (
    <>
      <NameInputModal
        userName={userName}
        onNameChange={setUserName}
        onSave={() => saveNewUserAvailability(Array.from(selectedSlots), userName)}
        onClose={() => {
          setIsSaving(false);
          setUserName("");
        }}
        hasConfirmedName={hasConfirmedName}
        setHasConfirmedName={setHasConfirmedName}
      />
      <div className="space-y-4">
        <div className="flex gap-6">
          <div className="flex-grow">
            {totalPages > 1 && <Paginator {...{ currentPage, setCurrentPage, totalPages }}/>}

            <CalendarGrid {...
              {
                hours,
                displayDates,
                timeSlots,
                isSelectionMode,
                event,
                setHoveredSlot,
                slotSelection
              }}
            />
          </div>

          <div className="sm:w-48 w-32 sticky top-0 self-start">
            {isSelectionMode ? (
              <ConfirmAvailabilitySelection
                onCancel={cancelSetUserAvailability}
                onSave={checkUser}
                isSaving={isSaving}
                setIsSaving={setIsSaving}
              />
            ) : (
              <RespondentsList
                users={event.users}
                hoveredSlot={hoveredSlot}
                isUserAvailable={isUserAvailable}
                onEditAvailability={editUserAvailability}
              />
            )}
          </div>
        </div>
      </div>
    </>
  );
};

export default AvailabilitiesCalendar;

