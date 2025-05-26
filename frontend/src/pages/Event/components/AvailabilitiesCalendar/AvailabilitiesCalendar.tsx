import React from "react";
import { postRequest, putRequest } from "../../../../util/api";
import { generateTimeData } from "./util";
import { Event } from "../../../../types";
import { Context } from "../../../../util/context";
import { errorCodeMap} from "../../../../err";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import { AvailabilitySlot } from "./types";
import useSelectTimes from "./hooks/useSelectTimes";
import Paginator from "./components/Paginator";
import DayHeadings from "./components/DayHeadings";

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
  const [hoveredSlot, setHoveredSlot] = React.useState<string | null>(null);
  const [currentPage, setCurrentPage] = React.useState(0);
  const [datesPerPage, setDatesPerPage] = React.useState(7);
  const [userId, setUserId] = React.useState<string | null>(null);
  const [userName, setUserName] = React.useState<string>("");
  const [isSaving, setIsSaving] = React.useState(false);
  const [hasConfirmedName, setHasConfirmedName] = React.useState(false);
  const { setErrorMessage } = React.useContext(Context);
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


  React.useEffect(() => {
    const handleResize = () => {
      if (window.innerWidth <= 600) { setDatesPerPage(4); }
      else if (window.innerWidth <= 900) { setDatesPerPage(5); }
      else { setDatesPerPage(7); }
    };
    window.addEventListener('resize', handleResize);
    handleResize();
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  const isUserAvailable = (userId: string, date: Date, time: string): boolean => {
    const dateStr = date.toISOString().split('T')[0];
    const timeSlot = `${dateStr}-${time}`;
    return event.availabilities[timeSlot]?.includes(userId) ?? false;
  };

  const getAvailableUsers = (date: string, time: string): string[] => {
    const timeSlot = `${date}-${time}`;
    return event.availabilities[timeSlot] ?? [];
  };

  const getUserAvailability = (userId: string) => {
    return new Set(
      Object.entries(event.availabilities)
        .filter(([_, userIds]) => userIds.includes(userId))
        .map(([timeSlot]) => timeSlot)
        .sort()
    );
  }

  const getAvailabilityColor = (availableCount: number, totalUsers: number): string => {
    if (availableCount === 0) return "transparent";

    const proportion = availableCount / totalUsers;
    const red = Math.round(255 - (255 - 34) * proportion);
    const green = Math.round(255 - (255 - 197) * proportion);
    const blue = Math.round(255 - (255 - 94) * proportion);

    return `rgb(${red}, ${green}, ${blue})`;
  };

  const getTimeSlotBackgroundColor = (slotId: string, date: string, timeSlot: string) => {
    const isSelected = selectedSlots.has(slotId);
    if (isSelectionMode) {
      return isSelected ? 'rgb(34, 197, 94)' : 'rgb(254, 202, 202)';
    } else {
      const availableUsers = getAvailableUsers(date, timeSlot);
      return getAvailabilityColor(
        availableUsers.length,
        Object.keys(event.users).length
      );
    }
  }

  const checkUser = () => {
    if (!userId) {
      (document.getElementById('name_input_modal') as HTMLDialogElement)?.showModal();
    } else {
      updateUserAvailability();
    }
  };

  const saveNewUserAvailability = () => {
    const userAvailability = {
      name: userName,
      availability: Array.from(selectedSlots)
    };
    postRequest(`/events/${event.id}/availability`, userAvailability)
      .then(() => {
        cancelSetUserAvailability();
      })
      .catch((err) => {
        setUserName("");
        setErrorMessage(errorCodeMap[err.response?.data] ?? "unexpected error, please try again later");
      })
      .finally(() => {
        (document.getElementById('name_input_modal') as HTMLDialogElement)?.close();
        setIsSaving(false);
        setHasConfirmedName(false);
      });
  };

  const updateUserAvailability = () => {
    const updatedAvailability = {availability: Array.from(selectedSlots)};
    putRequest(`/events/${event.id}/availability/${userId}`, updatedAvailability)
      .catch(() => {
        setErrorMessage('unable to edit availability, please try again later');
      })
      .finally(() => {
        cancelSetUserAvailability();
        setIsSaving(false);
      });
  };

  const editUserAvailability = (userId: string) => {
    setUserId(userId);
    setSelectedSlots(getUserAvailability(userId));
    setIsSelectionMode(true);
  };

  const cancelSetUserAvailability = () => {
    setUserName("");
    setIsSelectionMode(false);
    slotSelection.cancel();
  };

  return (
    <>
      <NameInputModal
        userName={userName}
        onNameChange={setUserName}
        onSave={saveNewUserAvailability}
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

            <div className="overflow-x-auto">
              <table className="table-auto table-compact w-full min-w-[190px]">
                <DayHeadings dates={displayDates.map(d => new Date(d))} />
                <tbody>
                  {hours.map((hour, hourIndex) => (
                    <React.Fragment key={`${hour}`}>
                      <tr>
                        <td className="font-bold border-b border-t border-r border-r-neutral border-b-base-100 border-t-base-100 text-right p-0 sm:pr-2 pr-1 sm:text-md text-sm select-none">
                          {hour}
                        </td>
                        {displayDates.map((date, col) => (
                          <td
                            key={`${date}-${hour}`}
                            className="p-0 border-b border-r border-neutral"
                          >
                            <div className="grid grid-rows-4">
                              {[0, 1, 2, 3].map((quarter) => {
                                const row = hourIndex * 4 + quarter;
                                const time = timeSlots[row];
                                const slot = new AvailabilitySlot(date, time, row, col);
                                const border = quarter === 1 ? "border-b border-dotted border-neutral" : "";
                                const backgroundColor = getTimeSlotBackgroundColor(slot.id, date, time);
                                return (
                                  <div
                                    key={`${date}-${hour}-${quarter}`}
                                    className={`h-3 ${border} cursor-pointer`}
                                    style={{ backgroundColor }}
                                    onPointerDown={(e) => {
                                      e.preventDefault();
                                      if (isSelectionMode) slotSelection.start(slot);
                                      e.currentTarget.releasePointerCapture(e.pointerId)
                                    }}
                                    onPointerEnter={() => {
                                      isSelectionMode ? slotSelection.move(slot) : setHoveredSlot(slot.id);
                                    }}
                                    onPointerUp={() => slotSelection.end()}
                                    onPointerLeave={() => !isSelectionMode && setHoveredSlot(null)}
                                  />
                                );
                              })}
                            </div>
                          </td>
                        ))}
                      </tr>
                      {hourIndex < hours.length - 1 && (
                        <tr className="h-0">
                          <td className="p-0"></td>
                          {displayDates.map((_, i) => (
                            <td key={i} className="p-0"></td>
                          ))}
                        </tr>
                      )}
                    </React.Fragment>
                  ))}
                </tbody>
              </table>
            </div>
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

