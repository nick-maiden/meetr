import React from "react";
import { ChevronLeft, ChevronRight } from "lucide-react";
import { postRequest, putRequest } from "../../../../util/api";
import { generateTimeData } from "./util";
import { Event } from "../../../../types";
import { Context } from "../../../../util/context";
import { errorCodeMap} from "../../../../err";
import ConfirmAvailabilitySelection from "./components/ConfirmAvailabilitySelection";
import RespondentsList from "./components/RespondentsList";
import NameInputModal from "./components/NameInputModal";
import useSlotSelection from "../../../../hooks/useSlotSelection/useSlotSelection";
import { Slots } from "../../../../hooks/useSlotSelection/types";
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
  const [hoveredSlot, setHoveredSlot] = React.useState<string | null>(null);
  const [currentPage, setCurrentPage] = React.useState(0);
  const [datesPerPage, setDatesPerPage] = React.useState(7);
  const [userId, setUserId] = React.useState<string | null>(null);
  const [userName, setUserName] = React.useState<string>("");
  const [isSaving, setIsSaving] = React.useState(false);
  const [hasConfirmedName, setHasConfirmedName] = React.useState(false);
  const { setErrorMessage } = React.useContext(Context);

  const serializeSlot = (date: string, time: string, row: number, col: number): AvailabilitySlot => {
    return { id: date + '-' + time, row, col }
  };

  const getSlotsInSelection = (start: AvailabilitySlot, end: AvailabilitySlot): Slots => {
    const minCol = Math.min(start.col, end.col);
    const maxCol = Math.max(start.col, end.col);
    const minRow = Math.min(start.row, end.row);
    const maxRow = Math.max(start.row, end.row);

    const slots = new Set<string>;

    for (let col = minCol; col <= maxCol; col++) {
      for (let row = minRow; row <= maxRow; row++) {
        const date = displayDates[col];
        const time = timeSlots[row];
        const slot = serializeSlot(date, time, row, col);
        slots.add(slot.id);
      }
    }
    return slots;
  };

  const {
    selectedSlots,
    setSelectedSlots,
    handleSelectionStart,
    handleSelectionMove,
    handleSelectionEnd,
    handleCancelSelection
  } = useSlotSelection(getSlotsInSelection);


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

  const getDates = (): string[] => {
    return event.dates;
  };

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
    handleCancelSelection();
  };

  const { hours, timeSlots } = generateTimeData(event.earliestTime, event.latestTime);
  const allDates = getDates();
  const totalPages = Math.ceil(allDates.length / datesPerPage);
  const displayDates = allDates.slice(
    currentPage * datesPerPage,
    (currentPage + 1) * datesPerPage
  );

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
            {totalPages > 1 && (
              <div className="flex justify-between items-center mb-4">
                <button
                  className="btn btn-outline sm:btn-sm btn-xs"
                  onClick={() => setCurrentPage(prev => Math.max(0, prev - 1))}
                  disabled={currentPage === 0}
                >
                  <ChevronLeft className="h-4 w-4" />
                </button>
                <span className="font-bold md:text-xl sm:text-lg text-md">
                  page {currentPage + 1} of {totalPages}
                </span>
                <button
                  className="btn btn-outline sm:btn-sm btn-xs"
                  onClick={() => setCurrentPage(prev => Math.min(totalPages - 1, prev + 1))}
                  disabled={currentPage === totalPages - 1}
                >
                  <ChevronRight className="h-4 w-4" />
                </button>
              </div>
            )}

            <div className="overflow-x-auto">
              <table className="table-auto table-compact w-full min-w-[190px]">
                <thead>
                  <tr>
                    <th className="w-0"></th>
                    {displayDates.map((date, i) => {
                      const [weekday, monthDay] = new Date(date).toLocaleDateString('en-US', {
                        weekday: 'short',
                        month: 'short',
                        day: '2-digit',
                      }).split(", ");
                      return (
                        <th key={i} className="text-center border-b border-neutral">
                          <div className="text-xs text-neutral-500">{monthDay}</div>
                          <div className="sm:text-lg text-md">{weekday}</div>
                        </th>
                      );
                    })}
                  </tr>
                </thead>
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
                                const slot = serializeSlot(date, time, row, col);
                                const border = quarter === 1 ? "border-b border-dotted border-neutral" : "";
                                const backgroundColor = getTimeSlotBackgroundColor(slot.id, date, time);
                                return (
                                  <div
                                    key={`${date}-${hour}-${quarter}`}
                                    className={`h-3 ${border} cursor-pointer`}
                                    style={{ backgroundColor }}
                                    onPointerDown={(e) => {
                                      e.preventDefault();
                                      if (isSelectionMode) handleSelectionStart(slot);
                                      e.currentTarget.releasePointerCapture(e.pointerId)
                                    }}
                                    onPointerEnter={() => {
                                      isSelectionMode ? handleSelectionMove(slot) : setHoveredSlot(slot.id);
                                    }}
                                    onPointerUp={handleSelectionEnd}
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

