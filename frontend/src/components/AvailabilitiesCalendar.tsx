import React from "react";
import { ChevronLeft, ChevronRight } from "lucide-react";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';
import { postRequest, putRequest } from "../util/api";


interface TimeData {
  displaySlots: string[];
  dataSlots: string[];
}

interface Position {
  hour: number;
  quarter: number;
  date: number;
}

type HoveredSlot = string | null;

// these need to be moved somewhere more appropriate
type User = {
  id: number,
  name: string
}

type Event = {
  id: number,
  name: string,
  users: {
    [userId: number]: User
  },
  dates: [string],
  earliestTime: string,
  latestTime: string,
  availabilities: {
    [timeSlot: string]: number[];
  }
}

interface Props {
  isSelectionMode: boolean;
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>;
  event: Event;
}

const AvailabilitiesCalendar: React.FC<Props> = ({ isSelectionMode, setIsSelectionMode, event }) => {
  const [hoveredSlot, setHoveredSlot] = React.useState<HoveredSlot>(null);
  const [currentPage, setCurrentPage] = React.useState(0);
  const [datesPerPage, setDatesPerPage] = React.useState(7);
  const [selectedSlots, setSelectedSlots] = React.useState<Set<string>>(new Set());
  const [isSelecting, setIsSelecting] = React.useState(false);
  const [startPosition, setStartPosition] = React.useState<Position | null>(null);
  const [isDeselecting, setIsDeselecting] = React.useState(false);
  const [userId, setUserId] = React.useState<number | null>(null);
  const [userName, setUserName] = React.useState<string>("");

  React.useEffect(() => {
    // Show an appropriate number of dates per page depending on screen size.
    const handleResize = () => {
      if (window.innerWidth <= 600) { setDatesPerPage(4); }
      else if (window.innerWidth <= 900) { setDatesPerPage(5); }
      else { setDatesPerPage(7); }
    };
    window.addEventListener('resize', handleResize);
    handleResize();
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  const generateTimeData = (): TimeData => {
    const displaySlots: string[] = [];
    const dataSlots: string[] = [];

    const startHour = parseInt(event.earliestTime.split(':')[0]);
    const endHour = parseInt(event.latestTime.split(':')[0]);

    for (let hour = startHour; hour < endHour; hour++) {
      const hour12 = hour % 12 || 12;
      const ampm = hour < 12 ? 'am' : 'pm';
      displaySlots.push(`${hour12}${ampm}`);

      for (let minute = 0; minute < 60; minute += 15) {
        dataSlots.push(
          `${hour.toString().padStart(2, '0')}:${minute.toString().padStart(2, '0')}`
        );
      }
    }

    return { displaySlots, dataSlots };
  };

  const getDates = (): Date[] => {
    return event.dates.map(dateStr => new Date(dateStr));
  };

  const isUserAvailable = (userId: number, date: Date, time: string): boolean => {
    const dateStr = date.toISOString().split('T')[0];
    const timeSlot = `${dateStr}-${time}`;
    return event.availabilities[timeSlot]?.includes(userId) ?? false;
  };

  const getAvailableUsers = (date: Date, time: string): number[] => {
    const dateStr = date.toISOString().split('T')[0];
    const timeSlot = `${dateStr}-${time}`;
    return event.availabilities[timeSlot] ?? [];
  };

  const getUserAvailability = (userId: number) => {
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

  const getTimeSlotBackgroundColor = (slotId: string, date: Date, timeSlot: string) => {
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

  const handleSelectionStart = (hourIndex: number, quarter: number, dateIndex: number, slotId: string) => {
    setIsSelecting(true);
    setStartPosition({ hour: hourIndex, quarter, date: dateIndex });
    setIsDeselecting(selectedSlots.has(slotId));

    setSelectedSlots(prev => {
      const next = new Set(prev);
      if (selectedSlots.has(slotId)) {
        next.delete(slotId);
      } else {
        next.add(slotId);
      }
      return next;
    });
  };

  const handleSelectionMove = (hourIndex: number, quarter: number, dateIndex: number) => {
  if (!isSelecting || !startPosition) return;

  const minDate = Math.min(startPosition.date, dateIndex);
  const maxDate = Math.max(startPosition.date, dateIndex);

  // Calculate actual slot positions (convert hour+quarter to absolute position)
  const startSlot = startPosition.hour * 4 + startPosition.quarter;
  const currentSlot = hourIndex * 4 + quarter;
  const minSlot = Math.min(startSlot, currentSlot);
  const maxSlot = Math.max(startSlot, currentSlot);

  setSelectedSlots(prev => {
    const next = new Set(prev);

    for (let date = minDate; date <= maxDate; date++) {
      for (let slot = minSlot; slot <= maxSlot; slot++) {
        const timeSlot = dataSlots[slot];
        const currentDate = displayDates[date];
        const slotId = `${currentDate.toISOString().split('T')[0]}-${timeSlot}`;

        if (isDeselecting) {
          next.delete(slotId);
        } else {
          next.add(slotId);
        }
      }
    }

    return next;
  });
};

  const handleSelectionEnd = () => {
    setIsSelecting(false);
    setStartPosition(null);
  };

  const checkUser = () => {
    if (!userId) {
      // New user, need to get their name first.
      document.getElementById('name_input_modal').showModal()
    } else {
      // Existing user, update their availability.
      updateUserAvailability();
    }
  };

  const saveNewUserAvailability = () => {
    document.getElementById('name_input_modal').close()
    const userAvailability = {
      name: userName,
      availability: Array.from(selectedSlots)
    }
    postRequest(`/events/${event.id}/availability`, userAvailability)
      .then((_) => {
        cancelSetUserAvailability();
      });
  };

  const updateUserAvailability = () => {
    const updatedAvailability = {availability: Array.from(selectedSlots)}
    putRequest(`/events/${event.id}/availability/${userId}`, updatedAvailability)
      .then(() => {
        cancelSetUserAvailability();
      });
  };

  const editUserAvailability = (userId: number) => {
    setUserId(userId);
    setSelectedSlots(getUserAvailability(userId));
    setIsSelectionMode(true);
  };

  const cancelSetUserAvailability = () => {
    setUserName("");
    setIsSelectionMode(false);
    setSelectedSlots(new Set());
    setIsSelecting(false);
    setStartPosition(null);
    setIsDeselecting(false);
  };

  const { displaySlots, dataSlots } = generateTimeData();
  const allDates = getDates();
  const totalPages = Math.ceil(allDates.length / datesPerPage);
  const displayDates = allDates.slice(
    currentPage * datesPerPage,
    (currentPage + 1) * datesPerPage
  );

  return (
    <>
      <dialog id="name_input_modal" className="modal">
        <div className="modal-box">
          <article className="prose">
            <h2 className="font-bold text-3xl">save availability</h2>
            <div className="flex flex-col gap-y-4">
              <input
                type="text"
                placeholder="enter your name..."
                className="input input-bordered w-full "
                autoFocus
                value={userName}
                onChange={(event) => setUserName(event.target.value)}
              />
              <button
                className="btn btn-secondary self-end w-[30%] text-lg"
                disabled={ userName.length === 0 }
                onClick={saveNewUserAvailability}
              >
                continue
              </button>
            </div>
          </article>
        </div>
        <form method="dialog" className="modal-backdrop">
          <button onClick={() => setUserName("")}>close</button>
        </form>
      </dialog>
      <div className="space-y-4">
        <div className="flex gap-6">
          <div className="flex-grow">
            {totalPages > 1 &&
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
            }

            <div className="overflow-x-auto">
              <table 
                className="table-auto table-compact w-full min-w-[190px]"
                onMouseUp={handleSelectionEnd}
                onMouseLeave={handleSelectionEnd}
              >
                <thead>
                  <tr>
                    <th className="w-0"></th>
                    {displayDates.map((date, i) => {
                      const [weekday, monthDay] = date.toLocaleDateString('en-US', {
                        weekday: 'short',
                        month: 'short',
                        day: '2-digit',
                      }).split(", ");
                      return (
                        <th key={i} className="text-center border-b border-neutral">
                          <div className="text-xs text-neutral-500">{monthDay}</div>
                          <div className="sm:text-lg text-md">{weekday}</div>
                        </th>
                      )
                    })}
                  </tr>
                </thead>
                <tbody>
                  {displaySlots.map((displayTime, hourIndex) => (
                    <>
                      <tr key={`hour-${hourIndex}`}>
                        <td className="font-bold border-b border-t border-r border-r-neutral border-b-base-100 border-t-base-100 text-right p-0 sm:pr-2 pr-1 sm:text-md text-sm select-none">
                          {displayTime}
                        </td>
                        {displayDates.map((date, dateIndex) => (
                          <td
                            key={`hour-${hourIndex}-${date.toISOString()}`}
                            className="p-0 border-b border-r border-neutral"
                          >
                            <div className="grid grid-rows-4">
                              {[0, 1, 2, 3].map((quarter) => {
                                const timeSlot = dataSlots[hourIndex * 4 + quarter];
                                const slotId = `${date.toISOString().split('T')[0]}-${timeSlot}`;
                                const border = quarter === 1 ? "border-b border-dotted border-neutral" : "";
                                const backgroundColor = getTimeSlotBackgroundColor(slotId, date, timeSlot);
                                return (
                                  <div
                                    key={`${date.toISOString()}-${quarter}`}
                                    className={`h-3 ${border} cursor-pointer`}
                                    style={{ backgroundColor }}
                                    onMouseDown={() => isSelectionMode && handleSelectionStart(hourIndex, quarter, dateIndex, slotId)}
                                    onMouseEnter={() => {
                                      if (isSelectionMode) {
                                        handleSelectionMove(hourIndex, quarter, dateIndex);
                                      } else {
                                        setHoveredSlot(`${date.toISOString().split('T')[0]}-${timeSlot}`);
                                      }
                                    }}
                                    onMouseLeave={() => !isSelectionMode && setHoveredSlot(null)}
                                  />
                                );
                              })}
                            </div>
                          </td>
                        ))}
                      </tr>
                      {hourIndex < displaySlots.length - 1 && (
                        <tr className="h-0">
                          <td className="p-0"></td>
                          {displayDates.map((_, i) => (
                            <td key={i} className="p-0"></td>
                          ))}
                        </tr>
                      )}
                    </>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          <div className="sm:w-48 w-32 sticky top-0 self-start">
            {isSelectionMode ? (
              <div className="bg-base-200 p-4 rounded-lg space-y-4">
                <button 
                  className="btn sm:btn-md btn-sm btn-outline btn-block sm:text-lg text-md"
                  onClick={cancelSetUserAvailability}
                >
                  cancel
                </button>

                <button 
                  className="btn sm:btn-md btn-sm btn-secondary btn-block sm:text-lg text-md"
                  onClick={checkUser}
                >
                  save
                </button>
              </div>
            ) : (
              <div className="bg-base-200 p-4 rounded-lg overflow-y-auto no-scrollbar max-h-[70vh]">
                <h2 className="font-bold md:text-2xl sm:text-xl text-md">respondents</h2>
                <div className="divider mt-2"></div>
                <ul className="sm:space-y-2 space-y-1.5">
                  {Object.values(event.users).length === 0 ? (
                    <p className="font-bold text-gray-500 text-sm">
                      no respondents yet 😢
                    </p>
                  ) : (
                    Object.values(event.users).map((user) => {
                      const isAvailable = hoveredSlot ?
                        isUserAvailable(
                          user.id,
                          new Date(hoveredSlot.split('-').slice(0, 3).join('-')),
                          hoveredSlot.split('-').slice(3).join('-')
                        ) : true;
                      return (
                        <li key={user.id}>
                          <div
                            className={`
                              flex justify-between
                              hover:pl-1
                              transition-[padding,text-decoration,color] duration-500 ease-in-out hover:ease-out
                              w-full font-bold md:text-base sm:text-sm text-xs
                              ${!isAvailable && hoveredSlot ? 'line-through text-gray-500' : ''}
                            `}
                          >
                            <p className="truncate">{user.name}</p>
                            <button
                              onClick={() => editUserAvailability(user.id)}
                            >
                              <FontAwesomeIcon icon={faPenToSquare} />
                            </button>
                          </div>
                        </li>
                      );
                    })
                  )}
                </ul>
              </div>
            )}
          </div>
        </div>
      </div>
    </>
  );
};

export default AvailabilitiesCalendar;
