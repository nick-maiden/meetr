import React from "react";
import { ChevronLeft, ChevronRight } from "lucide-react";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';

interface Availability {
  users: string[];
  startTime: string;
  endTime: string;
  dates: string[];
  availability: {
    [timeSlot: string]: string[];
  }
}

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

const DUMMY_DATA: Availability = {
  users: ["Alice Johnson", "Bob Smith", "Carol White", "David Brown", "Emily Clark"],
  startTime: "09:00",
  endTime: "23:00",
  dates: [
      "2025-01-17",
      "2025-01-18",
      "2025-01-19",
      "2025-01-20",
      "2025-02-05"
    ],
  availability: {
    // January 17
    "2025-01-17-09:00": ["Alice Johnson", "Bob Smith", "Carol White"],
    "2025-01-17-09:15": ["Alice Johnson", "Bob Smith", "Carol White", "David Brown"],
    "2025-01-17-09:30": ["Alice Johnson", "David Brown", "Emily Clark"],
    "2025-01-17-09:45": ["David Brown", "Emily Clark"],
    "2025-01-17-10:00": ["David Brown", "Emily Clark"],
    "2025-01-17-10:15": ["Emily Clark", "Bob Smith"],
    "2025-01-17-14:00": ["Alice Johnson", "Carol White", "David Brown"],
    "2025-01-17-14:15": ["Alice Johnson", "Carol White"],
    "2025-01-17-14:30": ["Alice Johnson", "Carol White", "Emily Clark"],
    "2025-01-17-14:45": ["Carol White", "Emily Clark", "Bob Smith"],
    "2025-01-17-15:00": ["Carol White", "Bob Smith"],

    // January 18
    "2025-01-18-09:00": ["Alice Johnson", "Carol White"],
    "2025-01-18-09:15": ["Alice Johnson", "Carol White", "David Brown"],
    "2025-01-18-09:30": ["Alice Johnson", "Carol White", "David Brown", "Emily Clark"],
    "2025-01-18-09:45": ["Alice Johnson", "Carol White", "Emily Clark"],
    "2025-01-18-10:00": ["Alice Johnson", "Emily Clark"],
    "2025-01-18-14:00": ["David Brown", "Bob Smith", "Emily Clark"],
    "2025-01-18-14:15": ["David Brown", "Bob Smith"],
    "2025-01-18-14:30": ["David Brown", "Emily Clark"],

    // January 19
    "2025-01-19-09:00": ["Alice Johnson", "Carol White", "David Brown", "Emily Clark", "Bob Smith"],
    "2025-01-19-09:15": ["Alice Johnson", "Carol White", "David Brown", "Emily Clark", "Bob Smith"],
    "2025-01-19-09:30": ["Alice Johnson", "Carol White", "David Brown", "Emily Clark", "Bob Smith"],
    "2025-01-19-14:00": ["David Brown", "Bob Smith"],
    "2025-01-19-14:15": ["David Brown", "Bob Smith", "Emily Clark"],
    "2025-01-19-14:30": ["David Brown", "Emily Clark", "Carol White"],
    "2025-01-19-15:00": ["David Brown", "Carol White"],

    // January 20
    "2025-01-20-09:00": ["Alice Johnson", "David Brown"],
    "2025-01-20-09:15": ["Alice Johnson", "David Brown", "Emily Clark"],
    "2025-01-20-09:30": ["Alice Johnson", "Emily Clark"],
    "2025-01-20-10:00": ["Emily Clark", "Carol White"],
    "2025-01-20-10:15": ["Emily Clark", "Carol White", "Bob Smith"],
    "2025-01-20-14:00": ["David Brown", "Bob Smith", "Emily Clark"],
    "2025-01-20-14:15": ["David Brown", "Bob Smith"],
    "2025-01-20-14:30": ["David Brown", "Emily Clark"],

    // February 5
    "2025-02-05-09:00": ["Carol White", "David Brown"],
    "2025-02-05-09:15": ["Carol White", "David Brown", "Emily Clark"],
    "2025-02-05-09:30": ["David Brown", "Emily Clark", "Bob Smith"],
    "2025-02-05-09:45": ["Emily Clark", "Bob Smith"],
    "2025-02-05-10:00": ["Alice Johnson", "Carol White", "Emily Clark"],  }
};

interface Props {
  isSelectionMode: boolean;
  setIsSelectionMode: React.Dispatch<React.SetStateAction<boolean>>
}

const AvailabilitiesCalendar: React.FC<Props> = ({ isSelectionMode, setIsSelectionMode }) => {
  const [hoveredSlot, setHoveredSlot] = React.useState<HoveredSlot>(null);
  const [currentPage, setCurrentPage] = React.useState(0);
  const [datesPerPage, setDatesPerPage] = React.useState(7);
  const [selectedSlots, setSelectedSlots] = React.useState<Set<string>>(new Set());
  const [isSelecting, setIsSelecting] = React.useState(false);
  const [startPosition, setStartPosition] = React.useState<Position | null>(null);
  const [isDeselecting, setIsDeselecting] = React.useState(false);
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

    const startHour = parseInt(DUMMY_DATA.startTime.split(':')[0]);
    const endHour = parseInt(DUMMY_DATA.endTime.split(':')[0]);

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
    return DUMMY_DATA.dates.map(dateStr => new Date(dateStr));
  };

  const isUserAvailable = (userName: string, date: Date, time: string): boolean => {
    const dateStr = date.toISOString().split('T')[0];
    const timeSlot = `${dateStr}-${time}`;
    return DUMMY_DATA.availability[timeSlot]?.includes(userName) ?? false;
  };

  const getAvailableUsers = (date: Date, time: string): string[] => {
    const dateStr = date.toISOString().split('T')[0];
    const timeSlot = `${dateStr}-${time}`;
    return DUMMY_DATA.availability[timeSlot] ?? [];
  };

  const getUserAvailability = (userName: string) => {
    return new Set(
      Object.entries(DUMMY_DATA.availability)
        .filter(([_, users]) => users.includes(userName))
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
        DUMMY_DATA.users.length
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
    if (userName === "") {
      // New user, need to get their name first.
      document.getElementById('name_input_modal').showModal()
    } else {
      // Existing user, update their availability.
      saveUserAvailability();
    }
  };

  const saveUserAvailability = () => {
    // TODO -> plug in to backend
    console.log(userName);
    console.log(selectedSlots);
    document.getElementById('name_input_modal').close()
    setUserName("");
    setIsSelectionMode(false);
    setSelectedSlots(new Set());
    setIsSelecting(false);
    setStartPosition(null);
    setIsDeselecting(false);
  };

  const editUserAvailability = (userName: string) => {
    setUserName(userName);
    setSelectedSlots(getUserAvailability(userName));
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
                onClick={saveUserAvailability}
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
                  {DUMMY_DATA.users.map(userName => {
                    const isAvailable = hoveredSlot ?
                      isUserAvailable(
                        userName,
                        new Date(hoveredSlot.split('-').slice(0, 3).join('-')),
                        hoveredSlot.split('-').slice(3).join('-')
                      ) : true;
  
                    return (
                      <li key={userName}>
                        <div
                          className={`
                            flex justify-between
                            hover:pl-1
                            transition-[padding,text-decoration,color] duration-500 ease-in-out hover:ease-out
                            w-full font-bold md:text-base sm:text-sm text-xs
                            ${!isAvailable && hoveredSlot ? 'line-through text-gray-500' : ''}
                          `}
                        >
                          <p className="truncate">{userName}</p>
                          <button
                            onClick={() => editUserAvailability(userName)}
                          >
                            <FontAwesomeIcon icon={faPenToSquare} />
                          </button>
                        </div>
                      </li>
                    );
                  })}
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
