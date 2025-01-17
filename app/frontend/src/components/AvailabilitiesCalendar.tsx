import { useState } from 'react';
import { ChevronLeft, ChevronRight } from "lucide-react";

interface Availability {
  users: string[];
  startTime: string;
  endTime: string;
  availability: {
    [timeSlot: string]: string[];
  }
}

interface TimeData {
  displaySlots: string[];
  dataSlots: string[];
}

type HoveredSlot = string | null;

const DUMMY_DATA: Availability = {
  users: ["Alice Johnson", "Bob Smith", "Carol White", "David Brown", "Emily Clark"],
  startTime: "09:00",
  endTime: "17:00",
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

const DATES_PER_PAGE = 4;

const AvailabilitiesCalendar: React.FC = () => {
  const [hoveredSlot, setHoveredSlot] = useState<HoveredSlot>(null);
  const [currentPage, setCurrentPage] = useState(0);

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
    const dates: Date[] = [];
    const today = new Date();
    for (let i = 0; i < 21; i++) {
      const date = new Date(today);
      date.setDate(today.getDate() + i);
      dates.push(date);
    }
    return dates;
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

  const getAvailabilityColor = (availableCount: number, totalUsers: number): string => {
    if (availableCount === 0) return "base";

    const proportion = availableCount / totalUsers;
    const red = Math.round(255 - (255 - 34) * proportion);
    const green = Math.round(255 - (255 - 197) * proportion);
    const blue = Math.round(255 - (255 - 94) * proportion);

    return `rgb(${red}, ${green}, ${blue})`;
  };

  const { displaySlots, dataSlots } = generateTimeData();
  const allDates = getDates();
  const totalPages = Math.ceil(allDates.length / DATES_PER_PAGE);
  const displayDates = allDates.slice(
    currentPage * DATES_PER_PAGE,
    (currentPage + 1) * DATES_PER_PAGE
  );

  return (
    <div className="flex gap-6">
      <div className="flex-grow">
        <div className="flex justify-between items-center mb-4">
          <button
            className="btn btn-outline btn-sm"
            onClick={() => setCurrentPage(prev => Math.max(0, prev - 1))}
            disabled={currentPage === 0}
          >
            <ChevronLeft className="h-4 w-4" />
          </button>
          <span className="font-bold text-xl">
            page {currentPage + 1} of {totalPages}
          </span>
          <button
            className="btn btn-outline btn-sm"
            onClick={() => setCurrentPage(prev => Math.min(totalPages - 1, prev + 1))}
            disabled={currentPage === totalPages - 1}
          >
            <ChevronRight className="h-4 w-4" />
          </button>
        </div>

        <div className="overflow-x-auto">
          <table className="table-auto table-compact w-full min-w-[180px]">
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
                      <div className="text-lg">{weekday}</div>
                    </th>
                  )
                })}
              </tr>
            </thead>
            <tbody>
              {displaySlots.map((displayTime, hourIndex) => (
                <>
                  <tr key={`hour-${hourIndex}`}>
                    <td className="font-bold border-b border-t border-r border-r-neutral border-b-base-100 border-t-base-100 text-right p-0 pr-2">
                      {displayTime}
                    </td>
                    {displayDates.map((date, _) => (
                      <td
                        key={`hour-${hourIndex}-${date.toISOString()}`} 
                        className="p-0 border-b border-r border-neutral"
                      >
                        <div className="grid grid-rows-4">
                          {[0, 1, 2, 3].map((quarter) => {
                            const timeSlot = dataSlots[hourIndex * 4 + quarter];
                            const availableUsers = getAvailableUsers(date, timeSlot);
                            const backgroundColor = getAvailabilityColor(
                              availableUsers.length,
                              DUMMY_DATA.users.length
                            );
                            const border = quarter === 1 ? "border-b border-dotted border-neutral" : "";

                            return (
                              <div
                                key={`${date.toISOString()}-${quarter}`}
                                className={`h-3 ${border}`}
                                style={{ backgroundColor }}
                                onMouseEnter={() => setHoveredSlot(`${date.toISOString().split('T')[0]}-${timeSlot}`)}
                                onMouseLeave={() => setHoveredSlot(null)}
                              >
                              </div>
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

      <div className="w-48 sticky top-0 self-start">
        <div className="bg-base-200 p-4 rounded-lg">
          <h2 className="font-bold text-xl mb-4">respondents</h2>
          <ul className="space-y-2">
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
                    className={`badge badge-neutral font-bold ${
                      !isAvailable && hoveredSlot ? 'line-through text-gray-500' : ''
                    }`}
                  >
                    {userName}
                  </div>
                </li>
              );
            })}
          </ul>
        </div>
      </div>
    </div>
  );
};

export default AvailabilitiesCalendar;
