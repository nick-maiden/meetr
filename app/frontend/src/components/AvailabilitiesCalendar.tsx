import { useState } from 'react';

interface Availability {
  users: string[];
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
  users: ["Alice Johnson", "Bob Smith", "Carol White", "David Brown"],
  availability: {
    "2025-01-15-09:00": ["Alice Johnson", "Bob Smith", "Carol White", "David Brown"],
    "2025-01-15-09:15": ["Alice Johnson", "Bob Smith"],
    "2025-01-15-09:30": ["Alice Johnson", "David Brown"],
    "2025-01-15-09:45": ["Carol White"]
  }
};

const AvailabilitiesCalendar: React.FC = () => {
  const [hoveredSlot, setHoveredSlot] = useState<HoveredSlot>(null);

  const generateTimeData = (): TimeData => {
    const displaySlots: string[] = ['09:00', '10:00'];
    const dataSlots: string[] = [];

    for (let hour = 9; hour <= 10; hour++) {
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
    for (let i = 0; i < 7; i++) {
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

    // Use a darker green as the base color: rgb(34, 197, 94)
    // Interpolate between white (255, 255, 255) and this green
    const red = Math.round(255 - (255 - 34) * proportion);
    const green = Math.round(255 - (255 - 197) * proportion);
    const blue = Math.round(255 - (255 - 94) * proportion);

    return `rgb(${red}, ${green}, ${blue})`;
  };

  const { displaySlots, dataSlots } = generateTimeData();
  const dates = getDates();

  return (
    <div className="flex gap-6">
      <div className="flex-grow overflow-x-auto">
        <table className="table table-compact w-full">
          <thead>
            <tr>
              <th className="w-20"></th>
              {dates.map((date, i) => {
                const [weekday, monthDay] = date.toLocaleDateString('en-US', {
                    weekday: 'short',
                    month: 'short',
                    day: 'numeric',
                  }).split(", ");
                return (
                  <th key={i} className="text-center">
                    <div>{monthDay}</div>
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
                  <td className="font-medium">{displayTime}</td>
                  {dates.map((date, dateIndex) => (
                    <td key={`hour-${dateIndex}`} className="p-0 border-b">
                      <div className="grid grid-rows-4">
                        {[0, 1, 2, 3].map((quarter) => {
                          const timeSlot = dataSlots[hourIndex * 4 + quarter];
                          const availableUsers = getAvailableUsers(date, timeSlot);
                          const backgroundColor = getAvailabilityColor(
                            availableUsers.length,
                            DUMMY_DATA.users.length
                          );
                          const border = quarter === 1 ? "border-b border-dashed" : "";

                          return (
                            <div
                              key={quarter}
                              className={`h-6 hover:!bg-secondary cursor-pointer ${border}`}
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
                    {dates.map((_, i) => (
                      <td key={i} className="p-0 "></td>
                    ))}
                  </tr>
                )}
              </>
            ))}
          </tbody>
        </table>
      </div>

      <div className="w-64 bg-base-200 p-4 rounded-lg">
        <h2 className="font-bold text-xl mb-4">Respondents</h2>
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
                  className={`badge badge-secondary font-bold ${
                    !isAvailable && hoveredSlot ? 'line-through text-gray-400' : ''
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
  );
};

export default AvailabilitiesCalendar;
