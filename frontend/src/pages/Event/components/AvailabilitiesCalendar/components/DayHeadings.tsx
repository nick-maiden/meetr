import React from "react";

interface Props {
  dates: Date[];
}

const DayHeadings: React.FC<Props> = ({ dates }) => {
  return (
    <thead>
      <tr>
        <th className="w-0"></th>
        {dates.map((date, i) => {
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
          );
        })}
      </tr>
    </thead>
  );
};

export default DayHeadings;

