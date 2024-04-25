import React from "react";

import TimeSlot from "./TimeSlot";
import { useAreaSelection } from "../area-selection";
import { SelectionContext } from "../util/contexts";

const Calendar = () => {
  const selectContainerRef = React.useRef<HTMLDivElement | null>(null);
  const selection = useAreaSelection({ container: selectContainerRef });
  const timeSlots = [...Array(32).keys()].map(() => <TimeSlot />);

  return (
    <>
      <SelectionContext.Provider value={selection}>
        <div ref={selectContainerRef} className="grid grid-cols-4" style={{ gridTemplateColumns: 'repeat(4, 60px)' }}>
          {timeSlots}
        </div>
      </SelectionContext.Provider>
    </>
  );
};


export default Calendar;
