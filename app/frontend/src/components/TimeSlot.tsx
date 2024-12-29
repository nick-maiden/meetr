import React from "react";
import { useSelected } from "../area-selection";
import { SelectionContext } from "../util/contexts";

const TimeSlot = () => {
  const ref = React.useRef(null);
  const selections = React.useContext(SelectionContext);
  const isSelected = useSelected(ref, selections);

  const timeSlotStyle = {
    backgroundColor: isSelected ? 'green' : '#FFC0CB',
    height: '20px',
    border: '1px solid black',
  };

  return (
    <div
      ref={ref}
      style={timeSlotStyle}
    />
  )
};

export default TimeSlot;
