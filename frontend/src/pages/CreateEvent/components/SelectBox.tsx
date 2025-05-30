import React from "react";

interface Props {
  value: string | undefined;
  setValue: React.Dispatch<React.SetStateAction<string | undefined>>;
  options: string[];
}

const SelectBox: React.FC<Props> = ({ value, setValue, options }) => {
  return (
    <select
      className="select select-bordered no-scrollbar font-bold"
      value={value}
      onChange={(event) => setValue(event.target.value)}
    >
      <option disabled selected>no earlier than</option>
      {options.map((h, i) => (<option key={i}>{h}</option>))}
    </select>
  )
};

export default SelectBox;

