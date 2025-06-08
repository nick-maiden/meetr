import React from "react";

interface Props {
  value: string | undefined;
  setValue: React.Dispatch<React.SetStateAction<string | undefined>>;
  options: string[];
  title: string;
}

const SelectBox: React.FC<Props> = ({ value, setValue, options, title }) => {
  return (
    <select
      className="select select-bordered no-scrollbar"
      value={value}
      onChange={(event) => setValue(event.target.value)}
    >
      <option disabled selected>{title}</option>
      {options.map((h, i) => (<option key={i}>{h}</option>))}
    </select>
  )
};

export default SelectBox;

