import { useState } from 'react';
import ThemeToggle from "./ThemeToggle";
import DateSelectionCalendar from "./DateSelectionCalendar";

const LandingPage = () => {
  const [selectedDates, setSelectedDates] = useState<[Date] | [Date, Date] | []>([]);

  const hours = Array.from({ length: 24 }, (_, i) => {
    const period = i < 12 ? "am" : "pm";
    const hour = i % 12 === 0 ? 12 : i % 12;
    return `${hour}:00 ${period}`;
  });

  return (
    <div className="flex flex-col w-screen h-screen px-10">

      <div className="flex items-center justify-between py-4">
        <article className="prose">
          <h1 className="pt-4 text-6xl">meetr.</h1>
        </article>
        <ThemeToggle />
      </div>

      <div className="flex flex-col overflow-auto px-[20%] mt-10">
        <article className="prose">
          <h2>event name</h2>
        </article>

        <label className="mx-auto mt-6">
          <input
            type="text"
            placeholder="workplace meeting"
            className="input input-bordered text-xl font-bold text-center"
          />
        </label>

        <article className="prose mt-10">
          <h2>what dates might work?</h2>
        </article>
        <p className="text-sm text-gray-500 font-bold mt-2">  click and drag to select</p>

        <DateSelectionCalendar
          className="mx-auto mt-6"
        />

        <article className="prose mt-10">
          <h2>what times might work?</h2>
        </article>

        <div className="flex justify-around mt-6">
          <select className="select select-bordered font-bold">
            <option disabled selected>no earlier than</option>
            {hours.map((h, i) => (<option key={i}>{h}</option>))}
          </select>
          <div className="divider divider-horizontal"></div>
          <select className="select select-bordered font-bold">
            <option disabled selected>no later than</option>
            {hours.map((h, i) => (<option key={i}>{h}</option>))}
          </select>
        </div>

        <button
          className="btn btn-primary text-xl mt-14 mb-10"
          disabled={true}
        >
          create event
        </button>
      </div>

    </div>
  )
};

export default LandingPage;
