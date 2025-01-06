import { useState } from 'react';
import ThemeToggle from "./ThemeToggle";
import DateSelectionCalendar from "./DateSelectionCalendar";

const LandingPage = () => {
  const [selectedDates, setSelectedDates] = useState<[Date] | [Date, Date] | []>([]);

  return (
    <div className="flex flex-col w-screen h-screen px-10">
      <div className="flex items-center justify-between py-4">
        <article className="prose">
          <h1 className="pt-4 text-6xl">meetr.</h1>
        </article>
        <ThemeToggle />
      </div>
      <div className="flex flex-col flex-1 px-[20%] mt-10">
        <label className="form-control w-[55%] mx-auto mb-10">
          <label className="input input-bordered flex items-center gap-4 font-bold">
            event name
            <input type="text" className="grow font-bold" placeholder="workplace meeting" />
          </label>
        </label>
        <DateSelectionCalendar
          className="mx-auto"
          selectedDates={selectedDates}
          setSelectedDates={setSelectedDates}
        />
      </div>
    </div>
  )
};

export default LandingPage;
