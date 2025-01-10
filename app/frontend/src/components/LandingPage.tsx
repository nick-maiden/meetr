import React from "react";
import { useNavigate } from 'react-router-dom';
import ThemeToggle from "./ThemeToggle";
import DateSelectionCalendar from "./DateSelectionCalendar";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faGithubAlt } from '@fortawesome/free-brands-svg-icons'

const LandingPage = () => {
  const [selectedDates, setSelectedDates] = React.useState<string[]>([]);
  const [eventName, setEventName] = React.useState('');
  const [earliestTime, setEarliestTime] = React.useState<string | undefined>(undefined);
  const [latestTime, setLatestTime] = React.useState<string | undefined>(undefined);
  const [creatingEvent, setCreatingEvent] = React.useState(false);
  const navigate = useNavigate();

  const canCreateEvent = () => {
    return selectedDates.length && eventName && earliestTime && latestTime;
  };

  const createEvent = () => {
    setCreatingEvent(true);
    setTimeout(() => {
      navigate("/event");
    }, 2000);
  };

  const hours = Array.from({ length: 24 }, (_, i) => {
    const period = i < 12 ? "am" : "pm";
    const hour = i % 12 === 0 ? 12 : i % 12;
    return `${hour}:00 ${period}`;
  });

  return (
    <div className="flex flex-col w-screen h-screen sm:px-10 px-5">

      <div className="flex items-center justify-between sm:pt-8 sm:pb-4 pt-6 pb-2">
        <article className="prose">
          <h1 className="sm:text-6xl text-5xl">meetr.</h1>
        </article>
        <div className="flex items-center justify-between gap-4">
          <ThemeToggle />
          <a
            href="https://github.com/nick-maiden/meetr"
            target="_blank"
            rel="noopener noreferrer"
          >
            <FontAwesomeIcon icon={faGithubAlt} size="xl"/>
          </a>
        </div>
      </div>

      <div className="flex flex-col overflow-auto no-scrollbar sm:px-[20%] px-2 sm:mt-8 mt-6">
        <article className="prose">
          <h2>event name</h2>
        </article>

        <label className="mx-auto sm:mt-6 mt-4">
          <input
            type="text"
            placeholder="workplace meeting"
            className="input input-bordered text-xl font-bold text-center"
            value={eventName}
            onChange={(event) => setEventName(event.target.value)}
          />
        </label>

        <article className="prose mt-10">
          <h2>what dates might work?</h2>
        </article>
        <p className="text-sm text-gray-500 font-bold mt-2">  click and drag to select</p>

        <DateSelectionCalendar
          className="mx-auto mt-6"
          setSelectedDates={setSelectedDates}
        />

        <article className="prose mt-10">
          <h2>what times might work?</h2>
        </article>

        <div className="flex justify-around mt-6">
          <select
            className="select select-bordered no-scrollbar font-bold"
            value={earliestTime}
            onChange={(event) => setEarliestTime(event.target.value)}
          >
            <option disabled selected>no earlier than</option>
            {hours.map((h, i) => (<option key={i}>{h}</option>))}
          </select>
          <div className="divider divider-horizontal"></div>
          <select
            className="select select-bordered no-scrollbar font-bold"
            value={latestTime}
            onChange={(event) => setLatestTime(event.target.value)}
          >
            <option disabled selected>no later than</option>
            {hours.map((h, i) => (<option key={i}>{h}</option>))}
          </select>
        </div>

        <button
          className="btn btn-primary text-xl mt-14 mb-10"
          disabled={!canCreateEvent()}
          onClick={createEvent}
        >
          {creatingEvent ?
            <span className="loading loading-spinner"></span> :
            <>create event</>
          }
        </button>
      </div>
    </div>
  )
};

export default LandingPage;
