import React from "react";
import { useNavigate } from 'react-router-dom';
import DateSelectionCalendar from "./DateSelectionCalendar";
import Navbar from "./Navbar";
import { postRequest } from '../util/api';
import { Context } from "../util/context";

const LandingPage = () => {
  const [selectedDates, setSelectedDates] = React.useState<string[]>([]);
  const [eventName, setEventName] = React.useState('');
  const [earliestTime, setEarliestTime] = React.useState<string | undefined>(undefined);
  const [latestTime, setLatestTime] = React.useState<string | undefined>(undefined);
  const [creatingEvent, setCreatingEvent] = React.useState(false);
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(Context);

  const canCreateEvent = () => {
    return selectedDates.length && eventName && earliestTime && latestTime;
  };

  const createEvent = () => {
    setCreatingEvent(true);
    const newEvent = {
      name: eventName,
      earliestTime: earliestTime ? convertTo24Hour(earliestTime) : undefined,
      latestTime: latestTime ? convertTo24Hour(latestTime) : undefined,
      dates: selectedDates,
      users: {},
      availabilities: {}
    }
    postRequest('/events', newEvent)
      .then(response => {
        navigate(`/events/${response.data.eventId}`);
      })
      .catch(_ => {
        setCreatingEvent(false);
        setErrorMessage('unable to create event, please try again later');
      });
  };

  const getHour = (timeStr: string): number => {
    const [hourStr, period] = timeStr.split(' ');
    let hour = parseInt(hourStr);
    if (period === 'pm' && hour !== 12) hour += 12;
    if (period === 'am' && hour === 12) hour = 0;
    return hour;
  };

const convertTo24Hour = (timeStr: string): string => {
  return `${getHour(timeStr).toString().padStart(2, '0')}:00`;
};

  const hours = Array.from({ length: 24 }, (_, i) => {
    const period = i < 12 ? "am" : "pm";
    const hour = i % 12 === 0 ? 12 : i % 12;
    return `${hour}:00 ${period}`;
  });

  const availableEarliestHours = hours.filter(h =>
    !latestTime || getHour(h) < getHour(latestTime)
  );

  const availableLatestHours = hours.filter(h =>
    !earliestTime || getHour(h) > getHour(earliestTime)
  );

  return (
    <div className="flex flex-col w-screen h-screen sm:px-10 px-5">

      <Navbar />

      <div className="flex flex-col overflow-auto no-scrollbar sm:px-[20%] px-2 sm:mt-8 mt-6">
        <article className="prose">
          <h2>event name</h2>
        </article>

        <label className="mx-auto sm:mt-6 mt-4">
          <input
            type="text"
            placeholder="..."
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
            {availableEarliestHours.map((h, i) => (<option key={i}>{h}</option>))}
          </select>
          <div className="divider divider-horizontal"></div>
          <select
            className="select select-bordered no-scrollbar font-bold"
            value={latestTime}
            onChange={(event) => setLatestTime(event.target.value)}
          >
            <option disabled selected>no later than</option>
            {availableLatestHours.map((h, i) => (<option key={i}>{h}</option>))}
          </select>
        </div>

        <button
          className="btn btn-secondary text-xl mt-14 mb-10"
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
