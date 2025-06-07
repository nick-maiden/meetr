import React from "react";
import Typewriter from 'typewriter-effect';
import { useNavigate } from "react-router-dom";
import DateSelectionCalendar from "./components/DateSelectionCalendar/DateSelectionCalendar";
import SelectBox from "./components/SelectBox";
import useTimeRangeSelector from "./hooks/useTimeRangeSelector";
import { convertTo24Hour } from "./utils";
import { createEvent } from "src/global/api";
import { AppContext } from "src/global/contexts";
import PageWrapper from "src/components/PageWrapper";

const CreateEvent = () => {
  const [name, setName] = React.useState('');
  const [dates, setDates] = React.useState<string[]>([]);
  const [isCreating, setIsCreating] = React.useState(false);
  const timeRangeSelector = useTimeRangeSelector();
  const earliestTime = timeRangeSelector.earliest.value;
  const latestTime = timeRangeSelector.latest.value;
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(AppContext);

  const canCreateEvent = () => Boolean(dates.length && name && earliestTime && latestTime);

  const onCreate = (): void => {
    setIsCreating(true);
    const newEvent = {
      name,
      earliestTime: convertTo24Hour(earliestTime!),
      latestTime: convertTo24Hour(latestTime!),
      dates
    };
    createEvent(newEvent)
      .then(res => navigate(`/events/${res.data.eventId}`))
      .catch(() => setErrorMessage('unable to create event, please try again later'))
      .finally(() => setIsCreating(false));
  };

  return (
    <PageWrapper innerContainerClassName="lg:px-[20%] md:px-[10%] px-2">
      <div className="inline-block px-4 py-2 mx-auto bg-base-200 rounded-lg">
        <h3 className="sm:text-2xl text-lg">
          <Typewriter onInit={(tw) => tw.typeString('create event.').start()} />
        </h3>
      </div>

      <article className="prose mt-6">
        <h3 className="sm:text-2xl text-xl">event name</h3>
      </article>

      <label className="mx-auto sm:mt-6 mt-4">
        <input
          type="text"
          placeholder="..."
          className="input input-bordered sm:text-xl text-lg text-center w-[300px]"
          value={name}
          onChange={(event) => setName(event.target.value)}
        />
      </label>

      <article className="prose mt-10">
        <h3 className="sm:text-2xl text-xl">what dates might work?</h3>
      </article>
      <p className="sm:text-sm text-xs text-gray-500 font-bold mt-2">
        click and drag to select
      </p>

      <DateSelectionCalendar
        className="mx-auto mt-6"
        setDates={setDates}
      />

      <article className="prose mt-10">
        <h3 className="sm:text-2xl text-xl">what times might work?</h3>
      </article>

      <div className="flex justify-around mt-6">
        <SelectBox {...timeRangeSelector.earliest} />
        <div className="divider divider-horizontal"></div>
        <SelectBox {...timeRangeSelector.latest} />
      </div>

      <div
        className={`${!canCreateEvent() && "tooltip"} mt-12`}
        data-tip={
        !canCreateEvent() &&
          `event missing: ${[
            !name && "a name",
            !dates.length && "dates", 
            !earliestTime && "earliest time",
            !latestTime && "latest time"
          ].filter(Boolean).join(", ")}`
      }>
        <button
          className="btn btn-secondary text-xl w-full"
          disabled={!canCreateEvent()}
          onClick={onCreate}
        >
          {isCreating
            ? <span className="loading loading-spinner"></span>
            : <>create event</>
          }
        </button>
      </div>
    </PageWrapper>
  )
};

export default CreateEvent;

