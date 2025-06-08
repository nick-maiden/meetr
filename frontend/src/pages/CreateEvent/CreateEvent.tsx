import React from "react";
import Typewriter from 'typewriter-effect';
import { useNavigate } from "react-router-dom";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faCircleInfo } from '@fortawesome/free-solid-svg-icons';
import DateSelectionCalendar from "./components/DateSelectionCalendar/DateSelectionCalendar";
import SelectBox from "./components/SelectBox";
import useTimeRangeSelector from "./hooks/useTimeRangeSelector";
import { convertTo24Hour, generateName } from "./utils";
import { createEvent } from "src/global/api";
import { AppContext } from "src/global/contexts";
import PageWrapper from "src/components/PageWrapper";
import Heading from "src/components/Heading";
import SubText from "src/components/SubText";

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

  const setRandomName = (): void => setName(generateName());

  return (
    <PageWrapper innerContainerClassName="lg:px-[20%] md:px-[10%] px-2">
      <div className="inline-block px-4 py-2 mx-auto bg-base-200 rounded-lg">
        <h3 className="sm:text-xl text-lg">
          <Typewriter onInit={(tw) => tw.typeString('create event.').start()} />
        </h3>
      </div>

      <Heading className="mt-10">event name</Heading>
      <SubText>stuck? <span className="link" onClick={setRandomName}>try this</span></SubText>

      <label className="mx-auto sm:mt-6 mt-4">
        <input
          type="text"
          placeholder="..."
          className="input input-bordered sm:text-base text-sm text-center w-[320px]"
          value={name}
          onChange={(event) => setName(event.target.value)}
        />
      </label>

      <Heading className="sm:mt-16 mt-10">what dates might work?</Heading>
      <SubText>click and drag to select</SubText>

      <DateSelectionCalendar
        className="mx-auto sm:mt-6 mt-4"
        setDates={setDates}
      />

      <div className="flex gap-2 sm:mt-16 mt-10">
        <Heading>what times might work?</Heading>
        <div
          className="tooltip tooltip-accent text-left right-28"
          data-tip="
          earliest: earliest the event can start
          latest:   latest it can finish"
        >
          <FontAwesomeIcon
            className="relative left-28 lg:top-1 top-0 lg:text-base md:text-sm text-xs"
            icon={faCircleInfo}
          />
        </div>
      </div>

      <div className="flex justify-around sm:mt-6 mt-4">
        <SelectBox {...timeRangeSelector.earliest} />
        <div className="divider divider-horizontal"></div>
        <SelectBox {...timeRangeSelector.latest} />
      </div>

      <div
        className={`${!canCreateEvent() && "tooltip tooltip-accent"} mt-12`}
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

