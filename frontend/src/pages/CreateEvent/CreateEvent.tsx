import DateSelectionCalendar from "./components/DateSelectionCalendar/DateSelectionCalendar";
import Navbar from "src/components/Navbar";
import SelectBox from "./components/SelectBox";
import useCreateEvent from "./hooks/useCreateEvent";

const CreateEvent = () => {
  const {
    eventName, setEventName,
    setSelectedDates,
    timeRangeSelector,
    creatingEvent,
    canCreateEvent,
    createEvent
  } = useCreateEvent();

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
          <SelectBox {...timeRangeSelector.earliest} />
          <div className="divider divider-horizontal"></div>
          <SelectBox {...timeRangeSelector.latest} />
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

export default CreateEvent;

