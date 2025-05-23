import React from "react";
import { useNavigate } from "react-router-dom";
import { Context } from "../../../util/context";
import useTimeRangeSelector from "./useTimeRangeSelector";
import { convertTo24Hour } from "../util";
import { postRequest } from "../../../util/api";

const useCreateEvent = () => {
  const [selectedDates, setSelectedDates] = React.useState<string[]>([]);
  const timeRangeSelector = useTimeRangeSelector();
  const [eventName, setEventName] = React.useState('');
  const [creatingEvent, setCreatingEvent] = React.useState(false);
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(Context);

  const canCreateEvent = () => {
    return (
      selectedDates.length
      && eventName
      && timeRangeSelector.earliest.value
      && timeRangeSelector.latest.value
    );
  };

  const createEvent = () => {
    setCreatingEvent(true);
    const newEvent = {
      name: eventName,
      earliestTime: convertTo24Hour(timeRangeSelector.earliest.value),
      latestTime: convertTo24Hour(timeRangeSelector.latest.value),
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

  return {
    eventName, setEventName,
    setSelectedDates,
    timeRangeSelector,
    creatingEvent,
    canCreateEvent,
    createEvent
  };
};

export default useCreateEvent;

