import React from "react";
import { useNavigate } from "react-router-dom";
import { AppContext } from "global/contexts";
import useTimeRangeSelector from "./useTimeRangeSelector";
import { convertTo24Hour } from "../utils";
import { createEvent } from "global/api";

interface UseCreateEventReturn {
  name: string;
  setName: React.Dispatch<React.SetStateAction<string>>;
  setDates: React.Dispatch<React.SetStateAction<string[]>>;
  timeRangeSelector: ReturnType<typeof useTimeRangeSelector>;
  isCreating: boolean;
  canCreateEvent: () => boolean;
  handleCreateEvent: () => void;
}

const useCreateEvent = (): UseCreateEventReturn => {
  const [dates, setDates] = React.useState<string[]>([]);
  const timeRangeSelector = useTimeRangeSelector();
  const [name, setName] = React.useState('');
  const [isCreating, setCreatingEvent] = React.useState(false);
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(AppContext);

  const canCreateEvent = (): boolean => {
    return Boolean(
      dates.length
      && name
      && timeRangeSelector.earliest.value
      && timeRangeSelector.latest.value
    );
  };

  const handleCreateEvent = (): void => {
    setCreatingEvent(true);
    const newEvent = {
      name,
      earliestTime: convertTo24Hour(timeRangeSelector.earliest.value!),
      latestTime: convertTo24Hour(timeRangeSelector.latest.value!),
      dates,
    }
    createEvent(newEvent)
      .then(response => {
        navigate(`/events/${response.data.eventId}`);
      })
      .catch(_ => {
        setCreatingEvent(false);
        setErrorMessage('unable to create event, please try again later');
      });
  };

  return {
    name, setName,
    setDates,
    timeRangeSelector,
    isCreating,
    canCreateEvent,
    handleCreateEvent
  };
};

export default useCreateEvent;

