import React from "react";
import { useNavigate } from "react-router-dom";
import { getEvent } from "src/global/api";
import { AppContext } from "global/contexts";
import { EventId, Event as EventType } from "global/types";

interface UseFetchEventReturn {
  event: EventType | null
}

const useFetchEvent = (
  eventId: EventId,
  refetchEvents: any[]
): UseFetchEventReturn => {
  const [event, setEvent] = React.useState<EventType | null>(null);
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(AppContext);

  React.useEffect(() => {
    getEvent(eventId)
      .then(response => setEvent(response.data))
      .catch(error => {
        if (error.response?.status === 404) {
          setErrorMessage("event not found, please double check event link");
        } else {
          setErrorMessage("failed to load event, please try again later");
        }
        navigate('/');
      });
  }, [eventId, ...refetchEvents]);

  return { event };
};

export default useFetchEvent;

