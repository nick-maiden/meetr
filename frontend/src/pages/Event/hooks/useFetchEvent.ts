import React from "react";
import { useNavigate } from "react-router-dom";
import { getRequest } from "../../../utils/api";
import { AppContext } from "../../../contexts";
import { Event as EventType } from "../../../types";

interface UseFetchEventReturn {
  event: EventType | null
}

const useFetchEvent = (
  eventId: string | undefined,
  refetchEvents: any[]
): UseFetchEventReturn => {
  const [event, setEvent] = React.useState<EventType | null>(null);
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(AppContext);

  React.useEffect(() => {
    getRequest(`/events/${eventId}`)
      .then(response => {
        setEvent(response.data);
      })
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

