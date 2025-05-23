import React from "react";
import { useNavigate } from "react-router-dom";
import { getRequest } from "../../../util/api";
import { Context } from "../../../util/context";
import { Event as EventType } from "../../../types";

const useFetchEvent = (eventId: string | undefined, refetchEvents: any[]): EventType | null => {
  const [event, setEvent] = React.useState<EventType | null>(null);
  const navigate = useNavigate();
  const { setErrorMessage } = React.useContext(Context);

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


  return event;

};

export default useFetchEvent;

