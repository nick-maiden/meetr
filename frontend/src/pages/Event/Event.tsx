import React from "react";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faShare } from '@fortawesome/free-solid-svg-icons';
import { useParams } from "react-router-dom";
import useFetchEvent from "./hooks/useFetchEvent";
import EventAvailability from "./components/EventAvailability/EventAvailability";
import { EventId } from "src/global/types";
import PageWrapper from "src/components/PageWrapper";
import Heading from "src/components/Heading";

const Event = () => {
  const { eventId } = useParams<EventId>();
  const [linkCopied, setLinkCopied] = React.useState(false);
  const [isSelectionMode, setIsSelectionMode] = React.useState(false);
  const { event } = useFetchEvent(eventId!, [isSelectionMode]);

  const copyLink = () => {
    const url = window.location.href;
    navigator.clipboard.writeText(url)
      .then(() => {
        setLinkCopied(true);
        setTimeout(() => {
          setLinkCopied(false);
        }, 2000);
      });
  }

  return (
    <>
      {linkCopied &&
        <div className="toast toast-top toast-center" style={{zIndex: 10000}}>
          <div className="alert bg-secondary text-secondary-content font-bold">
            link copied to clipboard!
          </div>
        </div>
      }

      <PageWrapper innerContainerClassName="md:px-[10%] sm:px-[5%]">
        <div className="flex justify-between items-center mb-8 ">
          <Heading>{event?.name}</Heading>
          <button
            className="btn sm:btn-md btn-sm md:text-base btn-outline"
            onClick={copyLink}
          >
            <p className="sm:block hidden">copy link</p>
            <FontAwesomeIcon icon={faShare} size="lg"/>
          </button>
        </div>

        {event ? (
          <EventAvailability
            isSelectionMode={isSelectionMode}
            setIsSelectionMode={setIsSelectionMode}
            event={event}
          />
        ) : (
          <span className="loading loading-spinner absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2"></span>
        )}
      </PageWrapper>
    </>
  )
};

export default Event;

