import React from "react";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faLink } from '@fortawesome/free-solid-svg-icons';
import { useParams } from "react-router-dom";
import useFetchEvent from "./hooks/useFetchEvent";
import AvailabilitiesCalendar from "./components/AvailabilitiesCalendar/AvailabilitiesCalendar";
import { EventId } from "src/global/types";
import PageWrapper from "src/components/PageWrapper";

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
      <PageWrapper innerContainerClassName="md:px-[10%] sm:px-[5%] px-1">
        <div className="flex justify-between items-center sm:mb-8 mb-5">
          <article className="prose">
            <h1 className="md:text-4xl sm:text-3xl text-2xl">
              {event?.name}
            </h1>
          </article>

          <div className="flex gap-1 sm:gap-3">

            <button
              className="btn btn-outline btn-xs sm:btn-sm md:btn-md md:text-lg"
              onClick={copyLink}
            >
              <p className="hidden sm:block">copy link</p>
              <FontAwesomeIcon icon={faLink} size="lg"/>
            </button>

            <button
              className="btn btn-secondary btn-xs sm:btn-sm md:btn-md md:text-lg"
              onClick={() => setIsSelectionMode(true)}
              disabled={isSelectionMode}
            >
              add availability
            </button>
          </div>
        </div>

        {event ? (
          <AvailabilitiesCalendar
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

