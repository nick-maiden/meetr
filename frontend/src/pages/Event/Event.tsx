import React from "react";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faLink } from '@fortawesome/free-solid-svg-icons';
import { useParams } from "react-router-dom";
import Navbar from "../../components/Navbar"
import AvailabilitiesCalendar from "./components/AvailabilitiesCalendar";
import { getRequest } from "../../util/api";

const Event = () => {
  const { eventId } = useParams();
  const [linkCopied, setLinkCopied] = React.useState(false);
  const [isSelectionMode, setIsSelectionMode] = React.useState(false);
  const [event, setEvent] = React.useState(null);
  const [isLoading, setIsLoading] = React.useState(true);

  React.useEffect(() => {
    getRequest(`/events/${eventId}`)
      .then(response => {
        console.log(response.data);
        setEvent(response.data);
        setIsLoading(false);
      });
  }, [isSelectionMode]);

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

  if (isLoading) {
    return <> loading </>
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
      <div className="flex flex-col w-screen h-screen ">

        <div className="sm:px-10 px-5">
          <Navbar />
        </div>
        <div className="flex flex-col overflow-auto no-scrollbar sm:mt-6 mt-6 pb-6">
          <div className="flex justify-between items-center sm:mb-8 mb-5 sm:px-[10%] px-6">
            <article className="prose">
              <h1 className="md:text-4xl sm:text-3xl text-2xl">{event.name}</h1>
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

          <div className="sm:px-[10%] px-2">
            {event &&
              <AvailabilitiesCalendar
                isSelectionMode={isSelectionMode}
                setIsSelectionMode={setIsSelectionMode}
                event={event}
              />
            }

          </div>
        </div>
      </div>
      </>
  )
};

export default Event;
