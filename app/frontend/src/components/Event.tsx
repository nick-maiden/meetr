import React from "react";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faLink } from '@fortawesome/free-solid-svg-icons';
import Navbar from "./Navbar"
import AvailabilitiesCalendar from "./AvailabilitiesCalendar";

const Event = () => {
  const eventName = "test event"
  const [linkCopied, setLinkCopied] = React.useState(false);

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
          <div className="alert font-bold">
            link copied to clipboard!
          </div>
        </div>
      }
      <div className="flex flex-col w-screen h-screen sm:px-10 px-5">

        <Navbar />

        <div className="flex flex-col overflow-auto no-scrollbar sm:px-[10%] px-2 sm:mt-6 mt-6 pb-6">
          <div className="flex justify-between items-center sm:mb-8 mb-5">
            <article className="prose">
              <h1 className="md:text-4xl sm:text-3xl text-2xl">{eventName}</h1>
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
                className="btn btn-neutral btn-xs sm:btn-sm md:btn-md md:text-lg"
              >
                add availability
              </button>
            </div>
          </div>

            <AvailabilitiesCalendar />
        </div>
      </div>
      </>
  )
};

export default Event;
