import React from "react";
import Navbar from "./Navbar"
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faLink } from '@fortawesome/free-solid-svg-icons';

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
          <div className="alert alert-info font-bold">
            link copied to clipboard!
          </div>
        </div>
      }
      <div className="flex flex-col w-screen h-screen sm:px-10 px-5">

        <Navbar />

        <div className="flex flex-col overflow-auto no-scrollbar sm:px-[10%] px-2 sm:mt-8 mt-6">
          <div className="flex justify-between">
            <article className="prose">
              <h1>{eventName}</h1>
            </article>

            <div className="flex gap-3">
              <button
                className="btn btn-outline text-lg"
                onClick={copyLink}
              >
                copy link
                <FontAwesomeIcon icon={faLink} size="lg"/>
              </button>
              <button
                className="btn btn-neutral text-lg"
              >
                add availability
              </button>
            </div>
          </div>
        </div>
      </div>
      </>
  )
};

export default Event;
