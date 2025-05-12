import React from "react";
import { Context } from "../util/context";

const ErrorPopup = () => {
  const { errorMessage, setErrorMessage } = React.useContext(Context);

  return (
    <>
      <div className="fixed inset-0 bg-black/40 backdrop-blur-sm z-40 pointer-events-auto" />
      <div
        className="
          fixed
          top-1/2
          left-1/2
          -translate-x-1/2
          -translate-y-1/2
          z-50
          flex
          items-center
          p-4
          text-sm
          text-error
          bg-error-content
          rounded-lg
          w-max
          max-w-[min(calc(100vw-2rem),700px)]"
        role="alert"
      >
        <svg
          className="shrink-0 inline w-4 h-4 me-3"
          aria-hidden="true"
          xmlns="http://www.w3.org/2000/svg"
          fill="currentColor"
          viewBox="0 0 20 20"
        >
          <path d="M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM9.5 4a1.5 1.5 0 1 1 0 3 1.5 1.5 0 0 1 0-3ZM12 15H8a1 1 0 0 1 0-2h1v-3H8a1 1 0 0 1 0-2h2a1 1 0 0 1 1 1v4h1a1 1 0 0 1 0 2Z"/>
        </svg>

        <p className="text-lg">{errorMessage}</p>

        <button
          className="
            ms-auto
            rounded-lg
            focus:ring-2
            focus:ring-red-400
            p-1.5
            hover:bg-red-200
            inline-flex
            items-center
            justify-center
            h-8
            w-8
            ml-4"
          type="button"
          data-dismiss-target="#alert-2"
          aria-label="Close"
          onClick={() => { setErrorMessage(""); }}
        >
          <span className="sr-only">Close</span>
          <svg
            className="w-3 h-3"
            aria-hidden="true"
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 14 14"
          >
            <path
              stroke="currentColor"
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="m1 1 6 6m0 0 6 6M7 7l6-6M7 7l-6 6"
            />
          </svg>
        </button>
      </div>
    </>
  );
};

export default ErrorPopup;

