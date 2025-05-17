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
          p-4
          text-sm
          text-error
          bg-error-content
          rounded-lg
          w-max
          max-w-[min(calc(100vw-2rem),700px)]"
        role="alert"
      >
        <div className="flex items-center">
          <svg className="shrink-0 w-4 h-4 me-2" aria-hidden="true" xmlns="http://www.w3.org/2000/svg" fill="currentColor" viewBox="0 0 20 20">
            <path d="M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM9.5 4a1.5 1.5 0 1 1 0 3 1.5 1.5 0 0 1 0-3ZM12 15H8a1 1 0 0 1 0-2h1v-3H8a1 1 0 0 1 0-2h2a1 1 0 0 1 1 1v4h1a1 1 0 0 1 0 2Z"/>
          </svg>
          <span className="sr-only">Info</span>
          <h3 className="text-lg font-bold">uh oh</h3>
        </div>
        <div className="mt-2 mb-4">
          <p className="text-base">{errorMessage}</p>
        </div>
        <div className="flex gap-2">
          <button
            type="button"
            className="flex-1 btn btn-error btn-sm"
            onClick={() => { window.open("https://github.com/nick-maiden/meetr/issues/new", "_blank"); }}
          >
            🐞 report a bug
          </button>
          <button
            type="button"
            className="flex-1 btn btn-error btn-outline btn-sm"
            onClick={() => { setErrorMessage(""); }}
          >
            dismiss
          </button>
        </div>
      </div>
    </>
  );
};

export default ErrorPopup;

