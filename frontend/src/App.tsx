import React from 'react'
import { BrowserRouter } from 'react-router-dom';
import PageList from './PageList.jsx';
import { Context } from './util/context.js';
import './App.css'

const App = () => {
  const [theme, setTheme] = React.useState<"light" | "dark">("light");

  return (
    <>
      <Context.Provider value={{ theme, setTheme }}>
        <html data-theme={theme == "light" ? "retro" : "dracula"}>
        <div
          role="alert"
          className="alert alert-error fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 z-50 shadow-lg "
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            className="h-6 w-6 shrink-0 stroke-current"
            fill="none"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="2"
              d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
            />
          </svg>
<div className="flex flex-col">
    <span>uh oh!</span>
    <p>Couldn't add availability, please try again later.</p>
  </div>
        </div>
          <BrowserRouter>
            <PageList />
          </BrowserRouter>
        </html>
      </Context.Provider>
    </>
  );
};

export default App;
