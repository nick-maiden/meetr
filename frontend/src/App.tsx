import React from 'react'
import { BrowserRouter } from 'react-router-dom';
import PageList from './PageList.jsx';
import { Context } from './util/context.js';
import './App.css'
import ErrorPopup from './components/ErrorPopup.js';

const App = () => {
  const [theme, setTheme] = React.useState<"light" | "dark">("light");
  const [errorMessage, setErrorMessage] = React.useState("");

  return (
    <>
      <Context.Provider value={{ theme, setTheme, errorMessage, setErrorMessage }}>
        <html data-theme={theme == "light" ? "retro" : "dracula"}>
          {errorMessage && <ErrorPopup />}
          <BrowserRouter>
            <PageList />
          </BrowserRouter>
        </html>
      </Context.Provider>
    </>
  );
};

export default App;
