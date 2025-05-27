import React from 'react'
import { BrowserRouter } from 'react-router-dom';
import PageList from './PageList.jsx';
import { Context } from './util/context.js';
import './App.css'
import ErrorPopup from './components/ErrorPopup.js';

const App = () => {
  const [errorMessage, setErrorMessage] = React.useState("");
  const [theme, setTheme] = React.useState<"light" | "dark">(() => {
    const savedTheme = localStorage.getItem("theme");
    return savedTheme === "dark" ? "dark" : "light";
  });

  React.useEffect(() => {
    localStorage.setItem("theme", theme);
  }, [theme]);

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
