import React from 'react'
import { BrowserRouter } from 'react-router-dom';
import PageList from './PageList.js';
import { AppContext } from 'global/contexts.js';
import ErrorPopup from 'src/components/ErrorPopup.js';

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
      <html data-theme={theme == "light" ? "retro" : "dracula"}>
        <AppContext.Provider value={{ theme, setTheme, errorMessage, setErrorMessage }}>
          {errorMessage && <ErrorPopup />}
          <BrowserRouter>
            <PageList />
          </BrowserRouter>
        </AppContext.Provider>
      </html>
    </>
  );
};

export default App;
