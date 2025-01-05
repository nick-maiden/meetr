import React from 'react'
import { BrowserRouter } from 'react-router-dom';
import PageList from './PageList.jsx';
import { Context } from './util/context.js';
import './App.css'

const App = () => {
  const [theme, setTheme] = React.useState("dracula");
  return (
    <>
      <Context.Provider value={{ theme, setTheme }}>
        <html data-theme={theme == "light" ? "nord" : "dracula"}>
          <BrowserRouter>
            <PageList />
          </BrowserRouter>
        </html>
      </Context.Provider>
    </>
  );
};

export default App;
