import React from 'react';

interface AppContextType {
  theme: "dark" | "light";
  setTheme: React.Dispatch<React.SetStateAction<"dark" | "light">>;
  errorMessage: string;
  setErrorMessage: React.Dispatch<React.SetStateAction<string>>;
}

const AppContext = React.createContext({} as AppContextType);

export { AppContext };

