import React from 'react';

type _Context = {
  theme: "dark" | "light"
  setTheme: React.Dispatch<React.SetStateAction<"dark" | "light">>
  errorMessage: string
  setErrorMessage: React.Dispatch<React.SetStateAction<string>>
}

const Context = React.createContext({} as _Context);

export { Context }
