import React from 'react';

type _Context = {
  theme: string
  setTheme: React.Dispatch<React.SetStateAction<string>>
}

const Context = React.createContext({} as _Context);

export { Context }
