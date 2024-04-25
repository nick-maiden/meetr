/* eslint-disable @typescript-eslint/no-unused-vars */

import Calendar from "./components/Calendar";
import './App.css'


const App = () => {
  return (
    <div className="app" style={{ display: 'flex', alignItems: 'center', justifyContent: 'center' }}>
      <Calendar />
    </div>
  );
};

export default App;
