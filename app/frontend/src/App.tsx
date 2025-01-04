import { BrowserRouter } from 'react-router-dom';
import PageList from './PageList.jsx';
import './App.css'

const App = () => {
  return (
    <BrowserRouter>
      <PageList />
    </BrowserRouter>
  );
};

export default App;
