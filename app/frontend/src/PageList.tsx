import { Route, Routes } from 'react-router-dom';
import LandingPage from './components/LandingPage';
import Event from './components/Event';

const PageList = () => {
  return (
    <>
      <Routes>
        <Route path='/' element={<LandingPage />} />
        <Route path='/event' element={<Event />} />
      </Routes>
    </>
  );
}

export default PageList;
