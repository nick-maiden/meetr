import { Route, Routes } from 'react-router-dom';
import LandingPage from './components/LandingPage';
import Event from './pages/Event/Event';
import NotFoundPage from './pages/NotFoundPage';

const PageList = () => {
  return (
    <>
      <Routes>
        <Route path='/' element={<LandingPage />} />
        <Route path='/events/:eventId' element={<Event />} />
        <Route path='/404' element={<NotFoundPage />} />
        <Route path='*' element={<NotFoundPage />} />
      </Routes>
    </>
  );
};

export default PageList;
