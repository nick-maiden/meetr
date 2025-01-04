import { Route, Routes } from 'react-router-dom';
import LandingPage from './components/LandingPage';

const PageList = () => {
  return (
    <>
      <Routes>
        <Route path='/' element={<LandingPage />} />
      </Routes>
    </>
  );
}

export default PageList;
