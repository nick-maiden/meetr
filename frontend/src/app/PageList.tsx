import { Route, Routes } from "react-router-dom";
import CreateEvent from "pages/CreateEvent/CreateEvent";
import Event from "pages/Event/Event";
import NotFoundPage from "pages/NotFoundPage";

const PageList = () => {
  return (
    <>
      <Routes>
        <Route path="/" element={<CreateEvent />} />
        <Route path="/events/:eventId" element={<Event />} />
        <Route path="/404" element={<NotFoundPage />} />
        <Route path="*" element={<NotFoundPage />} />
      </Routes>
    </>
  );
};

export default PageList;
