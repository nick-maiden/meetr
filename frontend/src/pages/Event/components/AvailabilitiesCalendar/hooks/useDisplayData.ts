import React from "react";
import { Event } from "global/types";
import useResponsiveDatesPerPage from "./useResponsiveDatesPerPage";
import { generateTimeData } from "../utils";

const useDisplayData = (event: Event) => {
  const [currentPage, setCurrentPage] = React.useState(0);
  const datesPerPage = useResponsiveDatesPerPage();
  const { hours, timeSlots } = generateTimeData(event.earliestTime, event.latestTime);
  const totalPages = Math.ceil(event.dates.length / datesPerPage);
  const displayDates = event.dates.slice(
    currentPage * datesPerPage,
    (currentPage + 1) * datesPerPage
  );

  return {
    currentPage,
    setCurrentPage,
    hours,
    timeSlots,
    totalPages,
    displayDates
  };
};

export default useDisplayData;

