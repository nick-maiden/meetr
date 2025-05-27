import React from "react";

const useResponsiveDatesPerPage = () => {
  const [datesPerPage, setDatesPerPage] = React.useState(7);

  React.useEffect(() => {
    const handleResize = () => {
      if (window.innerWidth <= 600) {
        setDatesPerPage(4);
      } else if (window.innerWidth <= 900) {
        setDatesPerPage(5);
      } else {
        setDatesPerPage(7);
      }
    };

    window.addEventListener('resize', handleResize);
    handleResize();

    return () => window.removeEventListener('resize', handleResize);
  }, []);

  return datesPerPage;
};

export default useResponsiveDatesPerPage;

