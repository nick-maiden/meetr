import React from "react";
import { getHour } from "../utils";

interface TimeRangeSelectorValue {
  value: string | undefined;
  setValue: React.Dispatch<React.SetStateAction<string | undefined>>;
  options: string[];
};

interface UseTimeRangeSelectorReturn {
  earliest: TimeRangeSelectorValue;
  latest: TimeRangeSelectorValue;
};

const useTimeRangeSelector = (): UseTimeRangeSelectorReturn => {
  const [earliestTime, setEarliestTime] = React.useState<string | undefined>(undefined);
  const [latestTime, setLatestTime] = React.useState<string | undefined>(undefined);

  const allHours = React.useMemo(() => {
    return Array.from({ length: 24 }, (_, i) => {
      const period = i < 12 ? "am" : "pm";
      const hour = i % 12 === 0 ? 12 : i % 12;
      return `${hour}:00 ${period}`;
    });
  }, []);

  const earliestOptions = React.useMemo(() => {
    if (!latestTime) return allHours;
    return allHours.filter(h => getHour(h) < getHour(latestTime));
  }, [allHours, latestTime]);

  const latestOptions = React.useMemo(() => {
    if (!earliestTime) return allHours;
    return allHours.filter(h => getHour(h) > getHour(earliestTime));
  }, [allHours, earliestTime]);

  return {
    earliest: {
      value: earliestTime,
      setValue: setEarliestTime,
      options: earliestOptions
    },
    latest: {
      value: latestTime,
      setValue: setLatestTime,
      options: latestOptions
    }
  };
};

export default useTimeRangeSelector;

