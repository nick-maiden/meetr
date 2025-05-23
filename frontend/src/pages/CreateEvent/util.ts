export const getHour = (timeStr: string): number => {
  const [hourStr, period] = timeStr.split(' ');
  let hour = parseInt(hourStr);
  if (period === 'pm' && hour !== 12) hour += 12;
  if (period === 'am' && hour === 12) hour = 0;
  return hour;
};

export const convertTo24Hour = (timeStr: string | undefined): string | undefined => {
  if (!timeStr) return undefined
  return `${getHour(timeStr).toString().padStart(2, '0')}:00`;
};

