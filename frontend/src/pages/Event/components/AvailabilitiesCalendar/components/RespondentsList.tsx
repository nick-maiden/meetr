import React from 'react';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';
import { AvailabilitySlot } from '../types';
import { AvailabilityContext } from '../AvailabilityContext';

const RespondentsList = () => {
  const {
    event,
    hoveredSlot,
    setUserId,
    slotSelection,
    setIsSelectionMode
  } = React.useContext(AvailabilityContext);

  const isUserAvailable = (userId: string, slot: AvailabilitySlot): boolean => {
    return event.availabilities[slot.id]?.includes(userId) ?? false;
  };

  const getUserAvailability = (userId: string) => {
    return new Set(
      Object.entries(event.availabilities)
        .filter(([_, userIds]) => userIds.includes(userId))
        .map(([timeSlot]) => timeSlot)
        .sort()
    );
  }

  const editUserAvailability = (userId: string) => {
    setUserId(userId);
    slotSelection.setSlots(getUserAvailability(userId));
    setIsSelectionMode(true);
  };

  return (
    <div className="bg-base-200 p-4 rounded-lg overflow-y-auto no-scrollbar max-h-[70vh]">
      <h2 className="font-bold md:text-2xl sm:text-xl text-md">respondents</h2>

      <div className="divider mt-2"></div>

      <ul className="sm:space-y-2 space-y-1.5">
        {Object.values(event.users).length === 0 ? (
          <p className="font-bold text-gray-500 text-sm">
            no respondents yet 😢
          </p>
        ) : (
          Object.values(event.users).map((user) => {
            const isAvailable = hoveredSlot
              ? isUserAvailable(user.id,hoveredSlot)
              : true;
            return (
              <li key={user.id}>
                <div className={`
                  flex justify-between
                  hover:pl-1
                  transition-[padding,text-decoration,color] duration-500 ease-in-out hover:ease-out
                  w-full font-bold md:text-base sm:text-sm text-xs
                  ${!isAvailable && hoveredSlot ? 'line-through text-gray-500' : ''}
                `}>
                  <p className="truncate">{user.name}</p>
                  <button onClick={() => editUserAvailability(user.id)}>
                    <FontAwesomeIcon icon={faPenToSquare} />
                  </button>
                </div>
              </li>
            );
          })
        )}
      </ul>
    </div>
  );
};

export default RespondentsList;

