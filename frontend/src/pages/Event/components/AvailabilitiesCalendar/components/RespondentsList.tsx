import React from 'react';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';
import { AvailabilitySlot } from '../types';
import { Event, UserId } from 'global/types';
import { isUserAvailable } from '../utils';

interface Props {
  event: Event;
  hoveredSlot: AvailabilitySlot | null;
  onEditAvailability: (userId: UserId) => void;
}

const RespondentsList: React.FC<Props> = ({
  event,
  hoveredSlot,
  onEditAvailability
}) => {

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
              ? isUserAvailable(user.id, hoveredSlot, event)
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
                  <button onClick={() => onEditAvailability(user.id)}>
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

