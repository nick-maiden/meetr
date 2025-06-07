import React from 'react';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';
import { AvailabilitySlot } from '../types';
import { Event, UserId } from 'global/types';
import { isUserAvailable } from '../utils';
import Heading from 'src/components/Heading';

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
    <div className="bg-base-200 sm:p-4 p-3 rounded-lg overflow-y-auto no-scrollbar max-h-[70vh]">
      <Heading size='sm'>people</Heading>

      <div className="divider mt-2"></div>

      <ul className="sm:space-y-2 space-y-1.5">
        {Object.values(event.users).length === 0 ? (
          <p className="text-gray-500 sm:text-sm text-xs text-center">
            none (yet)
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
                  w-full lg:text-base md:text-sm text-xs
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

