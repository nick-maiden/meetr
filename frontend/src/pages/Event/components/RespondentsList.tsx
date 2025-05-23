import React from 'react';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';
import { User } from '../../../types';

interface Props {
  users: { [userId: string]: User };
  hoveredSlot: string | null;
  isUserAvailable: (userId: string, date: Date, time: string) => boolean;
  onEditAvailability: (userId: string) => void;
}

const RespondentsList: React.FC<Props> = ({
  users,
  hoveredSlot,
  isUserAvailable,
  onEditAvailability
}) => {
  return (
    <div className="bg-base-200 p-4 rounded-lg overflow-y-auto no-scrollbar max-h-[70vh]">
      <h2 className="font-bold md:text-2xl sm:text-xl text-md">respondents</h2>

      <div className="divider mt-2"></div>

      <ul className="sm:space-y-2 space-y-1.5">
        {Object.values(users).length === 0 ? (
          <p className="font-bold text-gray-500 text-sm">
            no respondents yet 😢
          </p>
        ) : (
          Object.values(users).map((user) => {
            const isAvailable = hoveredSlot ?
              isUserAvailable(
                user.id,
                new Date(hoveredSlot.split('-').slice(0, 3).join('-')),
                hoveredSlot.split('-').slice(3).join('-')
              ) : true;
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

