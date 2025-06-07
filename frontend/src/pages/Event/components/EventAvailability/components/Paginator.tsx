import { ChevronLeft, ChevronRight } from "lucide-react";
import React from "react";

interface Props {
  currentPage: number;
  setCurrentPage: React.Dispatch<React.SetStateAction<number>>;
  totalPages: number;
}

const Paginator: React.FC<Props> = ({ currentPage, setCurrentPage, totalPages}) => {
  return (
    <div className="flex justify-between items-center mb-4">
      <button
        className="btn btn-outline sm:btn-sm btn-xs"
        onClick={() => setCurrentPage(prev => Math.max(0, prev - 1))}
        disabled={currentPage === 0}
      >
        <ChevronLeft className="h-4 w-4" />
      </button>
      <span className="md:text-xl sm:text-lg text-md">
        page {currentPage + 1} of {totalPages}
      </span>
      <button
        className="btn btn-outline sm:btn-sm btn-xs"
        onClick={() => setCurrentPage(prev => Math.min(totalPages - 1, prev + 1))}
        disabled={currentPage === totalPages - 1}
      >
        <ChevronRight className="h-4 w-4" />
      </button>
    </div>
  );
};

export default Paginator;

