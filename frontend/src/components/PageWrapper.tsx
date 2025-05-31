import React from "react";
import Navbar from "./Navbar";

interface Props {
  children: React.ReactNode;
  innerContainerClassName?: string;
}

const PageWrapper: React.FC<Props> = ({
  children,
  innerContainerClassName = ""
}) => {
  return (
    <div className="flex flex-col w-screen h-screen sm:px-10 px-5">
      <Navbar />
      <div className={`
        flex
        flex-col
        overflow-auto
        no-scrollbar
        sm:mt-8
        mt-6
        pb-8
        ${innerContainerClassName}
        `}
      >
        {children}
      </div>
    </div>
  );
};

export default PageWrapper;

