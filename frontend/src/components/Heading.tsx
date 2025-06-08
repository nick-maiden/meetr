interface Props {
  children: React.ReactNode;
  size?: "lg" | "md" | "sm";
  className?: string;
}

const Heading = ({ children, size = "md", className = "" }: Props) => {
  const dims =  size == "lg" ?  "md:text-3xl text-2xl" :
                size == "md" ?  "lg:text-3xl md:text-2xl text-xl" :
                                "lg:text-2xl md:text-xl text-lg";
  return (
    <p className={`${dims} ${className}`}>
      {children}
    </p>
  );
};

export default Heading;

