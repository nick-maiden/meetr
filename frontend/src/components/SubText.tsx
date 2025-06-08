interface Props {
  children: React.ReactNode;
  className?: string;
}

const SubText = ({ children, className = "" }: Props) => {
  return (
    <p className={`sm:text-sm text-xs text-gray-500 ${className}`}>
      {children}
    </p>
  );
};

export default SubText;

