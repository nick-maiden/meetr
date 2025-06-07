import ThemeToggle from "./ThemeToggle";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faGithubAlt } from '@fortawesome/free-brands-svg-icons'

const Navbar = () => {
  return (
    <div className="flex items-center justify-between py-4">
      <h1 className="lg:text-6xl sm:text-5xl text-4xl">meetr.</h1>
      <div className="flex items-center justify-between sm:gap-4 gap-3">
        <ThemeToggle />
        <a
          href="https://github.com/nick-maiden/meetr"
          target="_blank"
          rel="noopener noreferrer"
        >
          <FontAwesomeIcon icon={faGithubAlt} size="xl" className="sm:text-2xl text-xl"/>
        </a>
      </div>
    </div>
  )
};

export default Navbar;
