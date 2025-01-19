import ThemeToggle from "./ThemeToggle";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faGithubAlt } from '@fortawesome/free-brands-svg-icons'

const Navbar = () => {
  return (
    <div className="flex items-center justify-between sm:pt-6 sm:pb-4 pt-4 pb-0">
      <article className="prose">
        <h1 className="sm:text-6xl text-5xl">meetr.</h1>
      </article>
      <div className="flex items-center justify-between gap-4">
        <ThemeToggle />
        <a
          href="https://github.com/nick-maiden/meetr"
          target="_blank"
          rel="noopener noreferrer"
        >
          <FontAwesomeIcon icon={faGithubAlt} size="xl"/>
        </a>
      </div>
    </div>
  )
};

export default Navbar;
