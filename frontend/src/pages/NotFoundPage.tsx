import { Link } from "react-router-dom";
import background from "src/assets/404.webp";

const NotFoundPage = () => {
  return (
      <div
        className="flex items-center justify-center min-h-screen"
        style={{ backgroundImage: `url(${background})` }}
        >
        <div className="text-center text-zinc-300">
          <h1 className="text-9xl font-bold">404</h1>
          <h2 className="text-3xl md:text-4xl font-semibold mt-4">
            you're looking a little lost
          </h2>
          <p className="mt-2">
            might be time to <Link to="/" className="link">head home</Link>
          </p>
        </div>
      </div>
  );
};

export default NotFoundPage;

