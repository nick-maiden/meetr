import ThemeToggle from "./ThemeToggle";

const LandingPage = () => {
  return (
    <div className="relative w-screen h-screen px-10 pt-4">
      <div className="flex items-center justify-between">
        <article className="prose">
          <h1 className="pt-4 text-6xl">meetr.</h1>
        </article>
        <ThemeToggle />
      </div>
      <div className="absolute inset-0 flex items-center justify-center">
        <button className="btn btn-outline btn-lg text-2xl">
          create event
        </button>
      </div>
    </div>
  )
};

export default LandingPage;
