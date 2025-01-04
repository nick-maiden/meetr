
const LandingPage = () => {
  return (
    <div className="relative w-screen h-screen bg-gray-100">
      <header className="absolute top-0 left-0 w-full bg-gray-800 text-white px-4 py-2">
        <div className="flex items-center justify-between">
          <h2>meetr</h2>
          <nav>
            <a
              href="#"
              className="text-white hover:text-gray-300 transition duration-200"
            >
              Repo
            </a>
          </nav>
        </div>
      </header>

      {/* Centered Button */}
      <main className="absolute inset-0 flex items-center justify-center">
        <button className="px-6 py-3 bg-blue-500 text-white text-lg rounded shadow hover:bg-blue-600 transition duration-200">
          create event
        </button>
      </main>
    </div>
  )
};

export default LandingPage;
