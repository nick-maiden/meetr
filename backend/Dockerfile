FROM haskell:9.4.7

WORKDIR /app
COPY backend/ ./backend/

# Install dependencies first to leverage Docker cache

RUN cd backend && stack setup && stack build --only-dependencies

# Build the binary
RUN cd backend && stack build --copy-bins

