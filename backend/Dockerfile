FROM haskell:9.4.7

WORKDIR /app
COPY backend/ ./backend/

# Install dependencies first to leverage Docker cache

RUN cd backend && stack setup --install-ghc && stack build --only-dependencies --install-ghc

# Build the binary
RUN cd backend && stack build --copy-bins

