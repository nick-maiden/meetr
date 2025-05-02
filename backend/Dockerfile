FROM haskell:9.4.7

WORKDIR /app

# Copy dependency files first (to leverage Docker cache for dependencies)
COPY backend/stack.yaml backend/stack.yaml
COPY backend/package.yaml backend/package.yaml
COPY backend/stack.yaml.lock backend/stack.yaml.lock

# Install GHC and dependencies
RUN stack setup --install-ghc && stack build --only-dependencies

COPY backend/ ./backend/
RUN stack build --copy-bins

