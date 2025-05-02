FROM haskell:9.4.7

WORKDIR /app

# Install ghc and build app just with dependencies first
# to allow caching and reduce build time
COPY backend/stack.yaml backend/stack.yaml
COPY backend/package.yaml backend/package.yaml
COPY backend/stack.yaml.lock backend/stack.yaml.lock
RUN cd backend && stack setup --install-ghc && stack build --only-dependencies

COPY backend/ ./backend/
RUN cd backend && stack build --copy-bins

