#!/bin/bash

if [ $# -eq 0 ]; then
  # Default to dev mode if no argument is provided
  MODE="dev"
else
  MODE=$1
fi

case $MODE in
  dev)
    echo "Running for development ..."
    export $(grep -v '^#' .env.local | xargs)
    stack run
    ;;
  prod)
    echo "Building for production ..."
    export $(grep -v '^#' .env.production | xargs)
    stack build --copy-bins
    ;;
  *)
    echo "Invalid mode: $MODE. Use 'dev' or 'prod'"
    exit 1
    ;;
esac

