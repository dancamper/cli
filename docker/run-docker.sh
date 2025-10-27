#!/usr/bin/env bash
set -euo pipefail

# ------------------------------------------------------------
# run-docker.sh
# Helper script to run a CLI tool from the Docker image
# without worrying about mounting paths or typing docker args.
#
# Usage:
#   ./run-docker.sh <toolname> [args...]
#
# Example:
#   ./run-docker.sh json2ecl ~/Downloads/foo.json
#
# This mounts the current directory into /work in the container
# and runs the tool with all provided arguments.
# ------------------------------------------------------------

IMAGE="dancamper/cli:latest"

# Mount current directory and pass through all args
docker run --rm -it \
  -v "$(pwd)":/work \
  -w /work \
  "$IMAGE" "$@"
