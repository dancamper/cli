#!/usr/bin/env bash
set -euo pipefail

# --- Config ---
REPO_URL="https://github.com/dancamper/cli"
IMAGE_NAME="dancamper/cli"
TAG="${TAG:-latest}"

# --- Temp build context ---
BUILD_DIR=$(mktemp -d)
trap 'rm -rf "$BUILD_DIR"' EXIT

# --- Stage 1: build image ---
cat >"$BUILD_DIR/Dockerfile" <<'EOF'
# ===============================
# Stage 1: build all Lisp binaries
# ===============================
FROM debian:bookworm-slim AS builder

# Install SBCL and deps
RUN apt-get update && \
    apt-get install -y sbcl git curl make && \
    rm -rf /var/lib/apt/lists/*

# Install Quicklisp
# Install Quicklisp (non-interactive, no prompt)
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --quit
# Add Quicklisp setup manually (no prompts)
ENV SBCL_HOME=/usr/lib/sbcl
ENV QUICKLISP_HOME=/root/quicklisp
RUN echo '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' >> /root/.sbclrc

# Clone the repo and build binaries
WORKDIR /build
ARG REPO_URL
RUN git clone --depth=1 "$REPO_URL" .
RUN make all

# ===============================
# Stage 2: runtime image
# ===============================
FROM debian:bookworm-slim

# Copy only the binaries
WORKDIR /usr/local/bin
COPY --from=builder /build/bin_linux/ ./

# Optional: include man pages
COPY --from=builder /build/man/man1 /usr/share/man/man1

# Default to showing available commands
CMD ["bash", "-c", "ls /usr/local/bin && echo 'Run any command directly, e.g. toolname'"]
EOF

# --- Build and tag image ---
docker build -t "${IMAGE_NAME}:${TAG}" --build-arg REPO_URL="$REPO_URL" "$BUILD_DIR"

echo "Built ${IMAGE_NAME}:${TAG}"
echo "Try it with: ./run-docker.sh tailf --help"
