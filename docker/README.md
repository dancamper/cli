# Docker Support for Common Lisp CLI Tools

This directory contains helper scripts to build and run Docker images that package
the Common Lisp command-line tools from this repository.

All scripts assume that you have Docker installed and working on your system.

---

## 🏗️ Building the Image

To build a fresh Docker image containing prebuilt binaries:

```bash
cd docker
./dockerize.sh
```

This script:
1. Creates a temporary Docker build context.
2. Uses a two-stage build:
   - Stage 1 installs SBCL and Quicklisp, clones this repo, and runs `make all`.
   - Stage 2 copies the resulting binaries from `bin_linux/` into a minimal
     Debian runtime image.
3. Produces a final image named:

```
dancamper/cli:latest
```

You can tag the image manually if you wish:

```bash
docker tag dancamper/cli:latest dancamper/cli:v1.0
```

---

## ▶️ Running a Tool

Use `run-docker.sh` to execute any of the packaged tools directly:

```bash
cd docker
./run-docker.sh <toolname> [arguments...]
```

Examples:

```bash
./run-docker.sh json2ecl ~/Downloads/data.json
./run-docker.sh csv2plist ./examples/sample.csv
```

The script automatically:
- Mounts your current directory into the container at `/work`.
- Sets `/work` as the working directory.
- Forwards all arguments to the tool inside the container.

You can also install it globally:

```bash
chmod +x run-docker.sh
sudo cp run-docker.sh /usr/local/bin/cli
```

Then run any tool like this:

```bash
cd ~/Downloads
cli json2ecl data.json
```

---

## 🧹 Cleaning Up

Docker images can be removed at any time with:

```bash
docker rmi dancamper/cli:latest
```

Temporary build containers are automatically deleted after each run.

---

## 💡 Notes

- The image is based on `debian:bookworm-slim` for SBCL compatibility.
- Quicklisp is installed non-interactively inside the image.
- To update the image after changes, simply rerun `./dockerize.sh`.

---

**Maintainer:** [Dan Camper](https://github.com/dancamper)  
**License:** MIT
