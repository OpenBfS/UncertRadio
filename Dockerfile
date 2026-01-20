FROM debian:stable

ARG BUILD_TYPE=Release

ENV DEBIAN_FRONTEND=noninteractive
ENV GDK_BACKEND=x11
ENV NO_AT_BRIDGE=1

RUN    apt-get update \
    && apt-get dist-upgrade -y \
    && apt-get install -y --no-install-recommends \
       xvfb            \
       xauth           \
       libgl1          \
       libglx-mesa0    \
       scrot           \
       dbus-x11        \
    && rm -rf /var/lib/apt/lists/*
RUN    apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential     \
       cmake               \
       gfortran            \
       git                 \
       libgtk-3-dev        \
       libplplot-dev       \
       plplot-driver-cairo \
       liblapack-dev       \
       python3-sphinx      \
       python3-myst-parser \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .

RUN    cmake -S . -B build -DBUILD_DOCS=ON -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_INSTALL_PREFIX=./install \
    && cmake --build build --target install -j $(nproc)

ENTRYPOINT ["dbus-run-session", "--", "xvfb-run", "--auto-servernum", "--error-file", "/dev/stdout", "--server-args=-screen 0 1024x768x24", "stdbuf", "-oL", "-eL"]
CMD ["/app/install/bin/UncertRadio"]
