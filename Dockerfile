FROM debian:stable

ENV DEBIAN_FRONTEND=noninteractive

RUN    apt-get update \
    && apt-get dist-upgrade -y \
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

WORKDIR /project
COPY . .

RUN    cmake -S . -B build -DBUILD_DOCS=ON -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=./install \
    && cmake --build build --target install -j $(nproc)

CMD ["/project/install/bin/UncertRadio", "run_tests"]
