#!/usr/bin/env bash
set -euo pipefail

APPDIR_SRC="${PWD}/AppDir"
BUILD_DIR="${PWD}/build"

APPDIR="${BUILD_DIR}/AppDir"

# build UncertRadio
rm -fr "${BUILD_DIR}"
cmake -B "${BUILD_DIR}" -DCMAKE_BUILD_TYPE=Release
cmake --build "${BUILD_DIR}" -j $(nproc)

# copy the source dir to build
cp -a "${APPDIR_SRC}" "${BUILD_DIR}"

# build using install rules from cmake
rm -rf "${APPDIR}/usr"
DESTDIR="${APPDIR}" cmake --install "${BUILD_DIR}" \
    --prefix "/usr" --strip

# add additional runtime assets
cp "${PWD}/icons/ur2_symbol.png" "${APPDIR}/ur2_symbol.png"

PLPLOT_LIB="/usr/lib/plplot5.15.0"
PLPLOT_SHARE="/usr/share/plplot5.15.0"

if [[ -f /etc/os-release ]]; then
    . /etc/os-release
    case "$ID" in
        ubuntu|debian) PLPLOT_LIB="/usr/lib/x86_64-linux-gnu/plplot5.15.0"
        ;;
    esac
fi

# plplot runtime data
mkdir "${APPDIR}/usr/lib"
cp -a "$PLPLOT_LIB" "${APPDIR}/usr/lib/plplot5.15.0"
cp -a "$PLPLOT_SHARE" "${APPDIR}/usr/share/plplot5.15.0"
rm -rf "${APPDIR}/usr/share/plplot5.15.0/examples"

# glib data
cp -a /usr/share/glib-2.0 "${APPDIR}/usr/share/glib-2.0"

# themes & icons
mkdir "${APPDIR}/usr/share/themes"
mkdir "${APPDIR}/usr/share/icons"
cp -a /usr/share/themes/Adwaita "${APPDIR}/usr/share/themes/Adwaita"
cp -a /usr/share/icons/Adwaita "${APPDIR}/usr/share/icons/Adwaita"

# 4. library dependencies
echo "Collecting shared‑library dependencies..."
while read -r lib; do
    # absolute path of the real library file
    real=$(readlink -f "$lib")
    # copy the real file
    cp -L "$lib" "${APPDIR}/usr/lib/$(basename "$real")"
    # copy the symlink
    cp -P "$lib" "${APPDIR}/usr/lib/$(basename "$lib")"
done < <(ldd "${APPDIR}/usr/bin/UncertRadio" | awk '/=>/{print $3}')

echo "AppDir ready at: ${APPDIR}"