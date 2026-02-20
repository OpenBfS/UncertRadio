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
PIXBUF_LOADERS="/usr/lib/gdk-pixbuf-2.0"
if [[ -f /etc/os-release ]]; then
    . /etc/os-release
    case "$ID" in
        ubuntu|debian)
        PLPLOT_LIB="/usr/lib/x86_64-linux-gnu/plplot5.15.0"
        PIXBUF_LOADERS="/usr/lib/x86_64-linux-gnu/gdk-pixbuf-2.0"
        ;;
    esac
fi

mkdir "${APPDIR}/usr/lib"
# plplot runtime data
cp -a "$PLPLOT_LIB" "${APPDIR}/usr/lib/plplot5.15.0"
cp -a "$PLPLOT_SHARE" "${APPDIR}/usr/share/plplot5.15.0"
rm -rf "${APPDIR}/usr/share/plplot5.15.0/examples"

# pixbuf loaders
cp -a "$PIXBUF_LOADERS" "${APPDIR}/usr/lib/gdk-pixbuf-2.0"
sed -i -e "s#${PIXBUF_LOADERS}#/usr/lib/gdk-pixbuf-2.0#g" "${APPDIR}/usr/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"

# glib data
cp -a /usr/share/glib-2.0 "${APPDIR}/usr/share/glib-2.0"

# themes & icons
mkdir "${APPDIR}/usr/share/themes"
mkdir "${APPDIR}/usr/share/icons"
cp -a /usr/share/themes/Adwaita "${APPDIR}/usr/share/themes/Adwaita"
cp -a /usr/share/icons/hicolor "${APPDIR}/usr/share/icons/hicolor"
cp -a /usr/share/icons/Adwaita "${APPDIR}/usr/share/icons/Adwaita"

# 4. library dependencies
echo "Collecting shared library dependencies..."

ALLOWED_PREFIXES=(
  "libplplot.so"
  "libplplotfortran.so"
  "liblapack.so"
  "libgfortran.so"
  "libshp.so"
  "libcsirocsa.so"
  "libcsironn.so"
  "libqsastime.so"
  "libblas.so"
  "libqhull_r.so"
)

prefix_regex="^($(IFS='|'; echo "${ALLOWED_PREFIXES[*]}"))"

APP_LIBDIR="${APPDIR:-.}/usr/lib"
mkdir -p "$APP_LIBDIR"

while read -r libpath; do
    [[ -z "$libpath" ]] && continue
    libname=$(basename "$libpath")

    # Only include whitelisted prefixe
    if [[ ! $libname =~ $prefix_regex ]]; then
        continue
    fi

    # Resolve real (versioned) file path
    real=$(readlink -f -- "$libpath") || { echo "warning: cannot resolve $libpath"; continue; }
    [[ -z "$real" ]] && continue
    realname=$(basename "$real")

    # Copy the versioned library into the AppDir
    cp -L -- "$real" "$APP_LIBDIR/$realname"

    # Re-create the soname symlink inside AppDir.
    if [[ "$libname" != "$realname" ]]; then
        ln -snf -- "$realname" "$APP_LIBDIR/$libname"
    fi

done < <(ldd "${APPDIR}/usr/bin/UncertRadio" | awk '/=>/ {print $3}')
echo "AppDir ready at: ${APPDIR}"