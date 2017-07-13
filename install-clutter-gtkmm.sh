#!/bin/bash
set -ex

TAG="CLUTTER_GTKMM_0_91_8"
TMPZIP="/tmp/clutter-gtkmm.zip"
SRCDIR="clutter-gtkmm-${TAG}"
INSTALLDIR="${HOME}/.local"

if [ ! -d "${SRCDIR}" ]; then
  wget https://github.com/GNOME/clutter-gtkmm/archive/${TAG}.zip -O "${TMPZIP}"
  unzip "${TMPZIP}"
  rm -f "${TMPZIP}"
fi

mkdir -p "${INSTALLDIR}"

pushd "${SRCDIR}" > /dev/null

PATH="${PATH}:${INSTALLDIR}/bin" ACLOCAL_FLAGS="-I ${INSTALLDIR}/share/aclocal" ./autogen.sh --prefix="${INSTALLDIR}" --disable-documentation

make
make install

popd > /dev/null

set +x
echo "------------------------------------------------------------------------------------------------"
echo "Don't forget to add the following settings to your environment before building and running VHDE:"
echo ""
echo "  export PKG_CONFIG_PATH=\"${INSTALLDIR}/lib/pkgconfig:${INSTALLDIR}/share/pkgconfig\""
echo "  export LD_LIBRARY_PATH=\"\${LD_LIBRARY_PATH}:${INSTALLDIR}/lib\""
echo ""
echo "------------------------------------------------------------------------------------------------"

