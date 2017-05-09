#!/bin/bash
set -ex

TMPZIP="/tmp/cluttermm.zip"
SRCDIR="cluttermm-master"
INSTALLDIR="${HOME}/.local"

if [ ! -d "${SRCDIR}" ]; then
  CLUTTER_DIR=1.3.2
  wget https://github.com/GNOME/cluttermm/archive/${CLUTTER_DIR}.zip -O "${TMPZIP}"
  unzip "${TMPZIP}"
  rm -f "${TMPZIP}"
  mv cluttermm-${CLUTTER_DIR} ${SRCDIR}
fi

mkdir -p "${INSTALLDIR}"

pushd "${SRCDIR}" > /dev/null

ACLOCAL_FLAGS="-I ${INSTALLDIR}/share/aclocal" ./autogen.sh --prefix="${INSTALLDIR}" --disable-documentation
make
make install

popd > /dev/null

