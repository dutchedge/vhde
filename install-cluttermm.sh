#!/bin/bash
set -ex

TMPZIP="/tmp/cluttermm.zip"
SRCDIR="cluttermm-master"
INSTALLDIR="${HOME}/.local"

if [ ! -d "${SRCDIR}" ]; then
  wget https://github.com/GNOME/cluttermm/archive/cluttermm-1.2.zip -O "${TMPZIP}"
  unzip "${TMPZIP}"
  rm -f "${TMPZIP}"
  mv cluttermm-cluttermm-1.2 ${SRCDIR}
fi

mkdir -p "${INSTALLDIR}"

pushd "${SRCDIR}" > /dev/null

ACLOCAL_FLAGS="-I ${INSTALLDIR}/share/aclocal" ./autogen.sh --prefix="${INSTALLDIR}" --disable-documentation
make
make install

popd > /dev/null

