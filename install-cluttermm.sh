#!/bin/bash
set -ex

TMPZIP="/tmp/cluttermm.zip"
SRCDIR="cluttermm-master"
INSTALLDIR="${HOME}/.local"

if [ ! -d "${SRCDIR}" ]; then
  wget https://github.com/GNOME/cluttermm/archive/198efef4d0f0457f4132ef61d8ca34183b4120cf.zip -O "${TMPZIP}"
  unzip "${TMPZIP}"
  rm -f "${TMPZIP}"
  mv cluttermm-198efef4d0f0457f4132ef61d8ca34183b4120cf ${SRCDIR}
fi

mkdir -p "${INSTALLDIR}"

pushd "${SRCDIR}" > /dev/null

ACLOCAL_FLAGS="-I ${INSTALLDIR}/share/aclocal" ./autogen.sh --prefix="${INSTALLDIR}" --disable-documentation
make
make install

popd > /dev/null

