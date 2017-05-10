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
set +e
make
sed -i "0,/Texture_Class/s//Texture_Class1/" clutter/cluttermm/wrap_init.cc
set -e
make
make install

popd > /dev/null

