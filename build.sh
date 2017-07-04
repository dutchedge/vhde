#!/bin/bash
export CC=/usr/bin/gcc-5
export CXX=/usr/bin/g++-5

./install-mm-common.sh

ACLOCAL_PATH="/home/patric/.local/share/aclocal"
export ACLOCAL_PATH

./install-cluttermm.sh

export PKG_CONFIG_PATH="/home/patric/.local/lib/pkgconfig:/home/patric/.local/share/pkgconfig"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/home/patric/.local/lib"

cmake .
make
