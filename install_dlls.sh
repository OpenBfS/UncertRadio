#!/bin/bash

if [ "$1" ]; then
    install_prefix=$1
    echo 'install prefix: '$1
else
    echo 'no install_prefix found, falling back to: "install/"'
    install_prefix='install'
fi

# find all needed dll's
ldd "${install_prefix}/bin/UncertRadio.exe" | grep -i $MSYSTEM | cut -d'=' -f1 | sed -e 's/^[ \t]*//' > myddls.txt

# copy those to the bin directory
echo 'installing dlls to: '$install_prefix'/bin'
cat myddls.txt | xargs -I % sh -c 'cp -vu $MSYSTEM_PREFIX/bin/% $0/bin' $install_prefix
