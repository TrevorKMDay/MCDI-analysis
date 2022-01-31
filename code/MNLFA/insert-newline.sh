#!/bin/bash

for i in models/lex_3d/*.inp ; do

    ll=($(wc -L "${i}"))

    echo
    if [ "${ll[0]}" -gt 89 ] ; then
        echo "${i} has line length greater than 90"
    fi

    echo "${i}"
    sed -i -e 's/D_FIRSTB D_MOMCOL ;/D_FIRSTB\r\n D_MOMCOL ;/' \
        -e 's/ *//' \
        "${i}"

done
