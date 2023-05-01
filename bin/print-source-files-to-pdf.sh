#!/bin/sh -f

target_files=$*


a2ps --media=Letter          \
     --pretty-print=Fortran  \
     --font-size=11          \
     --prologue=fixed        \
     --columns=1             \
     --portrait              \
     --line-numbers=0        \
     --header=`pwd | rev | cut -d/ -f1-3 | rev | sed "s/^/...\//"` \
     --toc                   \
     --right-footer=" "      \
     --left-footer=" "       \
     $target_files           \
     -o list.ps

ps2pdf list.ps list.pdf && \
    rm list.ps && \
    echo '...Generated list.pdf'
