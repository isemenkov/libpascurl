#!/bin/bash

LAZBUILD="lazbuild"
PROJECT="/home/ixaker/code/libpascurl/unit-tests/libpascurl_testproject.lpi"

# Modify .lpr file in order to avoid nothing-to-do-bug (http://lists.lazarus.freepascal.org/pipermail/lazarus/2016-February/097554.html)
echo. >> "/home/ixaker/code/libpascurl/unit-tests/libpascurl_testproject.lpr"

if $LAZBUILD $PROJECT; then

  if [ $1 = "test" ]; then
    "/home/ixaker/code/libpascurl/unit-tests/libpascurl_testproject" 
  fi
fi
