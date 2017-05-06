#!/bin/bash

set -e

rm -rf javap_temp
mkdir -p javap_temp

for f in $(find src/main/scala/javap -name "*.scala" | sort); do
    echo "## $(basename $f)"
    echo

    echo '```scala'
    cat $f
    echo '```'
    echo

    scalac -target:jvm-1.8 $f -d javap_temp
    echo '```java'
    for c in $(find javap_temp -name "*.class" | sed -e 's/\//./g' | sed -e 's/\.class$//g'); do
        javap -p $c 2>/dev/null | grep -v "^Compiled from"
        echo
    done
    echo '```'

    echo

    rm -rf javap_temp/*
done

#rm -rf javap_temp
