#!/bin/sh

cd ~/git

for FOO in */
do
    cd $FOO
    git fetch
    cd ..
done
