#!/bin/sh

cd ~/git

for FOO in */
do
    cd $FOO
    git pull
    cd ..
done
