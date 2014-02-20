#! /bin/sh
cd ~/.i3

sed -n '/### BEGIN_GENERIC/, /### END_GENERIC/p' config.master > config;
echo 'NONE' > state;

for MODE
do
    sed -n '/### BEGIN_'$MODE'/, /### END_'$MODE'/p' config.master >> config;
    echo $MODE >> state;
done

i3 reload
