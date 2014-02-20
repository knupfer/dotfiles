#! /bin/sh
cd ~/.i3
cat config.master | sed -e 's/# i3 config/-> i3 config/ ;
                            /^# /d ;
                            /^ *$/d ;
                            s/-> i3 config/# i3 config/' > config.tmp;
sed -n '/### BEGIN_GENERIC/, /### END_GENERIC/p' config.tmp > config;
echo 'NONE' > state;

for MODE
do
    sed -n '/### BEGIN_'$MODE'/, /### END_'$MODE'/p' config.tmp >> config;
    echo $MODE >> state;
done

rm config.tmp;

i3 reload
