#! /bin/bash

bapbuild -clean
wait
bapbuild "gen_info.plugin"
wait
bapbundle install gen_info.plugin
