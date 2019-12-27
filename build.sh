#!/bin/bash

name=calc

xst -ifn $name.xst || exit 1
ngdbuild $name -uc $name.ucf || exit 1
map $name || exit 1
par -w $name.ncd "${name}_par.ncd" || exit 1
bitgen -w ${name}_par.ncd -g StartupClk:JTAGClk || exit 1
