#!/bin/sh
#
tag=20220903
. $HOME/rgdev/toolbox/misc/python_load.wcoss2

time python3 viirs_conc_grid.py $tag/*Conc* > out.$tag
