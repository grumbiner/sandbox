#!/bin/sh

xlf90 convert4.f -o convert4.x
xlf90 readvsdb.f -o readvsdb.x -L/nwprod/lib -lw3_4
xlf90 readvsdblike.f -o readvsdblike.x -L/nwprod/lib -lw3_4
