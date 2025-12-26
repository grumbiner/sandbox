#!/bin/sh

cp /gpfsuser/narr/output/run_1987/edas_1987070600/allevents.tm06.Z events.Z
uncompress events.Z
sed '1,109d' events > events.tmp
ln -sf events.tmp fort.20
ln -sf /gpfsuser/narr/output/run_1987/edas_1987070600/prepcq.1987070518 fort.21
ln -sf prepout fort.50
rrvents.x
