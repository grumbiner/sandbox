
all: compare scan accumulate update 

tomask : tomask.C
	ncepxlC -o tomask -DIBM -O2 -I /nwprod/lib/sorc/omb/include/ -L/nwprod/lib/  -lombf_4 -lm  tomask.C

compare : compare.C
	ncepxlC -o compare -DIBM -O2 -I /nwprod/lib/sorc/omb/include/ -L/nwprod/lib/  -lombf_4 -lm  compare.C

scan : scan.C 
	ncepxlC -o scan -DIBM -O2 -I /nwprod/lib/sorc/omb/include/ -L/nwprod/lib/  -lombf_4 -lm  scan.C

accumulate : accumulate.C
	ncepxlC -o accumulate -DIBM -O2 -I /nwprod/lib/sorc/omb/include/ -L/nwprod/lib/  -lombf_4 -lm  accumulate.C

update : update.C
	ncepxlC -o update -DIBM -O2 -I /nwprod/lib/sorc/omb/include/ -L/nwprod/lib/  -lombf_4 -lm  update.C

