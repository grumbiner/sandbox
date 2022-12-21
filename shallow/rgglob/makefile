INC=~/usrlocal/mmablib/include

all : rgglob

rgglob : rgglob.C rgglob.h omblibc
	g++ -Wall -O2 -DLINUX rgglob.C -I $(INC) omblibc -o rgglob

omblibc : arcdis.o w3ft01.o
	ar rv omblibc arcdis.o w3ft01.o

