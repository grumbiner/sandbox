#LINUX
CC=g++
OPTS=-O2 -DLINUX -Wall  -I/usr1/rmg3/svn/mmablib/include
LIBS=~/usrlocal/lib/libombf_4.a

#--------------------------------------
all : harmonics annuals first_pass

harmonics : harmonics.C
	$(CC) $(OPTS)   harmonics.C -o harmonics $(LIBS)

annuals : annuals.C
	$(CC) $(OPTS)   annuals.C -o annuals $(LIBS)

first_pass : first_pass.C
	$(CC) $(OPTS) first_pass.C -o first_pass $(LIBS)

postquarter : postquarter.C
	$(CC) $(OPTS) postquarter.C -o postquarter $(LIBS)

processquarter : processquarter.C 
	$(CC) $(OPTS) processquarter.C -o processquarter $(LIBS)
