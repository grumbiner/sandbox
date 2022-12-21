#linux 
CLD=g++ -O2 -Wall -DLINUX 
RPP=g++ -c -Wall -O2 -I$(INCDIR) -DLINUX 
#BASE=/u/Robert.Grumbine/save/
BASE=/usr1/rmg3/svn/
#INCDIR=$(BASE)/mmablib_v3.3.1/mmablib/include
INCDIR=$(BASE)/mmablib_v3.3.1/include
#LIBS=$(BASE)/mmablib_v3.3.1/mmablib/libombf_4.a -lm
LIBS=$(BASE)/mmablib_v3.3.1/libombf_4.a -lm

#Should be no changes below this point---------------------------------

all: bulk tally area

% : %.o
	$(CLD) $< $(LIBS) -o $@

% : %.C
	$(RPP) $<
