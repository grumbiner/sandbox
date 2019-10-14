/*
//*
//* *********************************************
//*     INPUT JCL FOR ARCTIC AND ANTARCTIC CASES
//* *********************************************
//*
//*O.FT08F001 DD DUMMY
//GO.FT08F001 DD UNIT=(3400-6,,DEFER),DISP=OLD,
//    LABEL=(1,NL,,IN),VOL=SER=100422,
//    DCB=(RECFM=FB,LRECL=5212,BLKSIZE=31272,DEN=4)
//* *********** ARCTIC CASE ABOVE
//*
//*
//* LABEL=(1,NL,,IN),VOL=SER=100767,
//* DCB=(RECFM=FB,LRECL=6728,BLKSIZE=26912,DEN=4)
//* ******** ANTARCTIC CASE ABOVE
//*
//* *********************************************
//*  OUTPUT JCL FOR ARCTIC AND ANTARCTIC CASES
//* *********************************************
//*
//GO.FT04F001 DD DUMMY
//*O.FT04F001 DD UNIT=(3400-6,,DEFER),DISP=NEW,
//*
//* ******** ARCTIC CASE
//* LABEL=(1,SL,,OUT),VOL=SER=100408,DSN=FEAST.OUTPUT,
//* DCB=(RECFM=FB,LRECL=6888,BLKSIZE=27552,DEN=4)
//*
//* ******** ANTARCTIC CASE
//* LABEL=(1,SL,,OUT),VOL=SER=100499,DSN=FEAST.OUTPUT,
//* DCB=(RECFM=FB,LRECL=6888,BLKSIZE=27552,DEN=4)
//*
