	PROGRAM head
C	header part of program, this version mainly a test of program
C	  rather than a playable version.
	IMPLICIT none
	
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	
	INTEGER i, j
	INTEGER px, py, pz, dist
	REAL dummy, random
	INTEGER seed

C*************************************************************************
C	Code excerpted from qwix.for	
	include HD.I40:MacFortran020:include files:memory.inc
* Variables and constants for increasing the stack size
	integer*4 appllimit				! address of current aplication limit 
	integer*4 newlimit				! new application limit
	parameter (appllimit=z'00000130')
	
* By default, applications running on the Mac II are allocated a stack of
* 24k. A Fortran program which uses a significant amount of local memory 
* (ie. declares large arrays as local variables) will exceed this limit. The
* following lines show one way of increasing the amount of stack available.


	newlimit = LONG(appllimit)		! get the current limit
	newlimit = newlimit-51200		! allocate an additional 50k of stack
	call toolbx(SETAPPLLIMIT,newlimit)  ! set the new application limit
C*************************************************************************	
	
C	Set up the seed for the random number generator
	seed = IABS(MOD(LONG(362), 65536))
X	PRINT *,'seed is ',seed
	
C	Put objects into the dungeon
X	PRINT *,'calling placer'
	PRINT *,LONG(362)
	CALL placer(seed)
	PRINT *,LONG(362)
X	PRINT *,'after placer '
X	WRITE (*,9001) ( objdat(i),i=1,nobjs*ldesc )
 9001 FORMAT (19I4)
 
C	List out the positions
X	PRINT *,'positions'
	DO 1000 i = 1, nobjs
	  CALL posit(i, px, py, pz)
X	  PRINT *, i, px, py, pz
 1000 CONTINUE

C	Print the distances between adjacent numbers -
	DO 1100 i = 1, nobjs-1
X	  PRINT *,'distance between ', i, i+1, dist(i,i+1)
 1100 CONTINUE
	
C	Print the sensory description
X	PRINT *,'entering description loop'
	DO 1200 i = 1, nobjs
	  PRINT *,dvis(i), daur(i), dodr(i), dtas(i), dtch(i)
 1200 CONTINUE

C	Print the class description
X	PRINT *,'entering class loop'
	DO 1300 i = 1, nobjs
	  PRINT *,dclc(i), dclb(i), dcla(i), dspfc(i)
 1300 CONTINUE
 
 	PAUSE

	END	
      SUBROUTINE bkgnd
	RETURN
	END	
      SUBROUTINE closer
C	Close down the game.

	CALL savegm
	CALL currsc
	CALL highsc

	RETURN
	END	
      SUBROUTINE crchar
	RETURN
	END	
      SUBROUTINE currsc
	RETURN
	END 	
      FUNCTION daur(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
     
      SAVE dataur
		
	DATA (dataur(j),j=1,90)/'air     ',
     1  'babble  ','bang    ','bark    ','bawl    ','bellow  ',
     2  'blare   ','blast   ','blow    ','bong    ','boom    ',
     3  'bray    ','breaking','buzz    ','buzzing ','cackle  ',
     4  'chanting','cheep   ','chime   ','chiming ','chirping',
     5  'chortle ','chuckle ','clamor  ','clangor ','clanking',
     6  'clash   ','clashing','clatter ','clicking','clink   ',
     7  'coughing','cracking','crash   ','creaking','crunch  ',
     8  'cry     ','drone   ','droning ','drum    ','drumming',
     9  'foghorn ','giggle  ','giggling','gong    ','grate   ',
     a  'grating ','grind   ','groaning','growl   ','grunting',
     1  'guffaw  ','gurgle  ','hack    ','harmony ','hawk    ',
     2  'hiss    ','hissing ','hooting ','horn    ','howl    ',
     3  'howling ','humming ','jingle  ','jingling','keen    ',
     4  'knocking','laugh   ','laughing','medley  ','melody  ',
     5  'moaning ','mumble  ','murmur  ','music   ','pat     ',
     6  'peep    ','pipe    ','popping ','pounding','pulse   ',
     7  'rap     ','rasp    ','rasping ','rattle  ','rattling',
     8  'ring    ','ringing ','roar    ','roaring '/
      DATA (dataur(j),j=91,ndsau)/'rustle  ',
     1  'rustling','scrabble','scratch ','screach ','scream  ',
     2  'scuttle ','shout   ','shriek  ','shuffle ','siren   ',
     3  'sizzle  ','slam    ','sliding ','slipping','slither ',
     4  'smash   ','snapping','snarl   ','sneezing','snicker ',
     5  'snore   ','sobbing ','spatter ','spit    ','splash  ',
     6  'splinter','spray   ','sprinkle','squawk  ','squeak  ',
     7  'squeal  ','strain  ','strum   ','sussurus','swash   ',
     8  'tap     ','tapping ','thrum   ','thud    ','thumping',
     9  'thunder ','tinkle  ','tinkling','tip     ','titter  ',
     a  'tocsin  ','toot    ','tootle  ','trill   ','trumpet ',
     1  'tune    ','twanging','twitter ','wail    ','wailing ',
     2  'warble  ','weep    ','wheezing','whimper ', 'whine   ',
     3  'whining ','whir    ','whisper ','whistle ',
     4  'yowl    '/

	daur = dataur(objdat((i-1)*ldesc+ptraur))
	
	RETURN
	END	
      FUNCTION dcla(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
     
      SAVE datcla
		
	DATA (datcla(j), j = 1, ndscla)/'money  ',
     2 'gems   ',
     3 'spices ',
     4 'jewelry',
     5 'meat   '/

	dcla = datcla(objdat((i-1)*ldesc+ptrclsa))
	
	RETURN
	END	
      FUNCTION dclb(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
     
      SAVE datclb
		
	DATA (datclb(j), j = 1, ndsclb)/'valuable',
     2 'food    ',
     3 'drink   ',
     4 'clothing',
     5 'mount   '/

	dclb = datclb(objdat((i-1)*ldesc+ptrclsb))
	
	RETURN
	END	
      FUNCTION dclc(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
     
      SAVE datclc
	
	DATA (datclc(j), j = 1, ndsclc)/'consumable',
     2 'wearable  ',
     3 'transport ',
     4 'handheld  ',
     5 'other     '/
	
	dclc = datclc(objdat((i-1)*ldesc+ptrclsc))
	
	RETURN
	END
	SUBROUTINE dictin(number,props)
C	Supply the definition of object number 'number'
	IMPLICIT none
	
	INCLUDE HD.I40:working:programming:game:objdef.inc
	
	INTEGER props(ldesc), number
	INTEGER entrys(ntyp*ldesc)
	INTEGER i, j
      
	SAVE entrys
     	
	DATA (entrys(i),i = 1, ntyp*ldesc)/
     1 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 10, 1, 0,
     2 1, 1, 1, 1, 0, 0, 0, 2, 2, 2, 1, 1, 1, 1, 1, 1,  1, 1, 0,
     3 1, 1, 1, 1, 0, 0, 0, 3, 3, 3, 1, 3, 1, 1, 1, 1,  1, 1, 0,
     4 1, 1, 1, 1, 0, 0, 0, 4, 4, 4, 1, 4, 1, 1, 1, 1,  1, 1, 0,
     5 1, 1, 1, 1, 0, 0, 0, 5, 5, 5, 1, 5, 1, 1, 1, 1,  1, 1, 0/

X	PRINT *,'in dictin, number = ',number
X	PRINT *, (entrys(i), i = 1, ldesc)
      DO 1000 j = 1, ldesc
X	  PRINT *,ldesc*(number-1)+j
        props(j) = entrys((number-1)*ldesc+j)
 1000 CONTINUE
 
 	RETURN
	END	
      FUNCTION dist(obja, objb)
C     compute the distance between two objects
	IMPLICIT none
	
	INTEGER obja, objb, dist
	INTEGER pxa, pxb, pya, pyb, pza, pzb
X	REAL pythag
	
	CALL posit(obja, pxa, pya, pza)
	CALL posit(objb, pxb, pyb, pzb)
												
	dist = (pxa-pxb)*(pxa-pxb) + (pya-pyb)*(pya-pyb) + 
     1       (pza-pzb)*(pza-pzb)
X	pythag = FLOAT(dist)
X	pythag = SQRT(pythag)
X	dist   = IFIX(pythag)
	dist = IFIX(SQRT(FLOAT(dist)))
	
	RETURN
	END	
      FUNCTION dist(obja, objb)
C     compute the distance between two objects
	IMPLICIT none
	
	INTEGER obja, objb, dist
	INTEGER pxa, pxb, pya, pyb, pza, pzb
X	REAL pythag
	
	CALL posit(obja, pxa, pya, pza)
	CALL posit(objb, pxb, pyb, pzb)
												
	dist = (pxa-pxb)*(pxa-pxb) + (pya-pyb)*(pya-pyb) + 
     1       (pza-pzb)*(pza-pzb)
X	pythag = FLOAT(dist)
X	pythag = SQRT(pythag)
X	dist   = IFIX(pythag)
	dist = IFIX(SQRT(FLOAT(dist)))
	
	RETURN
	END	
      FUNCTION dodr(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j

	SAVE datodr

	DATA (datodr(j), j = 1, ndsod)/'acidic  ',
     2  'acrid   ', 'aromatic', 'bitter  ', 'briney  ', 'chlorine',
     7  'clammy  ', 'damp    ', 'dank    ', 'earthy  ', 'fetid   ',
     2  'floral  ', 'fumy    ', 'gamy    ', 'high    ', 'manure  ',
     7  'metallic', 'mildewy ', 'moldered', 'mouldy  ', 'muggy   ',
     2  'ozone   ', 'pungent ', 'putrid  ', 'rotten  ', 'rotting ',
     7  'salty   ', 'savory  ', 'smokey  ', 'stale   ', 'stenchy ',
     2  'sticky  ', 'urine   ', 'vinegary',
     5  'wet     '/

	dodr = datodr(objdat((i-1)*ldesc+ptrodr))

	RETURN
	END	
      PROGRAM donjon
C	Dungeon adventuring game.
C	Main program
C	Bob Grumbine 10-29-89

	CALL prepar
	CALL play
	CALL closer
	
	END	 
      SUBROUTINE dressu
	RETURN
	END	
      FUNCTION dspfc(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
	
      SAVE datspfc
	
	DATA (datspfc(j), j = 1, ndsspfc)/'gold piece  ',
     2 'diamond     ',
     3 'copper piece',
     4 'fowl        ',
     5 'apple       '/
	
	dspfc = datspfc(objdat((i-1)*ldesc+ptrspfc))
	
	RETURN
	END	
      FUNCTION dspfc(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
	
      SAVE datspfc
	
	DATA (datspfc(j), j = 1, ndsspfc)/'gold piece  ',
     2 'diamond     ',
     3 'copper piece',
     4 'fowl        ',
     5 'apple       '/
	
	dspfc = datspfc(objdat((i-1)*ldesc+ptrspfc))
	
	RETURN
	END	
      FUNCTION dtas(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
	
	SAVE dattas
	
	DATA (dattas(j), j = 1, ndsta)/'acid    ',
     2  'acrid   ', 'bitter  ', 'briney  ', 'candied ', 'cloying ',
     7  'greasy  ', 'honied  ', 'oily    ', 'saline  ', 'salty   ',
     2  'soapy   ', 'sour    ', 'sugary  ', 'sweet   ', 'syrupy  ',
     7  'tart    ', 'vinegary'/

	dtas = dattas(objdat((i-1)*ldesc+ptrtas))
	
	RETURN
	END
	
	FUNCTION dtch(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j

	SAVE dattch
	
	DATA (dattch(j), j = 1, ndstc)/'bumpy   ',
     2  'coarse  ', 'creamy  ', 'doughy  ', 'downy   ', 'feathery',
     7  'firm    ', 'fluffy  ', 'furry   ', 'glassy  ', 'gooey   ',
     2  'greasy  ', 'hairy   ', 'hard    ', 'jagged  ', 'oily    ',
     7  'pasty   ', 'pebbled ', 'pliant  ', 'pulpy   ', 'ragged  ',
     2  'rocky   ', 'rough   ', 'satiny  ', 'scratchy', 'sharp   ',
     7  'silky   ', 'slick   ', 'slippery', 'smooth  ', 'soapy   ',
     2  'soft    ', 'soppy   ', 'stony   ', 'uneven  ', 'velvety ',
     7  'wet     ', 'woolly  '/

	dtch = dattch(objdat((i-1)*ldesc+ptrtch))

	RETURN
	END
	FUNCTION dvis(i)
C	Function to get the verbal description equivalent to 
C	  the description index
	IMPLICIT none
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INTEGER i, j
     
      SAVE datvis
		
	DATA (datvis(j), j = 1, ndsvi)/'glistening',
     2 'glowing   ',
     3 'shining   ',
     4 'oily      ',
     5 'dirty     '/

	dvis = datvis(objdat((i-1)*ldesc+ptrvis))
	
	RETURN
	END
		PROGRAM head
C	header part of program, this version mainly a test of program
C	  rather than a playable version.
	IMPLICIT none
	
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	INCLUDE HD.I40:MacFortran020:game:vdesc.inc
	
	INTEGER i, j
	INTEGER px, py, pz, dist
	REAL dummy, random
	INTEGER seed

C*************************************************************************
C	Code excerpted from qwix.for	
	include HD.I40:MacFortran020:include files:memory.inc
* Variables and constants for increasing the stack size
	integer*4 appllimit				! address of current aplication limit 
	integer*4 newlimit				! new application limit
	parameter (appllimit=z'00000130')
	
* By default, applications running on the Mac II are allocated a stack of
* 24k. A Fortran program which uses a significant amount of local memory 
* (ie. declares large arrays as local variables) will exceed this limit. The
* following lines show one way of increasing the amount of stack available.


	newlimit = LONG(appllimit)		! get the current limit
	newlimit = newlimit-51200		! allocate an additional 50k of stack
	call toolbx(SETAPPLLIMIT,newlimit)  ! set the new application limit
C*************************************************************************	
	
C	Set up the seed for the random number generator
	seed = IABS(MOD(LONG(362), 65536))
X	PRINT *,'seed is ',seed
	
C	Put objects into the dungeon
X	PRINT *,'calling placer'
	PRINT *,LONG(362)
	CALL placer(seed)
	PRINT *,LONG(362)
X	PRINT *,'after placer '
X	WRITE (*,9001) ( objdat(i),i=1,nobjs*ldesc )
 9001 FORMAT (19I4)
 
C	List out the positions
X	PRINT *,'positions'
	DO 1000 i = 1, nobjs
	  CALL posit(i, px, py, pz)
X	  PRINT *, i, px, py, pz
 1000 CONTINUE

C	Print the distances between adjacent numbers -
	DO 1100 i = 1, nobjs-1
X	  PRINT *,'distance between ', i, i+1, dist(i,i+1)
 1100 CONTINUE
	
C	Print the sensory description
X	PRINT *,'entering description loop'
	DO 1200 i = 1, nobjs
	  PRINT *,dvis(i), daur(i), dodr(i), dtas(i), dtch(i)
 1200 CONTINUE

C	Print the class description
X	PRINT *,'entering class loop'
	DO 1300 i = 1, nobjs
	  PRINT *,dclc(i), dclb(i), dcla(i), dspfc(i)
 1300 CONTINUE
 
 	PAUSE

	END	
      SUBROUTINE highsc
	RETURN
	END	
      SUBROUTINE mapmak
	RETURN
	END	
      SUBROUTINE menus
	RETURN
	END	
      SUBROUTINE prepar
C	Subroutine to prepare the game and the player
C	Bob Grumbine 10-29-89

C	Prepare the Macintosh
	CALL palet
	CALL windos
	CALL menus
	
C	Prepare the Player
	CALL bkgnd
	CALL rules
	CALL crchar
	CALL react

C	Prepare the dungeon
	CALL mapmak
	CALL plobjs
	CALL plmons
	CALL pltrks
	CALL ratpl
	CALL dressu
	
	RETURN
	END	
      SUBROUTINE palet
	RETURN
	END	
      SUBROUTINE placer(seed)
C	Place the objects in their proper positions
	IMPLICIT none

	INCLUDE HD.I40:working:programming:game:objdef.inc
	INCLUDE HD.I40:working:programming:game:objfil.inc
	
	INTEGER tobj(ldesc)
	INTEGER seed
	INTEGER i, ttyp, j
	REAL    random, dummy, dum2
	SAVE    seed, random

C     Dummy call of random to give a proper number	
	DO 1000 i = 1, nobjs
	  dummy = random(seed)
	  PRINT *,'The random number info is ',seed,dummy,FLOAT(ntyp)
	  dum2 = FLOAT(ntyp)*dummy
	  PRINT *,'The product of ntyp and dummy is',ntyp,dummy,dum2
	  ttyp = INT(dum2)+1
 	  PRINT *,'calling dictin',i,ttyp,seed,' ptrown = ',ptrown
	  CALL dictin(ttyp,tobj)
	  DO 1010 j = 1, ldesc
	    objdat((i-1)*ldesc+j ) = tobj(j)
 1010   CONTINUE
        objdat((i-1)*ldesc+ptrposx) = INT(1.E3*random(seed)+0.5)
	  objdat((i-1)*ldesc+ptrposy) = INT(1.E3*random(seed)+0.5)
	  objdat((i-1)*ldesc+ptrposz) = INT(1.E3*random(seed)+0.5)
	  IF (objdat((i-1)*ldesc+ptrown) .EQ. 0) THEN
	    objdat((i-1)*ldesc+ptrown) = i
	  ENDIF
X	  PRINT *,i,objdat((i-1)*ldesc+ptrown)
 1000 CONTINUE
	
	RETURN
	END	
      SUBROUTINE play
C	Subroutine to control the play of the game
C	Will be event driven (mouse and keyboard)
	RETURN
	END      
      SUBROUTINE plmons
      RETURN
      END	
      SUBROUTINE plobjs
	CALL placer(1)
	RETURN
	END	
      SUBROUTINE pltrks
	RETURN
	END	
      SUBROUTINE posit(dobj, px, py, pz)
C	Compute the position of an object (specified by nobj)
C	Allow for position offsets due to ownership
	IMPLICIT none
	
	INCLUDE HD.I40:MacFortran020:game:objdef.inc
	INCLUDE HD.I40:MacFortran020:game:objfil.inc
	
	INTEGER dobj, px, py, pz
	INTEGER mobj
	
C	Get first info on object:
	mobj = dobj
	px = objdat(ldesc*(dobj-1)+ptrposx)
	py = objdat(ldesc*(dobj-1)+ptrposy)
	pz = objdat(ldesc*(dobj-1)+ptrposz)
X	PRINT *,'in posit ',mobj,objdat(ldesc*(dobj-1)+ptrown)
	
 1000 IF (objdat(ldesc*(mobj-1)+ptrown) .NE. mobj) THEN
	  mobj =    objdat(ldesc*(mobj-1)+ptrown)
 	  px = px + objdat(ldesc*(mobj-1)+ptrposx)
	  py = py + objdat(ldesc*(mobj-1)+ptrposy)
	  pz = pz + objdat(ldesc*(mobj-1)+ptrposz)
	  PRINT *,'looping in posit'
	  GO TO 1000
	 ELSE
C       Have reached owner, quit
	ENDIF
	
	RETURN
	END	
      SUBROUTINE prepar
C	Subroutine to prepare the game and the player
C	Bob Grumbine 10-29-89

C	Prepare the Macintosh
	CALL palet
	CALL windos
	CALL menus
	
C	Prepare the Player
	CALL bkgnd
	CALL rules
	CALL crchar
	CALL react

C	Prepare the dungeon
	CALL mapmak
	CALL plobjs
	CALL plmons
	CALL pltrks
	CALL ratpl
	CALL dressu
	
	RETURN
	END	
      PROGRAM prepar
C	Subroutine to prepare the game and the player
C	Bob Grumbine 10-29-89
C     Preparprog is a variant to allow testing of only the
C       preparation routines

C	Prepare the Macintosh
	CALL palet
	CALL windos
	CALL menus
	
C	Prepare the Player
	CALL bkgnd
	CALL rules
	CALL crchar
	CALL react

C	Prepare the dungeon
	CALL mapmak
	CALL plobjs
	CALL plmons
	CALL pltrks
	CALL ratpl
	CALL dressu
	
	RETURN
	END
      FUNCTION random(seed)
      INTEGER seed
      REAL random
      INTEGER modulus, mult, incr
      PARAMETER ( modulus = 65536 )
      PARAMETER ( mult    = 25173 )
      PARAMETER ( incr    = 13849 )
      
      random = FLOAT(seed)/FLOAT(modulus)
      seed   = MOD(mult*seed+incr, modulus)
      
C      Algorithm from Grogono, p. 118
C      Random is in range [0., 1.)

      RETURN
      END	
      SUBROUTINE ratpl
	RETURN
	END	
      SUBROUTINE react
	RETURN
	END	
      SUBROUTINE rules
	RETURN
	END	
      SUBROUTINE savegm
	RETURN
	END	
      SUBROUTINE windos
	RETURN
	END
C	objdic -- object dictionary parameters
	INTEGER ntyp
	PARAMETER (ntyp  =  5)		! number of different types of objects available
C	objfil parm -- object file/array description
	INTEGER nobjs, ldesc
	PARAMETER (nobjs = 10)		! number of objects in dungeon
	PARAMETER (ldesc = 19)		! number of descriptive parameters
C	objdef -- definition of the pointers into objdat
	INTEGER ptrmass, ptrlenx, ptrleny, ptrlenz
	INTEGER ptrposx, ptrposy, ptrposz
	INTEGER ptrvis, ptraur, ptrodr, ptrtas, ptrtch
	INTEGER ptrspfc, ptrclsa, ptrclsb, ptrclsc
	INTEGER ptrpric, ptrnum, ptrown
	PARAMETER ( ptrmass =  1 )
	PARAMETER ( ptrlenx =  2 )
	PARAMETER ( ptrleny =  3 )
	PARAMETER ( ptrlenz =  4 )
	PARAMETER ( ptrposx =  5 )
	PARAMETER ( ptrposy =  6 )
	PARAMETER ( ptrposz =  7 )
	PARAMETER ( ptrvis  =  8 )
	PARAMETER ( ptraur  =  9 )
	PARAMETER ( ptrodr  = 10 )
	PARAMETER ( ptrtas  = 11 )
	PARAMETER ( ptrtch  = 12 )
	PARAMETER ( ptrspfc = 13 )
	PARAMETER ( ptrclsa = 14 )
	PARAMETER ( ptrclsb = 15 )
	PARAMETER ( ptrclsc = 16 )
	PARAMETER ( ptrpric = 17 )
	PARAMETER ( ptrnum  = 18 )
	PARAMETER ( ptrown  = 19 )C	The actual object storage array.
C	Place in common to be available generally
C	Must INCLUDE objdef first
	INTEGER objdat(nobjs*ldesc)
	COMMON /objects/ objdatC	vdesc -- file wih params for describing
C	  the verbal description function files
	INTEGER ndsvi,  ndsau,  ndsod,  ndsta, ndstc
	INTEGER ndsclc, ndsclb, ndscla, ndsspfc
	INTEGER ldsvi,  ldsau,  ldsod,  ldsta, ldstc
	INTEGER ldsclc, ldsclb, ldscla, ldsspfc
C	Number of descriptors
	PARAMETER (ndsvi   =   5 )
	PARAMETER (ndsau   = 156 )
	PARAMETER (ndsod   =  35 )
	PARAMETER (ndsta   =  18 )
	PARAMETER (ndstc   =  38 )
	PARAMETER (ndsclc  =   5 )
	PARAMETER (ndsclb  =   5 )
	PARAMETER (ndscla  =   5 )
	PARAMETER (ndsspfc =   5 )
C	Length of description
	PARAMETER (ldsvi   = 10 )
	PARAMETER (ldsau   =  8 )
	PARAMETER (ldsod   =  8 )
	PARAMETER (ldsta   =  8 )
	PARAMETER (ldstc   =  8 )
	PARAMETER (ldsclc  = 10 )
	PARAMETER (ldsclb  =  8 )
	PARAMETER (ldscla  =  7 )
	PARAMETER (ldsspfc = 12 )
C	Define the functions involved
	CHARACTER*10 dvis
	CHARACTER*8  daur
	CHARACTER*8  dodr
	CHARACTER*8  dtas
	CHARACTER*8  dtch
	CHARACTER*10 dclc
	CHARACTER*8  dclb
	CHARACTER*7  dcla
	CHARACTER*12 dspfc
C	Define the functions involved
	CHARACTER*10 datvis(ndsvi)
	CHARACTER*8  dataur(ndsau)
	CHARACTER*8  datodr(ndsod)
	CHARACTER*8  dattas(ndsta)
	CHARACTER*8  dattch(ndstc)
	CHARACTER*10 datclc(ndsclc)
	CHARACTER*8  datclb(ndsclb)
	CHARACTER*7  datcla(ndscla)
	CHARACTER*12 datspfc(ndsspfc)
	SUBROUTINE windos
	RETURN
	END