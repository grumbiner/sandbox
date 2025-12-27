Changes to land-surface package in operational Eta model related to adding:
- liquid soil moisture (3-d prognostic variable)
- dynamic albedo (2-d prognostic variable)*
- maximum snow albedo (2-d static field)

*surface flux scheme (SFLX.F) now calculates dynamic albedo (ALBEDO; snow effects included when snow present), but SFLX.F called after radiation scheme (RADTN.F), so ALBEDO must be carried as a prognostic variable.

array dimensions:
I,J   = 2-d grid (global Eta variable)
NS    = soil layer (global Eta variable)
NSOIL = soil layer (local SFLX variable)

SURFCE.F      call to SFLX.F  within SFLX.F  description/
(2-d field)   (point value)   (point value)   notes
-----------   --------------  -------------  -----------------
ALBEDO(I,J)   ALB2D           ALBEDO         dynamic albedo/1
ALBASE(I,J)   ALB             ALB            snow-free albedo/2
MXSNAL(I,J)   SNOALB          SNOALB         max snow albedo/3
SNO(I,J)      SNODPK          SNEQV          snow water equivalent/4
SI(I,J)       SNOWH           SNOWH          snow depth/5
SH2O(I,J,NS)  SH2OK(NSOIL)    SH2O           liquid soil moisture/6

notes:
1. need to pass dynamic albedo, ALBEDO(I,J), to RADTN.f (also see note 7)
2. change ALB to ALBASE in PHYS.comm, PHYS1.comm, PHYS2.comm
3. change WFK to MXSNAL in PHYS.comm, PHYS1.comm, PHYS2.comm
4. SNO already in PVRBLS.comm (see note 5)
5. change 'WET' in PVRBLS.comm to 'SI' (snowdepth)
6. add SH2OK to SOIL.comm (see note 8)
7. turn off dynamic albedo calculation in RADTN.F since dynamic albedo is now calculated in SFLX2.F; create new RADTN2.F.  ALBASE no longer needed in RADTN.F.
8. need version for cold start which has a first guess of liquid soil moisture via subroutine from Pablo Grunmann (FRH2O.f)

completion:
1. done - added to PHYS.comm, PHYS1.comm, PHYS2.comm
2. done
3. done
4. done
5. done
6. done
7. done
8. done (in grdeta)

--
Changes to following subroutines in Eta fcst code:
(M. Ek, Jan-Mar 2001)

CHKOUT.F       change WET to SI
               add SH2O
               add ALBEDO
GOSSIP.F       change WFK to MXSNAL
               change ALB to ALBASE
               change WET to SI
               add SH2O
               add ALBEDO
INIT.F         add SH2O
               comment out/eliminate relevant code:
                 C       PAD GROUND WETNESS IF IT IS TOO SMALL.
                 c ...
	         c	 WET(I,J)=AMAX1(WET(I,J),EPSWET)
	         c ...		 
PHYS1.comm     change WFK to MXSNAL
	       change ALB to ALBASE
	       add ALBEDO
PHYS2.comm     change WFK to MXSNAL
	       change ALB to ALBASE
	       add ALBEDO
PHYS.comm      change WFK to MXSNAL
	       change ALB to ALBASE
	       add ALBEDO
PVRBLS.comm    change WET to SI
QUILT.F        change read WET to SI
	       add SH2O
RADTN.F        turned off dynamic albedo (ALBEDO) calc (now done in SFLX)
	       change ALBEDO to ALBDO (local/private)
	       change ALB to ALBEDO (dynamic/input from common block)
READ_NHB.F     change read WFK to MXSNAL
	       change read ALB to ALBASE
READ_RESTRT2.F change read WET to SI
	       add read SH2O
	       add read ALBEDO
READ_RESTRT.F  change read WET to SI
	       add read SH2O
	       add read ALBEDO
SFLX.F         new NOAH LSM 2.2
SOIL.comm      add SH2O
SURFCE.F       new NOAH LSM 2.2 driver
WRTRST.F       change write WET to SI
               add write SH2O
               add write ALBEDO
makefile       same as orig
