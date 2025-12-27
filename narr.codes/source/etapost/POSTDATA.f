      BLOCK DATA POSTDATA

      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "OPTIONS.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "RQSTFLD.comm"
C
C
C     COMMON BLOCK OPTIONS CONTAINS USER OPTIONS
C        SPVAL :  FLAG FOR MISSING DATA ON OUTPUT GRID.  THIS
C	          CAN ARISE IF OUTPUT GRID EXTENDS BEYOND THE
C	          FILLED E-GRID.
C        IBESSL:  SWITCH FOR TYPE OF HORIZONTAL INTERPOLATION
C	          FROM FILLED E-GRID TO OUTPUT GRID.  
C	           -1 = 4-POINT LINEAR INTERPOLATION SCHEME
C 	                 WITH GRADIENT CORRECTION.  THIS OPTION
C	                 IS NOT AVAILABLE
C	            0 = 4-POINT LINEAR INTERPOLATION SCHEME
C	            1 = 16-POINT BESSEL INTERPOLATION SCHEME
C                       THIS OPTION IS NOT AVAILABLE.
C	 KSB   :  SQUARE ROOT OF THE NUMBER OF SUB-BOXES WHICH
C	          SUBROUTINE CETLIH USES.  SUBROUTINE CETLIH
C	          INTERPOLATES A H-POINT ETA GRID FIELD TO AN
C	          OUTPUT GRID CONSERVING THE AREA INTEGRAL OF
C	          THE E-GRID FIELD.  WHEN INTERPOLATING PRECIP
C                 AMOUNT TO A COARSER RESOLUTION GRID, KSB=3
C                 IS A GOOD CHOICE.  IF INTERPOLATING TO A
C                 FINER RESOLUTION GRID, KSB=8 IS A GOOD CHOICE.
C	 IOFFS :  PLUS/MINUS NUMBER OF POINTS ABOUT SOUTHWEST
C                 CORNER OUTPUT GRID POINT OVER WHICH CETLIH4.F
C	          SEARCHES THE CLOSEST OUTPUT GRID POINT TO
C	          EACH SUB-BOX.  WHEN INTERPOLATING TO A COARSER
C                 RESOLUTION GRID, IOFFS=2 OR 3 IS A GOOD CHOICE.
C                 IF INTERPOLATING TO A FINER RESOLUTION GRID,
C                 IOFFS=5 OR 6 IS A GOOD CHOICE.
C	 IFLAG :  CONTROLS OUTPUT FROM FILLV
C	            IFLAG=0 -> TAKE WHAT INTERPOLATION GIVES
C	            IFLAG=1 -> TRUNCATE OUTPUT AT ZERO
C	            IFLAG=2 -> TRUNCATE AT A VERY SMALL
C 	                       POSITIVE NUMBER (EPS)
C	 SATDEL:  FRACTIONAL VARIATION OF SATURATION.  SUBROUTINE
C                 NLYLI (N-LAYER LIFTED INDEX) LIFTS A MODIFIED 
C                 PARCEL.  PARCEL SPECIFIC HUMIDITY IS MODIFIED 
C	          ACCORDING TO THE FORMULA QMOD = (1+SATDEL)*QGIVEN.
C                 THE NGM USES SATDEL=0.05
C

      DATA SPVAL  / 999999./ 
      DATA IBESSL /   0    / 
      DATA KSB    /   3    /
      DATA IOFFS  /   2    /
      DATA IFLAG  /   0    / 
      DATA SATDEL / 0.05   /
C
      DATA RITEHD,RITE2 / .TRUE., .TRUE. /
C
      DATA STDOUT,LCNTRL /  6, 14 /
      DATA LUNCO, LUNLL  / 19, 29 /
      DATA LUNOUT        / 60     /
C
C
C     THIS FILE CONTAINS ALL THE UNIQUE FIELDS THE
C     ETA POST PROCESSOR CAN CURRENTLY GENERATE.  
C
C	IFILV IS SWITCH FOR FILLING MASS OR VELOCITY POINT E-GRID
C	   =0 DATA IS VELOCITY POINT AND MUST FILL MASS POINTS
C	   =1 DATA IS MASS POINT AND MUST FILL VELOCITY POINTS
C	AVBL IS CHARACTER STRING IDENTIFYING THE FIELD.
C	IQ  IS THE GRIB PDS OCTET 9 - PARAMETER (TABLE 2)
C	IS  IS THE GRIB PDS OCTET 10 - LEVEL TYPE (TABLE 3 & 3a)
C
C     WANT MORE/DIFFERENT FIELDS? 
C	(1) ADD CODE TO CALCULATE FIELD(S) IN APPROPRIATE ROUTINE(S),
C       (2) ADD FIELD(S) TO THIS LIST WITH A UNIQUE ITAG TAG,
C       (3) EDIT INPUT (CONTROL) FILE ACCORDINGLY,
C       (3) INCREASE PARAMETER MXFLD IN INCLUDE FILE "PARMOUT".
C
C     CURRENT NUMBER OF FIELDS LISTED:  180
C
C0       1         2         3         4         5         6         7
C234567890123456789012345678901234567890123456789012345678901234567890
      DATA IFILV(001),AVBL(001),IQ(001),IS(001)
     &                      /1,'PRESS ON ETA SFCS   ',001,119/
      DATA IFILV(077),AVBL(077),IQ(077),IS(077)
     &                      /1,'HEIGHT ON ETA SFCS  ',007,119/
      DATA IFILV(002),AVBL(002),IQ(002),IS(002)
     &                      /1,'TEMP ON ETA SFCS    ',011,119/
      DATA IFILV(003),AVBL(003),IQ(003),IS(003)
     &                      /1,'POT TEMP ON ETA SFCS',013,119/
      DATA IFILV(004),AVBL(004),IQ(004),IS(004)
     &                      /1,'DWPT TEMP ON ETA SFC',017,119/
      DATA IFILV(005),AVBL(005),IQ(005),IS(005)
     &                      /1,'SPEC HUM ON ETA SFCS',051,119/
      DATA IFILV(006),AVBL(006),IQ(006),IS(006)
     &                      /1,'REL HUM ON ETA SFCS ',052,119/
      DATA IFILV(083),AVBL(083),IQ(083),IS(083)
     &                      /1,'MST CNVG ON ETA SFCS',135,119/
      DATA IFILV(007),AVBL(007),IQ(007),IS(007)
     &                      /0,'U WIND ON ETA SFCS  ',033,119/
      DATA IFILV(008),AVBL(008),IQ(008),IS(008)
     &                      /0,'V WIND ON ETA SFCS  ',034,119/
      DATA IFILV(009),AVBL(009),IQ(009),IS(009)
     &                      /1,'OMEGA ON ETA SFCS   ',039,119/
      DATA IFILV(010),AVBL(010),IQ(010),IS(010)
     &                      /1,'ABS VORT ON ETA SFCS',041,119/
      DATA IFILV(084),AVBL(084),IQ(084),IS(084)
     &                      /1,'STRMFUNC ON ETA SFCS',035,119/
      DATA IFILV(011),AVBL(011),IQ(011),IS(011)
     &                      /1,'TRBLNT KE ON ETA SFC',158,119/
      DATA IFILV(111),AVBL(111),IQ(111),IS(111)
     &                      /1,'RCHDSN NO ON ETA SFC',254,119/
      DATA IFILV(146),AVBL(146),IQ(146),IS(146)
     &                      /1,'MASTER LENGTH SCALE ',226,119/
      DATA IFILV(147),AVBL(147),IQ(147),IS(147)
     &                      /1,'ASYMPT MSTR LEN SCL ',227,001/
      DATA IFILV(012),AVBL(012),IQ(012),IS(012)
     &                      /1,'HEIGHT OF PRESS SFCS',007,100/
      DATA IFILV(013),AVBL(013),IQ(013),IS(013)
     &                      /1,'TEMP ON PRESS SFCS  ',011,100/
      DATA IFILV(014),AVBL(014),IQ(014),IS(014)
     &                      /1,'POT TEMP ON P SFCS  ',013,100/
      DATA IFILV(015),AVBL(015),IQ(015),IS(015)
     &                      /1,'DWPT TEMP ON P SFCS ',017,100/
      DATA IFILV(016),AVBL(016),IQ(016),IS(016)
     &                      /1,'SPEC HUM ON P SFCS  ',051,100/
      DATA IFILV(017),AVBL(017),IQ(017),IS(017)
     &                      /1,'REL HUMID ON P SFCS ',052,100/
      DATA IFILV(085),AVBL(085),IQ(085),IS(085)
     &                      /1,'MST CNVG ON P SFCS  ',135,100/
      DATA IFILV(018),AVBL(018),IQ(018),IS(018)
     &                      /0,'U WIND ON PRESS SFCS',033,100/
      DATA IFILV(019),AVBL(019),IQ(019),IS(019)
     &                      /0,'V WIND ON PRESS SFCS',034,100/
      DATA IFILV(020),AVBL(020),IQ(020),IS(020)
     &                      /1,'OMEGA ON PRESS SFCS ',039,100/
      DATA IFILV(021),AVBL(021),IQ(021),IS(021)
     &                      /1,'ABS VORT ON P SFCS  ',041,100/
      DATA IFILV(086),AVBL(086),IQ(086),IS(086)
     &                      /1,'STRMFUNC ON P SFCS  ',035,100/
      DATA IFILV(022),AVBL(022),IQ(022),IS(022)
     &                      /1,'TRBLNT KE ON P SFCS ',158,100/
      DATA IFILV(153),AVBL(153),IQ(153),IS(153)
     &                      /1,'CLOUD WATR ON P SFCS',153,100/
      DATA IFILV(166),AVBL(166),IQ(166),IS(166)
     &                      /1,'CLOUD ICE ON P SFCS ',178,100/
      DATA IFILV(023),AVBL(023),IQ(023),IS(023)
     &                      /1,'MESINGER MEAN SLP   ',130,102/
      DATA IFILV(105),AVBL(105),IQ(105),IS(105)
     &                      /1,'SHUELL MEAN SLP     ',002,102/
      DATA IFILV(138),AVBL(138),IQ(138),IS(138)
     &                      /1,'SHELTER PRESSURE    ',001,105/
      DATA IFILV(106),AVBL(106),IQ(106),IS(106)
     &                      /1,'SHELTER TEMPERATURE ',011,105/
      DATA IFILV(112),AVBL(112),IQ(112),IS(112)
     &                      /1,'SHELTER SPEC HUMID  ',051,105/
      DATA IFILV(113),AVBL(113),IQ(113),IS(113)
     &                      /1,'SHELTER DEWPOINT    ',017,105/
      DATA IFILV(114),AVBL(114),IQ(114),IS(114)
     &                      /1,'SHELTER REL HUMID   ',052,105/
      DATA IFILV(064),AVBL(064),IQ(064),IS(064)
     &                      /0,'U WIND AT ANEMOM HT ',033,105/
      DATA IFILV(065),AVBL(065),IQ(065),IS(065)
     &                      /0,'V WIND AT ANEMOM HT ',034,105/
      DATA IFILV(158),AVBL(158),IQ(158),IS(158)
     &                      /1,'POT TEMP AT 10 M    ',013,105/
      DATA IFILV(159),AVBL(159),IQ(159),IS(159)
     &                      /1,'SPEC HUM AT 10 M    ',051,105/
      DATA IFILV(024),AVBL(024),IQ(024),IS(024)
     &                      /1,'SURFACE PRESSURE    ',001,001/
      DATA IFILV(025),AVBL(025),IQ(025),IS(025)
     &                      /1,'SURFACE HEIGHT      ',007,001/
      DATA IFILV(027),AVBL(027),IQ(027),IS(027)
     &                      /1,'SURFACE POT TEMP    ',013,001/
      DATA IFILV(028),AVBL(028),IQ(028),IS(028)
     &                      /1,'SURFACE SPEC HUMID  ',051,001/
      DATA IFILV(029),AVBL(029),IQ(029),IS(029)
     &                      /1,'SURFACE DEWPOINT    ',017,001/
      DATA IFILV(076),AVBL(076),IQ(076),IS(076)
     &                      /1,'SURFACE REL HUMID   ',052,001/
      DATA IFILV(026),AVBL(026),IQ(026),IS(026)
     &                      /1,'SFC (SKIN) TEMPRATUR',011,001/
      DATA IFILV(115),AVBL(115),IQ(115),IS(115)
     &                      /1,'BOTTOM SOIL TEMP    ',085,111/
      DATA IFILV(116),AVBL(116),IQ(116),IS(116)
     &                      /1,'SOIL TEMPERATURE    ',085,112/
      DATA IFILV(117),AVBL(117),IQ(117),IS(117)
     &                      /1,'SOIL MOISTURE       ',144,112/
      DATA IFILV(036),AVBL(036),IQ(036),IS(036)
     &                      /1,'TOTAL SOIL MOISTURE ',086,112/
      DATA IFILV(118),AVBL(118),IQ(118),IS(118)
     &                      /1,'PLANT CANOPY SFC WTR',223,001/
      DATA IFILV(119),AVBL(119),IQ(119),IS(119)
     &                      /1,'SNOW WATER EQUIVALNT',065,001/
      DATA IFILV(120),AVBL(120),IQ(120),IS(120)
     &                      /1,'PERCENT SNOW COVER  ',238,001/
      DATA IFILV(169),AVBL(169),IQ(169),IS(169)
     &                      /1,'SFC EXCHANGE COEF   ',208,001/
      DATA IFILV(170),AVBL(170),IQ(170),IS(170)
     &                      /1,'GREEN VEG COVER     ',087,001/
      DATA IFILV(171),AVBL(171),IQ(171),IS(171)
     &                      /1,'SOIL MOISTURE AVAIL ',207,112/
      DATA IFILV(152),AVBL(152),IQ(152),IS(152)
     &                      /1,'INST GROUND HEAT FLX',155,001/
      DATA IFILV(030),AVBL(030),IQ(030),IS(030)
     &                      /1,'LIFTED INDEX--SURFCE',131,101/
      DATA IFILV(031),AVBL(031),IQ(031),IS(031)
     &                      /1,'LIFTED INDEX--BEST  ',132,116/
      DATA IFILV(075),AVBL(075),IQ(075),IS(075)
     &                      /1,'LIFTED INDEX--BNDLYR',024,116/
      DATA IFILV(032),AVBL(032),IQ(032),IS(032)
     &                      /1,'CNVCT AVBL POT ENRGY',157,001/
      DATA IFILV(107),AVBL(107),IQ(107),IS(107)
     &                      /1,'CNVCT INHIBITION    ',156,001/
      DATA IFILV(080),AVBL(080),IQ(080),IS(080)
     &                      /1,'PRECIPITABLE WATER  ',054,200/
      DATA IFILV(162),AVBL(162),IQ(162),IS(162)
     &                      /0,'STORM REL HELICITY  ',190,106/
      DATA IFILV(163),AVBL(163),IQ(163),IS(163)
     &                      /0,'U COMP STORM MOTION ',196,106/
      DATA IFILV(164),AVBL(164),IQ(164),IS(164)
     &                      /0,'V COMP STORM MOTION ',197,106/
      DATA IFILV(087),AVBL(087),IQ(087),IS(087)
     &                      /1,'ACM TOTAL PRECIP    ',061,001/
      DATA IFILV(033),AVBL(033),IQ(033),IS(033)
     &                      /1,'ACM CONVCTIVE PRECIP',063,001/
      DATA IFILV(034),AVBL(034),IQ(034),IS(034)
     &                      /1,'ACM GRD SCALE PRECIP',062,001/
      DATA IFILV(035),AVBL(035),IQ(035),IS(035)
     &                      /1,'ACM SNOWFALL        ',065,001/
      DATA IFILV(121),AVBL(121),IQ(121),IS(121)
     &                      /1,'ACM SNOW TOTAL/MELT ',099,001/
      DATA IFILV(122),AVBL(122),IQ(122),IS(122)
     &                      /1,'ACM STORM SFC RNOFF ',235,001/
      DATA IFILV(123),AVBL(123),IQ(123),IS(123)
     &                      /1,'ACM BSFL-GDWR RNOFF ',234,001/
      DATA IFILV(160),AVBL(160),IQ(160),IS(160)
     &                      /1,'INSTANT PRECIP TYPE ',140,001/
      DATA IFILV(167),AVBL(167),IQ(167),IS(167)
     &                      /1,'INSTANT PRECIP RATE ',059,001/
      DATA IFILV(172),AVBL(172),IQ(172),IS(172)
     &                      /1,'FROZEN FRAC CLD SCHM',194,001/
      DATA IFILV(124),AVBL(124),IQ(124),IS(124)
     &                      /1,'CLD WTR ON ETA SFCS ',153,119/
      DATA IFILV(125),AVBL(125),IQ(125),IS(125)
     &                      /1,'CLD ICE ON ETA SFCS ',178,119/
      DATA IFILV(145),AVBL(145),IQ(145),IS(145)
     &                      /1,'CLD FRAC ON ETA SFCS',071,119/
      DATA IFILV(037),AVBL(037),IQ(037),IS(037)
     &                      /1,'LOW CLOUD FRACTION  ',073,214/
      DATA IFILV(038),AVBL(038),IQ(038),IS(038)
     &                      /1,'MID CLOUD FRACTION  ',074,224/
      DATA IFILV(039),AVBL(039),IQ(039),IS(039)
     &                      /1,'HIGH CLOUD FRACTION ',075,234/
      DATA IFILV(161),AVBL(161),IQ(161),IS(161)
     &                      /1,'TOTAL CLD FRACTION  ',071,200/
      DATA IFILV(144),AVBL(144),IQ(144),IS(144)
     &                      /1,'AVG TOTAL CLD FRAC  ',071,200/
      DATA IFILV(139),AVBL(139),IQ(139),IS(139)
     &                      /1,'AVG STRAT CLD FRAC  ',213,200/
      DATA IFILV(143),AVBL(143),IQ(143),IS(143)
     &                      /1,'AVG CNVCT CLD FRAC  ',072,200/
      DATA IFILV(148),AVBL(148),IQ(148),IS(148)
     &                      /1,'CLOUD BOT PRESSURE  ',001,002/
      DATA IFILV(149),AVBL(149),IQ(149),IS(149)
     &                      /1,'CLOUD TOP PRESSURE  ',001,003/
      DATA IFILV(109),AVBL(109),IQ(109),IS(109)
     &                      /1,'LCL AGL HEIGHT      ',007,005/
      DATA IFILV(110),AVBL(110),IQ(110),IS(110)
     &                      /1,'LCL PRESSURE        ',001,005/
      DATA IFILV(078),AVBL(078),IQ(078),IS(078)
     &                      /1,'AVE GRDSCL RN TMPTDY',241,119/
      DATA IFILV(079),AVBL(079),IQ(079),IS(079)
     &                      /1,'AVE CNVCT RN TMPTDY ',242,119/
      DATA IFILV(168),AVBL(168),IQ(168),IS(168)
     &                      /1,'CLOUD TOP TEMPS     ',011,003/
      DATA IFILV(140),AVBL(140),IQ(140),IS(140)
     &                      /1,'RADFLX CNVG TMP TNDY',216,119/
      DATA IFILV(040),AVBL(040),IQ(040),IS(040)
     &                      /1,'SW RAD TEMP TNDY    ',250,119/
      DATA IFILV(041),AVBL(041),IQ(041),IS(041)
     &                      /1,'LW RAD TEMP TNDY    ',251,119/
      DATA IFILV(141),AVBL(141),IQ(141),IS(141)
     &                      /1,'INSTN OUT SFC SW RAD',211,001/
      DATA IFILV(142),AVBL(142),IQ(142),IS(142)
     &                      /1,'INSTN OUT SFC LW RAD',212,001/
      DATA IFILV(126),AVBL(126),IQ(126),IS(126)
     &                      /1,'AVE INCMG SFC SW RAD',204,001/
      DATA IFILV(127),AVBL(127),IQ(127),IS(127)
     &                      /1,'AVE INCMG SFC LW RAD',205,001/
      DATA IFILV(128),AVBL(128),IQ(128),IS(128)
     &                      /1,'AVE OUTGO SFC SW RAD',211,001/
      DATA IFILV(129),AVBL(129),IQ(129),IS(129)
     &                      /1,'AVE OUTGO SFC LW RAD',212,001/
      DATA IFILV(130),AVBL(130),IQ(130),IS(130)
     &                      /1,'AVE OUTGO TOA SW RAD',211,008/
      DATA IFILV(131),AVBL(131),IQ(131),IS(131)
     &                      /1,'AVE OUTGO TOA LW RAD',212,008/
      DATA IFILV(156),AVBL(156),IQ(156),IS(156)
     &                      /1,'INSTN INC SFC SW RAD',204,001/
      DATA IFILV(157),AVBL(157),IQ(157),IS(157)
     &                      /1,'INSTN INC SFC LW RAD',205,001/
      DATA IFILV(044),AVBL(044),IQ(044),IS(044)
     &                      /1,'ROUGHNESS LENGTH    ',083,001/
      DATA IFILV(045),AVBL(045),IQ(045),IS(045)
     &                      /1,'FRICTION VELOCITY   ',253,001/
      DATA IFILV(132),AVBL(132),IQ(132),IS(132)
     &                      /1,'SFC DRAG COEFFICIENT',252,001/
      DATA IFILV(133),AVBL(133),IQ(133),IS(133)
     &                      /0,'SFC U WIND STRESS   ',124,001/
      DATA IFILV(134),AVBL(134),IQ(134),IS(134)
     &                      /0,'SFC V WIND STRESS   ',125,001/
      DATA IFILV(043),AVBL(043),IQ(043),IS(043)
     &                      /1,'AVE SFC SENHEAT FX  ',122,001/
      DATA IFILV(135),AVBL(135),IQ(135),IS(135)
     &                      /1,'AVE GROUND HEAT FX  ',155,001/
      DATA IFILV(136),AVBL(136),IQ(136),IS(136)
     &                      /1,'AVE SNO PHSCNG HT FX',229,001/
      DATA IFILV(042),AVBL(042),IQ(042),IS(042)
     &                      /1,'AVE SFC LATHEAT FX  ',121,001/
      DATA IFILV(046),AVBL(046),IQ(046),IS(046)
     &                      /1,'AVE SFC MOMENTUM FX ',172,001/
      DATA IFILV(047),AVBL(047),IQ(047),IS(047)
     &                      /1,'ACC SFC EVAPORATION ',057,001/
      DATA IFILV(137),AVBL(137),IQ(137),IS(137)
     &                      /1,'ACC POT EVAPORATION ',228,001/
      DATA IFILV(154),AVBL(154),IQ(154),IS(154)
     &                      /1,'INST SFC SENHEAT FX ',122,001/
      DATA IFILV(155),AVBL(155),IQ(155),IS(155)
     &                      /1,'INST SFC LATHEAT FX ',121,001/
      DATA IFILV(048),AVBL(048),IQ(048),IS(048)
     &                      /1,'LATITUDE            ',176,001/
      DATA IFILV(049),AVBL(049),IQ(049),IS(049)
     &                      /1,'LONGITUDE           ',177,001/
      DATA IFILV(050),AVBL(050),IQ(050),IS(050)
     &                      /1,'LAND/SEA MASK       ',081,001/
      DATA IFILV(051),AVBL(051),IQ(051),IS(051)
     &                      /1,'SEA ICE MASK        ',091,001/
      DATA IFILV(052),AVBL(052),IQ(052),IS(052)
     &                      /1,'MASS POINT ETA SFC  ',173,001/
      DATA IFILV(053),AVBL(053),IQ(053),IS(053)
     &                      /1,'VEL POINT ETA SFC   ',174,001/
      DATA IFILV(150),AVBL(150),IQ(150),IS(150)
     &                      /1,'SFC MIDDAY ALBEDO   ',084,001/
      DATA IFILV(151),AVBL(151),IQ(151),IS(151)
     &                      /1,'SEA SFC TEMPERATURE ',080,001/
      DATA IFILV(054),AVBL(054),IQ(054),IS(054)
     &                      /1,'PRESS AT TROPOPAUSE ',001,007/
      DATA IFILV(055),AVBL(055),IQ(055),IS(055)
     &                      /1,'TEMP AT TROPOPAUSE  ',011,007/
      DATA IFILV(108),AVBL(108),IQ(108),IS(108)
     &                      /1,'POTENTL TEMP AT TROP',013,007/
      DATA IFILV(056),AVBL(056),IQ(056),IS(056)
     &                      /0,'U WIND AT TROPOPAUSE',033,007/
      DATA IFILV(057),AVBL(057),IQ(057),IS(057)
     &                      /0,'V WIND AT TROPOPAUSE',034,007/
      DATA IFILV(058),AVBL(058),IQ(058),IS(058)
     &                      /1,'SHEAR AT TROPOPAUSE ',136,007/
      DATA IFILV(059),AVBL(059),IQ(059),IS(059)
     &                      /1,'TEMP AT FD HEIGHTS  ',011,103/
      DATA IFILV(060),AVBL(060),IQ(060),IS(060)
     &                      /0,'U WIND AT FD HEIGHTS',033,103/
      DATA IFILV(061),AVBL(061),IQ(061),IS(061)
     &                      /0,'V WIND AT FD HEIGHTS',034,103/
      DATA IFILV(062),AVBL(062),IQ(062),IS(062)
     &                      /1,'HEIGHT OF FRZ LVL   ',007,004/
      DATA IFILV(063),AVBL(063),IQ(063),IS(063)
     &                      /1,'REL HUMID AT FRZ LVL',052,004/
      DATA IFILV(165),AVBL(165),IQ(165),IS(165)
     &                      /1,'HIGHEST FREEZE LVL  ',007,204/
      DATA IFILV(067),AVBL(067),IQ(067),IS(067)
     &                      /1,'PRESS IN BNDRY LYR  ',001,116/
      DATA IFILV(068),AVBL(068),IQ(068),IS(068)
     &                      /1,'TEMP IN BNDRY LYR   ',011,116/
      DATA IFILV(069),AVBL(069),IQ(069),IS(069)
     &                      /1,'POT TMP IN BNDRY LYR',013,116/
      DATA IFILV(070),AVBL(070),IQ(070),IS(070)
     &                      /1,'DWPT IN BNDRY LYR   ',017,116/
      DATA IFILV(071),AVBL(071),IQ(071),IS(071)
     &                      /1,'SPC HUM IN BNDRY LYR',051,116/
      DATA IFILV(072),AVBL(072),IQ(072),IS(072)
     &                      /1,'REL HUM IN BNDRY LYR',052,116/
      DATA IFILV(088),AVBL(088),IQ(088),IS(088)
     &                      /1,'MST CNV IN BNDRY LYR',135,116/
      DATA IFILV(089),AVBL(089),IQ(089),IS(089)
     &                      /1,'P WATER IN BNDRY LYR',054,116/
      DATA IFILV(073),AVBL(073),IQ(073),IS(073)
     &                      /0,'U WIND IN BNDRY LYR ',033,116/
      DATA IFILV(074),AVBL(074),IQ(074),IS(074)
     &                      /0,'V WIND IN BNDRY LYR ',034,116/
      DATA IFILV(090),AVBL(090),IQ(090),IS(090)
     &                      /1,'OMEGA IN BNDRY LYR  ',039,116/
      DATA IFILV(066),AVBL(066),IQ(066),IS(066)
     &                      /1,'LFM 0.33-1.00 RELHUM',052,108/
      DATA IFILV(081),AVBL(081),IQ(081),IS(081)
     &                      /1,'LFM 0.66-1.00 RELHUM',052,108/
      DATA IFILV(082),AVBL(082),IQ(082),IS(082)
     &                      /1,'LFM 0.33-0.66 RELHUM',052,108/
      DATA IFILV(104),AVBL(104),IQ(104),IS(104)
     &                      /1,'LFM 0.33-1.00 PWAT  ',054,108/
      DATA IFILV(091),AVBL(091),IQ(091),IS(091)
     &                      /1,'NGM 0.98230 PRESSURE',001,107/
      DATA IFILV(092),AVBL(092),IQ(092),IS(092)
     &                      /1,'NGM 0.98230 TMPRATUR',011,107/
      DATA IFILV(093),AVBL(093),IQ(093),IS(093)
     &                      /1,'NGM 0.98230 SPC HUM ',051,107/
      DATA IFILV(094),AVBL(094),IQ(094),IS(094)
     &                      /1,'NGM 0.98230 REL HUM ',052,107/
      DATA IFILV(095),AVBL(095),IQ(095),IS(095)
     &                      /0,'NGM 0.98230 U WIND  ',033,107/
      DATA IFILV(096),AVBL(096),IQ(096),IS(096)
     &                      /0,'NGM 0.98230 V WIND  ',034,107/
      DATA IFILV(097),AVBL(097),IQ(097),IS(097)
     &                      /1,'NGM 0.89671 TMPRATUR',011,107/
      DATA IFILV(098),AVBL(098),IQ(098),IS(098)
     &                      /1,'NGM 0.78483 TMPRATUR',011,107/
      DATA IFILV(099),AVBL(099),IQ(099),IS(099)
     &                      /1,'NGM 0.47-1.00 RELHUM',052,108/
      DATA IFILV(100),AVBL(100),IQ(100),IS(100)
     &                      /1,'NGM 0.47-0.96 RELHUM',052,108/
      DATA IFILV(101),AVBL(101),IQ(101),IS(101)
     &                      /1,'NGM 0.18-0.47 RELHUM',052,108/
      DATA IFILV(102),AVBL(102),IQ(102),IS(102)
     &                      /1,'NGM 0.84-0.98 RELHUM',052,108/
      DATA IFILV(103),AVBL(103),IQ(103),IS(103)
     &                      /1,'NGM 0.85-1.00 QCONVG',135,108/
      DATA IFILV(173),AVBL(173),IQ(173),IS(173)
     &                      /1,'MAX WIND PRESS LEVEL',001,006/
      DATA IFILV(174),AVBL(174),IQ(174),IS(174)
     &                      /1,'MAX WIND HGHT LEVEL ',007,006/
      DATA IFILV(175),AVBL(175),IQ(175),IS(175)
     &                      /0,'U COMP MAX WIND     ',033,006/
      DATA IFILV(176),AVBL(176),IQ(176),IS(176)
     &                      /0,'V COMP MAX WIND     ',034,006/
      DATA IFILV(177),AVBL(177),IQ(177),IS(177)
     &                      /1,'HEIGHT AT TROPOPAUSE',007,007/
      DATA IFILV(178),AVBL(178),IQ(178),IS(178)
     &                      /1,'CLOUD BOTTOM HEIGHT ',007,002/
      DATA IFILV(179),AVBL(179),IQ(179),IS(179)
     &                      /1,'CLOUD TOP HEIGHT    ',007,003/
      DATA IFILV(180),AVBL(180),IQ(180),IS(180)
     &                      /1,'VISIBILITY          ',020,001/
C
      DATA IFILV(185),AVBL(185),IQ(185),IS(185)
     &                      /1,'TEMP AT 10 M        ',011,105/
      DATA IFILV(186),AVBL(186),IQ(186),IS(186)
     &                      /1,'PRESS AT 10 M       ',001,105/
      DATA IFILV(189),AVBL(189),IQ(189),IS(189)
     &                      /1,'TEMP AT 30 M        ',011,105/
      DATA IFILV(190),AVBL(190),IQ(190),IS(190)
     &                      /1,'PRESS AT 30 M       ',001,105/
      DATA IFILV(191),AVBL(191),IQ(191),IS(191)
     &                      /1,'POT TEMP AT 30 M    ',013,105/
      DATA IFILV(192),AVBL(192),IQ(192),IS(192)
     &                      /1,'SPEC HUM AT 30 M    ',051,105/
      DATA IFILV(193),AVBL(193),IQ(193),IS(193)
     &                      /0,'U WIND AT 30 M      ',033,105/
      DATA IFILV(194),AVBL(194),IQ(194),IS(194)
     &                      /0,'V WIND AT 30 M      ',034,105/
C
      DATA IFILV(200),AVBL(200),IQ(200),IS(200)
     &                      /1,'DQADV               ',209,200/
      DATA IFILV(201),AVBL(201),IQ(201),IS(201)
     &                      /1,'WVCONV              ',237,200/
      DATA IFILV(202),AVBL(202),IQ(202),IS(202)
     &                      /1,'WCCONV              ',241,200/
      DATA IFILV(203),AVBL(203),IQ(203),IS(203)
     &                      /0,'WVUFLX              ',242,200/
      DATA IFILV(204),AVBL(204),IQ(204),IS(204)
     &                      /0,'WVVFLX              ',243,200/
      DATA IFILV(205),AVBL(205),IQ(205),IS(205)
     &                      /0,'WCUFLX              ',244,200/
      DATA IFILV(206),AVBL(206),IQ(206),IS(206)
     &                      /0,'WCVFLX              ',245,200/
      DATA IFILV(207),AVBL(207),IQ(207),IS(207)
     &                      /1,'WVINC               ',232,200/
      DATA IFILV(208),AVBL(208),IQ(208),IS(208)
     &                      /1,'WCINC               ',233,200/
! H CHUANG--ADD FIXED AND LSM FIELDS
      DATA IFILV(218),AVBL(218),IQ(218),IS(218)
     &                      /1,'VEGETATION TYPE     ',225,001/
      DATA IFILV(219),AVBL(219),IQ(219),IS(219)
     &                      /1,'SOIL TYPE           ',224,001/
      DATA IFILV(220),AVBL(220),IQ(220),IS(220)
     &                      /1,'CANOPY CONDUCTANCE  ',181,001/
      DATA IFILV(221),AVBL(221),IQ(221),IS(221)
     &                      /1,'PBL HEIGHT          ',221,001/
      DATA IFILV(223),AVBL(223),IQ(223),IS(223)
     &                      /1,'SLOPE TYPE          ',222,001/
      DATA IFILV(224),AVBL(224),IQ(224),IS(224)
     &                      /1,'SNOW DEPTH          ',066,001/
      DATA IFILV(225),AVBL(225),IQ(225),IS(225)
     &                      /1,'LIQUID SOIL MOISTURE',160,112/
      DATA IFILV(226),AVBL(226),IQ(226),IS(226)
     &                      /1,'SNOW FREE ALBEDO    ',170,001/
      DATA IFILV(227),AVBL(227),IQ(227),IS(227)
     &                      /1,'MAXIMUM SNOW ALBEDO ',159,001/
      DATA IFILV(228),AVBL(228),IQ(228),IS(228)
     &                      /1,'CANOPY WATER EVAP   ',200,001/
      DATA IFILV(229),AVBL(229),IQ(229),IS(229)
     &                      /1,'DIRECT SOIL EVAP    ',199,001/
      DATA IFILV(230),AVBL(230),IQ(230),IS(230)
     &                      /1,'PLANT TRANSPIRATION ',210,001/
      DATA IFILV(231),AVBL(231),IQ(231),IS(231)
     &                      /1,'SNOW SUBLIMATION    ',198,001/
      DATA IFILV(232),AVBL(232),IQ(232),IS(232)
     &                      /1,'AIR DRY SOIL MOIST  ',231,001/
      DATA IFILV(233),AVBL(233),IQ(233),IS(233)
     &                      /1,'SOIL MOIST POROSITY ',240,001/
      DATA IFILV(234),AVBL(234),IQ(234),IS(234)
     &                      /1,'MIN STOMATAL RESIST ',203,001/
      DATA IFILV(235),AVBL(235),IQ(235),IS(235)
     &                      /1,'NO OF ROOT LAYERS   ',171,001/
      DATA IFILV(236),AVBL(236),IQ(236),IS(236)
     &                      /1,'SOIL MOIST WILT PT  ',219,001/
      DATA IFILV(237),AVBL(237),IQ(237),IS(237)
     &                      /1,'SOIL MOIST REFERENCE',230,001/
      DATA IFILV(238),AVBL(238),IQ(238),IS(238)
     &                      /1,'CANOPY COND SOLAR   ',246,001/
      DATA IFILV(239),AVBL(239),IQ(239),IS(239)
     &                      /1,'CANOPY COND TEMP    ',247,001/
      DATA IFILV(240),AVBL(240),IQ(240),IS(240)
     &                      /1,'CANOPY COND HUMID   ',248,001/
      DATA IFILV(241),AVBL(241),IQ(241),IS(241)
     &                      /1,'CANOPY COND SOILM   ',249,001/
      DATA IFILV(242),AVBL(242),IQ(242),IS(242)
     &                      /1,'POTENTIAL EVAP      ',145,001/

! DUSAN JOVIC - TWO ADDITIONAL FIELDS FOR NARR
!               INTERPOLATED IN PRDGEN USING NEAREST NEIGHBOR OPTION
!
      DATA IFILV(245),AVBL(245),IQ(245),IS(245)
     &                      /1,'SURFACE HEIGHT NN   ',218,001/
      DATA IFILV(246),AVBL(246),IQ(246),IS(246)
     &                      /1,'ACM TOTAL PRECIP NN ',202,001/
      DATA IFILV(247),AVBL(247),IQ(247),IS(247)
     &                      /1,'SURFACE PRESSURE NN ',134,001/

      END
