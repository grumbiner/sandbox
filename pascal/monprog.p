(* Robert Grumbine
   Evaluate monsters from D+D manual
   amusement 1985 *)
PROGRAM monval (INPUT, OUTPUT);

TYPE
  monster =
    RECORD
      name    : PACKED ARRAY[1..20] OF CHAR;
      book    : PACKED ARRAY[1..2 ] OF CHAR;
      ac      : INTEGER                    ;
      avd     : REAL                       ;
      hit     : REAL                       ;
      hd      : REAL                       ;
      special : REAL                       ;
      normxp  : REAL                       ;
      normav  : REAL                       ;
      perhp   : REAL                       ;
      spec    : REAL                       ;
      avg     : REAL
    END;  (* record *)

VAR
  tempchar  : ARRAY [1..20] OF CHAR; (* Array for temporary storage of
                                          name , or notes.            *)
  a, b, c, d: REAL                 ; (* Generic reals                 *)
  i, j, k   : INTEGER              ; (* Generic integers              *)
  x         : CHAR                 ; (* Character to read letters into
                                          for data set-up.            *)
  done      : BOOLEAN              ; (* Generic logical variable      *)
  bn        : ARRAY[1..2] OF CHAR  ; (* Array for temp. storage of book
                                          name.                       *)
  length    : INTEGER              ; (* Length of monster list        *)
  temmonst  : monster              ; (* Record for temporary assignment
                                          of monsters, i.e. during sorts*)
  manual    : ARRAY[1..10] OF monster; (* Array of monsters           *)

PROCEDURE readin(VAR k : INTEGER);
VAR
  i, j  : INTEGER;

BEGIN
  i    := 0     ;
  k    := 0     ;
  done := FALSE ;  (* Initialize the counters. *)
  WHILE NOT done DO BEGIN  (* Find the name of the monster. *)
    k := k+1    ;
    REPEAT
      READ (x);
      i := i+1;
      IF x=':'
       THEN BEGIN
        FOR j := i to 20 DO
          tempchar[j] := ' '  (* end for *)
        END  (* then *)
       ELSE tempchar[i] := x
    UNTIL (j=20) OR (i=20)  ;
    PACK(tempchar,1,manual[k].name);  (* Put the name into the manual. *)
    WRITELN (' ',manual[k].name)   ;

    READ (bn[1], bn[2])         ;
    PACK (bn, 1, manual[k].book);

    WITH manual[k] DO
      READLN (ac, avd, hit, hd, special, normxp);

    IF manual[k].name = 'xxxxxxxxxxxxxxxxxxxx'
      THEN done := TRUE ;

    i := 0;
    j := 0;

  END (* While *)
END;  (* Procedure *)

PROCEDURE basic1;
BEGIN
  WITH manual[i] do
    a := 10*(21-hit)*avd/(9*(ac+5))
END;

PROCEDURE basic2;
BEGIN
  WITH manual[i] do
    b := 324*(21-hit)*avd/(117*(ac+8))
END;


FUNCTION hp(hd : REAL) : REAL;
BEGIN
  hp := 4.5*TRUNC(hd) + 10*(hd-TRUNC(hd))
END;



PROCEDURE spattack;

VAR
  vals : ARRAY [ 0..16, 1..2] OF REAL;
  l    : INTEGER                     ;
  m    : INTEGER                     ;
  a    : REAL                        ;

BEGIN
  vals[ 0,1] :=    2   ;   vals[ 0,2] :=   25 ;
  vals[ 1,1] :=    4   ;   vals[ 1,2] :=   35 ;
  vals[ 2,1] :=    8   ;   vals[ 2,2] :=   45 ;
  vals[ 3,1] :=   15   ;   vals[ 3,2] :=   55 ;
  vals[ 4,1] :=   25   ;   vals[ 4,2] :=   65 ;
  vals[ 5,1] :=   40   ;   vals[ 5,2] :=   75 ;
  vals[ 6,1] :=   75   ;   vals[ 6,2] :=  125 ;
  vals[ 7,1] :=  125   ;   vals[ 7,2] :=  175 ;
  vals[ 8,1] :=  175   ;   vals[ 8,2] :=  275 ;
  vals[ 9,1] :=  300   ;   vals[ 9,2] :=  400 ;
  vals[10,1] :=  450   ;   vals[10,2] :=  600 ;
  vals[11,1] :=  700   ;   vals[11,2] :=  850 ;
  vals[12,1] :=  950   ;   vals[12,2] := 1200 ;
  vals[13,1] := 1250   ;   vals[13,2] := 1600 ;
  vals[14,1] := 1550   ;   vals[14,2] := 2000 ;
  vals[15,1] := 2100   ;   vals[15,2] := 2500 ;
  vals[16,1] := 2600   ;   vals[16,2] := 3000 ;
 
  FOR l := 1 TO length DO BEGIN
    a := manual[l].hd  ;
    IF a < 0.4 THEN m:=0
      ELSE IF a<= 1 THEN m:= 1
      ELSE IF a<= 2 THEN m:= 2
      ELSE IF a<= 3 THEN m:= 3
      ELSE IF a<= 4 THEN m:= 4
      ELSE IF a<= 5 THEN m:= 5
      ELSE IF a<= 6 THEN m:= 6
      ELSE IF a<= 7 THEN m:= 7
      ELSE IF a<= 8 THEN m:= 8
      ELSE IF a<= 9 THEN m:= 9
      ELSE IF a< 11 THEN m:=10
      ELSE IF a< 13 THEN m:=11
      ELSE IF a< 15 THEN m:=12
      ELSE IF a< 17 THEN m:=13
      ELSE IF a< 19 THEN m:=14
      ELSE IF a< 21 THEN m:=15
        ELSE                 m:=16;

    WITH manual[l] DO
      spec := TRUNC(special)*vals[m,1]+
               10*(special-TRUNC(special))*vals[m,2]

  END   (* For       *)
END;    (* Procedure *)



PROCEDURE writeout;
VAR
  l   : INTEGER ;

BEGIN
  FOR l := 1 to length DO BEGIN
    WITH manual[l] DO
      WRITELN (' ',name, book, ac:4, avd:5:1, hit:3, hd:5:2,
                   special:8:3, normxp:8:2, normav:8:2, perhp:5:1,
                   spec:5, avg:8:2, ABS(normav-avg)/normav:6:4   )
  END (* For *)
END;  (* Procedure *)

(* Main Program *)
BEGIN
  readin(k) ;
  length:=k ;
  FOR i := 1 TO length DO BEGIN
    basic1  ;
    IF (a*hp(manual[i].hd)) > 300.0 THEN BEGIN
      basic2;
      manual[i].perhp := b
     END  (* Then *)
     ELSE
      manual[i].perhp := a
  END; (* For *)
  spattack ;
  FOR i := 1 to length DO BEGIN
    WITH manual[i] DO BEGIN
      a     := hp(hd)         ;
      avg   := a*perhp + spec ;
      normav:= TRUNC(normxp)+(normxp-TRUNC(normxp))*a*10
    END  (* With *)
  END;   (* For  *)
  writeout
END.     (* Program *)
