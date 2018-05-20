(* Robert Grumbine
   Sort results of monster manual evaluation
   amusement 1985 *)
PROCEDURE alfasort;
VAR
  DUMMY : monster;
  jump  : INTEGER;
  i     : INTEGER;

BEGIN
  jump := length  ;
  WHILE jump>1 DO BEGIN
    jump := jump DIV 2;
    REPEAT
      done := TRUE  ;
      FOR i := 1 TO length-jump DO BEGIN
        j := i+jump  ;
        IF manual[j].name < manual[i].name THEN BEGIN
          dummy := manual[i] ;
          manual[i] := manual[j] ;
          manual[j] := dummy     ;
          done      := FALSE     ;
        END  (* If *)
      END    (* For *)
    UNTIL done
  END     (* While  *)
END;      (* Procedure *)

PROCEDURE numsort;
VAR
  dummy : monster;
  jump  : INTEGER;
  i     : INTEGER;

BEGIN
  jump := length;
  WHILE jump > 1 DO BEGIN
    jump := jump DIV 2 ;
    REPEAT
      done := TRUE ;
      FOR i := 1 TO length-jump DO BEGIN
        j := i+jump;
        IF manual[j].f[k] < manual[i].f[k] THEN BEGIN
          dummy    := manual[i];
          manual[i]:= manual[j];
          manual[j]:= dummy    ;
          done     := FALSE
        END   (* If *)
      END     (* For *)
    UNTIL done
  END         (* While *)
END;          (* Procedure *)

PROCEDURE TEXPHP;

VAR
  i     : INTEGER;

BEGIN
  FOR i := 1 TO length DO BEGIN
    WITH manual[i] DO BEGIN
      f[9 ] := ROUND(f[9]*10/hp(f[4]))/10;
      f[10] := f[9] + f[8]               ;
    END  (* with *)
  END    (* for  *)
END;     (* Procedure *)

(* Main Program *)
BEGIN
  READLN (length);  (* Read in the number of monsters *)
  readin         ;
  texphp         ;
  WRITELN('1')   ;
  alfasort       ;
  writeout       ;
  FOR k := 1 TO 12 DO BEGIN
    IF (k<>5) AND (k<>6) THEN BEGIN
      WRITELN('1');
      numsort     ;
      writeout
    END  (* If  *)
  END    (* For *)
END.     (* program *)
 
