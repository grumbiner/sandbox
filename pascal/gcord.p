PROGRAM gcord(input, output, A, DICT);
TYPE
  wordtype = PACKED ARRAY [1..20] OF CHAR;
  deftype  = PACKED ARRAY [1..40] OF CHAR;
  gword    =
    RECORD
      word    : wordtype;
      def     : deftype;
      frequen : INTEGER
    END;

VAR
  diction    : ARRAY [1..1000] of gword;
  size       : INTEGER;             (* size of the dictionary *)
  A          : PACKED FILE OF CHAR; (* input test file         *)
  DICT       : text;
  temgword   : wordtype;


PROCEDURE findword(VAR temgword : wordtype );
VAR
  i, j    : INTEGER;
  temword : ARRAY [1..20] OF CHAR;
  x       : CHAR;

BEGIN
  IF NOT EOF(A) THEN (* read until finding a non-terminal character *)
    REPEAT
      READ (A, x) ; 
    UNTIL EOF(A) OR ( ((ORD(x)<=ORD('Z')) AND (ORD(x)>=ORD('A'))) OR 
                      ((ORD(x)<=ORD('z')) AND (ORD(x)>=ORD('a'))) );
    IF NOT EOF(A) THEN BEGIN
      temword[1] := x;
      i          := 1;
      REPEAT
        i := i+1;
        READ (A, x);          (* read from the input file *)
        IF ( ((ORD(x) <= ORD('Z')) AND (ORD(x) >= ORD('A'))) OR
             ((ORD(x) <= ORD('z')) AND (ORD(x) >= ORD('a'))) ) 
           AND NOT EOF(A) THEN
          temword[i] := x
         ELSE FOR j := i TO 20 DO   (* fill in with blanks *)
          temword[j] := ' ';
      UNTIL ( (i = 20) OR (j = 20) )
    END;  (* if *)
    PACK (temword, 1, temgword)
(*    WRITELN(temgword,' findword')   *)
END;      (* procedure *)


PROCEDURE addword(VAR temgword : wordtype; VAR size : INTEGER );
VAR
  i,j  : INTEGER;
  same : BOOLEAN;

BEGIN
  (* see if this is a new word *)
  i       := 0   ;
  REPEAT
    i  := i+1;
    
    j    := 1;
    same := TRUE;
    REPEAT 
      IF (temgword[j] = diction[i].word[j] ) THEN
        j := j + 1 
       ELSE
        same := FALSE
    UNTIL ( (j = 20) OR (NOT same) ); 

    IF ( same ) THEN BEGIN
      diction[i].frequen := diction[i].frequen + 1
    END  (* if *)
  UNTIL ( (i >= size) OR ( same ) );

  IF (NOT same) THEN BEGIN
    size := size+1;
    FOR j := 1 TO 20 DO
      diction[size].word[j] := temgword[j];
    diction[size].frequen   := 1
  END  (* if *)
(*  WRITELN (temgword, diction[size].word, size, ' addword')  *)

END;   (* procedure *)

PROCEDURE alfasort;
VAR
  dummy  : gword;
  jump   : INTEGER;
  i, j   : INTEGER;
  done   : BOOLEAN;

BEGIN
  jump := size;
  WHILE jump > 1 DO BEGIN
    jump := jump DIV 2;
    REPEAT
      done := TRUE;
      FOR i := 1 to size-jump DO BEGIN
        j := i + jump;
        IF diction[j].word < diction[i].word THEN BEGIN
          dummy      := diction[i] ;
          diction[i] := diction[j];
          diction[j] := dummy     ;
          done       := FALSE
        END   (* IF *)
      END     (* FOR *)
    UNTIL done
  END         (* While *)
END;          (* Procedure *)


PROCEDURE writeout;
VAR
  i : INTEGER;

BEGIN
  FOR i := 1 to size DO BEGIN
    WITH diction[i] DO BEGIN
      WRITELN (DICT,word,' ', def,' ', frequen:4);  
      WRITELN (word,' ', def,' ', frequen:4);
    END  (* With *)
  END    (* For  *)
END;     (* Procedure *)

BEGIN  (*This is the Main Program *)
  size := 1;
  RESET   (A) ;
  REWRITE (DICT);
  REPEAT
    findword (temgword);         (* Look for the Next Word *)
    addword  (temgword, size)    (* Add it to the List   *)
(*    WRITELN('main ', temgword, size)  *)
  UNTIL EOF(A);

  alfasort;    
  writeout
              
END.
