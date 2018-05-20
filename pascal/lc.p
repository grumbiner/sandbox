(* Robert Grumbine
   Generate concordance from intput text
   Computational Linguistics 1983 *)
PROGRAM cord(input, output, A, DICT);
TYPE
  wordtype = PACKED ARRAY [1..20] OF CHAR;
  gword    =
    RECORD
      word    : CHAR;
      freq    : INTEGER
    END;
  
VAR
  diction    : ARRAY [1..600] of gword;
  ptrs       : ARRAY [1..600] of INTEGER;
  size       : INTEGER;             (* size of the dictionary *)
  A          : PACKED FILE OF CHAR; (* input test file         *)
  DICT       : text;
  tword      : CHAR;


PROCEDURE findword(VAR tword : CHAR );
VAR
  i, j    : INTEGER;
  temword : ARRAY [1..20] OF CHAR;
  x       : CHAR;

BEGIN
  IF NOT EOF(A) THEN BEGIN 
      READ (A, x) ; 
      tword := x;
  END;  (* if *)
(*    WRITELN(tword,' findword')   *)
END;      (* procedure *)


PROCEDURE addword(VAR tword : CHAR; VAR size : INTEGER );
LABEL 1, 2;
VAR
  i, nmin, nmax : INTEGER;
  loc, ntest    : INTEGER;
  new           : BOOLEAN;

BEGIN
(* WRITELN(' ',tword); *)
(* WRITELN(' Making first test'); *)
  IF (tword < diction[ptrs[1]].word) THEN BEGIN
    loc  := 1;
    new  := TRUE;
    nmax := 1;
    GOTO 2;
  END ;
(* WRITELN(' Making second test'); *)
  IF (tword > diction[ptrs[size]].word) THEN BEGIN
    diction[size+1].word := tword;
    diction[size+1].freq := 1;
    ptrs[size+1]         := size+1;
    size                 := size+1;
    GOTO 1; (*end of routine*)
  END ;
  nmax := size;
  nmin := 1;
  ntest := (nmax + nmin) DIV 2 ;
(* WRITELN(' entering repeat loop'); *)
  REPEAT
    IF tword > diction[ptrs[ntest]].word  THEN
      nmin := ntest 
     ELSE
      nmax := ntest; 
    ntest := (nmax+nmin) DIV 2;
  UNTIL ((tword = diction[ptrs[nmax]].word)
       OR (tword = diction[ptrs[ntest]].word)
       OR (tword = diction[ptrs[nmin]].word)
       OR (nmin > (nmax-2))                    );
(* WRITELN(' leaving repeat loop'); *)

  new := TRUE;
  IF tword = diction[ptrs[nmin]].word THEN BEGIN
    loc := nmin;
    new := FALSE;
    END (* else *)
   ELSE IF tword = diction[ptrs[ntest]].word  THEN BEGIN
    loc := ntest;
    new := FALSE; 
    END (* else *)
   ELSE IF tword = diction[ptrs[nmax]].word  THEN BEGIN
    loc := nmax;
    new := FALSE;
    END; (* else *)

 2 :
   IF (NOT NEW) THEN 
     diction[ptrs[loc]].freq := diction[ptrs[loc]].freq+1
    ELSE BEGIN
     loc := nmax;
     FOR i := size DOWNTO nmax DO BEGIN
       ptrs[i+1] := ptrs[i] ;
     END; (* FOR *)
     ptrs[nmax] := size+1;
     diction[ptrs[nmax]].word := tword;
     diction[ptrs[nmax]].freq := 1;
     size := size+1;
    END; (* ELSE *)
 1  :  
END;   (* procedure *)


PROCEDURE writeout;
VAR
  i : INTEGER;

BEGIN
  FOR i := 1 to size DO BEGIN
    WITH diction[ptrs[i]] DO BEGIN
      WRITELN (DICT,word,'   ', freq:4);  
(*    WRITELN (word,' ',  freq:4); *)
    END  (* With *)
  END    (* For  *)
END;     (* Procedure *)

BEGIN  (*This is the Main Program *)
  size := 1;
  RESET   (A) ;
  findword(tword);
  diction[1].word := tword;
  diction[1].freq := 1;
  ptrs[1]         := 1;
  REPEAT
    findword (tword);         (* Look for the Next Word *)
    addword  (tword, size)    (* Add it to the List   *)
(*    WRITELN('main ', tword, size)  *)
  UNTIL EOF(A);

(* alfasort;   *) 
  REWRITE (DICT);
  writeout
              
END.
