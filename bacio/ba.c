/* ba.f -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__9 = 9;
static integer c__3 = 3;

/* Main program */ MAIN__()
{
    /* Format strings */
    static char fmt_9001[] = "(a80)";

    /* Builtin functions */
    integer s_wsfi(), do_fio(), e_wsfi(), s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer fdes, mode, jret, size, i__;
    extern integer bacio_();
    static char fname[80];
    static integer start, no;
    static char dataary[1*3072];
    static integer nactual;

    /* Fortran I/O blocks */
    static icilist io___3 = { 0, fname, 0, fmt_9001, 80, 1 };
    static cilist io___11 = { 0, 6, 0, 0, 0 };
    static icilist io___13 = { 0, fname, 0, fmt_9001, 80, 1 };


    mode = (float)28.;
    s_wsfi(&io___3);
    do_fio(&c__1, "file1", 5L);
    e_wsfi();
    start = 0;
    size = 1;
    no = 3072;
    jret = bacio_(&mode, &start, &size, &no, &nactual, &fdes, fname, dataary, 
	    80L, 1L);
    s_wsle(&io___11);
    do_lio(&c__9, &c__1, "jret, nactual = ", 16L);
    do_lio(&c__3, &c__1, (char *)&jret, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&nactual, (ftnlen)sizeof(integer));
    e_wsle();
    if (jret >= 0 && nactual >= 0) {
	for (i__ = 1; i__ <= 3072; ++i__) {
/* D          PRINT *,i,ICHAR(dataary(i)) */
/* L1000: */
	}
    }
    s_wsfi(&io___13);
    do_fio(&c__1, "file1.out", 9L);
    e_wsfi();
    start = 0;
    size = 1;
    mode = 42;
    jret = bacio_(&mode, &start, &size, &no, &nactual, &fdes, fname, dataary, 
	    80L, 1L);
} /* MAIN__ */

/* Main program alias */ int ba_ () { MAIN__ (); }
