#include <stdio.h>                /* Tell the compiler to get the standard
                                       io library */
main()                            /* This is a comment */
{                                 /* Comments may appear anywhere */
  int c;                          /* They must start with the slash star, */
  c = 10;                         /*  and must end with a star slash */
  printf(" Hello, c = %d \n", c); /* Note that a single comment, as */
                                   /* the first may cross multiple lines. */
              /* You must be sure, though, not to mix the executable in with
                   the comments, though, or you will get peculiar results */
}
