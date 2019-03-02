void trimchar(char *rec1, char *rec2, const char trim)
{
 int ispace, i, j;
 char space;

  space = ' ';
  j = 0;
  ispace = 0;

  ispace     = strpos(rec1, space, j);
  if (ispace > strpos(rec1, trim, j) )
    {
/*    There is at least one null character to be deleted */
      j = -1;
      for (i = 0 ; i <= ispace; i++)
        {
          if (*(rec1+i) != trim)
            {
             j += 1;
             *(rec2+j) = *(rec1+i);
            }
         }
      }
    else
      {
        for (i = 0 ; i <= ispace ; i++)
          { *(rec2+i) = *(rec1+i);  }
      }
  return;
} 
 