/* Find the location of a desired character, starting from the given location */
/* Bob Grumbine 6 June 1994 */
int strpos(char *rec1, const char b, const int i)
{
int k, j;

  j = i;
  k = 0;
  while (*(rec1+j) != b && j < 1600)
  {
      j += 1;
  }
  if (*(rec1+j) == b) { k = j;}
  return k;

}
