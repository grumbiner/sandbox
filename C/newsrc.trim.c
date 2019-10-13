#include <stdio.h>

const int MAXLEN=2000;

int main(int argc, char *argv[])
{
  FILE *in, *out;
  char newsinfo[70000];
  char outinfo[2000];
  int i;

  newsinfo[70000-1]='\0';

  in = fopen(argv[1], "r");
  out = fopen(argv[2], "w");

  while (!feof(in))
  {
    fgets(newsinfo, 70000, in);
    i = 0;
    while (newsinfo != '\0' && i < 2000) {
       outinfo[i] = newsinfo[i];
       i++;
    }
    outinfo[i-1]='\0';
    if (i == 2000) outinfo[i-1] = '\n';
    fprintf(out, "%s", outinfo);
  } 

  return 0;

}
