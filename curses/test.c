#include <stdio.h>
#include <curses.h>

int main(void) 
{
  int c;

  initscr();
  savetty();
  noecho();
  nonl();
  cbreak();

  mvaddch(10, 10, '*');
  refresh();
  addch('*');
  refresh();

  c = '*';

  while ( c != -1 ) {
    c=getch();  
    addch(c);
    refresh();
  }

  resetty();
  endwin();

  return 0;

}
