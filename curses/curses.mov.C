#include "move.h"
#include <curses.h>
#include <unistd.h>
  
int main(void) 
{
  mover alpha;
  point beta, delta;
  int c, i;

  initscr();
  savetty();
  noecho();
  nonl();
  cbreak();

  beta.x = 1.0; beta.y = 2.0; beta.z = 3.0;
  delta.x = 2.0; delta.y = 2.0; delta.z = 3.0; 

  alpha.move_abs(beta);
  alpha.vel_abs(delta);
  alpha.display();

  for (i = 0; i <= 10; i++) {
    alpha.move_vel(0.1*i);
    alpha.display();
    sleep(1);
  }

  c='x';
  while ( c !=  'q' ) {
    c=getch();
    switch (c) {
     case '4' :
       alpha.move_rel(left);
       break;
     case '6' : 
       alpha.move_rel(right);
       break;
     case '2' : 
       alpha.move_rel(down);
       break;
     case '8' :
       alpha.move_rel(up);
       break;
     default:
       break;
    }
    alpha.display();

    refresh();
  }

  resetty();
  endwin();
  return 0;

}
