#include <stdio.h>
#include <curses.h>

/* Declare a class for points and operations on them */

class point {
  public :
    float x, y, z;
    point operator*(float);
    point operator+(point);
};

point left = {-1., 0., 0.};
point right= {+1., 0., 0.};
point up   = {0., -1., 0.};
point down = {0., +1., 0.};

point point::operator*(float mul)
{
  point tmp;
  tmp.x = x*mul;
  tmp.y = y*mul;
  tmp.z = z*mul;
  return tmp; 
}
point point::operator+(point dx)
{
  point tmp;
  tmp.x = x+dx.x;
  tmp.y = y+dx.y;
  tmp.z = z+dx.z;
  return tmp;
}

/* Declare a class for things that move.  This will include members of
   class point */
class mover {
  public :
    void move_abs(point);
    void move_rel(point);
    void vel_abs(point);
    void vel_rel(point);
    void move_vel(float);
    void display();
  private :
    point posit;
    point velocity;
};

void mover::move_vel(float dt)
{
  point dx;
  dx = velocity*dt;
  move_rel(dx);
  return;
}

void mover::vel_abs(point v)
{
  velocity.x = v.x;
  velocity.y = v.y;
  velocity.z = v.z;
  return;
}
void mover::vel_rel(point dv)
{
  velocity = velocity + dv;
  return;
} 

void mover::move_abs(point x)
{
  posit.x = x.x;
  posit.y = x.y;
  posit.z = x.z;
  return;
}
void mover::move_rel(point dx)
{
  posit = posit + dx; 
  return;
}

void mover::display(void)
{
  int i, j;

  j = (int)posit.y+0.5;
  i = (int)posit.x+0.5;

  if ( j > LINES ) {
    j = j % LINES;
  }
  if (i > COLS ) {
    i = i % COLS;
  }

  move( j, i );
  clear();
  addch('*');
  refresh();
  return;
}
