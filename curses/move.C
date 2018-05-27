#include <stdio.h>
#include <curses.h>

/* Declare a class for points and operations on them */
class point {
  public :
    float x, y, z;
    point operator*(float);
    point operator+(point);
};
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
  printf("%f %f %f  %f %f %f\n", posit.x, posit.y, posit.z, velocity.x, velocity.y, velocity.z);
  return;
}
  
int main(void) 
{
  mover alpha;
  point beta, delta;
  int c;

  beta.x = 1.0;
  beta.y = 2.0;
  beta.z = 3.0;

  delta.x = 2.0;
  delta.y = 2.0;
  delta.z = 3.0; 

  alpha.move_abs(beta);
  alpha.vel_abs(delta);
  alpha.display();

  alpha.move_rel(beta);
  alpha.display();

  alpha.vel_rel(delta);
  alpha.display();

  alpha.move_vel(0.2);
  alpha.display();


  while ( c != (int) 'q' ) {
    c=getch();
    putc(c, stdout);
/*    printf("%d\n", c ); */
  }

  return 0;

}
