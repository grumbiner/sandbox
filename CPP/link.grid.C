#include <stdio.h>
#include "ncepgrids.h"

//Test notion of 4-way linked list, corresponding to a grid
// -- template it, however, so that we have a grid of grids

template<class T>
class link4 {
  private:
    T* prival;
  public:
    T val;
    ijpt loc;
    link4 *left, *right, *up, *down;
    link4();
    link4 & operator=(link4 &);
    void show();
    void unlink(); 
};
template<class T>
link4<T>::link4() {
  prival = new T;
  val = *prival;
  loc.i = -1;
  loc.j = -1;
  left  = (link4 *) NULL;
  right = (link4 *) NULL;
  up    = (link4 *) NULL;
  down  = (link4 *) NULL;
}
template<class T>
link4<T>& link4<T>::operator=(link4 &x) {
  val = x.val;
  loc = x.loc;
  left  = x.left;
  right = x.right;
  up    = x.up;
  down  = x.down;
  return *this;
}
template<class T>
void link4<T>::show() {
  printf("%f %d %d  \n",(float)val, loc.i, loc.j);
}
template<class T>
void link4<T>::unlink() {
  if (right != (link4 *) NULL) { 
         right->left = (link4 *) NULL; 
  }
  if (left  != (link4 *) NULL) { 
         left->right = (link4 *) NULL; 
  }
  if (up    != (link4 *) NULL) { 
         up->down = (link4 *) NULL; 
  }
  if (down  != (link4 *) NULL) { 
         down->up = (link4 *) NULL; 
  }
  if (right != (link4 *) NULL) {right = (link4 *) NULL; }
  if (left  != (link4 *) NULL) {left  = (link4 *) NULL; }
  if (up    != (link4 *) NULL) {up    = (link4 *) NULL; }
  if (down  != (link4 *) NULL) {down  = (link4 *) NULL; }
}

int main(void) {
  northgrid<unsigned char> ice, tag;
// We have pointers to 4-way linked northgrids
  link4<northgrid<unsigned char> > *links;
  link4<northgrid<unsigned char> > tlink;
  ijpt loc, up, down, left, right, dup, ddown, dleft, dright;
  FILE *fin;

  dup.i    =  0; dup.j    = 1;
  ddown.i  =  0; ddown.j  = -1;
  dleft.i  = -1; dleft.j  = 0;
  dright.i =  1; dright.j = 0;

  fin = fopen("north","r");
  ice.binin(fin);
  fclose(fin);
  tag.set((unsigned char) 20);

// Run around the interior and initialize the grid of link4's.  Tedious,
//   but not difficult.

  for (loc.j = 0; loc.j < ice.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < ice.xpoints() ; loc.i++) {
     if (ice[loc] > (unsigned char) 100 && ice[loc] < (unsigned char) 128) {
       ice[loc] = (unsigned char) 100;
     }
     else if (ice[loc] < (unsigned char) 15) {
       ice[loc] = (unsigned char) 0;
     }
   }
   }

  links = new link4<northgrid<unsigned char> >[25]; // 25 linked grids

  links[0].val = ice;
  links[1].val = tag;

  loc.j = 20; loc.i = 20;
  printf("%d  %d  \n", links[0].val[loc], links[1].val[loc]);
  links[0].val += links[1].val;
  printf("%d  %d  \n", links[0].val[loc], links[1].val[loc]);

  return 0;
}
