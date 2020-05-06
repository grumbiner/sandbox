#include <stdio.h>
#include "ncepgrids.h"

//Test notion of 4-way linked list, corresponding to a grid

class link4 {
  public:
    float val;
    ijpt loc;
    link4 *left, *right, *up, *down;
    link4();
    link4 & operator=(link4 &);
    void show();
    void unlink(); 
};
link4::link4() {
  val = 0;
  loc.i = -1;
  loc.j = -1;
  left  = (link4 *) NULL;
  right = (link4 *) NULL;
  up    = (link4 *) NULL;
  down  = (link4 *) NULL;
}
link4& link4::operator=(link4 &x) {
  val = x.val;
  loc = x.loc;
  left  = x.left;
  right = x.right;
  up    = x.up;
  down  = x.down;
  return *this;
}
void link4::show() {
  printf("%f %d %d  \n",val, loc.i, loc.j);
}
void link4::unlink() {
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
//Note that since we grid the link4's, all of them are pre-allocated t
//  memory.  The content needs to be set, however
  grid2<link4> linked_ice(ice.xpoints(), ice.ypoints() );
  link4 tlink;
  ijpt loc, up, down, left, right, dup, ddown, dleft, dright;
  FILE *fin;

  dup.i    =  0; dup.j    = 1;
  ddown.i  =  0; ddown.j  = -1;
  dleft.i  = -1; dleft.j  = 0;
  dright.i =  1; dright.j = 0;

  fin = fopen("north","r");
  ice.binin(fin);
  fclose(fin);
  tag.set((unsigned char) 0);

// Run around the interior and initialize the grid of link4's.  Tedious,
//   but not difficult.
  for (loc.j = 1; loc.j < ice.ypoints()-1; loc.j++) {
  for (loc.i = 1; loc.i < ice.xpoints()-1; loc.i++) {
     up = loc; up += dup;
     down = loc; down += ddown;
     left = loc; left += dleft;
     right = loc; right += dright;
     if (ice[loc] > (unsigned char) 100 && ice[loc] < (unsigned char) 128) {
       ice[loc] = (unsigned char) 100;
     }
     else if (ice[loc] < (unsigned char) 15) {
       ice[loc] = (unsigned char) 0;
     }
     linked_ice[loc].loc    = loc;
     linked_ice[loc].val    = (float) ice[loc];
     linked_ice[loc].up     = &linked_ice[up];
     linked_ice[loc].down   = &linked_ice[down];
     linked_ice[loc].left   = &linked_ice[left];
     linked_ice[loc].right  = &linked_ice[right];
     printf("%3d %3d  %f %d\n", loc.i, loc.j, linked_ice[loc].val, ice[loc]);
     if (linked_ice[loc].val < 15.0 && linked_ice[loc].val != 0.) {
       printf(" t %3d %3d  %f %d\n", loc.i, loc.j, linked_ice[loc].val, ice[loc]);
     }
   }
   }
   for (loc.i = 1; loc.i < ice.xpoints()-1; loc.i++) {
     loc.j = 0;
     up = loc; up += dup; 
     down = loc; down += ddown;
     left = loc; left += dleft;
     right = loc; right += dright;
     linked_ice[loc].loc    = loc;
     linked_ice[loc].val    = (float) ice[loc];
     linked_ice[loc].left   = &linked_ice[left];
     linked_ice[loc].right  = &linked_ice[right];
     linked_ice[loc].up   = &linked_ice[up];
     loc.j = ice.ypoints() - 1;
     up = loc; up += dup; 
     down = loc; down += ddown;
     left = loc; left += dleft;
     right = loc; right += dright;
     linked_ice[loc].loc    = loc;
     linked_ice[loc].val    = (float) ice[loc];
     linked_ice[loc].left   = &linked_ice[left];
     linked_ice[loc].right  = &linked_ice[right];
     linked_ice[loc].down   = &linked_ice[down];
   }
  for (loc.j = 1; loc.j < ice.ypoints()-1; loc.j++) {
     loc.i = 0;
     up = loc; up += dup;
     down = loc; down += ddown;
     left = loc; left += dleft;
     right = loc; right += dright;
     linked_ice[loc].loc    = loc;
     linked_ice[loc].val    = (float) ice[loc];
     linked_ice[loc].up     = &linked_ice[up];
     linked_ice[loc].down   = &linked_ice[down];
     linked_ice[loc].right  = &linked_ice[right];
     loc.i = ice.xpoints() - 1;
     up = loc; up += dup;
     down = loc; down += ddown;
     left = loc; left += dleft;
     right = loc; right += dright;
     linked_ice[loc].loc    = loc;
     linked_ice[loc].val    = (float) ice[loc];
     linked_ice[loc].up     = &linked_ice[up];
     linked_ice[loc].down   = &linked_ice[down];
     linked_ice[loc].left   = &linked_ice[left];
   }
   loc.i = 0;
   loc.j = 0;
   up = loc; up += dup;
   down = loc; down += ddown;
   left = loc; left += dleft;
   right = loc; right += dright;
   linked_ice[loc].loc    = loc;
   linked_ice[loc].val    = (float) ice[loc];
   linked_ice[loc].up     = &linked_ice[up];
   linked_ice[loc].right  = &linked_ice[right];
   loc.i = 0;
   loc.j = ice.ypoints() - 1;
   up = loc; up += dup;
   down = loc; down += ddown;
   left = loc; left += dleft;
   right = loc; right += dright;
   linked_ice[loc].loc    = loc;
   linked_ice[loc].val    = (float) ice[loc];
   linked_ice[loc].down   = &linked_ice[down];
   linked_ice[loc].right  = &linked_ice[right];
   loc.i = ice.xpoints() - 1;
   loc.j = 0;
   up = loc; up += dup;
   down = loc; down += ddown;
   left = loc; left += dleft;
   right = loc; right += dright;
   linked_ice[loc].loc    = loc;
   linked_ice[loc].val    = (float) ice[loc];
   linked_ice[loc].up     = &linked_ice[up];
   linked_ice[loc].left   = &linked_ice[left];
   loc.i = ice.xpoints() - 1;
   loc.j = ice.ypoints() - 1;
   up = loc; up += dup;
   down = loc; down += ddown;
   left = loc; left += dleft;
   right = loc; right += dright;
   linked_ice[loc].loc    = loc;
   linked_ice[loc].val    = (float) ice[loc];
   linked_ice[loc].down   = &linked_ice[down];
   linked_ice[loc].left   = &linked_ice[left];


// Now try building 'trails'
//  Let a trail be a route of minimum gradient through the 4-linked list.
//  In principle, each grid will require a number of trails, as it is
//  entirely possible to travel around into a node from which all points
//  have already been visited (we'll NULL'ify the appropriate neighbors
//  as we add a point to a trail).
{
   int count = 0, trail = 0, tlen = 0;
   float updel, downdel, leftdel, rightdel;
   float tmin;
   ijpt sequence, next, deldir;
   bool found_new;
   loc.j = 0; loc.i = 0;
   sequence = loc;
   up    = loc; up    += dup;
   down  = loc; down  += ddown;
   left  = loc; left  += dleft;
   right = loc; right += dright;
   tlink = linked_ice[loc];
   tag[loc] = 1;
   count = 1;
 while (count < 2*ice.xpoints() * ice.ypoints() - 1 &&
            sequence.j < ice.ypoints() && sequence.i < ice.xpoints() &&
            trail < ice.xpoints() * ice.ypoints() - 1) {
   tlen = 1;
   printf("trail number %d\n",trail);
   while (! (tlink.right == (link4 *) NULL && 
             tlink.left == (link4 *) NULL && 
             tlink.up   == (link4 *) NULL && 
             tlink.down == (link4 *) NULL) &&
            count < 2* ice.xpoints()*ice.ypoints() ) {
     if (tlink.right != (link4 *) NULL) {
         rightdel = tlink.right->val - tlink.val; }
       else { rightdel = FLT_MAX; }; 
     if (tlink.left != (link4 *) NULL)  {
         leftdel = tlink.left->val - tlink.val; }
       else { leftdel = FLT_MAX; }; 
     if (tlink.up != (link4 *) NULL)    {
         updel = tlink.up->val - tlink.val; }
       else { updel = FLT_MAX; }; 
     if (tlink.down != (link4 *) NULL)  {
         downdel = tlink.down->val - tlink.val; }
       else { downdel = FLT_MAX; }; 
     tmin = FLT_MAX;
     tmin = min(tmin, (float) fabs(updel));
     tmin = min(tmin, (float) fabs(downdel));
     tmin = min(tmin, (float) fabs(rightdel));
     tmin = min(tmin, (float) fabs(leftdel));
     if (fabs(tmin) > 500. ) {
       printf("error, wound up with all being max %d %d\n",loc.i, loc.j);
       printf("%f %f %f %f\n",updel, downdel, rightdel, leftdel);
       return 3;
     }
     if (fabs(rightdel) == tmin) {
       deldir = dright;
       next = right;
     }
     else if (fabs(leftdel) == tmin) {
       deldir = dleft;
       next = left;
     }
     else if (fabs(updel) == tmin) {
       deldir = dup;
       next = up;
     }
     else if (fabs(downdel) == tmin) {
       deldir = ddown;
       next = down;
     }
     else {
       printf("should never have made it here, crashing\n");
       return 1;
     }
     printf("%3d %3d %5.1f  %3d %3d %5.1f  %6.1f\n",
         loc.i, loc.j, tlink.val, next.i, next.j, 
         linked_ice[next].val , tlink.val - linked_ice[next].val ); 
     // Need to strike out the link between loc and next
     linked_ice[loc].unlink();
     tlink = linked_ice[next];
     loc   = next;
     up    = loc; up    += dup;
     down  = loc; down  += ddown;
     left  = loc; left  += dleft;
     right = loc; right += dright;
     count += 1;
     tlen += 1;
     tag[loc] = 1;
   } // end of the 'while' for running around on a single trail (note that we
     // could redefine our 'trails' so that a large delta would also end it.
   if (sequence.j < ice.ypoints() && sequence.i < ice.xpoints() ) {
     loc = sequence;
     next = sequence;
     found_new = false;
     do {
       if (next.i != ice.xpoints() - 1) {
         next.i++;
       }
       else {
         next.i = 0;
         next.j += 1;
       }
       if ( ! (linked_ice[next].right == (link4 *) NULL &&
               linked_ice[next].left  == (link4 *) NULL &&
               linked_ice[next].up    == (link4 *) NULL &&
               linked_ice[next].down  == (link4 *) NULL) ) {
            sequence = next;
            found_new = true;
       }
     } while (!found_new && sequence.i <= ice.xpoints() - 1 && 
                      sequence.j <= ice.ypoints() - 1    ) ;
   }
   if (loc != sequence) {
     printf("next point to start trail from %3d %3d\n",
           sequence.i, sequence.j);
     loc = sequence;
     up    = loc; up    += dup;
     down  = loc; down  += ddown;
     left  = loc; left  += dleft;
     right = loc; right += dright;
     tlink = linked_ice[loc];
     tag[loc] = 1;
     printf("trail %d length %d\n", trail, tlen);
     trail   += 1;
     tlen     = 0;
   }
   else {
     printf("reached 'else' clause of sequence updating\n");
     sequence.i = ice.xpoints() + 1;
     sequence.j = ice.ypoints() + 1;
     loc = sequence;
   } // ran through last sequence point
 } // while count loop

  for (loc.j = 0; loc.j < tag.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < tag.xpoints() ; loc.i++) {
     if (tag[loc] != 1) {
       printf("unfound point: %d %d\n",loc.i, loc.j);
     }
  }
  }


} // end of our sub-block for making trails
       

  return 0;
}
