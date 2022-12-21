#include "program.h"
#include "cell.h"

///////////////////////////////////Begin program
#define NPTRS 16
#define NPROGS 1000
#define NCELLS 500

#define SET 0
#define ADD 1
#define MUL 2
#define EQUATE 3
#define NGENERATIONS 36

void interpreter(program &pgm, cell &a0, cell &a1, cell &a2, cell &a3) {
   cell *ptrtab[NPTRS], dummycells[NPTRS - 8];
   cell c0, c1, c2, tmp;
   int i, j, val, pc;
   int p1, p2;

   ptrtab[0] = &a0;
   ptrtab[1] = &a1;
   ptrtab[2] = &a2;
   ptrtab[3] = &a3;
   ptrtab[4] = &c0;
   ptrtab[5] = &c1;
   ptrtab[6] = &c2;
   ptrtab[7] = &tmp;
   for (i = 8; i < NPTRS; i++) {
      ptrtab[i] = &(dummycells[i - 8]);
   } 

// Clear out the working cells, leaving alone the input data cells
   for (i = 3; i < NPTRS; i++) {
      ptrtab[i]->zero();
   }

// PC = program counter
   for (pc = 0; pc < pgm.length() ; pc++) { 
     switch ( (int) pgm[pc] ) {
      case SET :
         if (pc + 4 >= pgm.length()) break;
         pc += 1; p1 = pgm[pc]; 
         pc += 1;  i = pgm[pc]; 
         pc += 1;  j = pgm[pc]; 
         pc += 1; val = pgm[pc]; 
         if (p1 < 0 || p1 >= NPTRS) break;
         ptrtab[p1]->set(i,j,val);
         break;

      case ADD :
         if (pc + 2 >= pgm.length()) break;
         pc += 1; p1 = pgm[pc]; 
         pc += 1; p2 = pgm[pc]; 
         if (p1 >= NPTRS || p2 >= NPTRS || p1 < 0 || p2 < 0) break;
         ptrtab[p1]->add(*ptrtab[p2]) ;
         break;

      case MUL :
         if (pc + 2 >= pgm.length()) break;
         pc += 1; p1 = pgm[pc]; 
         pc += 1; p2 = pgm[pc]; 
         if (p1 >= NPTRS || p2 >= NPTRS || p1 < 0 || p2 < 0) break;
         ptrtab[p1]->mul(*ptrtab[p2]) ;
         break;
 
      case EQUATE :
         if (pc + 2 >= pgm.length()) { 
           break; 
         }
         pc += 1; p1 = pgm[pc]; 
         pc += 1; p2 = pgm[pc];
         if (p1 >= NPTRS || p2 >= NPTRS || p1 < 0 || p2 < 0) break;
         *ptrtab[p1] = *ptrtab[p2];
         break;

      default : // do nothing
         break;
     } // switch

   } // end looping through program

   return;
}

