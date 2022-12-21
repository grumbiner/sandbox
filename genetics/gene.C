#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "program.h"
#include "cell.h"

///////////////////////////////////Begin program

#include "interp.C"
#include "all.selectors.C"

void interpreter(program &pgm, cell &a0, cell &a1, cell &a2, cell &a3);
void selector2(program *x, int nprogs);

int main(int argc, char *argv[]) {
   program x[NPROGS] ; //create 1000 programs for evolving
   float score_table[NPROGS]; // table of scores
   cell a0, a1, a2, a3, aret;  // last 3 days cells, the true fourth, and 
                               //   the returned (predicted) cell
   int i, j, generation;
   float scorenet;
   FILE *fin;

   if (argc > 1) {
     srand(atoi(argv[1]) );
   }
   else {
     srand(0);
   }
// Establish an initial population of programs
   for (i = 0; i < NPROGS; i++) {
     x[i].resize(24);
   } 

   x[0][0] = SET;
   x[0][1] = 4; // which one
   x[0][2] = 1; // i
   x[0][3] = 1; // j
   x[0][4] = 1;  // val (from prior mutant)
   x[0][5] = SET;
   x[0][6] = 4; // which one
   x[0][7] = 0; // i
   x[0][8] = 0; // j
   x[0][9] = 1;  // val (from prior mutant)
   x[0][10] = SET;
   x[0][11] = 4; // which one
   x[0][12] = 2; // i
   x[0][13] = 2; // j
   x[0][14] = 1;  // val (from prior mutant)
   x[0][15] = EQUATE;
   x[0][16] = 5;
   x[0][17] = 4;
   x[0][18] = MUL;
   x[0][19] = 5;
   x[0][20] = 2;
   x[0][21] = EQUATE;
   x[0][22] = 3;
   x[0][23] = 5;
   printf("Initial species\n");
   fin = fopen("cellout", "r");
   for (i = 0; i < NCELLS; i++) {
     a0.binin(fin);
     a1.binin(fin);
     a2.binin(fin);
     aret.zero();
     interpreter(x[0], a0, a1, a2, aret);
     a3.binin(fin);
     scorenet += score3(a3, aret); 
     a3.show();
     aret.show();
     printf("\n");
   }
   x[0].list(stdout);
 
   for (j = 1; j < NPROGS; j++) {
      //printf("Making mutant %d\n",j);
      x[0].mutate(x[j]);
   }
   
// First Genus: Score 1,1 only Now go through and see how they do
   printf("Starting first genus -- 1,1 pt only\n"); fflush(stdout);
   for (generation = 0; generation < NGENERATIONS; generation++) {
     printf("Generation %d\n",generation); fflush(stdout);
     for (j = 0; j < NPROGS; j++) {
       fin = fopen("cellout", "r");
       scorenet = 0.0;
       for (i = 0; i < NCELLS; i++) {
         a0.binin(fin);
         a1.binin(fin);
         a2.binin(fin);
         aret.zero();
         interpreter(x[j], a0, a1, a2, aret);
         a3.binin(fin);
         scorenet += score(a3, aret); 
       }
       score_table[j] = sqrt(scorenet/NCELLS);
       x[j].value = score_table[j];

       fclose(fin);
     }
     selector2(x, NPROGS);

   } // end of string of generations

// Second Genus: Score 1,1 and 2,2 Now go through and see how they do
   printf("Starting on Second Genus\n"); fflush(stdout);
   for (generation = 0; generation < NGENERATIONS; generation++) {
     printf("Generation %d\n",generation); fflush(stdout);
     for (j = 0; j < NPROGS; j++) {
       fin = fopen("cellout", "r");
       scorenet = 0.0;
       for (i = 0; i < NCELLS; i++) {
         a0.binin(fin);
         a1.binin(fin);
         a2.binin(fin);
         aret.zero();
         interpreter(x[j], a0, a1, a2, aret);
         a3.binin(fin);
         scorenet += score2(a3, aret); 
       }

       score_table[j] = sqrt(scorenet/NCELLS);
       x[j].value = score_table[j];

       fclose(fin);
     }

     selector2(x, NPROGS);
   } // end of string of generations

// Third Genus: Score 0,0, 1,1 and 2,2 Now go through and see how they do
   printf("Starting on Second Genus\n"); fflush(stdout);
   for (generation = 0; generation < NGENERATIONS; generation++) {
     printf("Generation %d\n",generation); fflush(stdout);
     for (j = 0; j < NPROGS; j++) {
       fin = fopen("cellout", "r");
       scorenet = 0.0;
       for (i = 0; i < NCELLS; i++) {
         a0.binin(fin);
         a1.binin(fin);
         a2.binin(fin);
         aret.zero();
         interpreter(x[j], a0, a1, a2, aret);
         a3.binin(fin);
         scorenet += score3(a3, aret); 
       }

       score_table[j] = sqrt(scorenet/NCELLS);
       x[j].value = score_table[j];

       fclose(fin);
     }

     selector2(x, NPROGS);
   } // end of string of generations

   printf("Final species\n");
   fin = fopen("cellout", "r");
   for (i = 0; i < NCELLS; i++) {
     a0.binin(fin);
     a1.binin(fin);
     a2.binin(fin);
     aret.zero();
     interpreter(x[0], a0, a1, a2, aret);
     a3.binin(fin);
     scorenet += score3(a3, aret); 
     a3.show();
     aret.show();
     printf("\n");
   }
   x[0].list(stdout);

   return 0;

}
