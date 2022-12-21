#include "mvector.h"
#include "program.h"

// Construct an interpreter.  It accepts a 'program' (mvector of floats)
//  a mvector of inputs, and produces a mvector of outputs.
// For use in genetic algorithm development
// Robert Grumbine 6 May 1999

void interpreter(program &pgm, mvector<float> &inputs, mvector<float> &outputs)
{
  int i, j, pc;
  int nxin = inputs.xpoints();
  mvector<float> registers(MAXREGISTERS);

  //Copy over the inputs to the first registers, we'll take the outputs
  //  off the last registers.  (Duplicate inputs as many times as there's
  //  room for.)
  //for (j = 0; j < MAXREGISTERS/nxin ; j++) {
  //for (i = 0; i < nxin ; i++)                 {
//     registers[i+j*nxin ] = inputs[i];
//  }
//  }
  for (j = 0; j < MAXREGISTERS; j++) {
    registers[j] = inputs[j%nxin];
  }

#define VALEQUATE 0
#define REGEQUATE 1
#define INMUL     2
//Begin program interpretation.
  for (pc = 0; pc < pgm.length() ; pc++) {
    //CDprintf("%d %f %f %f %f\n",pc, pgm[pc], pgm[pc+1], pgm[pc+2], pgm[pc+3]);
    fflush(stdout);
    switch( (int) pgm[pc]) {
       case VALEQUATE :
         if (pgm[pc+1] > MAXREGISTERS-1 || pgm[pc+1] < 0 ||
              pc + 2 >= pgm.length() ) break;
         pc += 1;
         #ifdef VERBOSE
           printf("valequate %f %f\n",pgm[pc], pgm[pc+1] ); fflush(stdout);
         #endif
         registers[(int) pgm[pc] ] = pgm[pc +1];
         pc += 1;
         break;
       case REGEQUATE :
         if (pgm[pc+1] > MAXREGISTERS-1 || pgm[pc+1] < 0 ||
             pgm[pc+2] > MAXREGISTERS-1 || pgm[pc+2] < 0 ||
             pc + 2 >= pgm.length()       )       break;
         #ifdef VERBOSE
         printf("regequate %5.2f %5.2f\n",pgm[pc+1], pgm[pc+2]); fflush(stdout);
         #endif
         pc += 1;
         registers[(int) pgm[pc] ] = registers[(int) pgm[pc+1] ];
         pc += 1;
         break;
       case INMUL : // multiply item pointed to by pc by value from program
         if (pgm[pc+1] > MAXREGISTERS-1 || pgm[pc+1] < 0 ||
              pc + 2 >= pgm.length() ) break;
         pc += 1;
         #ifdef VERBOSE
           printf("inmul %f %f\n",pgm[pc], pgm[pc+1] ); fflush(stdout);
         #endif
         registers[(int) pgm[pc] ] *= pgm[pc+1];
         pc += 1;
         break;
       default :
         break;
    }
  }
  fflush(stdout);

  //Produce the output
  for (i = 0; i < outputs.xpoints(); i++) {
     outputs[i] = registers[registers.xpoints() - 1 - i];
  }

  return;
}

