#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#define _POSIX_HIWAT 512

/* Sample of divided processing -- receive a message on the designated */
/*   port, modify the data field being passed, and return the result */

#define NX 385
#define NY 465

int main(void) {
  int socno, domain, type, protocol, serlen; /* Parameters for socket */
  struct sockaddr_in server;         /* Parameters for bind   */ 
  int loglen = NY*NX/_POSIX_HIWAT;   /* Parameter for listen  */
  int length, rval, msgsock;
  int tmpcnt, left;
  char *buf;
  int *map;
  int i, j;

  length = sizeof(int)*NX*NY;
  tmpcnt = length / _POSIX_HIWAT;
  left   = length - tmpcnt * _POSIX_HIWAT;
  map = (int *) malloc(length);
  buf = (char *) malloc(length);
/* Create the socket */ 
  domain = AF_INET;
  type   = SOCK_STREAM;
  protocol = 0;
  socno = socket(domain, type, protocol); 
  printf("socket number = %d \n", socno);

/* Bind to it so that you can do something with it */
  server.sin_family      = domain;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port        = 4201;
  bind(socno, (struct sockaddr*) &server, sizeof(server) );
 
/* Find its port number */
  serlen = sizeof(server);
  getsockname(socno, (struct sockaddr*) &server, &serlen);
  printf("Port number = %d \n", ntohs(server.sin_port) ); 
  if (ntohs(server.sin_port) <= 0) {printf("port error \n") ; exit (-2); }

/* Listen to the port (required or there's nothing to do!) */
  rval = listen(socno, loglen);
  printf("Listen rval = %d\n", rval );
  
  do {
       msgsock = accept(socno, (struct sockaddr*) 0, (int *) 0) ;
       if (msgsock == -1) {
         perror("error in accept \n");
       }
       else do {
          memset(buf, 0, length ); /* Zero the buffer */

/* Receive the message in a batch of bits */
          for (j = 0; j < tmpcnt; j++) {
            rval = recv(msgsock, (char*)&buf[j*_POSIX_HIWAT], _POSIX_HIWAT, 0);
            if (rval < 0) {
              perror("error in reading socket\n");
            }
            if (rval == 0) {
              shutdown(msgsock, 0);
              printf("Ending connection j = %d\n",j);
              break;
            }
            if (rval != _POSIX_HIWAT && rval > 0) {
  printf("Processor %d only received %d of %d\n",j, rval,_POSIX_HIWAT);
  rval += recv(msgsock, (char*)&buf[j*_POSIX_HIWAT+rval], _POSIX_HIWAT-rval, 0);
  printf("retry gave %d of %d\n",rval,_POSIX_HIWAT);
            }
          }
          if (left != 0) {
            rval = recv(msgsock, (char*)&buf[j*_POSIX_HIWAT], left, 0);
          }
          if (rval < 0) {
            perror("error in reading socket\n");
          }
          if (rval == 0) {
            shutdown(msgsock, 0);
            printf("Ending connection j = %d\n",j);
            break;
          }
          else {
            /* Have a valid message */
            /* printf("socket %d %s\n", msgsock, buf); */
            map = (int *) buf;
            printf("Socket %d in operation %d\n",msgsock, map[NY/2*NX+NX/2] );
            for (j = 0; j < NY; j++) {
            /* printf("j = %d\n",j); */
            for (i = 0; i < NX; i++) {
               /* printf("i = %d\n",i); */
               printf("%3d %3d %d\n",i,j,map[j*NX + i] ); 
               map[j*NX + i] -= 1;
            }
            }
            printf("About to write back to the message socket %d\n",(int) clock());fflush(stdout);
      
/* Return the message also in a bunch of bits */
            buf = (char *) map;
            for (j = 0; j < tmpcnt; j++) { 
            /* send(msgsock, (char *)&buf[j*_POSIX_HIWAT], _POSIX_HIWAT, 0 ); */
             rval = write(msgsock, (char *)&buf[j*_POSIX_HIWAT], _POSIX_HIWAT );
              if (rval != _POSIX_HIWAT) {
              printf("Processor %d only sent %d of %d\n",j,rval, _POSIX_HIWAT); 
              }
            }
            if (left != 0) {
              write(msgsock, (char *)&buf[j*_POSIX_HIWAT], left );
              printf("sent left \n"); fflush(stdout);
            }

          }
        } while (rval != 0);
        shutdown(msgsock, 0);
   } while (1);

   exit(0);

   return 0;
}
