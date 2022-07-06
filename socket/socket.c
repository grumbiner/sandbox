#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define _POSIX_HIWAT 512

/* Sample to send messages to a socket */
#define NX 385
#define NY 465


int main(int argc, char *argv[]) {
  int socno, domain, type, protocol; /* Parameters for socket */
  struct sockaddr_in server;         /* Parameters for bind or connect   */ 
  struct hostent *hp;                /* Parameters for  */
  int length, rval, msgsock;
  char *buf;
  int *map;
  int i, j, msg2, tmpcnt, left;
  char retmesg[2*NY];

  buf = (char *) malloc(sizeof(int)*NX*NY) ;
  map = (int *) buf;
  printf("finished mallocation\n"); fflush(stdout);

/* Create the socket on which to send*/ 
  domain = AF_INET;
  type   = SOCK_STREAM;
  protocol = 0;
  socno = socket(domain, type, protocol);
  printf("socket number = %d \n", socno);

  hp = gethostbyname(argv[1]);
  if (hp == 0) {
     printf("Failed to get the host %s\n", argv[1]);
     exit(2);
  }

/* Connect to the server */
  memcpy( (char*)&server.sin_addr, (char*)hp->h_addr, hp->h_length);
  server.sin_family      = domain;
  server.sin_port        = htons(atoi(argv[2]) ) ;
  rval = connect(socno, (struct sockaddr*) &server, sizeof(server) );

/* Set up a dummy data map, to be processed by a remote process */
  for (j = 0; j < NY; j++) {
  for (i = 0; i < NX; i++) {
     map[j*NX + i] = i;
  }
  }
  printf("finished building dummy map\n"); fflush(stdout);
/* Send a message to the host */
  
  length = sizeof(int)*NX*NY;
  for (i = 0; i < 10; i++) {
/* Break up the message to individual blobs equal to the size of the */
/*    limit for pipes */
    tmpcnt = length / _POSIX_HIWAT;
    left   = length - tmpcnt*_POSIX_HIWAT;
    printf("left = %d\n",left);
    for (j = 0; j < tmpcnt; j++) {
      rval = send(socno, buf, _POSIX_HIWAT, 0);
      if (rval != _POSIX_HIWAT) printf("sendto return code = %d\n", rval);
    }
    if (left != 0) {
      rval = send(socno, buf, left, 0);
      if (rval != left) printf("sendto return code = %d of %d\n", rval, left);
    }
    fflush(stdout);

/* Need to reassemble from a bunch of blobs */    
    for (j = 0; j < tmpcnt; j++) {
      rval = recv(socno, (char *)&buf[j*_POSIX_HIWAT], _POSIX_HIWAT, 0); 
      if (rval != _POSIX_HIWAT) {
 printf("%d returned %d vs. %d\n", j, rval, _POSIX_HIWAT); fflush(stdout);
rval += recv(socno, (char *)&buf[j*_POSIX_HIWAT+rval], _POSIX_HIWAT-rval, 0); 
 printf("retry gave %d vs. %d\n", rval, _POSIX_HIWAT); fflush(stdout);
      }
    }
    if (left != 0) {
      rval = recv(socno, (char *)&buf[j*_POSIX_HIWAT], left, 0);
      if (rval != length) printf("sendto return code = %d\n", rval);
    }

/* At last, do something with the numbers */
    printf("i = %d \n",i);
    for (j = 0; j < NX; j++) {
      printf("%3d ", map[NY/2*NX+ j]);
    }
    printf("\n");
    
    /*sleep(2); */
  }
 
  rval = shutdown(socno,0);
  printf("close return code = %d\n", rval);
  

  return 0;
}  
