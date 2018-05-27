#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>

int main(void) {
  int socno, domain, type, protocol; /* Parameters for socket */
  struct sockaddr_in server;         /* Parameters for bind   */ 
  int loglen = 10;                   /* Parameter for listen  */
  int length, rval, msgsock;
  char buf[485*365];


/* Create the socket */ 
  domain = AF_INET;
  type   = SOCK_STREAM;
  protocol = 0;
  socno = socket(domain, type, protocol);
    printf("socket number = %d \n", socno);

/* Bind to it so that you can do something with it */
  server.sin_family      = domain;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port        = 0;
  bind(socno, (struct sockaddr*) &server, sizeof(server) );
 
/* Find its port number */
  length = sizeof(server);
  getsockname(socno, (struct sockaddr*) &server, &length);
  printf("Port number = %d \n", ntohs(server.sin_port) ); 

/* Listen to the port (required or there's nothing to do!) */
  rval = listen(socno, loglen);
  printf("Listen rval = %d\n", rval );
  
  do {
       msgsock = accept(socno, (struct sockaddr*) 0, (int *) 0) ;
       if (msgsock == -1) {
         perror("error in accept \n");
       }
       else do {
          memset(buf, 0, sizeof(buf)); /* Zero the buffer */
          rval = recv(msgsock, buf, 485*365, 0);
          if (rval < 0) {
            perror("error in reading socket\n");
          }
          if (rval == 0) {
            printf("Ending connection\n");
          }
          else {
            /* Have a valid message */
            printf("socket %d %s\n", msgsock, buf);
          }
        } while (rval != 0);
        shutdown(msgsock, 0);
   } while (1);

   exit(0);

   return 0;
}
