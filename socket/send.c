
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Sample to send messages to a socket */

int main(int argc, char *argv[]) {
  int socno, domain, type, protocol; /* Parameters for socket */
  struct sockaddr_in server;         /* Parameters for bind or connect   */ 
  struct hostent *hp;                /* Parameters for  */
  int length, rval, msgsock;
  char buf[485*365];
  int i;
  char retmesg[900];

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

/* Send a message to the host */
  
  length = sizeof("This is a message from the send");
  for (i = 0; i < 10; i++) {
    rval = send(socno, "This is a message from the send", 
           length, 0);
    if (rval != length) printf("sendto return code = %d\n", rval);
    rval = recv(socno, retmesg, 90, 0); 
    printf("Rval = %d mesg = %s\n", rval, retmesg);
    
    sleep(2);
  }
 
  rval = shutdown(socno,0);
  printf("close return code = %d\n", rval);
  

  return 0;
}  
