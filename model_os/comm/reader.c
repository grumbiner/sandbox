/* https://stackoverflow.com/questions/2784500/how-to-send-a-simple-string-between-two-programs-using-pipes */
/* 20 March 2018 */

#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAX_BUF (12*1024)

int main() {
    int fd, pfd;
    int i, x[20];
    char* myfifo = "/tmp/dycore";
    char* process = "/tmp/process";

    /* open, read, and display the message from the FIFO */
    fd = open(myfifo, O_RDONLY);
    pfd = open(process, O_WRONLY);

  for (int k = 0; k < 10; k++) {

    /* read, and display the message from the FIFO */
    read(fd, x, MAX_BUF);
    printf("Received: \n");
    for (i = 0; i < 20; i++) {
      printf("%2d %d\n",i,x[i]);
    }
    sleep(1);

  }

    /* Now that we're done, close the pipe */
    close(fd);

    return 0;
}
