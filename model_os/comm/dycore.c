/* Ancestor program: */
/* https://stackoverflow.com/questions/2784500/how-to-send-a-simple-string-between-two-programs-using-pipes */ 
/* 20 March 2018 */

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argv, char *argc[]) {
    int fd, pfd;
    int x[20], i; 
    char *myfifo = "/tmp/dycore";
    char *process = "/tmp/process";

    /* create the FIFO (named pipe) */
    mkfifo(myfifo, 0666);
    fd = open(myfifo, O_WRONLY);

    pfd = open(process, O_RDONLY);

    /* Initialize a matrix/array */
    for (i = 0; i < 20; i++) {
      x[i] = i;
    }

  for (int k = 0; k < 10; k++) {
    /* don't want to write until we know previous one has been read */
    write(fd, &x[0], sizeof(int)*20);

    /* advance model */
    for (i = 0 ; i < 20; i++) {
      x[i] += 1;
    }
    /* try not waiting, see what reader does -- reads full message, but
       unpredictable sync */
  }

    close(fd);

    /* remove the FIFO */
    unlink(myfifo);

    return 0;
}
