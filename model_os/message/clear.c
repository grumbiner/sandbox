#include <stdio.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

#include <errno.h>

#define ICE       0x02
#define MES_SIZE  800

int main(void) {
  key_t key;
  struct msqid_ds msq_stat;
  int retval, permflags, msg_id;

/* Remove pre-existing file */
  for (msg_id = 65530; msg_id < 65599; msg_id++) {
/* Check the queue status */
    retval = msgctl(msg_id, IPC_STAT, &msq_stat);
    if (retval == 0) {
      printf("msgid %d retval = %d\n",msg_id,  retval);
      printf("# mess      %lu\n", msq_stat.msg_qnum);
      printf("byte limit  %lu\n", msq_stat.msg_qbytes);
      printf("bytes pres. %lu\n", msq_stat.msg_cbytes);
      retval = msgctl(msg_id, IPC_RMID, &msq_stat);
      printf("in removal, id %d val = %d\n",msg_id, retval);
    }
  }
  return 0;

}
