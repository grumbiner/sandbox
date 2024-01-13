#include <stdio.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

#include <errno.h>

#define ICE       0x02
#define MES_SIZE  800
  struct {
      long mtype;
      char mtext[MES_SIZE +1 ] ;
  } message;


int main(void) {
  key_t key;
  struct msqid_ds msq_stat;

  int retval, permflags, msg_id;

/* beware of pre-existing queue */

/* Establish the message queue */
  key  = ICE;
  permflags = 0660 | IPC_CREAT | IPC_EXCL;
  msg_id = msgget(key, permflags);
  printf("msg_id = %d\n",msg_id);

/* Try to raise the queue limit: -- fails on osx even for root */
//  msq_stat.msg_qbytes *= 4;
//  retval = msgctl(msg_id, IPC_SET, &msq_stat);
//  printf("retval for ipc set %d\n",retval);

/* Check the queue status */
  retval = msgctl(msg_id, IPC_STAT, &msq_stat);
  printf("retval = %d\n", retval);
  printf("# mess      %lu\n", msq_stat.msg_qnum);
  printf("byte limit  %lu\n", msq_stat.msg_qbytes);
  printf("bytes pres. %lu\n", msq_stat.msg_cbytes);

/* Put a message on the queue and look at the status */
  message.mtype = 5;
  sprintf(message.mtext,"%s","this is a test message");
  retval = msgsnd(msg_id, &message, MES_SIZE, IPC_NOWAIT);
  printf("retval on send %d\n",retval);
  retval = msgctl(msg_id, IPC_STAT, &msq_stat);

  printf("retval = %d\n", retval);
  printf("# mess      %lu\n", msq_stat.msg_qnum);
  printf("byte limit  %lu\n", msq_stat.msg_qbytes);
  printf("bytes pres. %lu\n", msq_stat.msg_cbytes);

/* When done, remove queue */
  retval = msgctl(msg_id, IPC_RMID, &msq_stat);
  printf("removal on id %d retval = %d\n",msg_id, retval);


  return 0;
}
