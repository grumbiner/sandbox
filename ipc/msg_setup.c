#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

#define ICE       0x01
#define MES_SIZE  800

int main(void) {
  key_t key;
  int retval, permflags, msg_id;
  struct {
      long mtype;
      char mtext[MES_SIZE +1 ] ;
  } message;
  struct msqid_ds msq_stat;
  
/* Establish the message queue */
  key  = ICE;
  permflags = 0660 | IPC_CREAT | IPC_EXCL;
  msg_id = msgget(key, permflags);
  printf("msg_id = %d\n",msg_id);

/* Check the queue status */
  retval = msgctl(msg_id, IPC_STAT, &msq_stat);
  printf("retval = %d\n", retval);
  printf("# mess      %d\n", msq_stat.msg_qnum);
  printf("byte limit  %d\n", msq_stat.msg_qbytes);
  printf("bytes pres. %d\n", msq_stat.__msg_cbytes);

/* Put a message on the queue and look at the status */
  message.mtype = 5;
  sprintf(message.mtext,"%s","this is a test message");
  retval = msgsnd(msg_id, &message, MES_SIZE, IPC_NOWAIT);
  printf("retval on send %d\n",retval);
  retval = msgctl(msg_id, IPC_STAT, &msq_stat);
  printf("retval = %d\n", retval);
  printf("# mess      %d\n", msq_stat.msg_qnum);
  printf("byte limit  %d\n", msq_stat.msg_qbytes);
  printf("bytes pres. %d\n", msq_stat.__msg_cbytes);



  return 0; 
}
