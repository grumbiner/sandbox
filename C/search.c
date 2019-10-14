#include <stdio.h>   /* SEEK_SET , etc are in here */
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#define RECORD_LEN 80
#define KEY_LEN 10
#define BUFFER_LEN RECORD_LEN-KEY_LEN

int main ( int argc, char **argv)
{
	int fd, numBytes, i;
        int min=0, max, mid;
	char key[KEY_LEN+1];
	char buffer[BUFFER_LEN+1];

	strncpy(key,"",KEY_LEN+1);        /* Init key   */
	strncpy(buffer,"",BUFFER_LEN+1);  /* Init buffer*/


	/* Open the File */

	if (fd == -1) {
		printf("\nCouldn't open file:%s\n\n", argv[1]);
		exit(0);
	}

	/* Find end of file */


	while (max >= min) {
	
		/* Position the read-write pointer */
		mid = (max+min)/2;         


		numBytes = read(fd, key, KEY_LEN); /* Read the key */

		/* Compare to search item */
		switch (strcmp(argv[2], key) ) {
			case -1:
				max = mid-1; break;
			case  1:
				min = mid+1; break;

			case  0:
				printf("\nFound item:%s\n", key);
				if (BUFFER_LEN != read(fd, buffer, BUFFER_LEN)){
					printf("\nError reading from %s\n", argv[1]);
					exit(0);
				}
				printf("Record:'%s'\n\n", buffer);
				exit(0);
				break;

		}  /* End switch */

	}  /* End while */

	printf("\nItem '%s' not in file.\n\n", argv[2]);
}
