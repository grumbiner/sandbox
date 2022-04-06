pgcc -acc -ta=nvidia,cc60,nofma -Minfo=accel -Msafeptr test1.c -o test1
pgcc -acc -ta=nvidia,cc60,nofma -Minfo=accel -Msafeptr test2.c -o test2 -lm
