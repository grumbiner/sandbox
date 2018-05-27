all : date_svc rdate
date_svc : date_svc.o date_clnt.o
	$(CC) date_svc.o date_clnt.o -o date_svc -lrpcsvc
rdate : rdate.o date_proc.o
	$(CC) rdate.o date_proc.o -o rdate -lrpcsvc

