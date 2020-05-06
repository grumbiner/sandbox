import os
cwd=os.getcwd()
print cwd

cmd='ls -l'
fp=os.popen(cmd)
res=fp.read()
stat=fp.close()
print stat
