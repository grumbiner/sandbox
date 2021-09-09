import os
import sys

#result of ls -ls {} 
nsize = 5
fname = 9
oldsize = 0
oldline = "1 1 1 1 1 1 1 1 1 1 1 "
oldname = oldline.split()[fname]
k = 0

# use argument for fname
fin = open(sys.argv[1],"r")

# if path or name includes a space, 
#   a) skip
#   b) replace ( with \(, ) with \), ' with \'

for line in fin:
  words = line.split()

  size = words[nsize] 
  name = words[fname]
  if ( '\\' in name or '\ ' in name):
    #print("have a blank in ",name)
    continue
  
  if (size == oldsize):
    k += 1
    #print("echo ",size)
    print("cmp ",oldname, name)
    print("if [ $? -eq 0 ] ; then")
    print("  echo can rm one of ",oldname, name)
    print("fi\n")
  oldsize = size
  oldline = line
  oldname = name

print("exit")
print(k, "possible matches")
