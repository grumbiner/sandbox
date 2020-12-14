import os

dirs=["finished_archive", "ak", "rec1", "g", "sk2", "sk2.polar"]
#declare stats as vector[len(dirs)] type os.stat
print "number of elements in dirs = ",len(dirs)

#for ymd = 20000101 to 20180427

i = 0
while i < len(dirs):
  print dirs[i]
  i += 1

i = 0
name="sk2/alaska.20110101"
#stats[i] = os.stat('sk2/alaska.20110101')
stats = os.stat(name)
print stats.st_size

