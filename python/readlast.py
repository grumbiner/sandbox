import sys

#edge.f1.f2.from.valid
#fcst ncep nsidc_north ims

x = sys.argv[1]
#print(x)
print(x.split("."))
words = x.split(".")
source = words[1]
valid  = words[2]
fromdate = int(words[3])
todate   = int(words[4])
print(source, todate)

fin = open(x,"r")
with fin as f:
    last_line = f.readlines()[-1]
fin.seek(0)

print(last_line)
fin = open(x,"r")
