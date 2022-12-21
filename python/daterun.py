import sys
import datetime

#arguments are 2 8 digit dates, start and end.
#may run up or down
stag1 = sys.argv[1]
stag2 = sys.argv[2]

(yy1,mm1,dd1) = (int(stag1)/10000,(int(stag1)%10000)/100,int(stag1)%100)
print(yy1,mm1,dd1)
tag1 = datetime.date(int(yy1), int(mm1), int(dd1))

(yy2,mm2,dd2) = (int(stag2)/10000,(int(stag2)%10000)/100,int(stag2)%100)
tag2 = datetime.date(int(yy2), int(mm2), int(dd2))

span = tag2.toordinal() - tag1.toordinal()
if (span < 0):
  dt = datetime.timedelta(-1)
  span *= -1
else:
  dt = datetime.timedelta(1)

tag = tag1
for i in range(0, span+1):
  print(tag)
#do stuff
  tag += dt


