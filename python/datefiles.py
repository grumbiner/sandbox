import sys
import os
import datetime

#arguments are 2 8 digit dates, start and end.
#may run up or down
stag1 = sys.argv[1]
stag2 = sys.argv[2]
print(os.getenv('LOGNAME'))

(yy1,mm1,dd1) = (int(int(stag1)/10000),int((int(stag1)%10000)/100),int(stag1)%100)
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
  x=os.system('ls -l')
  print('returned from system ls with $? = ',x)

  y=os.environ
  print(os.getenv('LOGNAME'))  # part of standard environment

  y['etaval']='fred'
  print(os.getenv('etaval'))  # our run-time environment

  if (not os.path.isdir('beta')):
    os.mkdir('beta')
  os.chdir('beta')

  for sat in ('avhrr-only', 'avhrr+amsre'):
    print(sat)

  if (os.path.isfile('avhrr-only.'+tag.strftime('%Y%m%d')) ):
    print('working with avhrr-only.'+tag.strftime('%Y%m%d'))
  else:
    print('could not find avhrr-only.'+tag.strftime('%Y%m%d'))

# done doing stuff
  os.chdir('..')
  tag += dt


