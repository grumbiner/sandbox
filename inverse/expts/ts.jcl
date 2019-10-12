cd /ptmp1/wd21rg


echo 29 > tin5
for tag in 0915 0916 0917 0918 0919 0920 0921 0922 0923 0924 \
           0925 0926 0927 0928 0929 0930 1001 1002 1002 1004 \
           1005 1006 1007 1008 1009 1010 1011 1012 1013
do
  tar xvf filt2.94$tag revi.94$tag r0.94$tag
  echo revi.94$tag >> tin5
  echo r0.94$tag >> tin5
done
 
time $HOME3/inverse/overhead/ts tin5 tout5
