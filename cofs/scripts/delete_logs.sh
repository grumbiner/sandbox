#! /bin/sh
# delete_logs.sh 
# This shell script uses the unix 'find' command
# to delete any "daily_update*log" files older than 20 days
# August 2004   William O'Connor 
#---------------------------------------------------------
set -x 
work_dir='/usr1/cfspom/logs'
echo 'work_dir = ' ${work_dir}
cd ${work_dir}
pwd  
#
find ${work_dir} -name "daily_update*log" -atime +20 -exec rm {} \;  

# end 
