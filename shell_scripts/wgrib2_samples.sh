fn=gep01.t00z.pgrb2a_bcf384
grep U ${fn}.idx | grep ground | wgrib2 -i $fn -ijbox 1:360   181:181 fred text
grep U ${fn}.idx | grep ground | wgrib2 -i $fn -lola  0:360:1 90:1:1 george text
