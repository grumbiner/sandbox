all : main 
#gfs_obs

main : main.C
	g++ -Wall -O2 -DLINUX -I ~/usrlocal/mmablib/include $(@).C -o $(@) 

liquidmos : liquidmos.C
	g++ -Wall -O2 -DLINUX -I ~/usrlocal/mmablib/include $(@).C -o $(@) 

gfs_obs : gfs_obs.c
	gcc $< -O2 -Wall -o $(@)
