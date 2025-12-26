'open vsdblike_03-21.ctl'
'open vsdblike_06-00.ctl'

'set t 1 last'
*total times: ntime=4times/day*90days=360
* Get the dimension environment
'q dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,11)
time2 = subwrd(line5,13)
*ntime = subwrd(line5,13)
*ntime=120

* define new variables which need be plotted for verftime=03,09,15,21
'set t 1 'time2''
'set z 1'
'define lhrms1=sqrt(lhtfl(z=4)-2*lhtfl(z=3)+lhtfl(z=5))'
'define lhdif1=lhtfl(z=1)-lhtfl(z=2)'
'define shrms1=sqrt(shtfl(z=4)-2*shtfl(z=3)+shtfl(z=5))'
'define shdif1=shtfl(z=1)-shtfl(z=2)'

* calculate rms of defined new variables
'define avelhrms1=ave(lhrms1,t=1,t='time2')'
'define sdlhrms1=sqrt(ave(pow((lhrms1-avelhrms1),2),t=1,t='time2')))'
'define avelhdif1=ave(lhdif1,t=1,t='time2')'
'define sdlhdif1=sqrt(ave(pow((lhdif1-avelhdif1),2),t=1,t='time2')))'

'define aveshrms1=ave(shrms1,t=1,t='time2')'
'define sdshrms1=sqrt(ave(pow((shrms1-aveshrms1),2),t=1,t='time2')))'
'define aveshdif1=ave(shdif1,t=1,t='time2')'
'define sdshdif1=sqrt(ave(pow((shdif1-aveshdif1),2),t=1,t='time2')))'

* define new variables which need be plotted for verftime=06,12,18,00
'set dfile 2'
'set t 1 'time2''
'set z 1'
'define lhrms2=sqrt(lhtfl(z=4)-2*lhtfl(z=3)+lhtfl(z=5))'
'define lhdif2=lhtfl(z=1)-lhtfl(z=2)'
'define shrms2=sqrt(shtfl(z=4)-2*shtfl(z=3)+shtfl(z=5))'
'define shdif2=shtfl(z=1)-shtfl(z=2)'

* calculate rms of defined new variables
'define avelhrms2=ave(lhrms2,t=1,t='time2')'
'define sdlhrms2=sqrt(ave(pow((lhrms2-avelhrms2),2),t=1,t='time2')))'
'define avelhdif2=ave(lhdif2,t=1,t='time2')'
'define sdlhdif2=sqrt(ave(pow((lhdif2-avelhdif2),2),t=1,t='time2')))'

'define aveshrms2=ave(shrms2,t=1,t='time2')'
'define sdshrms2=sqrt(ave(pow((shrms2-aveshrms2),2),t=1,t='time2')))'
'define aveshdif2=ave(shdif2,t=1,t='time2')'
'define sdshdif2=sqrt(ave(pow((shdif2-aveshdif2),2),t=1,t='time2')))'

'set dfile 1'
plottime=time2+1
'set t 1 'plottime''
'enable print plotvsdblike.gmf'

vpheight=1.2
*'vpheight' is the only variable maybe need to be changed
*here vpheight=(11-2*0.5)/8~=1.1, 8 is the number of total charts

vpheightfst=vpheight+0.5
vplow=0
vptop=vplow+vpheightfst
plotlow=0.5
plottop=vpheightfst-0.2

'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 'plotlow' 8 'plotlow''
'draw line 1 'plotlow' 1 'vpheightfst''
'draw string 1.2 'plottop' sqrt(mean(RR**2)-2mean(RR*GR)+mean(GR**2))[latent heat,verftime 03,09,15,21]'
'set grads off'
'set frame off'
'set grid off'
'q defval avelhrms1 1 1'
aa=subwrd(result,3)
'q defval sdlhrms1 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd avelhrms1+3*sdlhrms1'
'set cmark 0'
'd avelhrms1-3*sdlhrms1'
'set cmark 0'
'd lhrms1'

vplow=vptop
vptop=vplow+vpheight
plotlow=0
plottop=vpheight-0.2
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 1.2 'plottop' sqrt(mean(RR**2)-2mean(RR*GR)+mean(GR**2))[latent heat,verftime 06,12,18,00]'
'set grads off'
'set frame off'
'set grid off'
'set xlab off'
'q defval avelhrms2 1 1'
aa=subwrd(result,3)
'q defval sdlhrms2 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd avelhrms2+3*sdlhrms2'
'set cmark 0'
'd avelhrms2-3*sdlhrms2'
'set cmark 0'
'd lhrms2'
'set cmark 0'
'd avelhrms2'

vplow=vptop
vptop=vplow+vpheight
plotlow=0
plottop=vpheight-0.2
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 2.7 'plottop' mean(RR-GR)[latent heat,verftime 03,09,15,21]'
'set grads off'
'set frame off'
'set grid off'
'set xlab off'
'q defval avelhdif1 1 1'
aa=subwrd(result,3)
'q defval sdlhdif1 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd avelhdif1+3*sdlhdif1'
'set cmark 0'
'd avelhdif1-3*sdlhdif1'
'set cmark 0'
'd lhdif1'
'set cmark 0'
'd avelhdif1'

vplow=vptop
vptop=vplow+vpheight
plotlow=0
plottop=vpheight-0.2
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 2.7 'plottop' mean(RR-GR)[latent heat,verftime 06,12,18,00]'
'set grads off'
'set frame off'
'set grid off'
'set xlab off'
'q defval avelhdif2 1 1'
aa=subwrd(result,3)
'q defval sdlhdif2 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd avelhdif2+3*sdlhdif2'
'set cmark 0'
'd avelhdif2-3*sdlhdif2'
'set cmark 0'
'd lhdif2'
'set cmark 0'
'd avelhdif2'

vplow=vptop
vptop=vplow+vpheight
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 1.2 'plottop' sqrt(mean(RR*2)-2mean(RR*GR)+mean(GR**2))[sensible heat,verftime 03,09,15,21]'
'set grads off'
'set frame off'
'set grid off'
'q defval aveshrms1 1 1'
aa=subwrd(result,3)
'q defval sdshrms1 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd shrms1'
'set cmark 0'
'd aveshrms1'
'set cmark 0'
'd aveshrms1+3*sdshrms1'
'set cmark 0'
'd aveshrms1-3*sdshrms1'

vplow=vptop
vptop=vplow+vpheight
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 1.2 'plottop' sqrt(mean(RR*2)-2mean(RR*GR)+mean(GR**2))[sensible heat,verftime 06,12,18,00]'
'set grads off'
'set frame off'
'set grid off'
'q defval aveshrms2 1 1'
aa=subwrd(result,3)
'q defval sdshrms2 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd shrms2'
'set cmark 0'
'd aveshrms2'
'set cmark 0'
'd aveshrms2+3*sdshrms2'
'set cmark 0'
'd aveshrms2-3*sdshrms2'

vplow=vptop
vptop=vplow+vpheight
plotlow=0
plottop=vpheight-0.2
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 2.7 'plottop' mean(RR-GR)[sensible heat,verftime 03,09,15,21]'
'set grads off'
'set frame off'
'set grid off'
'set xlab off'
'q defval aveshdif1 1 1'
aa=subwrd(result,3)
'q defval sdshdif1 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd aveshdif1+3*sdshdif1'
'set cmark 0'
'd aveshdif1-3*sdshdif1'
'set cmark 0'
'd shdif1'
'set cmark 0'
'd aveshdif1'

vplow=vptop
vptop=vplow+vpheight
plotlow=0
plottop=vpheight-0.2
'set vpage 0 8.5 'vplow' 'vptop''
'set parea 1 8 'plotlow' 'plottop''
'draw line 1 0 1 'vpheight''
'draw string 2.7 'plottop' mean(RR-GR)[sensible heat,verftime 06,12,18,00]'
'set grads off'
'set frame off'
'set grid off'
'set xlab off'
'q defval aveshdif2 1 1'
aa=subwrd(result,3)
'q defval sdshdif2 1 1'
bb=subwrd(result,3)
y1=aa-5*bb
y2=aa+5*bb
'set vrange 'y1' 'y2''
'set ylopts 1 4 0.05'
'set cmark 0'
'd aveshdif2+3*sdshdif2'
'set cmark 0'
'd aveshdif2-3*sdshdif2'
'set cmark 0'
'd shdif2'
'set cmark 0'
'd aveshdif2'

'print'

'disable print plotvsdblike.gmf'
'printim g2g.png'
*'reinit'

'quit'
