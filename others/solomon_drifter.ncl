begin
      
 dummy1 =  new(400,graphic)
 dummy2 =  new(400,graphic)

 wks = gsn_open_wks("ps","DriftTrac")
 gsn_define_colormap(wks,"amwg")

 res = True

 res@gsnSpreadColors     = True                ; use full colormap
 res@gsnSpreadColorStart = 2 
 res@gsnSpreadColorEnd   = -3 

 res@mpFillOn = False 
 res@lbOrientation = "Vertical"
 
 res@gsnFrame = False
 res@gsnDraw = False

 res@lbLabelBarOn         = True ; no individual label bars

 pres = True
 pres@gsnMaximise = True

  res@mpProjection          = "Stereographic"

  i = NhlNewColor(wks,0.7,0.7,0.7)              ; add gray to colormap

  res@mpGridAndLimbOn        = True
  res@mpGridAndLimbDrawOrder = "PostDraw"
  res@mpGridMaskMode         = "MaskNotOcean"
  res@mpFillOn               = True

    res@mpLimitMode       = "Corners"
    res@mpLeftCornerLatF  =  88.0
    res@mpLeftCornerLonF  = 160.0
    res@mpRightCornerLatF =  88.0
    res@mpRightCornerLonF = 340.0
;;arctic ocean
;;   res@mpLeftCornerLatF  =  64.0
;;    res@mpLeftCornerLonF  = 160.0
;;    res@mpRightCornerLatF =  76.0
;;    res@mpRightCornerLonF = 340.0
;;alaska region
;;    res@mpLeftCornerLatF  =  67.0
;;    res@mpLeftCornerLonF  = 190.0
;;    res@mpRightCornerLatF =  78.0
;;    res@mpRightCornerLonF = 235.0
    res@mpLandFillColor      = "grey"

    res@mpMinLatF = 71
    res@mpCenterLonF = 210

    res@mpGeophysicalLineThicknessF = 0.0

    res@cnFillOn = True
    res@cnLinesOn = False 

    res@gsnScalarContour = True
    res@vcRefMagnitudeF       = 8.0              ; vec magnitude
    res@vcRefLengthF          = 0.05             ; size of ref vector
    res@vcMonoLineArrowColor  = True
    res@vcMinDistanceF        = 0.025              ; thins arrows near pole

    opts_txres = True
    opts_txres@txFontHeightF = 0.015

    res@cnFillColors = (/0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,16,16/)
    res@cnMonoLineColor = False
    res@cnMonoLineLabelFontColor = False
    res@cnLineColors = (/0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,16,16/)
    res@cnLineLabelFontColors = (/0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,16,16/)

    res@cnLevelSelectionMode = "ExplicitLevels"     ; set manual contour levels
    res@cnLevels = 0.1*(/0.01,0.1,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14./)

    res2 = res
    res2@cnLinesOn = True 
    res2@cnFillColors = (/3,4,6,8,10,12,14,15,16,17,10,11,12,13,14,15,17,16/)
    res2@cnLineColors = (/3,4,6,8,10,12,14,15,16,17,10,11,12,13,14,15,17,16/)
    res2@cnLineLabelFontColors = (/3,4,6,8,10,12,14,15,16,17,10,11,12,13,14,15,17,16/)
    res2@cnLevelSelectionMode = "ExplicitLevels"     ; set manual contour levels
    res2@cnLevels = 0.01*(/0.01,0.1,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14./)
    res2@cnFillDrawOrder = "PostDraw"
    res2@gsnContourLineThicknessesScale = 5.0
    res2@cnFillOn = False
    res2@cnLineLabelsOn       =   False
    res2@cnInfoLabelOn       =   False

    pres2                  = True               ; polyline resources
    pres2@gsLineThicknessF = 8.0                ; line thickness
    pres2@gsLineColor = "black"

    mres = True
    mres@gsMarkerIndex  = 5           ; marker style 
    mres@gsMarkerSizeF  = 12.0          ; marker size
    mres@gsMarkerThicknessF  = 2.0          ; marker size
    mres@gsMarkerColor  = "black"      ; maker color

ncol=13
buoyname =  "300234066036110"
buoytxt = "buoy6.txt"
;;buoyname =  "300234066031190"
;;buoytxt = "buoy5.txt"
;;buoyname =  "300234066030330"
;;buoytxt = "buoy4.txt"
;;buoyname =  "300234066030190"
;;buoytxt = "buoy14.txt"

;;ncol=12
;;buoyname =  "300234061872720"
;;buoytxt = "buoy13.txt"
;;buoyname =  "300234062880820"
;;buoytxt = "buoy10.txt"
;;buoyname =  "300234065802030"
;;buoytxt = "buoy12.txt"
;;buoyname =  "300234065801030"
;;buoytxt = "buoy11.txt"
;;buoyname =  "300234065495020"
;;buoytxt = "buoy2.txt"
;;buoyname =  "300234063991680"
;;buoytxt = "buoy3.txt"
;;buoyname =  "300234063803010"
;;buoytxt = "buoy1.txt"

;;ncol=10
;;buoyname =  "300234066830700" ;;first day 106
;;buoytxt = "buoy9.txt"


buoy     = asciiread(buoytxt,(/1,ncol/),"float")

    do icnt=0,0

    addday = -3 + icnt
    d1 = 0 + addday 
    d2 = 10 + addday

    if (d2.eq.0) then
    FullDate2  = systemfunc("date '+%F'")
    day2   = stringtointeger(systemfunc("date '+%d'"))
    mon2       = stringtointeger(systemfunc("date '+%m'"))
    else
    FullDate2  = systemfunc("date -v '"+d2+"d' '+%F'")
    day2   = stringtointeger(systemfunc("date -v '"+d2+"d' '+%d'"))
    mon2       = stringtointeger(systemfunc("date -v '"+d2+"d' '+%m'"))
    end if

    if (d1.eq.0) then
    FullDate   = systemfunc("date '+%F'")
    day1   = stringtointeger(systemfunc("date '+%d'"))
    day3   = stringtointeger(systemfunc("date '+%j'"))
    mon1       = stringtointeger(systemfunc("date '+%m'"))
    year1       = stringtointeger(systemfunc("date '+%Y'"))
    else
    FullDate   = systemfunc("date -v '"+d1+"d' '+%F'")
    day1   = stringtointeger(systemfunc("date -v '"+d1+"d' '+%d'"))
    day3   = stringtointeger(systemfunc("date -v '"+d1+"d' '+%j'"))
    mon1       = stringtointeger(systemfunc("date -v '"+d1+"d' '+%m'"))
    year1       = stringtointeger(systemfunc("date  -v '"+d1+"d' '+%Y'"))
    end if

    FullDate3 = FullDate+"-00000"      

    print((icnt+" "+FullDate))
    print((/buoy(icnt,6)/))
     
time=12 ;divide the 6-hour output in 30 minute steps

n=0

P0t = new((/2,3/),float)


P0t(0,0)=buoy(icnt,9)
if (P0t(0,0).lt.0.0) then
P0t(0,0) = P0t(0,0) + 360.0
end if

P0t(1,0)=buoy(icnt,8)

P0 = new((/492,2,3/),float)
driftX = new((/492,3/),float)
driftY = new((/492,3/),float)
driftX=0.0
driftY=0.0

print(P0t(1,0)+" "+P0t(0,0))

d2r = 3.14159265359/180.0

P0 = 0.0
P0(0,:,:)=P0t
P=P0(0,:,:)

    fh = 0
    idum = 0

    do i1 = d1,d2

    if (i1.eq.0) then
      day2   = stringtointeger(systemfunc("date '+%d'"))
      iyear       = stringtointeger(systemfunc("date '+%Y'"))
    else if (i1.lt.0) then
      day2   = stringtointeger(systemfunc("date -v '"+i1+"d' '+%d'"))
      iyear       = stringtointeger(systemfunc("date -v '"+i1+"d' '+%Y'"))
    else 
      day2   = stringtointeger(systemfunc("date -v +'"+i1+"d' '+%d'"))
      iyear       = stringtointeger(systemfunc("date -v '"+i1+"d' '+%Y'"))
    end if
    end if

    if (day2.lt.day1) then
       imon = mon2
    else
       imon = mon1
    end if

    if (i1.eq.d1) then
      x1=21600
      x2=84600
    else if (i1.eq.d2) then
      x1=0
      x2=0
    else
      x1=0
      x2=84600
    end if 
    end if 

    do j1 = x1,x2,21600

    fh = fh + 6
    idum = idum + 1

    if (imon.lt.10) then

    if (day2.lt.10) then
    if (j1.eq.0) then
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-0"+imon+"-0"+day2+"-00000.nc"
      FullDate2 = iyear+"-0"+imon+"-0"+day2+"-00000"      
    else
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-0"+imon+"-0"+day2+"-"+j1+".nc"
      FullDate2 = iyear+"-0"+imon+"-0"+day2+"-"+j1      
    end if
    else
    if (j1.eq.0) then
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-0"+imon+"-"+day2+"-00000.nc"
      FullDate2 = iyear+"-0"+imon+"-"+day2+"-00000"      
    else
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-0"+imon+"-"+day2+"-"+j1+".nc"
      FullDate2 = iyear+"-0"+imon+"-"+day2+"-"+j1      
    end if
    end if

    else

    if (day2.lt.10) then
    if (j1.eq.0) then
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-"+imon+"-0"+day2+"-00000.nc"
      FullDate2 = iyear+"-"+imon+"-0"+day2+"-00000"      
    else
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-"+imon+"-0"+day2+"-"+j1+".nc"
      FullDate2 = iyear+"-"+imon+"-0"+day2+"-"+j1      
    end if
    else
    if (j1.eq.0) then
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-"+imon+"-"+day2+"-00000.nc"
      FullDate2 = iyear+"-"+imon+"-"+day2+"-00000"      
    else
      srcGridName = "/Volumes/RASM-ESRL/forecasts_2018/ice/"+FullDate+"/REB2.cice.h_06h."+iyear+"-"+imon+"-"+day2+"-"+j1+".nc"
      FullDate2 = iyear+"-"+imon+"-"+day2+"-"+j1      
    end if
    end if

    end if

;;    print(srcGridName+" ")

    src_file = addfile(srcGridName,"r")
    ilon    = src_file->TLON
    ilat    = src_file->TLAT

    apond   = src_file->apond_h(0,:,:)
    hi      = src_file->hi_h(0,:,:)
    hs      = src_file->hs_h(0,:,:)

    utmp   = src_file->uatm_h(0,:,:)
    vtmp   = src_file->vatm_h(0,:,:)
    anglet = src_file->ANGLET
    uatm = utmp*cos(anglet) - vtmp*sin(anglet)
    vatm = vtmp*cos(anglet) + utmp*sin(anglet)
    delete(utmp)
    delete(vtmp)
    uatm@long_name = "CICE surface winds"
    uatm@units    = "m/s"
    vatm@long_name = "CICE meridional current"
    vatm@units    = "m/s"

    utmp  = src_file->uvel_h(0,:,:)
    vtmp  = src_file->vvel_h(0,:,:)   
    mice  = sqrt(utmp*utmp+vtmp*vtmp)
    mice@long_name = "ice speed"
    mice@units    = "m/s"
    angle = src_file->ANGLE
    uice = 1.0*(utmp*cos(angle) - vtmp*sin(angle))
    vice = 1.0*(vtmp*cos(angle) + utmp*sin(angle))
    delete(utmp)
    delete(vtmp)
    uice@lat2d = ilat
    uice@lon2d = ilon
    vice@lat2d = ilat
    vice@lon2d = ilon
    
do h=0,time-1

n=n+1
   
do h2 = 0 , 0
;%%%%Interpolation

distances=(sqrt((ilon-P(0,h2))^2.0 + (ilat-P(1,h2))^2));

dims = dimsizes(distances)
x1d = ndtooned(distances)      ; convert 2D array to 1D for use in minind
inds = ind_resolve(minind (x1d), dims)    ; convert 1D array back to 2D 
i2 = inds(0,0)        ; select the latitude index where the X array is at its' minimum  
i3 = inds(0,1)        ; select the longitude index where the X array is at its' minimum  

if (i3.gt.1) then
uc_interp=uice(i2-1:i2+1,i3-1:i3+1)
vc_interp=vice(i2-1:i2+1,i3-1:i3+1)

uc2=rcm2points(ilat(i2-1:i2+1,i3-1:i3+1),ilon(i2-1:i2+1,i3-1:i3+1),uc_interp,P(1,h2),P(0,h2),2);
vc2=rcm2points(ilat(i2-1:i2+1,i3-1:i3+1),ilon(i2-1:i2+1,i3-1:i3+1),vc_interp,P(1,h2),P(0,h2),2);

;%%%%advance the position in time
m_per_deg_lat = 111132.92 - 559.82*cos(2*ilat(i2,i3)*d2r) + 1.175*cos(4*ilat(i2,i3)*d2r) - 0.0023*cos(6*ilat(i2,i3)*d2r)
m_per_deg_lon = 111412.84*cos(ilat(i2,i3)*d2r) - 93.5*cos(3*ilat(i2,i3)*d2r) + 0.118*cos(5*ilat(i2,i3)*d2r)
      
P(0,h2)=P0(n-1,0,h2) + (uc2/m_per_deg_lon)*30.0*60.0
P(1,h2)=P0(n-1,1,h2) + (vc2/m_per_deg_lat)*30.0*60.0

P0(n,:,h2)=P(:,h2)
end if

end do
end do

driftX(0:n,:)=P0(0:n,0,:)
driftY(0:n,:)=P0(0:n,1,:)

    end do
    end do
  
if (icnt.eq.0) then
    hi@lat2d = ilat
    hi@lon2d = ilon
    plot1 = gsn_csm_contour_map(wks,hi,res)
end if

   pres2@gsLineColor = icnt+2
   dummy1(icnt)=gsn_add_polyline(wks,plot1,driftX(0:480:48,0),driftY(0:480:48,0),pres2)      
   dummy2(icnt)=gsn_add_polymarker(wks,plot1,driftX(0,0),driftY(0,0),mres)
    
  days  = fspan(0.0,10,11)

  tmp1    = new((/11/),"integer")
  tmp2    = new((/11/),"float")
  tmp3    = new((/11/),"float")
  tmp4    = new((/11/),"float")
  tmp1 = year1
  tmp2 = day3 + days(0:10)
  tmp4 = driftX(0:480:48,0)
  tmp3 = driftY(0:480:48,0)

  tmp4 = where(tmp4.gt.180,tmp4-360.0,tmp4)
  P0t(0,0) = where(P0t(0,0).gt.180,P0t(0,0)-360.0,P0t(0,0))

  cr = tochar(10)
  opt = True
  if (day3.lt.10) then
  fname = "esrl001_SeaIceVelocity_"+buoyname+"_"+year1+"-00"+day3+".000_001.txt"
  header = "GroupID: esrl001"+cr+"MethodID: SeaIceVelocity"+cr+"TargetID: "+buoyname+cr+"InitYear: "+year1+""+cr+"InitDayOfYear: 00"+day3+".000"+cr+"InitLat: "+ P0t(1,0)+cr+"InitLon: "+P0t(0,0)+cr+"EnsMemNum: 001"+cr+"### end of header"+cr+"Year        DayOfYear      Lat          Lon"
  else if (day3.lt.100) then
  fname = "esrl001_SeaIceVelocity_"+buoyname+"_"+year1+"-0"+day3+".000_001.txt"
  header = "GroupID: esrl001"+cr+"MethodID: SeaIceVelocity"+cr+"TargetID: "+buoyname+cr+"InitYear: "+year1+""+cr+"InitDayOfYear: 0"+day3+".000"+cr+"InitLat: "+ P0t(1,0)+cr+"InitLon: "+P0t(0,0)+cr+"EnsMemNum: 001"+cr+"### end of header"+cr+"Year        DayOfYear      Lat          Lon"
  else
  fname = "esrl001_SeaIceVelocity_"+buoyname+"_"+year1+"-"+day3+".000_001.txt"
  header = "GroupID: esrl001"+cr+"MethodID: SeaIceVelocity"+cr+"TargetID: "+buoyname+cr+"InitYear: "+year1+""+cr+"InitDayOfYear: "+day3+".000"+cr+"InitLat: "+ P0t(1,0)+cr+"InitLon: "+P0t(0,0)+cr+"EnsMemNum: 001"+cr+"### end of header"+cr+"Year        DayOfYear      Lat          Lon"
  end if
  end if
  
  fmtx = "i4,6x,f8.3,8x,f8.5,5x,f10.5"
 
  hlist = [/header/]
  alist = [/tmp1,tmp2,tmp3,tmp4/]

  write_table(fname,"w",hlist,"%s")
  write_table(fname,"a",alist,"%4i%13.3f%16.5f%14.5f")

end do      

    gsn_panel(wks,(/plot1/),(/1,1/),pres)

return

 end

      
