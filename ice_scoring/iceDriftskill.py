'''Victor Ogunbamiwo, 2/1/17
    Outputs skill scores to log given a directory of sk2 files.
    Python 2.6+ and Python 3.0+
Intern with Robert Grumbine

Reads in a directory containing sk2 files. Get intial coordinates,
lead, X coordinates, distance for each lead time.
Then write those arrays/data to file.
Does this for a whole month or year based on amount of sk2 files given.
Then runs skillscorecalc on data log after reading in data and writes to log.
Max. Distance for point and buoy match = 50 km'''

import sys
from sys import version_info
import math
import glob
import time
import importlib
import calendar
import numpy as np
import scipy.spatial.distance as spsd
import scipy.stats as sps
#from geographiclib.geodesic import Geodesic

print("To set up f2py modules: In working directory run")
print("f2py<f2py3.5 if python3> -c --fcompiler=gnu95 -m <choose_name> <name_of_fortran_script>")
print("Works in current directory, if not in same dir as fortran function type full path")
print("Default config file name is 'SIDS.config\nprogram needs python package geographiclib installed'")

py3 = version_info[0] > 2

if py3:
    configf = input('Full path of config file?: ') #Default name of config file is SIDS.config
else:
    configf = raw_input('Full path of config file?: ')

configfile = open(configf, 'r')
for line in configfile:
    if 'containing' in line[:57]:
        pathm = line[57:].strip()
    elif '(arcdis)' in line[:42]:
        modnamea = line[42:].strip()
    elif '(vcc)' in line[:38]:
        modnamev = line[38:].strip()
    elif 'C20XX' in line[:24]:
        pathc = line[24:].strip()
    elif 'sk2' in line[:32]:
        pathmf = line[32:].strip()
    elif 'forecast' in line[:34]:
        pathfp = line[34:].strip()
    elif 'log' in line[:35]:
        pathlog = line[35:].strip()

sys.path.insert(0, pathm)

if py3:
    moda = importlib.import_module(modnamea, package=None)
    modv = importlib.import_module(modnamev, package=None)
else:
    moda = __import__(modnamea)
    modv = __import__(modnamev)

start_time = time.time()  #Timings
listofsk2files = glob.glob('{}*'.format(pathmf))
fpfile = open(pathfp, 'r') #1-207 points file
Cfile = open(pathc, 'r') #C<YEAR> file
logfile = open('{}adist.log'.format(pathlog), 'w') #raw data log for points <207
logfile2 = open('{}SIDS.log'.format(pathlog), 'w') #Skill score set log
logfileo = open('{}adistt2.log'.format(pathlog), 'w')#raw data log for points >207


def retrievepointdata():
    modelfile.seek(0)
    fpfile.seek(0)
    datapt = []   #array of point number; should match up with dataloc entries
    dataloc = [[], []]     #Latitude --0 Longtitude --1
    catcher = '48-Hour Forecast ice drift'
    for i in range(5):  #Skip first 6 lines of file
        modelfile.readline()
    for line in modelfile:
        line = line.strip()
        if catcher in line:
            break
        if 'Point' in line:
            if py3:
                line = next(modelfile)
            else:
                line = modelfile.next()
            point = int(line[0:5])
        if len(line[0:5].strip()) is 0:
            pass
        else:
            try:
                point = int(line[0:5])
            except ValueError:
                if py3:
                    line = next(modelfile)
                else:
                    line = modelfile.next()
                point = int(line[0:5])
        if point < 207:
            for line2 in fpfile:
                dataloc[1].append(float(line2[21:30].strip()))
                dataloc[0].append(float(line2[6:15]))
                datapt.append(int(line2[0:3].strip()))
        if point > 207 and len(line[8:15].strip()) is not 0:
            dataloc[1].append(float(line[8:15]))
            dataloc[0].append(float(line[17:23].strip()))
            datapt.append(point)
    return dataloc, datapt

def rmse(rms_arr):
    tot = 0
    for i, ob in enumerate(rms_arr[0]):
        tot += (rms_arr[1][i] - ob) ** 2
    tot /= len(rms_arr[0])
    rmse = np.sqrt(tot)
    return rmse

def skillscorecalc(ad, adob, sortrms, ad2, length_arr):
    #Vector Correlation
    comob = [[], []] #0=a 1=b
    compr = [[], []]
    for i, x in enumerate(sortrms[0]):
        comob[0].append(ad2[0][i]*math.sin(x)) #a
        comob[1].append(ad2[0][i]*math.cos(x)) #b
    for i, x in enumerate(sortrms[1]):
        compr[0].append(ad2[1][i]*math.sin(x)) #side a
        compr[1].append(ad2[1][i]*math.cos(x)) #side b
    comob0 = np.array(comob)
    compr0 = np.array(compr)
    comob0 = np.asfortranarray(comob0, dtype=np.float64)
    compr0 = np.asfortranarray(compr0, dtype=np.float64)
    vcorr = modv.vcc(comob0[0], comob0[1], compr0[0], compr0[1], length_arr)
    #Distance Correlation
    discorr = spsd.correlation(ad2[1], ad2[0])
    #Error Radius
    errad = (sum(ad[1])/length_arr)
    #RMS Direction Error
    rmsde = rmse(sortrms) % 360
    #Slope of Regression
    slope, intecept, rrval, rpval, stder = sps.linregress(ad2[0], ad2[1])
    print('V.corr.: {}\nDis.corr.: {}\nError Radius: {}\nRmsde(degrees): {}\nSlope: {}\nY-Int.: {}'.format(vcorr, discorr, errad, rmsde, slope, intecept))
    return vcorr, discorr, rmsde, slope, intecept, errad

def findpointcalc(point, dayend):
    point = str(point)
    modelfile.seek(0)
    y = 1
    w = ''
    for line in modelfile:
        if point in line.strip()[0:len(point)]:
            if (line.strip()[len(point)]).isspace():
                results = [float(i) for i in line.split()]
                if y == dayend:
                    w = results
                    return w
                y += 1
    return w

#d= r in polar coordinate
#theta = theta of polar coordinate
#Replace haversine and haversine2 with geographiclib for final lat,lon, bearing, and possibly distance(with would replace arcdis) OR make 2 versions with and without geographiclib.
def haversine(lat1, long1, d, theta):
    r = 6370.949 #earth's radius
    d *= 1.852 #nautial miles to km
    theta = math.radians(theta)
    lat2 = math.asin(math.sin(lat1) * math.cos(d/r) + math.cos(lat1)*math.sin(d/r) * math.cos(theta))
    long2 = long1 + math.atan2(math.sin(theta) * math.sin(d/r) * math.cos(lat1), math.cos(d/r)-math.sin(lat1)* math.sin(lat2))
    lat2 = math.degrees(lat2)
    long2 = math.degrees(long2)
    if long2 < 0:
        long2 += 360
    return lat2, long2
    
def haversine2(lat1, long1, lat2, long2):
    lat1 = math.radians(lat1)
    long1 = math.radians(long1)
    lat2 = math.radians(lat2)
    long2 = math.radians(long2)
    y = math.sin(long2-long1) * math.cos(lat2)
    x = math.cos(lat1) * math.sin(lat2) - math.sin(lat1) * math.cos(lat2) * math.cos(long2-long1)
    brng = math.degrees(math.atan2(y, x))
    if brng < 0:
        brng += 360
    return brng #brng in degrees

def getenddrift(point, dayend):
    endl = findpointcalc(point, dayend)
    if int(point) > 207:
        fdir = endl[3]
        fdist = endl[4]
    else:
        fdir = endl[1]
        fdist = endl[2]
    #fdist *= 1852 # nmi to meters
    if int(point) > 207:
        lat2, long2 = haversine(math.radians(endl[2]), math.radians(endl[1]), fdist, fdir)
        #long2 = Geodesic.WGS84.Direct(endl[2], endl[1], fdist, fdir)['lon2']
        #lat2 = Geodesic.WGS84.Direct(endl[2], endl[1], fdist, fdir)['lat2']
    else:
        fpfile.seek(0)
        for line in fpfile:
            if point in line.strip()[0:len(point)]:
                result = [float(i) for i in line.split()] #flat1=result[1]  flong1=result[2]
                lat2, long2 = haversine(math.radians(result[1]), math.radians(result[2]), fdist, fdir)
               #long2 = Geodesic.WGS84.Direct(result[1], result[2], fdist, fdir)['lon2']
                #lat2 = Geodesic.WGS84.Direct(result[1], result[2], fdist, fdir)['lat2']
    #if long2 < 0:
        #long2 += 360
    return long2, lat2


def gettingobserv(mglong, mglat, mgmonth, mgday):
    maxdis = 50 / 1.852 # km to nautial miles, arcdis returns nautial miles
    distp2b = 27 #50 km = 26.9978 nmi
    Cfile.seek(0)
    for line in Cfile:
        mese = int(line[5:7])
        giorno = int(line[7:9])
        try:
            if mgmonth == mese and mgday == giorno:
                conv = float(line[38:48].strip())
                if conv < 0:
                    conv = round(float(conv)+360.0, 5)
                distp2b = moda.arcdis(conv, float(line[29:38]), mglong, mglat)
                if distp2b < maxdis and distp2b != -1:
                    matchedlat = float(line[28:38])
                    matchedlong = conv
                    matchedbid = int(line[12:29])
                    ihour = int(line[9:12])
                    return matchedlat, matchedlong, matchedbid, distp2b, mglong, mglat, ihour
        except ValueError:
            try:
                if mgmonth == mese and mgday == giorno:
                    try:
                        conv = float(line[39:49].strip()) #for when longtitude is -999.000
                    except ValueError: #break out of nested if; not for loop
                        break
                    if conv < 0:
                        conv = round(float(conv)+360.0, 5)
                    distp2b = moda.arcdis(conv, float(line[30:39]), mglong, mglat)
                    if distp2b < maxdis and distp2b != -1:
                        matchedlat = float(line[29:39])
                        matchedlong = conv
                        matchedbid = int(line[13:30])
                        ihour = int(line[10:12])
                        return matchedlat, matchedlong, matchedbid, distp2b, mglong, mglat, ihour
            except ValueError:
                return None

def arrofobserv(mgmonth, mgday, matbid):
    imgday = mgday
    imgmonth = mgmonth
    observ = [[], []]
    Cfile.seek(0)
    observlead = [[], []]
    for line in Cfile:
        try:
            if matbid == int(line[12:29]) and mgmonth == int(line[5:7]) and mgday == int(line[7:9]):
                if calendarcheck(leapyear, line, mgmonth, mgday, imgmonth, imgday):
                    return observ, observlead
                conv = float(line[38:48].strip())
                if conv < 0:
                    conv = round(float(conv)+360.0, 5)
                observ[0].append(float(line[28:38])) #latitude
                observ[1].append(conv)  #Longitude
                observlead[0].append(int(line[5])) #Month
                observlead[1].append(int(line[7:9])) # day
                mgday += 1
                mgmonth, mgday = endofmonthcheck(mgmonth, mgday)
                if mgday > 31 and mgmonth == 12:
                    return observ, observlead
            else:
                if len(observlead[1]) > 0:
                    if matbid == int(line[12:29]) and mgmonth == int(line[5:7]) and int(line[7:9]) not in observlead[1]:
                        if calendarcheck(leapyear, line, mgmonth, mgday, imgmonth, imgday):
                            return observ, observlead
                        conv = float(line[38:48].strip())
                        if conv < 0:
                            conv = round(float(conv)+360.0, 5)
                        observ[0].append(float(line[28:38])) #latitude
                        observ[1].append(conv)  #Longitude
                        observlead[0].append(int(line[5])) #Month
                        observlead[1].append(int(line[7:9])) # day
                        mgday = observlead[1][-1]+1
                        mgmonth, mgday = endofmonthcheck(mgmonth, mgday)
                        if mgday > 31 and mgmonth == 12:
                            return observ, observlead
        except ValueError:
            if matbid == int(line[13:29]) and mgmonth == int(line[5:7]) and mgday == int(line[7:9]):
                if calendarcheck(leapyear, line, mgmonth, mgday, imgmonth, imgday):
                    return observ, observlead
                conv = float(line[39:49].strip())
                if conv < 0:
                    conv = round(float(conv)+360.0, 5)
                observ[0].append(float(line[29:38]))    #latitude
                observ[1].append(conv)  #Longitude
                observlead[0].append(int(line[5])) #Month
                observlead[1].append(int(line[7:9])) # day
                mgday += 1
                mgmonth, mgday = endofmonthcheck(mgmonth, mgday)
                if mgday > 31 and mgmonth == 12:
                    return observ, observlead
            else:
                if len(observlead[1]) > 0:
                    if matbid == int(line[13:29]) and mgmonth == int(line[5:7]) and int(line[7:9]) not in observlead[1]:
                        if calendarcheck(leapyear, line, mgmonth, mgday, imgmonth, imgday):
                            return observ, observlead
                        conv = float(line[39:49].strip())
                        if conv < 0:
                            conv = round(float(conv)+360.0, 5)
                        observ[0].append(float(line[29:38]))#latitude
                        observ[1].append(conv)  #Longitude
                        observlead[0].append(int(line[5])) #Month
                        observlead[1].append(int(line[7:9])) # day
                        mgday = observlead[1][-1]+1
                        mgmonth, mgday = endofmonthcheck(mgmonth, mgday)
                        if mgday > 31 and mgmonth == 12:
                            return observ, observlead
    return observ, observlead

def calendarcheck(leapyear, line, mgmonth, mgday, imgmonth, imgday):
    if leapyear:
        febday = 29
    else:
        febday = 28
    if imgmonth != mgmonth:
        if (imgmonth%2 == 1 and imgmonth < 8) or (imgmonth%2 == 0 and imgmonth > 7):
            if imgday+16 < int(line[7:9])+31:
                return True
        elif (imgmonth%2 == 0 and imgmonth != 2 and imgmonth < 8) or (imgmonth%2 == 1 and imgmonth > 7):
            if imgday+16 < int(line[7:9])+30:
                return True
        elif imgmonth is 2:
            if imgday+16 < int(line[7:9])+febday:
                return True
    else:
        if imgday+16 < int(line[7:9]):
            return True
    return False

def endofmonthcheck(mgmonth, mgday):
    if leapyear:
        febday = 29
    else:
        febday = 28
    if (mgmonth%2 == 1 and mgmonth < 8) or (mgmonth%2 == 0 and mgmonth > 7):
        if mgday > 31:
            mgday = 1
            mgmonth += 1
    elif (mgmonth%2 == 0 and mgmonth != 2 and mgmonth < 8) or (mgmonth%2 == 1 and mgmonth > 7):
        if mgday > 30:
            mgday = 1
            mgmonth += 1
    elif mgmonth is 2:
        if mgday > febday:
            mgday = 1
            mgmonth += 1
    return mgmonth, mgday


def calcheck2(leapyear, mgmonth):
    if leapyear:
        febday = 29
    else:
        febday = 28
    if mgmonth%2 == 0:
        if mgmonth > 7:
            return 31
        elif mgmonth != 2 and mgmonth < 8:
            return 30
    if mgmonth%2 == 1:
        if mgmonth < 8:
            return 31
        else:
            return 30
    if mgmonth is 2:
        return febday

def arcdisforskill(observ, predict, mgmonth, mgday, disbtw, leadtime, point):
    adist = []
    adistob = [[], []]
    adist2 = [[], []]
    adistob[0].append(0) #observe
    adistob[1].append(0) #predict
    adist.append(disbtw)
    adist2[0].append(0) #observe
    adist2[1].append(0) #predict
    marking = []
    for z, it in enumerate(leadtime):
        if z != 0:
            adistmoda = moda.arcdis(observ[1][z], observ[0][z], predict[1][z], predict[0][z])
            obmoda = moda.arcdis(observ[1][z-1], observ[0][z-1], observ[1][z], observ[0][z])
            prmoda = moda.arcdis(predict[1][z-1], predict[0][z-1], predict[1][z], predict[0][z])
            adistob2 = moda.arcdis(observ[1][0], observ[0][0], observ[1][z], observ[0][z])
            adistpr2 = moda.arcdis(predict[1][0], predict[0][0], predict[1][z], predict[0][z])
            if obmoda < (200 / 1.852):
                adist.append(adistmoda)
                adistob[0].append(obmoda)
                adistob[1].append(prmoda)
                adist2[0].append(adistob2)
                adist2[1].append(adistpr2)
            else:
                print('Marking bad data')
                marking.append(z)
    for i in marking:
        observ[1][i] = None
        observ[0][i] = None
        predict[1][i] = None
        predict[0][i] = None
        leadtime[i] = None
    try:
        for x in range(len(observ[1])):
            while not observ[1][x]:
                del observ[1][x]
                del observ[0][x]
                del predict[0][x]
                del predict[1][x]
                del leadtime[x]
    except IndexError:
        pass
    return adist, adistob, observ, predict, leadtime, adist2

def getleadtimes(leapyear, observlead, lenofob):
    leadtime = []
    leadsize = 0
    leadtime.append(leadsize)
    for z in range(1, lenofob):
        if observlead[0][z] == observlead[0][z-1]: #same month
            leadsize = leadsize+(observlead[1][z]-observlead[1][z-1])
            leadtime.append(leadsize)
        else:
            leadsize = leadsize+(calcheck2(leapyear, observlead[0][z-1])-observlead[1][z-1])+(observlead[1][z])
            leadtime.append(leadsize)
    return leadtime

def getdata(): # This will retreive adist data from log into array for calc.
    logfile.seek(0)
    catcher3 = '=====Buoy ID'
    ender = 'Arcdi'
    ender2 = 'Predict la'
    ender3 = 'Predict lo'
    ender4 = 'Observe la'
    ender5 = 'Observe lo'
    ender6 = 'Adistob o'
    ender7 = 'Adistob p'
    ender8 = 'Adist2 o'
    ender9 = 'Adist2 p'
    ender10 = 'Rmsdir o'
    ender11 = 'Rmsdir p'
    robserv = [[], []]#0=lat 1=long
    rpredict = [[], []]
    radistob = [[], []]#0 observe #1 predict
    adist2 = [[], []]
    rmsdir = [[], []]
    #buoyid=[]
    forecastpoint = []
    adistarray = [[], []]  #0=leadsize 1=arcdis
    for cline in logfile:
        if catcher3 in cline:
            #buoyid.append(cline[9:25].strip())
            forecastpoint.append(int(cline[-5:].strip()))
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                adistarray[0].append(int(x))
        if ender in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                adistarray[1].append(float(x))
        if ender2 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                rpredict[0].append(float(x))
        if ender3 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                rpredict[1].append(float(x))
        if ender4 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                robserv[0].append(float(x))
        if ender5 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                robserv[1].append(float(x))
        if ender6 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                radistob[0].append(float(x))
        if ender7 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                radistob[1].append(float(x))
        if ender8 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                adist2[0].append(float(x))
        if ender9 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                adist2[1].append(float(x))
        if ender10 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                rmsdir[0].append(float(x))
        if ender11 in cline:
            cline = (logfile.readline()).strip()
            cline = cline.split(',')
            for x in cline[0:-1]:
                rmsdir[1].append(float(x))
    return adistarray, robserv, rpredict, radistob, adist2, forecastpoint, rmsdir

def getskill(b, adist, adistob, rmsdir, adist2):
    sortad = [[], []]
    #sortob = [[], []]
    #sortpr = [[], []]
    sortadob = [[], []]
    sortrms = [[], []]
    sadist2 = [[], []]
    for i, stu in enumerate(adist[0]):
        if stu == b:
            sortad[0].append(stu)
            sortad[1].append(adist[1][i])
            #sortob[0].append(observ[0][i])
            #sortob[1].append(observ[1][i])
            #sortpr[0].append(predict[0][i])
            #sortpr[1].append(predict[1][i])
            sortadob[0].append(adistob[0][i])
            sortadob[1].append(adistob[1][i])
            sortrms[0].append(rmsdir[0][i])
            sortrms[1].append(rmsdir[1][i])
            sadist2[0].append(adist2[0][i])
            sadist2[1].append(adist2[1][i])
    numofstuff = len(sortad[0])   #shouldnt matter which array is used for this
    vcorr, discorr, rmsde, slope, intecept, errad = skillscorecalc(sortad, sortadob, sortrms, sadist2, numofstuff)
    logfile2.write('{0}     {1}     {2}     {3}     {4}     {5}     {6}     {7}\n'.format(b, numofstuff, round(vcorr, 4), round(discorr, 4), round(rmsde, 4), round(slope, 5), round(intecept, 5), round(errad, 5)))
    return

#This will return skill by forecast point when done
def getfpskill(x, adist, adistob, rmsdir, adist2, fp):
    sortad = [[], []]
    #sortob = [[], []]
    #sortpr = [[], []]
    sortadob = [[], []]
    sortrms = [[], []]
    sadist2 = [[], []]
    if x in fp:
        i = fp.index(x)
        sortad[0].append(x)
        sortad[1].append(adist[1][i])
        #sortob[0].append(observ[0][i])
        #sortob[1].append(observ[1][i])
        #sortpr[0].append(predict[0][i])
        #sortpr[1].append(predict[1][i])
        sortadob[0].append(adistob[0][i])
        sortadob[1].append(adistob[1][i])
        sortrms[0].append(rmsdir[0][i])
        sortrms[1].append(rmsdir[1][i])
        sadist2[0].append(adist2[0][i])
        sadist2[1].append(adist2[1][i])
    else:
        return
    numofstuff = len(sortad[0])   #shouldnt matter which array is used for this
    print(numofstuff)
    vcorr, discorr, rmsde, slope, intecept, errad = skillscorecalc(sortad, sortadob, sortrms, sadist2, numofstuff)
    logfile2.write('{0}     {1}     {2}     {3}     {4}     {5}     {6}     {7}\n'.format(x, numofstuff, vcorr, discorr, rmsde, slope, intecept, errad))
    return

def rmsdir_create(ro, rp):
    rmsdir = [[], []] #0= observ #1=predict
    for coord, itd in enumerate(ro[0]):
        #x=Geodesic.WGS84.Inverse(ro[0][0], ro[1][0], ro[0][coord], ro[1][coord])['azi1']
        #if x == -180:
          #  x = 0
        #elif x < 0:
        #    x += 360
        rmsdir[0].append(haversine2(ro[0][0], ro[1][0], ro[0][coord], ro[1][coord]))
        #rmsdir[0].append(x)
    for cord, itd in enumerate(rp[0]):#could use theta values from getenddrift but writing and rereading text file to get thetas might be slower than doing haversine2. Test this and value computated
       #y=Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord])['azi1']
       # if y == -180:
            #y = 0
        #elif y < 0:
        #    y += 360
        rmsdir[1].append(haversine2(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord]))
        #rmsdir[1].append(y)
    return rmsdir
    
year = listofsk2files[0]
year = year[-12:]
leapyear = calendar.isleap(int(year[4:8]))
logfile2.write('Lead size  Observ.#  VCorr.  DCorr.  RMSDE   R.Slope   R.Int    Err.Radius\n')
logfile2.write('============================================================================\n')
predict = [[], [], []]
disbtw = 27
for f, file in enumerate(listofsk2files):
    modelfile = open(file, 'r')
    dataloc, datapt = retrievepointdata()
    print(file)
    mgdate = file[-12:]
    mgmonth = int(mgdate[8:10])
    mgday = int(mgdate[10:12])
    #start_time3=time.time()
    #print("--- %s seconds --- setup" % (time.time() - start_time))
    for i, item in enumerate(datapt):
        try:
            matlat, matlong, matbid, disbtw, mglong, mglat, ihour = gettingobserv(dataloc[1][i], dataloc[0][i], mgmonth, mgday)
            print('Buoy {0} matched to forecast point {1}'.format(matbid, item))
        except TypeError:
            #print("--- {} seconds Point {} ---".format((time.time() - start_time3), item))
            pass
        if disbtw > 50 / 1.852:
            distbtw = 27
        else:
            observ, observlead = arrofobserv(mgmonth, mgday, matbid)
            print(observlead)
            x = observ[0][0]
            if x <= 0:
                pass
            else:
                predict[0].append(mglat)
                predict[1].append(mglong)
                predict[2].append(0)
                leadtime = getleadtimes(leapyear, observlead, len(observ[0]))
                print(leadtime)
                for x in leadtime[1:]:
                    long2, lat2 = getenddrift(str(item), x)
                    predict[0].append(lat2)
                    predict[1].append(long2)
                    predict[2].append(x)
                for x, y in enumerate(observ[0]):
                    if y <= 0:
                        print('Matchup has some invalid data, removing...')
                        observ[1][x] = None
                        observ[0][x] = None
                        predict[1][x] = None
                        predict[0][x] = None
                        leadtime[x] = None
                try:
                    for x in range(len(observ[0])):
                        while not observ[0][x]:
                            del observ[1][x]
                            del observ[0][x]
                            del predict[0][x]
                            del predict[1][x]
                            del leadtime[x]
                except IndexError:
                    pass
                adist, adistob, observ, predict, leadtime, adist2 = arcdisforskill(observ, predict, mgmonth, mgday, disbtw, leadtime, item)
                rmsdir = rmsdir_create(observ, predict)
                if item <= 208:
                    logfile.write('\n=====Buoy ID: {0} Forecast point: {1}\n'.format(matbid, format(item, '04')))
                    for num in leadtime:
                        logfile.write(('{},').format(num))
                    logfile.write('\nArcdis\n')
                    for num in adist:
                        logfile.write(('{},').format(num))
                    logfile.write('\nPredict lat\n')
                    for num in predict[0]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nPredict lon\n')
                    for num in predict[1]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nObserve lat\n')
                    for num in observ[0]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nObserve lon\n')
                    for num in observ[1]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nAdistob observe\n')
                    for num in adistob[0]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nAdistob predict\n')
                    for num in adistob[1]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nAdist2 observe\n')
                    for num in adist2[0]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nAdist2 predict\n')
                    for num in adist2[1]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nRmsdir observ\n')
                    for num in rmsdir[0]:
                        logfile.write(('{},').format(num))
                    logfile.write('\nRmsdir predict\n')
                    for num in rmsdir[1]:
                        logfile.write(('{},').format(num))
                else:
                    logfileo.write('\n=====Buoy ID: {0} Forecast point: {1}\n'.format(matbid, format(item, '04')))
                    for num in leadtime:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nArcdis\n')
                    for num in adist:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nPredict lat\n')
                    for num in predict[0]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nPredict lon\n')
                    for num in predict[1]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nObserve lat\n')
                    for num in observ[0]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nObserve lon\n')
                    for num in observ[1]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nAdistob observe\n')
                    for num in adistob[0]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nAdistob predict\n')
                    for num in adistob[1]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nAdist2 observe\n')
                    for num in adist2[0]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nAdist2 predict\n')
                    for num in adist2[1]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nRmsdir observ\n')
                    for num in rmsdir[0]:
                        logfileo.write(('{},').format(num))
                    logfileo.write('\nRmsdir predict\n')
                    for num in rmsdir[1]:
                        logfileo.write(('{},').format(num))
                disbtw = 27
                predict = [[], [], []]
    #exit(0)

logfileo.close()
logfile.close()
fpfile.close()
modelfile.close()
Cfile.close()
logfile = open('{}adist.log'.format(pathlog), 'r')

if py3:
    for x in range(2):
        if x != 0:
            logfile = open('{}adistt2.log'.format(pathlog), 'r')
            logfile2.write('Arctic ice pack edge\n')
            logfile2.write('-------------------------------\n')
        ad, ro, rp, adob, ad2, fp, rmsdir = getdata() #ad[[],[]] has leadsize in [0]
        #rmsdir = [[], []] #0= observ #1=predict
        #for coord, itd in enumerate(ro[0]):
        #    rmsdir[0].append(Geodesic.WGS84.Inverse(ro[0][0], ro[1][0], ro[0][coord], ro[1][coord])['azi1'])
            #rmsdir[0].append(haversine2(ro[0][0], ro[1][0], ro[0][coord], ro[1][coord]))
        #for cord, itd in enumerate(rp[0]):#could use theta values from getenddrift but writing and rereading text file to get thetas might be slower than doing haversine2. Test this and value computated
         #   rmsdir[1].append(Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord])['azi1'])
            #rmsdir[1].append(haversine2(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord]))
        for b in range(1, 17):
            print('--------Lead size: {}'.format(b))
            getskill(b, ad, adob, rmsdir, ad2)
        logfile.close()
else:
    for x in xrange(2):
        if x != 0:
            logfile = open('{}adistt2.log'.format(pathlog), 'r')
            logfile2.write('Arctic ice pack edge\n')
            logfile2.write('-------------------------------\n')
        ad, ro, rp, adob, ad2, fp, rmsdir = getdata() #ad[[],[]] has leadsize in [0]
        #rmsdir = [[], []] #0= observ #1=predict
        #for coord, itd in enumerate(ro[0]):
        #    rmsdir[0].append(Geodesic.WGS84.Inverse(ro[0][0], ro[1][0], ro[0][coord], ro[1][coord])['azi1'])
        #for cord, itd in enumerate(rp[0]):#could use theta values from getenddrift but writing and rereading text file to get thetas might be slower than doing haversine2. Test this and value computated
        #    rmsdir[1].append(Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord])['azi1'])
        for b in xrange(1, 17):
            print('--------Lead size: {}'.format(b))
            getskill(b, ad, adob, rmsdir, ad2)
        logfile.close()

'''logfile2.write('\nSorted by Forecast Point Below\n')
#test this section again reference forecast pt: 30, 45, 1001; rewrite section
logfile=open('{}adistt2.log'.format(pathlog), 'r')
if py3:
    for x in range(2):
        if x != 0:
            logfile = open('{}adist.log'.format(pathlog), 'r')
            logfile2.write('FP points 1 - 208\n')
            logfile2.write('-------------------------------\n')
        ad, ro, rp, adob, ad2, fp, rmsdir = getdata() #ad[[],[]] has leadsize in [0]
        #rmsdir = [[], []] #0= observ #1=predict
        #for coord, itd in enumerate(ro[0]):
         #   rmsdir[0].append(Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][coord], rp[1][coord])['azi1'])
        #for cord, itd in enumerate(rp[0]):#could use theta values from getenddrift but writing and rereading text file to get thetas might be slower than doing haversine2. Test this and value computated
          #  rmsdir[1].append(Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord])['azi1'])
        for b in range(1, 1500):
            getfpskill(b, ad, adob, rmsdir, ad2, fp)
        logfile.close()
else:
    for x in xrange(2):
        if x != 0:
            logfile = open('{}adist.log'.format(pathlog), 'r')
            logfile2.write('FP 1 - 208')
            logfile2.write(print('\n-------------------------------\n'))
        ad, ro, rp, adob, ad2, fp, rmsdir = getdata() #ad[[],[]] has leadsize in [0]
        #rmsdir = [[], []] #0= observ #1=predict
        #for coord, itd in enumerate(ro[0]):
         #   rmsdir[0].append(Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][coord], rp[1][coord])['azi1'])
        #for cord, itd in enumerate(rp[0]):#could use theta values from getenddrift but writing and rereading text file to get thetas might be slower than doing haversine2. Test this and value computated
         #   rmsdir[1].append(Geodesic.WGS84.Inverse(rp[0][0], rp[1][0], rp[0][cord], rp[1][cord])['azi1'])
        for b in xrange(1, 1500):
            print('--------Lead size: {}'.format(b))
            getfpskill(b, ad, adob, rmsdir, ad2, fp)
        logfile.close()'''

#Check RMSDE scores
#Do a test run with 2010 data and compare with 2010 scores and redo 2015 scores

logfile2.close()
print('Finished. Data saved to SIDS.log')
print("--- {} seconds ---".format(time.time() - start_time))
