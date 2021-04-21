import csv

with open('a.csv') as csvfile:
    k = 0
    sreader = csv.reader(csvfile, delimiter=",")
    for line in sreader:
        day = float(line[0])
        t2m_gfs = float(line[1])
        td_gfs = float(line[2])
        print(k, day, t2m_gfs, td_gfs)
        k += 1
      
