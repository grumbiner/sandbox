rm betters nevers orig_improvement
for f in extracted/a983 extracted/a978 extracted/a967 extracted/a958 extracted/a957 extracted/a939 extracted/a938 extracted/a931 extracted/a924 extracted/a909 extracted/a902 extracted/a899 extracted/a884 extracted/a881 extracted/a872 extracted/a870 extracted/a864 extracted/a2965 extracted/a2946 extracted/a2917 extracted/a2892 extracted/a2857 extracted/a2823 extracted/a2821 extracted/a2818 extracted/a2783 extracted/a2756 extracted/a2753 extracted/a2656 extracted/a2638 extracted/a2621 extracted/a2598 extracted/a2566 extracted/a2547 extracted/a2544 extracted/a2530 extracted/a2519 extracted/a2514 extracted/a2502 extracted/a2476 extracted/a2455 extracted/a2451 extracted/a2444 extracted/a2425 extracted/a2411 extracted/a2407 extracted/a2405 extracted/a2399 extracted/a2396 extracted/a2395 extracted/a2383 extracted/a2379 extracted/a2360 extracted/a2354 extracted/a2352 extracted/a2336 extracted/a2334 extracted/a2332 extracted/a2317 extracted/a2311 extracted/a2289 extracted/a2284 extracted/a2272 extracted/a2247 extracted/a2219 extracted/a2218 extracted/a2209 extracted/a2197 extracted/a2196 extracted/a2169 extracted/a2165 extracted/a2155 extracted/a2153 extracted/a2137 extracted/a2127 extracted/a2115 extracted/a2085 extracted/a2084 extracted/a2082 extracted/a2062 extracted/a2055 extracted/a2024 extracted/a2012 extracted/a2009 extracted/a2006 extracted/a2004 extracted/a1981 extracted/a1961 extracted/a1954 extracted/a1953 extracted/a1952 extracted/a1945 extracted/a1943 extracted/a1929 extracted/a1926 extracted/a1923 extracted/a1922 extracted/a1917 extracted/a1910 extracted/a1908 extracted/a1906 extracted/a1901 extracted/a1877 extracted/a1841 extracted/a1830 extracted/a1825 extracted/a1819 extracted/a1814 extracted/a1799 extracted/a1780 extracted/a1766 extracted/a1759 extracted/a1736 extracted/a1732 extracted/a1730 extracted/a1688 extracted/a1659 extracted/a1603 extracted/a1570 extracted/a1569 extracted/a1566 extracted/a1559 extracted/a1554 extracted/a1552 extracted/a1530 extracted/a1516 extracted/a1491 extracted/a1477 extracted/a1476 extracted/a1467 extracted/a1464 extracted/a1454 extracted/a1451 extracted/a1446 extracted/a1436 extracted/a1424 extracted/a1410 extracted/a1409 extracted/a1402 extracted/a1391 extracted/a1375 extracted/a1358 extracted/a1332 extracted/a1321 extracted/a1317 extracted/a1314 extracted/a1311 extracted/a1287 extracted/a1277 extracted/a1272 extracted/a1268 extracted/a1265 extracted/a1262 extracted/a1259 extracted/a1222 extracted/a1219 extracted/a1218 extracted/a1217 extracted/a1203 extracted/a1201 extracted/a1190 extracted/a1182 extracted/a1180 extracted/a1177 extracted/a1156 extracted/a1155 extracted/a1129 extracted/a1124 extracted/a1089 extracted/a1087 extracted/a1078 extracted/a1062 extracted/a1058 extracted/a1031 extracted/a1028 extracted/a1027 extracted/a1022 extracted/a1012
do
  tag=`basename $f | cut -c2-90`

  if [ ! -f b$tag ] ; then
    time ./liquidmos $f > b$tag
  fi

  x=`grep -c never b$tag`
  if [ $x -gt 0 ] ; then
    grep a$tag means >> nevers
  else
    grep a$tag means | tail -1 >> betters
    grep orig b$tag --with-filename | tail -1 >> orig_improvement 
  fi
done
sort -nr -k 4,4 orig_improvement > o.s
