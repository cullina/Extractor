module ExpertData where

b1 = map (0 <) y1

b2 = map (0 <) y2

b3 = expand 1 3001 z1
b4 = expand 1 3001 z2


expand :: Int -> Int -> [Int] -> [Bool]
expand a b [] = replicate (b - a) False
expand a b (x:xs)
  | a == b    = []
  | a == x    = True  : expand (a+1) b xs
  | otherwise = False : expand (a+1) b (x:xs)

y1 = 
  [1,1,0,0,0,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0
  ,0,0,1,1,0,1,0,0,1,1,0,0,1,1,0,0,1,1,1,0
  ,0,0,1,1,1,0,0,1,1,1,0,0,0,1,1,0,1,0,0,1
  ,1,0,0,1,1,0,0,0,1,1,1,0,0,1,1,0,0,1,1,0
  ,0,1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,1
  ,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,1,1,1,0
  ,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,1,1,0,0,0
  ,1,1,0,0,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0
  ,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0
  ,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0,0,1
  ,1,0,0,1,0,0,0,1,1,1,0,0,0,1,1,1,1,0,0,1
  ,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0
  ,1,1,0,0,0,1,1,1,0,0,1,1,0,0,0,1,1,1,1,0
  ,1,1,0,0,1,1,1,1,1,1,1,0,0,0,1,1,0,0,1,1
  ,0,0,1,1,0,0,0,0,1,1,0,0,0,1,1,1,0,1,1,0
  ,0,1,1,1,1,1,1,1,1,0,0,1,1,0,0,0,1,1,0,0
  ,0,1,1,0,0,1,1,0,0,0,0,1,1,0,0,0,1,1,0,0
  ,0,1,1,0,0,1,1,1,1,1,1,0,0,1,1,0,0,1,1,1
  ,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,1,0,0,0,1
  ,1,0,0,0,1,1,1,0,0,0,1,1,1,0,1,1,1,1,1,0
  ,0,0,0,1,1,0,1,0,1,0,0,1,1,1,0,0,1,1,0,0
  ,0,1,1,1,0,0,1,1,0,0,0,1,0,0,0,1,1,0,0,1
  ,1,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0
  ,0,0,0,1,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0
  ,0,0,1,1,0,0,1,1,0,0,0,1,1,0,0,1,1,1,1,1
  ]
  
y2 =
  [1,1,0,1,1,0,0,0,1,1,1,0,0,0,1,0,1,0,0,0
  ,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,1,1,0
  ,0,0,1,1,0,0,1,1,0,1,0,0,0,0,1,0,0,0,0,0
  ,1,1,0,0,0,0,1,0,0,0,0,1,1,0,1,0,1,0,0,0
  ,1,1,0,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,1,1
  ,0,0,0,1,0,0,0,0,1,0,1,0,1,1,0,1,0,0,0,0
  ,1,1,1,1,0,0,0,1,0,0,0,1,0,1,0,1,1,0,0,0
  ,1,1,0,1,1,0,1,0,0,0,1,1,0,0,0,1,1,1,0,0
  ,0,1,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0
  ,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,0
  ,0,0,0,1,1,1,1,0,0,0,0,1,0,1,1,0,1,1,0,0
  ,0,1,0,0,0,0,0,0,1,1,0,0,1,1,1,1,0,1,0,0
  ,1,1,0,0,1,1,0,0,1,0,1,0,1,0,1,0,0,1,0,0
  ,0,1,0,0,0,1,1,0,0,0,1,1,1,1,0,1,1,0,0,0
  ,1,1,0,1,0,1,0,0,0,0,1,0,1,0,0,0,1,1,0,0
  ,1,1,1,0,0,0,1,1,1,0,0,0,0,1,1,0,0,0,1,1
  ,0,1,0,0,1,1,1,1,1,0,0,1,0,1,0,0,0,1,1,0
  ,0,0,1,0,1,0,0,0,1,1,0,1,0,0,0,1,0,0,0,0
  ,1,0,0,0,1,1,0,0,0,1,1,1,0,0,0,1,0,1,0,1
  ,0,1,0,0,0,1,1,0,1,0,0,1,1,0,0,0,1,0,1,0
  ,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,0,0,0,0
  ,1,1,0,1,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0
  ,1,1,1,0,1,0,1,0,1,0,0,0,1,1,1,1,0,0,0,0
  ,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,0
  ,0,0,1,0,0,0,1,0,0,0,1,1,0,0,0,1,1,1,0,0
  ]

z1 :: [Int]
z1 = [2,
   8,
   11,
   14,
   17,
   20,
   23,
   24,
   27,
   30,
   31,
   34,
   37,
   40,
   43,
   54,
   57,
   60,
   80,
   83,
   86,
   89,
   92,
   95,
   98,
   101,
   104,
   107,
   113,
   116,
   119,
   122,
   125,
   146,
   149,
   152,
   155,
   194,
   197,
   200,
   203,
   206,
   221,
   271,
   274,
   276,
   279,
   282,
   285,
   288,
   291,
   300,
   303,
   306,
   309,
   312,
   315,
   318,
   324,
   330,
   331,
   334,
   337,
   340,
   343,
   355,
   361,
   364,
   366,
   375,
   381,
   384,
   387,
   390,
   393,
   396,
   399,
   402,
   405,
   408,
   411,
   414,
   417,
   420,
   423,
   426,
   429,
   432,
   435,
   438,
   441,
   442,
   445,
   448,
   451,
   454,
   457,
   460,
   463,
   466,
   469,
   472,
   475,
   478,
   491,
   493,
   496,
   499,
   502,
   505,
   506,
   509,
   512,
   515,
   521,
   525,
   544,
   547,
   556,
   557,
   560,
   567,
   570,
   579,
   582,
   591,
   608,
   611,
   625,
   628,
   638,
   641,
   651,
   653,
   656,
   659,
   682,
   710,
   713,
   716,
   719,
   722,
   725,
   728,
   737,
   740,
   743,
   764,
   786,
   789,
   806,
   820,
   823,
   825,
   850,
   853,
   868,
   871,
   874,
   880,
   904,
   907,
   919,
   922,
   923,
   926,
   937,
   939,
   959,
   962,
   965,
   974,
   986,
   989,
   992,
   1005,
   1008,
   1027,
   1033,
   1036,
   1039,
   1042,
   1054,
   1060,
   1062,
   1065,
   1068,
   1078,
   1081,
   1084,
   1102,
   1105,
   1108,
   1111,
   1113,
   1115,
   1125,
   1128,
   1130,
   1131,
   1134,
   1149,
   1153,
   1156,
   1168,
   1178,
   1184,
   1193,
   1207,
   1213,
   1216,
   1219,
   1222,
   1223,
   1226,
   1229,
   1232,
   1235,
   1238,
   1241,
   1244,
   1247,
   1262,
   1265,
   1268,
   1271,
   1283,
   1286,
   1292,
   1298,
   1301,
   1304,
   1307,
   1310,
   1313,
   1316,
   1317,
   1329,
   1332,
   1390,
   1391,
   1392,
   1395,
   1398,
   1399,
   1402,
   1404,
   1407,
   1413,
   1416,
   1425,
   1426,
   1432,
   1434,
   1435,
   1449,
   1452,
   1476,
   1482,
   1485,
   1501,
   1504,
   1507,
   1510,
   1513,
   1516,
   1519,
   1521,
   1524,
   1527,
   1530,
   1564,
   1566,
   1569,
   1572,
   1575,
   1585,
   1587,
   1593,
   1594,
   1597,
   1600,
   1603,
   1606,
   1612,
   1613,
   1616,
   1619,
   1622,
   1625,
   1628,
   1650,
   1663,
   1666,
   1669,
   1672,
   1675,
   1681,
   1692,
   1705,
   1708,
   1720,
   1732,
   1735,
   1748,
   1751,
   1760,
   1766,
   1778,
   1822,
   1825,
   1867,
   1870,
   1876,
   1879,
   1883,
   1886,
   1889,
   1907,
   1910,
   1913,
   1916,
   1925,
   1943,
   1946,
   1948,
   1951,
   1954,
   1986,
   1996,
   2012,
   2018,
   2021,
   2024,
   2027,
   2030,
   2033,
   2036,
   2039,
   2042,
   2045,
   2064,
   2070,
   2073,
   2084,
   2085,
   2086,
   2095,
   2099,
   2102,
   2104,
   2114,
   2117,
   2120,
   2123,
   2129,
   2132,
   2134,
   2135,
   2137,
   2140,
   2143,
   2146,
   2149,
   2152,
   2158,
   2161,
   2164,
   2182,
   2188,
   2191,
   2199,
   2202,
   2205,
   2208,
   2214,
   2226,
   2232,
   2235,
   2236,
   2239,
   2242,
   2245,
   2248,
   2259,
   2262,
   2271,
   2272,
   2287,
   2294,
   2297,
   2312,
   2315,
   2318,
   2321,
   2324,
   2327,
   2330,
   2333,
   2336,
   2339,
   2340,
   2343,
   2352,
   2355,
   2358,
   2361,
   2364,
   2370,
   2410,
   2434,
   2437,
   2441,
   2452,
   2455,
   2458,
   2466,
   2484,
   2485,
   2489,
   2494,
   2508,
   2511,
   2519,
   2534,
   2537,
   2540,
   2549,
   2552,
   2555,
   2556,
   2558,
   2561,
   2564,
   2567,
   2570,
   2573,
   2642,
   2661,
   2664,
   2667,
   2670,
   2673,
   2687,
   2709,
   2721,
   2724,
   2730,
   2733,
   2736,
   2741,
   2753,
   2756,
   2772,
   2779,
   2782,
   2788,
   2789,
   2792,
   2795,
   2798,
   2801,
   2802,
   2805,
   2808,
   2810,
   2813,
   2816,
   2819,
   2822,
   2825,
   2828,
   2831,
   2834,
   2837,
   2840,
   2843,
   2846,
   2849,
   2852,
   2855,
   2858,
   2861,
   2890,
   2893,
   2896,
   2908,
   2911,
   2914,
   2917,
   2920]
     
     
z2 :: [Int]
z2 = [5,
 46,
 48,
 51,
 63,
 66,
 68,
 71,
 74,
 77,
 110,
 128,
 131,
 134,
 137,
 140,
 143,
 158,
 161,
 163,
 165,
 167,
 168,
 171,
 172,
 175,
 178,
 179,
 180,
 183,
 186,
 188,
 191,
 209,
 211,
 214,
 215,
 218,
 224,
 227,
 230,
 233,
 236,
 239,
 242,
 243,
 246,
 249,
 252,
 255,
 258,
 261,
 264,
 265,
 268,
 294,
 297,
 321,
 327,
 346,
 349,
 352,
 358,
 369,
 371,
 373,
 378,
 481,
 482,
 485,
 488,
 518,
 524,
 526,
 529,
 532,
 535,
 538,
 541,
 550,
 553,
 563,
 564,
 573,
 576,
 585,
 588,
 594,
 597,
 598,
 601,
 604,
 605,
 614,
 615,
 618,
 619,
 622,
 631,
 632,
 635,
 644,
 645,
 648,
 662,
 664,
 667,
 670,
 673,
 676,
 679,
 685,
 686,
 689,
 692,
 695,
 698,
 701,
 704,
 707,
 725,
 731,
 734,
 746,
 749,
 752,
 755,
 758,
 761,
 767,
 770,
 772,
 775,
 778,
 780,
 783,
 792,
 795,
 798,
 800,
 803,
 809,
 811,
 814,
 817,
 828,
 829,
 832,
 835,
 838,
 841,
 844,
 847,
 856,
 859,
 862,
 864,
 865,
 877,
 883,
 884,
 887,
 890,
 893,
 896,
 898,
 901,
 910,
 913,
 916,
 929,
 932,
 933,
 934,
 942,
 944,
 947,
 950,
 951,
 954,
 955,
 958,
 968,
 971,
 977,
 980,
 983,
 995,
 996,
 999,
 1002,
 1011,
 1014,
 1017,
 1019,
 1020,
 1021,
 1024,
 1030,
 1045,
 1048,
 1051,
 1057,
 1071,
 1074,
 1075,
 1087,
 1090,
 1093,
 1096,
 1099,
 1118,
 1121,
 1123,
 1127,
 1137,
 1140,
 1143,
 1146,
 1152,
 1153,
 1159,
 1162,
 1165,
 1171,
 1172,
 1175,
 1181,
 1187,
 1190,
 1196,
 1197,
 1200,
 1203,
 1204,
 1210,
 1250,
 1253,
 1256,
 1259,
 1274,
 1277,
 1280,
 1289,
 1295,
 1298,
 1320,
 1323,
 1326,
 1335,
 1338,
 1341,
 1344,
 1347,
 1350,
 1353,
 1356,
 1359,
 1362,
 1365,
 1368,
 1371,
 1374,
 1375,
 1378,
 1381,
 1384,
 1387,
 1410,
 1419,
 1422,
 1429,
 1438,
 1441,
 1443,
 1446,
 1455,
 1458,
 1461,
 1464,
 1467,
 1470,
 1473,
 1479,
 1486,
 1489,
 1492,
 1495,
 1498,
 1533,
 1536,
 1539,
 1540,
 1543,
 1546,
 1549,
 1552,
 1553,
 1555,
 1558,
 1561,
 1578,
 1579,
 1582,
 1590,
 1609,
 1631,
 1634,
 1637,
 1640,
 1641,
 1644,
 1646,
 1648,
 1649,
 1650,
 1653,
 1655,
 1656,
 1657,
 1660,
 1678,
 1683,
 1686,
 1689,
 1695,
 1696,
 1699,
 1702,
 1711,
 1714,
 1717,
 1723,
 1726,
 1729,
 1738,
 1740,
 1743,
 1745,
 1754,
 1757,
 1763,
 1769,
 1772,
 1775,
 1781,
 1784,
 1787,
 1790,
 1792,
 1795,
 1798,
 1801,
 1804,
 1805,
 1807,
 1808,
 1811,
 1814,
 1817,
 1819,
 1828,
 1831,
 1834,
 1837,
 1840,
 1843,
 1846,
 1849,
 1852,
 1853,
 1856,
 1859,
 1862,
 1864,
 1873,
 1882,
 1892,
 1895,
 1898,
 1901,
 1904,
 1919,
 1922,
 1928,
 1931,
 1934,
 1937,
 1940,
 1957,
 1960,
 1963,
 1966,
 1969,
 1972,
 1975,
 1977,
 1978,
 1980,
 1983,
 1989,
 1990,
 1993,
 1999,
 2002,
 2004,
 2006,
 2009,
 2015,
 2048,
 2051,
 2054,
 2057,
 2058,
 2061,
 2067,
 2076,
 2078,
 2081,
 2089,
 2092,
 2098,
 2107,
 2108,
 2111,
 2126,
 2155,
 2167,
 2168,
 2170,
 2173,
 2176,
 2179,
 2185,
 2194,
 2195,
 2198,
 2211,
 2217,
 2220,
 2223,
 2229,
 2251,
 2252,
 2255,
 2256,
 2265,
 2268,
 2272,
 2275,
 2278,
 2280,
 2283,
 2284,
 2290,
 2293,
 2300,
 2303,
 2306,
 2309,
 2346,
 2349,
 2367,
 2373,
 2376,
 2379,
 2382,
 2385,
 2388,
 2391,
 2394,
 2397,
 2398,
 2399,
 2402,
 2405,
 2407,
 2413,
 2416,
 2419,
 2422,
 2425,
 2428,
 2431,
 2440,
 2443,
 2446,
 2449,
 2461,
 2463,
 2469,
 2472,
 2474,
 2477,
 2480,
 2481,
 2488,
 2491,
 2497,
 2500,
 2502,
 2505,
 2514,
 2516,
 2522,
 2525,
 2528,
 2531,
 2543,
 2546,
 2576,
 2578,
 2581,
 2584,
 2587,
 2588,
 2589,
 2592,
 2595,
 2598,
 2601,
 2604,
 2605,
 2606,
 2609,
 2612,
 2615,
 2618,
 2621,
 2624,
 2627,
 2630,
 2633,
 2634,
 2637,
 2639,
 2645,
 2648,
 2651,
 2654,
 2657,
 2659,
 2676,
 2679,
 2682,
 2684,
 2690,
 2691,
 2694,
 2697,
 2700,
 2703,
 2706,
 2712,
 2713,
 2715,
 2718,
 2727,
 2739,
 2744,
 2747,
 2750,
 2759,
 2760,
 2763,
 2766,
 2769,
 2775,
 2776,
 2785,
 2864,
 2866,
 2869,
 2872,
 2873,
 2876,
 2878,
 2881,
 2884,
 2887,
 2899,
 2902,
 2905]
