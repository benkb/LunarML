fun call f arg = print ((Int16.toString (f arg) handle Overflow => "Overflow" | Div => "Div") ^ "\n")
fun callS f arg = print ((f arg handle Overflow => "Overflow" | Div => "Div") ^ "\n")
fun bin (x, y) = ( print (Int16.toString x ^ " + " ^ Int16.toString y ^ " = ")
                 ; call Int16.+ (x, y)
                 ; print (Int16.toString x ^ " - " ^ Int16.toString y ^ " = ")
                 ; call Int16.- (x, y)
                 ; print (Int16.toString x ^ " * " ^ Int16.toString y ^ " = ")
                 ; call Int16.* (x, y)
                 ; print (Int16.toString x ^ " div " ^ Int16.toString y ^ " = ")
                 ; call Int16.div (x, y)
                 ; print (Int16.toString x ^ " mod " ^ Int16.toString y ^ " = ")
                 ; call Int16.mod (x, y)
                 ; print (Int16.toString x ^ " quot " ^ Int16.toString y ^ " = ")
                 ; call Int16.quot (x, y)
                 ; print (Int16.toString x ^ " rem " ^ Int16.toString y ^ " = ")
                 ; call Int16.rem (x, y)
                 );
fun un x = ( print ("~ " ^ Int16.toString x ^ " = ")
           ; call Int16.~ x
           ; print ("abs " ^ Int16.toString x ^ " = ")
           ; call Int16.abs x
           (* ; print ("fmt BIN " ^ Int16.toString x ^ " = ")
           ; callS (Int16.fmt StringCvt.BIN) x ... not implemented yet *)
           ; print ("fmt OCT " ^ Int16.toString x ^ " = ")
           ; callS (Int16.fmt StringCvt.OCT) x
           ; print ("fmt DEC " ^ Int16.toString x ^ " = ")
           ; callS (Int16.fmt StringCvt.DEC) x
           ; print ("fmt HEX " ^ Int16.toString x ^ " = ")
           ; callS (Int16.fmt StringCvt.HEX) x
           );
List.app un
[~32768
,~32767
,~77
,~3
,~2
,~1
,0
,1
,2
,3
,78
,32766
,32767
,~7632
,~15721
,24671
,~1265
,208
,30162
,24465
,7703
,~19829
,~6049
,19956
,306
,31419
,~19579
,~13938
,~16599
,28408
,~3490
,~25252
,~8613
,17645
,26751
,~31678
,2569
,~30389
,~15717
,4218
,21016
,~8559
,~19232
,30026
,~30558
,~32030
,~14613
,~4577
,32680
,13275
,~21982
,9682
,~18755
,15432
,17003
,~5136
,23933
,~24938
,25528
,28535
,~23137
,12277
,~8154
];
List.app bin
[(~32768,~32768)
,(~32768,~32767)
,(~32768,~35)
,(~32768,~3)
,(~32768,~2)
,(~32768,~1)
,(~32768,0)
,(~32768,1)
,(~32768,2)
,(~32768,3)
,(~32768,99)
,(~32768,32766)
,(~32768,32767)
,(~32767,~32768)
,(~32767,~32767)
,(~32767,~35)
,(~32767,~3)
,(~32767,~2)
,(~32767,~1)
,(~32767,0)
,(~32767,1)
,(~32767,2)
,(~32767,3)
,(~32767,99)
,(~32767,32766)
,(~32767,32767)
,(~77,~32768)
,(~77,~32767)
,(~77,~35)
,(~77,~3)
,(~77,~2)
,(~77,~1)
,(~77,0)
,(~77,1)
,(~77,2)
,(~77,3)
,(~77,99)
,(~77,32766)
,(~77,32767)
,(~3,~32768)
,(~3,~32767)
,(~3,~35)
,(~3,~3)
,(~3,~2)
,(~3,~1)
,(~3,0)
,(~3,1)
,(~3,2)
,(~3,3)
,(~3,99)
,(~3,32766)
,(~3,32767)
,(~2,~32768)
,(~2,~32767)
,(~2,~35)
,(~2,~3)
,(~2,~2)
,(~2,~1)
,(~2,0)
,(~2,1)
,(~2,2)
,(~2,3)
,(~2,99)
,(~2,32766)
,(~2,32767)
,(~1,~32768)
,(~1,~32767)
,(~1,~35)
,(~1,~3)
,(~1,~2)
,(~1,~1)
,(~1,0)
,(~1,1)
,(~1,2)
,(~1,3)
,(~1,99)
,(~1,32766)
,(~1,32767)
,(0,~32768)
,(0,~32767)
,(0,~35)
,(0,~3)
,(0,~2)
,(0,~1)
,(0,0)
,(0,1)
,(0,2)
,(0,3)
,(0,99)
,(0,32766)
,(0,32767)
,(1,~32768)
,(1,~32767)
,(1,~35)
,(1,~3)
,(1,~2)
,(1,~1)
,(1,0)
,(1,1)
,(1,2)
,(1,3)
,(1,99)
,(1,32766)
,(1,32767)
,(2,~32768)
,(2,~32767)
,(2,~35)
,(2,~3)
,(2,~2)
,(2,~1)
,(2,0)
,(2,1)
,(2,2)
,(2,3)
,(2,99)
,(2,32766)
,(2,32767)
,(3,~32768)
,(3,~32767)
,(3,~35)
,(3,~3)
,(3,~2)
,(3,~1)
,(3,0)
,(3,1)
,(3,2)
,(3,3)
,(3,99)
,(3,32766)
,(3,32767)
,(78,~32768)
,(78,~32767)
,(78,~35)
,(78,~3)
,(78,~2)
,(78,~1)
,(78,0)
,(78,1)
,(78,2)
,(78,3)
,(78,99)
,(78,32766)
,(78,32767)
,(32766,~32768)
,(32766,~32767)
,(32766,~35)
,(32766,~3)
,(32766,~2)
,(32766,~1)
,(32766,0)
,(32766,1)
,(32766,2)
,(32766,3)
,(32766,99)
,(32766,32766)
,(32766,32767)
,(32767,~32768)
,(32767,~32767)
,(32767,~35)
,(32767,~3)
,(32767,~2)
,(32767,~1)
,(32767,0)
,(32767,1)
,(32767,2)
,(32767,3)
,(32767,99)
,(32767,32766)
,(32767,32767)
,(~13170,25347)
,(11361,17494)
,(26960,~1870)
,(19675,~12979)
,(25921,9163)
,(~18778,28218)
,(28803,~11014)
,(~11845,24199)
,(24196,361)
,(~9699,8684)
,(23266,20749)
,(~6676,31390)
,(4572,~20453)
,(19127,3257)
,(19730,5568)
,(~25917,14280)
,(~15060,~13028)
,(~4510,~7383)
,(29253,~29837)
,(13714,26546)
,(~6723,~18648)
,(~13997,~27687)
,(~12371,~31249)
,(~888,~942)
,(~25137,598)
,(~25073,8510)
,(24014,~8828)
,(~29139,~20374)
,(~18384,6266)
,(~22543,~2313)
,(~11598,9310)
,(~2770,~9077)
,(17281,11059)
,(16745,14655)
,(22839,47)
,(4183,~31328)
,(~6015,12465)
,(6369,8108)
,(~28837,~30603)
,(~27680,21164)
,(17409,~21698)
,(14250,~26060)
,(~12541,12606)
,(~16506,~6777)
,(~4505,~5287)
,(29606,~8283)
,(32540,~21585)
,(~1553,~10037)
,(15480,~27709)
,(12413,~28584)
,(4124,3531)
,(8660,5357)
,(5455,30522)
,(24721,~16093)
,(~11221,17617)
,(17989,~10782)
,(8759,30973)
,(24722,12953)
,(13074,~28206)
,(~21226,~30247)
,(~2056,249)
,(26987,~11831)
,(20720,2972)
,(~10420,~4772)
,(~26189,~733)
,(15040,~30467)
,(~7906,29449)
,(~3446,9540)
,(~659,~21302)
,(~10117,~65)
,(13141,17945)
,(~561,23084)
,(~27695,~10543)
,(12411,~21597)
,(~14122,~26529)
,(23233,17382)
,(~4619,~23752)
,(31978,~12460)
,(~30481,~30831)
,(~30148,~26617)
,(8783,~5585)
,(15398,~1381)
,(~25341,17802)
,(9432,31350)
,(~29725,32382)
,(~17410,17039)
,(~29555,~24485)
,(~4700,21784)
,(~12128,~31189)
,(~11570,~3737)
,(~27850,23263)
,(28806,31135)
,(13363,~6400)
,(~8435,~5270)
,(~22698,~19125)
,(~19586,~10002)
,(~627,30583)
,(22169,~29787)
,(~29687,~1535)
,(31578,~24855)
,(~808,12033)
,(27698,~27478)
,(12083,~21425)
,(28707,29581)
,(31570,~14865)
,(~5766,~10050)
,(3037,20187)
,(~20249,16969)
,(6490,19780)
,(2529,~17397)
,(~5546,~32546)
,(30343,~17163)
,(~31230,~21087)
,(~32349,16050)
,(10603,13173)
,(13981,12394)
,(29843,22059)
,(~25928,17421)
,(7925,~29279)
,(9697,828)
,(~30921,24172)
,(30680,~6004)
,(8064,19468)
,(31932,28432)
,(~22366,16309)
,(21582,~1117)
,(~18876,30596)
,(9252,30764)
,(12818,9850)
,(30061,6142)
,(~12823,~29150)
,(18948,20135)
,(~17881,28460)
,(24898,~19915)
,(3902,~3760)
,(~4343,~27571)
,(390,~12692)
,(6281,~11359)
,(~6513,~25381)
,(~26920,~10852)
,(28687,~4131)
,(23887,30630)
,(~31039,~934)
,(12001,~23392)
,(20906,~11778)
,(21294,~28265)
,(20354,~10485)
,(26107,3312)
,(~7294,29148)
,(~1679,~14956)
,(~30742,26985)
,(14065,14547)
,(17340,~31980)
,(9397,~4009)
,(22990,~4587)
,(25583,~16225)
,(~23130,21911)
,(12360,8954)
,(~28648,7726)
,(~1024,12190)
,(11381,~31799)
,(2672,~11442)
,(7238,8597)
,(~30160,29911)
,(~29274,~20338)
,(~29175,32455)
,(~19864,27794)
,(16220,26833)
,(26720,22041)
,(582,27329)
,(~22226,~1284)
,(~19656,7820)
,(3709,~20677)
,(~7148,14088)
,(~28779,28383)
,(27237,6378)
,(15802,5023)
,(~11576,~28933)
,(~489,~23874)
,(16259,9487)
,(23293,23522)
,(6870,~7562)
,(~12572,~14812)
,(~5685,9060)
,(25329,16966)
,(15277,~299)
,(~12763,~28937)
,(10903,~15774)
,(~6805,26113)
,(3357,~14282)
,(~6631,~29444)
,(~27829,19518)
,(~12438,13112)
,(~2067,31064)
,(30427,30667)
,(~30870,29269)
,(21741,~24493)
,(~31767,1946)
,(24474,~18021)
,(20104,~3625)
,(~20324,~9443)
,(~31531,27205)
,(~6245,13353)
,(~28587,4109)
,(4548,16260)
,(~9818,~21451)
,(21318,7608)
,(30588,~16365)
,(4814,4187)
,(~22486,~16534)
,(26690,~11135)
,(~9344,32189)
,(16836,26554)
,(~23857,9234)
,(15612,~31623)
,(~14746,~25866)
,(~6917,~4893)
,(~24995,~17664)
,(11546,12133)
,(11038,~6599)
,(~20250,23556)
,(~1854,15406)
,(~4011,21184)
,(16571,~746)
,(26737,~1028)
,(~19423,4155)
,(22652,22506)
,(~19940,~30561)
,(~18029,26511)
,(~3690,7699)
,(12952,~25006)
,(~23910,8522)
,(~5237,7656)
,(~3040,3773)
,(~10812,~24741)
,(21678,~1323)
,(~28154,~27701)
,(~3620,~11305)
,(~29846,~5068)
,(8174,31367)
,(1138,~23968)
,(~29727,~13097)
,(13829,~16051)
,(~26831,8941)
,(~10753,21642)
,(~9456,26739)
,(~9790,29173)
,(~3176,12611)
,(19221,~20108)
,(~11143,15257)
,(~5195,3924)
,(~1448,31874)
,(~25131,7897)
,(~29709,~16157)
,(~7967,~1200)
,(15743,~20554)
,(~14380,~28422)
,(~30533,~1295)
,(~10465,5327)
,(30776,~32130)
,(~28098,~30251)
,(~5148,5573)
,(~16936,~7854)
,(~32218,8643)
,(~15283,17877)
,(~21199,~2397)
,(13871,21418)
,(~5962,22857)
,(4097,16996)
,(7619,~19086)
,(~30782,~15042)
,(~29348,~9153)
,(22767,~2705)
,(7631,~21637)
,(28305,~23468)
,(1789,~23744)
,(~15158,~7952)
,(23906,9131)
,(16943,~8595)
,(~29404,12159)
,(~24272,27476)
,(~23876,25408)
,(2106,20938)
,(~11312,17551)
,(~14061,1521)
,(~17213,25596)
,(21613,~170)
,(~10891,~9489)
,(~13173,~29356)
,(~8577,9947)
,(20558,21511)
,(~23200,~25568)
,(18909,8192)
,(24486,~2854)
,(~23683,5199)
,(7419,~3452)
,(~23897,31264)
,(~19306,20870)
,(19477,~23747)
,(29562,~15176)
,(~23137,~30328)
,(~25789,228)
,(~24203,~10930)
,(~17915,~20260)
,(~26074,655)
,(~16174,~18071)
,(2022,30832)
,(~13078,~5777)
,(28399,14649)
,(~20867,5085)
,(~1266,30072)
,(~22669,~20633)
,(22673,~12539)
,(~28493,~29915)
,(~24181,4663)
,(~5692,12423)
,(30055,~8166)
,(~20078,~32043)
,(~24608,~26554)
,(29346,1068)
,(13123,28120)
,(19149,~32067)
,(~16503,18088)
,(20911,10308)
,(~30501,15028)
,(~4199,~6991)
,(32269,27117)
,(18239,~9862)
,(~30935,~32601)
,(26043,14353)
,(26059,24175)
,(18723,~16909)
,(~16067,29610)
,(11612,22920)
,(4366,14002)
,(~13198,21406)
,(~7754,~451)
,(19668,29129)
,(~12470,~23588)
,(~18435,~19412)
,(~4773,~29601)
,(15457,~20957)
,(4508,2172)
,(6977,~22600)
,(28350,~9084)
,(~6298,~12113)
,(6440,~20498)
,(~12641,~12214)
,(22806,13424)
,(~12005,20032)
,(584,9510)
,(8869,~9184)
,(~21016,~17474)
,(~32210,~28345)
,(~8029,405)
,(~11119,28353)
,(~828,~27482)
,(~279,~7562)
,(~1040,2137)
,(12563,~30543)
,(7621,5081)
,(21821,~8107)
,(18610,7876)
,(1440,12281)
,(~14271,~5223)
,(12589,13139)
,(~31137,~26107)
,(4365,12250)
,(~25675,9082)
,(~32648,28211)
,(~19013,21092)
,(11173,25744)
,(8590,28325)
,(~22212,~921)
,(29505,~14359)
,(~2020,2519)
,(~12008,15516)
,(32037,20430)
,(~3115,9271)
,(~3694,9951)
,(~28169,23371)
,(~27587,~337)
,(25954,30613)
,(9968,8925)
,(21905,~22243)
,(~9120,8585)
,(~15370,2358)
,(~6722,~16332)
,(~31433,~13508)
,(~8255,25585)
,(~21799,17459)
,(~6083,~8617)
,(1269,18395)
,(~16368,5872)
,(~14928,28311)
,(~29896,~8089)
,(~9280,2095)
,(~5469,~22646)
,(~18397,15295)
,(~20038,24728)
,(~27258,~25398)
,(~2186,14107)
,(30175,30855)
,(7630,9795)
,(31689,~32283)
,(17574,7223)
,(~26667,19404)
,(29539,21788)
,(30426,~24692)
,(~15045,~13909)
,(~5299,29855)
,(19056,26156)
,(1688,~18763)
,(~27890,20224)
,(17368,18879)
,(~18270,~5950)
,(29015,28634)
,(~6629,~18681)
,(~12183,~26860)
,(26248,~18538)
,(~27345,30505)
,(8079,24363)
,(~7685,1476)
,(24653,13683)
,(15578,~25868)
,(~19660,13811)
,(~19055,~25448)
,(~5913,9299)
,(~29790,~23988)
,(~8870,~21613)
,(~19339,21270)
,(~20244,~30700)
,(~10990,~12178)
,(~29741,~28325)
,(~5002,~25380)
,(~6074,~18657)
,(~14812,~24442)
,(~1628,~28728)
,(16348,15289)
,(3451,~32651)
,(5742,~14053)
,(~19802,~11870)
,(22973,31712)
,(~28627,~11842)
,(24968,26748)
,(~366,1536)
,(9315,12948)
,(~9337,~8342)
,(~4484,~10375)
,(6407,168)
,(29346,~6278)
,(29426,13558)
,(32476,~20715)
,(~30646,32384)
,(1789,~18496)
,(~11663,~9692)
,(15760,12626)
,(6437,~31074)
,(~23023,12049)
,(~9119,30710)
,(~28606,~14047)
,(~12281,~5858)
,(4966,~20673)
,(~11371,~16542)
,(11341,~6336)
,(~477,~16748)
,(~3028,28877)
,(23284,~17691)
,(2732,27981)
,(277,29300)
,(~3859,~13506)
,(~24491,30545)
,(11960,14917)
,(30652,~26444)
,(30559,15650)
,(~28518,17685)
,(~27612,~19439)
,(~15609,~6363)
,(~13792,5357)
,(~21536,23574)
,(~356,27861)
,(~11126,4163)
,(2176,6572)
,(~3392,~13465)
,(~16473,~9648)
,(24857,~30574)
,(22543,19588)
,(~24181,4970)
,(29261,~1765)
,(7540,~7134)
,(1167,25603)
,(11598,27828)
,(8483,24691)
,(25192,~11707)
,(182,9589)
,(7740,~1936)
,(2691,~8801)
,(~11052,13907)
,(~17192,~28098)
,(19805,~12170)
,(~23018,5444)
,(~5338,23591)
,(~28591,~17465)
,(~24780,30703)
,(~8383,31708)
,(~23089,13262)
,(~16871,32214)
,(~26649,~1614)
,(27854,~620)
,(8650,~29169)
,(~13459,22717)
,(~4030,19943)
,(7531,30791)
,(30682,5194)
,(~21811,~211)
,(22446,3897)
,(~27311,~26739)
,(24848,~3203)
,(~3259,~5738)
,(~15048,12081)
,(~5382,~16347)
,(18771,17219)
,(~9277,24832)
,(23175,30076)
,(~18609,23949)
,(~5709,8791)
,(16986,18134)
,(23614,10061)
,(~17172,18895)
,(29612,2966)
,(24961,~15541)
,(248,13954)
,(~28571,23592)
,(6372,~32686)
,(~15930,~13289)
,(21329,~5529)
,(~18267,15909)
,(22073,~18542)
,(~31978,~16001)
,(~750,27050)
,(17341,~14314)
,(23583,~14021)
,(25544,17080)
,(20702,~601)
,(~4580,~841)
,(~4988,~12009)
,(3181,~14097)
,(3695,5512)
,(~31445,~175)
,(~17953,31303)
,(31842,19108)
,(4392,16353)
,(29236,~27805)
,(~30581,~3797)
,(~9090,4122)
,(~23630,~15981)
,(~14806,~12733)
,(~1581,~19664)
,(~21678,~22370)
,(20814,1072)
,(15510,22920)
,(25111,6416)
,(17223,~15931)
,(19102,~21578)
,(~23786,~6427)
,(~21216,~12060)
,(30431,~23151)
,(25039,26499)
,(28360,30299)
,(10010,~512)
,(4205,10024)
,(24063,~11076)
,(29147,~5016)
,(15667,~17526)
,(15111,11220)
,(23934,21642)
,(21519,19375)
,(~13035,~28201)
,(~21814,~7892)
,(2558,~22105)
,(~8977,~21541)
,(~21422,~23483)
,(31777,~18484)
,(~14842,~25416)
,(2796,~21956)
,(~15941,27706)
,(4778,27324)
,(~1676,~7727)
,(~11684,3610)
,(22908,~16918)
,(12364,10064)
,(6658,~18358)
,(22199,~24259)
,(15964,3403)
,(15748,~26080)
,(~10246,17989)
,(~17534,~27519)
,(~32657,~15615)
,(15927,30728)
,(~12611,~15650)
,(~1130,~2496)
,(~30829,3417)
,(19569,~28011)
,(~13379,~29456)
,(~12996,12511)
,(~18803,~10184)
,(~10282,29586)
,(~10057,839)
,(~10643,~32104)
,(18558,3017)
,(~29959,~2982)
,(~23378,5433)
,(~11579,~24626)
,(~10334,~4722)
,(~28888,4955)
,(7534,1098)
,(11019,32628)
,(24701,21174)
,(~2388,~16907)
,(857,26546)
,(15589,~28660)
,(~23723,13285)
,(~12763,~27426)
,(12528,~22170)
,(31955,19573)
,(5382,~13327)
,(27988,19214)
,(~30705,~11517)
,(19350,~1038)
,(135,25910)
,(~11448,13035)
,(28579,~12799)
,(~12081,27883)
,(~29010,20016)
,(~31494,~2323)
,(19275,10924)
,(28513,22646)
,(~1649,2321)
,(28930,~20651)
,(3324,~9396)
,(6752,7289)
,(~7702,~2026)
,(9959,~29035)
,(~15039,29019)
,(~625,25241)
,(~4318,~28317)
,(11891,~3728)
,(21342,~20842)
,(~24013,~17910)
,(14485,~24552)
,(28900,20490)
,(27127,4407)
,(30492,~11276)
,(~26005,1519)
,(~6659,~3516)
,(~28610,~12669)
,(~30495,~20523)
,(~9433,32371)
,(~8866,~26946)
,(29473,1602)
,(20769,23924)
,(~3471,3800)
,(2219,~14139)
,(25970,~30360)
,(~20609,3971)
,(~7311,12687)
,(~20393,22288)
,(11004,~19882)
,(~26534,~13706)
,(~7191,~11280)
,(27816,~1542)
,(~16246,23529)
,(~23894,~27719)
,(~12123,~2698)
,(~10340,~26440)
,(~1206,12058)
,(2835,~18480)
,(~17682,11546)
,(29752,~25609)
,(5796,~18274)
,(24512,375)
,(~12014,32765)
,(~5915,30432)
,(~12644,~12942)
,(19363,~2587)
,(18562,25234)
,(~13487,30788)
,(519,25866)
,(~18018,22290)
,(10463,~22599)
,(~29564,~30510)
,(~27204,25090)
,(4582,3410)
,(~17217,~14189)
,(6067,~18091)
,(~14279,~23411)
,(~7368,~18197)
,(~19138,18120)
,(19199,~6133)
,(~20850,12613)
,(~6491,17323)
,(~28335,~3465)
,(10698,~3580)
,(~9709,27887)
,(~7860,31764)
,(~16663,~4861)
,(23458,~1630)
,(~20845,30109)
,(~593,~14289)
,(~9048,10721)
,(~7690,29148)
,(~9305,15974)
,(~20460,18157)
,(11919,~18452)
,(15461,~27077)
,(6985,9017)
,(11981,23105)
,(10730,22687)
,(30869,~12272)
,(4190,25508)
,(12946,29240)
,(19705,9907)
,(~18078,~21689)
,(30131,774)
,(17387,3482)
,(~12388,~23187)
,(5033,~23179)
,(2319,30207)
,(~25738,~26469)
,(15227,24769)
,(20557,~5467)
,(~29623,~9396)
,(~25608,~21349)
,(27641,2569)
,(18342,3270)
,(~6792,~26004)
,(15014,8139)
,(~6572,~11425)
,(11945,~3058)
,(19129,~1469)
,(30089,11332)
,(~24067,~10313)
,(~21870,~26038)
,(~727,18726)
,(~12689,13622)
,(~9560,~29681)
,(~3433,~28062)
,(8788,~17643)
,(28877,~32272)
,(~16390,~32541)
,(26378,14188)
,(~4270,~17519)
,(~27803,29731)
,(222,13502)
,(11733,~12112)
,(8078,28266)
,(~25777,10605)
,(~30703,13488)
,(26966,18090)
,(3756,~32684)
,(~27787,~23390)
,(~27581,7043)
,(22763,11632)
,(~29435,20284)
,(22228,~6854)
,(25743,4536)
,(23460,17641)
,(~30570,~6050)
,(19818,~2425)
,(8638,24290)
,(~21332,~5308)
,(11565,~2976)
,(~14190,~12894)
,(~11104,~14687)
,(~23593,22112)
,(~24947,~22167)
,(4975,25073)
,(~29104,28659)
,(7059,~12011)
,(11884,~20917)
,(~8839,~28789)
,(13980,~29882)
,(~17851,~6956)
,(10695,30472)
,(11832,14995)
,(~11472,8592)
,(~7538,~11856)
,(10673,~10164)
,(~17560,~21617)
,(~968,340)
,(32715,31196)
,(7893,23462)
,(16755,21680)
,(~14162,28309)
,(8901,~1475)
,(~5860,~9857)
,(~8374,~2592)
,(~19764,~5080)
,(10819,~21844)
,(~25874,~19474)
,(~8145,~31352)
,(~26708,~23286)
,(32309,~25542)
,(~16993,2841)
,(~6316,~18572)
,(~14485,~2585)
,(~17056,22808)
,(~2166,28386)
,(~12153,29113)
,(~7446,~4839)
,(8637,~19381)
,(~19951,~32123)
,(~25270,26715)
,(~28298,16521)
,(2587,11206)
,(~4575,3614)
,(~19078,~23735)
,(~14363,~7799)
,(~17011,10054)
,(~6860,2560)
,(~6521,~19774)
,(28214,19021)
,(~21784,~25784)
,(10696,~13066)
,(9209,~226)
,(~10989,~1748)
,(~24962,~4736)
,(31001,16262)
,(~23011,16998)
,(29546,~28058)
,(23679,23001)
,(~4197,~14870)
,(~10025,18828)
,(~20014,28766)
,(20930,23287)
,(~8004,8419)
,(~28064,9723)
,(28876,11912)
,(~26425,~15504)
,(10500,~27493)
,(~9383,~25973)
,(~17626,3213)
,(3889,~17087)
,(26789,3456)
,(~10910,26489)
,(~11468,~31584)
,(29231,19552)
,(10425,22321)
,(~10684,~10978)
,(~21249,~9857)
,(~12071,19309)
,(25050,~27715)
,(18476,20434)
,(21093,15109)
,(27676,~8314)
,(8271,~5820)
,(~11034,31212)
,(15366,10066)
,(24413,~15848)
,(21578,~18012)
,(~11709,16040)
,(~17015,23176)
,(~13228,31460)
,(~14458,32324)
,(21226,~22940)
,(17040,20595)
,(18481,14365)
,(24127,8602)
,(23172,~23927)
,(~8146,20806)
,(174,31497)
,(925,~909)
,(~29261,~24360)
,(~23936,~3603)
,(~31202,17081)
,(~17299,15257)
,(~8409,~32201)
,(~10153,12583)
,(20113,20120)
,(8917,~16516)
,(~16900,16626)
,(~25692,28844)
,(16141,~21671)
,(20564,~14246)
,(21846,18776)
,(~12420,~25557)
,(~7268,11178)
,(27109,13109)
,(~3941,~32318)
,(~17024,~17451)
,(14439,26660)
,(20092,20517)
,(20318,~7270)
,(26506,~24435)
,(17455,16680)
,(733,~11779)
,(~7104,~19600)
,(24491,~14247)
,(~30237,~11341)
,(3814,~27164)
,(14698,27963)
,(~20146,28592)
,(13416,~27200)
,(18678,6661)
,(438,~20105)
,(10812,~5268)
,(~12087,31276)
,(27439,8819)
,(~8312,605)
,(~17260,23659)
,(~5213,~13943)
,(~30819,8559)
,(204,~193)
,(~19224,30358)
,(~32514,~21452)
,(~1816,~8326)
,(~15769,~24148)
,(13378,~11068)
,(24230,~20973)
,(7014,~24755)
,(~27965,~5280)
,(5266,~7216)
,(~24179,~14673)
,(~25198,15101)
,(20922,31420)
,(~1401,3802)
,(17964,1801)
,(~27445,~10971)
,(28542,~17179)
,(14760,19544)
,(411,30768)
,(~9891,18956)
,(9774,~9213)
,(17063,~9243)
,(~25254,710)
,(~28862,~30159)
,(~23028,~2076)
,(219,29743)
,(2235,~13126)
,(8318,23413)
,(~6943,~25970)
,(~19728,31850)
,(~15866,~13788)
,(~6289,~1960)
,(24772,687)
,(5444,25458)
,(~23461,~14499)
,(10506,~40)
,(~19290,~6996)
,(~28004,2196)
,(~31380,~25358)
,(6477,5289)
,(11523,~2882)
,(~18964,~32104)
,(~28408,3191)
,(~6216,~27761)
,(~3119,8059)
,(10864,25123)
,(~16294,~7869)
,(4219,8793)
,(~12438,~1046)
,(~32756,~21703)
,(19729,~28872)
,(9366,2575)
,(22291,~8293)
,(~16223,~32363)
,(24967,~9272)
,(~16198,8529)
,(~4432,~15184)
,(~30795,~24985)
,(2835,~13291)
,(~2577,~10926)
,(~15193,~32095)
,(13061,~5274)
,(32491,~249)
,(11836,5957)
,(~32158,20790)
,(~7366,~23429)
,(2774,23742)
,(25585,10492)
,(14314,~28599)
,(14630,17795)
,(~16024,19130)
,(5182,9016)
,(21018,~21465)
,(~27161,29601)
,(116,~5785)
,(24402,~14644)
,(~2544,18946)
,(~23021,~10949)
,(10600,28773)
,(26965,~17064)
,(2651,~26892)
,(5393,351)
,(16799,~31092)
,(~9483,3492)
,(~17578,16291)
,(22820,~4811)
,(~11698,~30653)
,(~21458,~7237)
];
