
# Set working directory
# setwd()


new_data <- read.table('nlsy97.dat', sep=' ')
names(new_data) <- c('E5111701',
'E5111702',
'E5111703',
'E5111704',
'E5111705',
'E5111706',
'E5111707',
'E5111708',
'E5111709',
'E5111710',
'E5111711',
'E5111712',
'E5111801',
'E5111802',
'E5111803',
'E5111804',
'E5111805',
'E5111806',
'E5111807',
'E5111808',
'E5111809',
'E5111810',
'E5111811',
'E5111812',
'E5111901',
'E5111902',
'E5111903',
'E5111904',
'E5111905',
'E5111906',
'E5111907',
'E5111908',
'E5111909',
'E5111910',
'E5111911',
'E5111912',
'E5112001',
'E5112002',
'E5112003',
'E5112004',
'E5112005',
'E5112006',
'E5112007',
'E5112008',
'E5112009',
'E5112010',
'E5112011',
'E5112012',
'E5112101',
'E5112102',
'E5112103',
'E5112104',
'E5112105',
'E5112106',
'E5112107',
'E5112108',
'E5112109',
'E5112110',
'E5112111',
'E5112112',
'E5112201',
'E5112202',
'E5112203',
'E5112204',
'E5112205',
'E5112206',
'E5112207',
'E5112208',
'E5112209',
'E5112210',
'E5112211',
'E5112212',
'E5112301',
'E5112302',
'E5112303',
'E5112304',
'E5112305',
'E5112306',
'E5112307',
'E5112308',
'E5112309',
'E5112310',
'E5112311',
'E5112312',
'E5112401',
'E5112402',
'E5112403',
'E5112404',
'E5112405',
'E5112406',
'E5112407',
'E5112408',
'E5112409',
'E5112410',
'E5112411',
'E5112412',
'E5112501',
'E5112502',
'E5112503',
'E5112504',
'E5112505',
'E5112506',
'E5112507',
'E5112508',
'E5112509',
'E5112510',
'E5112511',
'E5112512',
'E5112601',
'E5112602',
'E5112603',
'E5112604',
'E5112605',
'E5112606',
'E5112607',
'E5112608',
'E5112609',
'E5112610',
'E5112611',
'E5112612',
'E5112701',
'E5112702',
'E5112703',
'E5112704',
'E5112705',
'E5112706',
'E5112707',
'E5112708',
'E5112709',
'E5112710',
'E5112711',
'E5112712',
'E5112801',
'E5112802',
'E5112803',
'E5112804',
'E5112805',
'E5112806',
'E5112807',
'E5112808',
'E5112809',
'E5112810',
'E5112811',
'E5112812',
'E5112901',
'E5112902',
'E5112903',
'E5112904',
'E5112905',
'E5112906',
'E5112907',
'E5112908',
'E5112909',
'E5112910',
'E5112911',
'E5112912',
'E5113001',
'E5113002',
'E5113003',
'E5113004',
'E5113005',
'E5113006',
'E5113007',
'E5113008',
'E5113009',
'E5113010',
'E5113011',
'E5113012',
'E5113101',
'E5113102',
'E5113103',
'E5113104',
'E5113105',
'E5113106',
'E5113107',
'E5113108',
'E5113109',
'E5113110',
'E5113111',
'E5113112',
'E5113201',
'E5113202',
'E5113203',
'E5113204',
'E5113205',
'E5113206',
'E5113207',
'E5113208',
'E5113209',
'E5113210',
'E5113211',
'E5113212',
'E5113301',
'E5113302',
'E5113303',
'E5113304',
'E5113305',
'E5113306',
'E5113307',
'E5113308',
'E5113309',
'E5113310',
'E5113311',
'E5113312',
'E5113401',
'E5113402',
'E5113403',
'E5113404',
'E5113405',
'E5113406',
'E5113407',
'E5113408',
'E5113409',
'E5113410',
'E5113411',
'E5113412',
'E5113501',
'E5113502',
'E5113503',
'E5113504',
'E5113505',
'E5113506',
'E5113507',
'E5113508',
'E5113509',
'E5113510',
'E5113511',
'E5113512',
'E5113601',
'E5113602',
'E5113603',
'E5113604',
'E5113605',
'E5113606',
'E5113607',
'E5113608',
'E5113609',
'E5113610',
'E5113611',
'E5113612',
'E5113701',
'E5113702',
'E5113703',
'E5113704',
'E5113705',
'E5113706',
'E5113707',
'E5113708',
'E5113709',
'E5113710',
'E5113711',
'E5113712',
'E5113801',
'E5113802',
'E5113803',
'E5113804',
'E5113805',
'E5113806',
'E5113807',
'E5113808',
'E5113809',
'E5113810',
'E5113811',
'E5113812',
'E5113901',
'E5113902',
'E5113903',
'E5113904',
'E5113905',
'E5113906',
'E5113907',
'E5113908',
'E5113909',
'E5113910',
'E5113911',
'E5113912',
'E5114001',
'E5114002',
'E5114003',
'E5114004',
'E5114005',
'E5114006',
'E5114007',
'R0000100',
'R0536300',
'R0536401',
'R0536402',
'R1193900',
'R1200200',
'R1201400',
'R1204500',
'R1204600',
'R1204700',
'R1209300',
'R1235800',
'R1236101',
'R1302400',
'R1302500',
'R1302600',
'R1302700',
'R1482600',
'R2553400',
'R2560001',
'R2563300',
'R2568200',
'R3876200',
'R3881501',
'R3884900',
'R3890100',
'R5453600',
'R5460601',
'R5464100',
'R5472200',
'R7215900',
'R7224201',
'R7227800',
'R7236000',
'R9792900',
'R9829600',
'R9871900',
'S1531300',
'S1538001',
'S1541700',
'S1550800',
'S2000900',
'S2007701',
'S2011500',
'S2020700',
'S3801000',
'S3808501',
'S3821900',
'S5400900',
'S5408900',
'S5421900',
'S7501100',
'S7509700',
'S7524000',
'T0008400',
'T0013000',
'T0024400',
'T2011000',
'T2015900',
'T2019300',
'T3601400',
'T3606200',
'T3609900',
'T5201300',
'T5206600',
'T5210300',
'T6651200',
'T6656400',
'T6661300',
'T8123500',
'T8128800',
'T8132800',
'U0001700',
'U0008700',
'U0013100',
'U1838400',
'U1845300',
'U1850600',
'U3437800',
'U3443800',
'U3450100',
'Z9033700',
'Z9033800',
'Z9033900',
'Z9034000',
'Z9083800',
'Z9084100',
'Z9084200',
'Z9084300',
'Z9084400',
'Z9084500',
'Z9084600',
'Z9084700',
'Z9085100')


# Handle missing values

#new_data[new_data == -1] = NA  # Refused
#new_data[new_data == -2] = NA  # Dont know
#new_data[new_data == -3] = NA  # Invalid missing
#new_data[new_data == -4] = NA  # Valid missing
#new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$E5111701 <- factor(data$E5111701,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111702 <- factor(data$E5111702,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111703 <- factor(data$E5111703,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111704 <- factor(data$E5111704,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111705 <- factor(data$E5111705,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111706 <- factor(data$E5111706,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111707 <- factor(data$E5111707,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111708 <- factor(data$E5111708,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111709 <- factor(data$E5111709,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111710 <- factor(data$E5111710,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111711 <- factor(data$E5111711,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111712 <- factor(data$E5111712,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111801 <- factor(data$E5111801,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111802 <- factor(data$E5111802,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111803 <- factor(data$E5111803,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111804 <- factor(data$E5111804,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111805 <- factor(data$E5111805,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111806 <- factor(data$E5111806,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111807 <- factor(data$E5111807,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111808 <- factor(data$E5111808,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111809 <- factor(data$E5111809,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111810 <- factor(data$E5111810,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111811 <- factor(data$E5111811,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111812 <- factor(data$E5111812,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111901 <- factor(data$E5111901,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111902 <- factor(data$E5111902,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111903 <- factor(data$E5111903,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111904 <- factor(data$E5111904,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111905 <- factor(data$E5111905,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111906 <- factor(data$E5111906,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111907 <- factor(data$E5111907,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111908 <- factor(data$E5111908,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111909 <- factor(data$E5111909,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111910 <- factor(data$E5111910,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111911 <- factor(data$E5111911,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5111912 <- factor(data$E5111912,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112001 <- factor(data$E5112001,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112002 <- factor(data$E5112002,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112003 <- factor(data$E5112003,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112004 <- factor(data$E5112004,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112005 <- factor(data$E5112005,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112006 <- factor(data$E5112006,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112007 <- factor(data$E5112007,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112008 <- factor(data$E5112008,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112009 <- factor(data$E5112009,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112010 <- factor(data$E5112010,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112011 <- factor(data$E5112011,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112012 <- factor(data$E5112012,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112101 <- factor(data$E5112101,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112102 <- factor(data$E5112102,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112103 <- factor(data$E5112103,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112104 <- factor(data$E5112104,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112105 <- factor(data$E5112105,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112106 <- factor(data$E5112106,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112107 <- factor(data$E5112107,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112108 <- factor(data$E5112108,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112109 <- factor(data$E5112109,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112110 <- factor(data$E5112110,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112111 <- factor(data$E5112111,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112112 <- factor(data$E5112112,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112201 <- factor(data$E5112201,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112202 <- factor(data$E5112202,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112203 <- factor(data$E5112203,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112204 <- factor(data$E5112204,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112205 <- factor(data$E5112205,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112206 <- factor(data$E5112206,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112207 <- factor(data$E5112207,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112208 <- factor(data$E5112208,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112209 <- factor(data$E5112209,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112210 <- factor(data$E5112210,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112211 <- factor(data$E5112211,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112212 <- factor(data$E5112212,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112301 <- factor(data$E5112301,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112302 <- factor(data$E5112302,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112303 <- factor(data$E5112303,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112304 <- factor(data$E5112304,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112305 <- factor(data$E5112305,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112306 <- factor(data$E5112306,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112307 <- factor(data$E5112307,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112308 <- factor(data$E5112308,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112309 <- factor(data$E5112309,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112310 <- factor(data$E5112310,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112311 <- factor(data$E5112311,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112312 <- factor(data$E5112312,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112401 <- factor(data$E5112401,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112402 <- factor(data$E5112402,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112403 <- factor(data$E5112403,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112404 <- factor(data$E5112404,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112405 <- factor(data$E5112405,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112406 <- factor(data$E5112406,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112407 <- factor(data$E5112407,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112408 <- factor(data$E5112408,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112409 <- factor(data$E5112409,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112410 <- factor(data$E5112410,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112411 <- factor(data$E5112411,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112412 <- factor(data$E5112412,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112501 <- factor(data$E5112501,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112502 <- factor(data$E5112502,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112503 <- factor(data$E5112503,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112504 <- factor(data$E5112504,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112505 <- factor(data$E5112505,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112506 <- factor(data$E5112506,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112507 <- factor(data$E5112507,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112508 <- factor(data$E5112508,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112509 <- factor(data$E5112509,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112510 <- factor(data$E5112510,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112511 <- factor(data$E5112511,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112512 <- factor(data$E5112512,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112601 <- factor(data$E5112601,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112602 <- factor(data$E5112602,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112603 <- factor(data$E5112603,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112604 <- factor(data$E5112604,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112605 <- factor(data$E5112605,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112606 <- factor(data$E5112606,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112607 <- factor(data$E5112607,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112608 <- factor(data$E5112608,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112609 <- factor(data$E5112609,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112610 <- factor(data$E5112610,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112611 <- factor(data$E5112611,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112612 <- factor(data$E5112612,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112701 <- factor(data$E5112701,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112702 <- factor(data$E5112702,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112703 <- factor(data$E5112703,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112704 <- factor(data$E5112704,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112705 <- factor(data$E5112705,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112706 <- factor(data$E5112706,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112707 <- factor(data$E5112707,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112708 <- factor(data$E5112708,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112709 <- factor(data$E5112709,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112710 <- factor(data$E5112710,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112711 <- factor(data$E5112711,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112712 <- factor(data$E5112712,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112801 <- factor(data$E5112801,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112802 <- factor(data$E5112802,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112803 <- factor(data$E5112803,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112804 <- factor(data$E5112804,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112805 <- factor(data$E5112805,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112806 <- factor(data$E5112806,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112807 <- factor(data$E5112807,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112808 <- factor(data$E5112808,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112809 <- factor(data$E5112809,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112810 <- factor(data$E5112810,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112811 <- factor(data$E5112811,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112812 <- factor(data$E5112812,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112901 <- factor(data$E5112901,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112902 <- factor(data$E5112902,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112903 <- factor(data$E5112903,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112904 <- factor(data$E5112904,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112905 <- factor(data$E5112905,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112906 <- factor(data$E5112906,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112907 <- factor(data$E5112907,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112908 <- factor(data$E5112908,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112909 <- factor(data$E5112909,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112910 <- factor(data$E5112910,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112911 <- factor(data$E5112911,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5112912 <- factor(data$E5112912,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113001 <- factor(data$E5113001,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113002 <- factor(data$E5113002,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113003 <- factor(data$E5113003,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113004 <- factor(data$E5113004,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113005 <- factor(data$E5113005,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113006 <- factor(data$E5113006,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113007 <- factor(data$E5113007,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113008 <- factor(data$E5113008,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113009 <- factor(data$E5113009,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113010 <- factor(data$E5113010,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113011 <- factor(data$E5113011,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113012 <- factor(data$E5113012,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113101 <- factor(data$E5113101,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113102 <- factor(data$E5113102,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113103 <- factor(data$E5113103,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113104 <- factor(data$E5113104,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113105 <- factor(data$E5113105,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113106 <- factor(data$E5113106,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113107 <- factor(data$E5113107,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113108 <- factor(data$E5113108,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113109 <- factor(data$E5113109,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113110 <- factor(data$E5113110,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113111 <- factor(data$E5113111,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113112 <- factor(data$E5113112,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113201 <- factor(data$E5113201,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113202 <- factor(data$E5113202,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113203 <- factor(data$E5113203,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113204 <- factor(data$E5113204,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113205 <- factor(data$E5113205,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113206 <- factor(data$E5113206,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113207 <- factor(data$E5113207,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113208 <- factor(data$E5113208,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113209 <- factor(data$E5113209,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113210 <- factor(data$E5113210,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113211 <- factor(data$E5113211,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113212 <- factor(data$E5113212,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113301 <- factor(data$E5113301,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113302 <- factor(data$E5113302,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113303 <- factor(data$E5113303,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113304 <- factor(data$E5113304,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113305 <- factor(data$E5113305,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113306 <- factor(data$E5113306,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113307 <- factor(data$E5113307,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113308 <- factor(data$E5113308,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113309 <- factor(data$E5113309,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113310 <- factor(data$E5113310,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113311 <- factor(data$E5113311,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113312 <- factor(data$E5113312,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113401 <- factor(data$E5113401,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113402 <- factor(data$E5113402,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113403 <- factor(data$E5113403,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113404 <- factor(data$E5113404,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113405 <- factor(data$E5113405,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113406 <- factor(data$E5113406,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113407 <- factor(data$E5113407,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113408 <- factor(data$E5113408,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113409 <- factor(data$E5113409,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113410 <- factor(data$E5113410,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113411 <- factor(data$E5113411,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113412 <- factor(data$E5113412,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113501 <- factor(data$E5113501,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113502 <- factor(data$E5113502,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113503 <- factor(data$E5113503,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113504 <- factor(data$E5113504,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113505 <- factor(data$E5113505,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113506 <- factor(data$E5113506,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113507 <- factor(data$E5113507,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113508 <- factor(data$E5113508,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113509 <- factor(data$E5113509,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113510 <- factor(data$E5113510,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113511 <- factor(data$E5113511,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113512 <- factor(data$E5113512,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113601 <- factor(data$E5113601,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113602 <- factor(data$E5113602,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113603 <- factor(data$E5113603,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113604 <- factor(data$E5113604,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113605 <- factor(data$E5113605,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113606 <- factor(data$E5113606,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113607 <- factor(data$E5113607,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113608 <- factor(data$E5113608,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113609 <- factor(data$E5113609,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113610 <- factor(data$E5113610,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113611 <- factor(data$E5113611,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113612 <- factor(data$E5113612,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113701 <- factor(data$E5113701,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113702 <- factor(data$E5113702,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113703 <- factor(data$E5113703,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113704 <- factor(data$E5113704,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113705 <- factor(data$E5113705,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113706 <- factor(data$E5113706,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113707 <- factor(data$E5113707,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113708 <- factor(data$E5113708,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113709 <- factor(data$E5113709,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113710 <- factor(data$E5113710,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113711 <- factor(data$E5113711,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113712 <- factor(data$E5113712,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113801 <- factor(data$E5113801,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113802 <- factor(data$E5113802,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113803 <- factor(data$E5113803,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113804 <- factor(data$E5113804,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113805 <- factor(data$E5113805,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113806 <- factor(data$E5113806,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113807 <- factor(data$E5113807,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113808 <- factor(data$E5113808,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113809 <- factor(data$E5113809,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113810 <- factor(data$E5113810,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113811 <- factor(data$E5113811,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113812 <- factor(data$E5113812,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113901 <- factor(data$E5113901,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113902 <- factor(data$E5113902,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113903 <- factor(data$E5113903,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113904 <- factor(data$E5113904,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113905 <- factor(data$E5113905,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113906 <- factor(data$E5113906,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113907 <- factor(data$E5113907,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113908 <- factor(data$E5113908,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113909 <- factor(data$E5113909,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113910 <- factor(data$E5113910,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113911 <- factor(data$E5113911,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5113912 <- factor(data$E5113912,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114001 <- factor(data$E5114001,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114002 <- factor(data$E5114002,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114003 <- factor(data$E5114003,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114004 <- factor(data$E5114004,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114005 <- factor(data$E5114005,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114006 <- factor(data$E5114006,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$E5114007 <- factor(data$E5114007,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Not enrolled in college",
"Enrolled in 2-year college",
"Enrolled in 4-year college",
"Enrolled in Graduate program"))
  data$R0536300 <- factor(data$R0536300,
levels=c(0.0,1.0,2.0),
labels=c("No Information",
"Male",
"Female"))
  data$R0536401 <- factor(data$R0536401,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
labels=c("1: January",
"2: February",
"3: March",
"4: April",
"5: May",
"6: June",
"7: July",
"8: August",
"9: September",
"10: October",
"11: November",
"12: December"))
  data$R1201400 <- factor(data$R1201400,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$R1204600 <- factor(data$R1204600,
levels=c(1.0,2.0),
labels=c("Parent",
"Youth"))
  data$R1235800 <- factor(data$R1235800,
levels=c(0.0,1.0),
labels=c("Oversample",
"Cross-sectional"))
  data$R1302400 <- factor(data$R1302400,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302500 <- factor(data$R1302500,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302600 <- factor(data$R1302600,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1302700 <- factor(data$R1302700,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("NONE",
"1ST GRADE",
"2ND GRADE",
"3RD GRADE",
"4TH GRADE",
"5TH GRADE",
"6TH GRADE",
"7TH GRADE",
"8TH GRADE",
"9TH GRADE",
"10TH GRADE",
"11TH GRADE",
"12TH GRADE",
"1ST YEAR COLLEGE",
"2ND YEAR COLLEGE",
"3RD YEAR COLLEGE",
"4TH YEAR COLLEGE",
"5TH YEAR COLLEGE",
"6TH YEAR COLLEGE",
"7TH YEAR COLLEGE",
"8TH YEAR COLLEGE OR MORE",
"UNGRADED"))
  data$R1482600 <- factor(data$R1482600,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Black",
"Hispanic",
"Mixed Race (Non-Hispanic)",
"Non-Black / Non-Hispanic"))
  data$R2560001 <- factor(data$R2560001,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$R3881501 <- factor(data$R3881501,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$R5460601 <- factor(data$R5460601,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$R7224201 <- factor(data$R7224201,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$S1538001 <- factor(data$S1538001,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$S2007701 <- factor(data$S2007701,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$S3808501 <- factor(data$S3808501,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$S5408900 <- factor(data$S5408900,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$S7509700 <- factor(data$S7509700,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$T0013000 <- factor(data$T0013000,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$T2015900 <- factor(data$T2015900,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$T3606200 <- factor(data$T3606200,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$T5206600 <- factor(data$T5206600,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$T6656400 <- factor(data$T6656400,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$T8128800 <- factor(data$T8128800,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U0008700 <- factor(data$U0008700,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U1845300 <- factor(data$U1845300,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$U3443800 <- factor(data$U3443800,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0),
labels=c("Not enrolled, no high school degree, no GED",
"Not enrolled, GED",
"Not enrolled, high school degree",
"Not enrolled, some college",
"Not enrolled, 2-year college graduate",
"Not enrolled, 4-year college graduate",
"Not enrolled, graduate degree",
"Enrolled in grades 1-12, not a high school graduate",
"Enrolled in a 2-year college",
"Enrolled in a 4-year college",
"Enrolled in a graduate program"))
  data$Z9033700 <- factor(data$Z9033700,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0),
labels=c("Have not yet received the scores",
"200 - 300",
"301 - 400",
"401 - 500",
"501 - 600",
"601 - 700",
"701 - 800"))
  data$Z9033800 <- factor(data$Z9033800,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0),
labels=c("Round 1",
"Round 2",
"Round 3",
"Round 4",
"Round 5",
"Round 6",
"Round 7",
"Round 8",
"Round 9",
"Round 10",
"Round 11",
"Round 12",
"Round 13",
"Round 14",
"Round 15",
"Round 16",
"Round 17",
"Round 18",
"Round 19"))
  data$Z9033900 <- factor(data$Z9033900,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0),
labels=c("Have not yet received the scores",
"200 - 300",
"301 - 400",
"401 - 500",
"501 - 600",
"601 - 700",
"701 - 800"))
  data$Z9034000 <- factor(data$Z9034000,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0),
labels=c("Round 1",
"Round 2",
"Round 3",
"Round 4",
"Round 5",
"Round 6",
"Round 7",
"Round 8",
"Round 9",
"Round 10",
"Round 11",
"Round 12",
"Round 13",
"Round 14",
"Round 15",
"Round 16",
"Round 17",
"Round 18",
"Round 19"))
  data$Z9083800 <- factor(data$Z9083800,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
labels=c("None",
"1st grade",
"2nd grade",
"3rd grade",
"4th grade",
"5th grade",
"6th grade",
"7th grade",
"8th grade",
"9th grade",
"10th grade",
"11th grade",
"12th grade",
"1st year college",
"2nd year college",
"3rd year college",
"4th year college",
"5th year college",
"6th year college",
"7th year college",
"8th year college or more",
"Ungraded"))
  data$Z9085100 <- factor(data$Z9085100,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0),
labels=c("Round 1",
"Round 2",
"Round 3",
"Round 4",
"Round 5",
"Round 6",
"Round 7",
"Round 8",
"Round 9",
"Round 10",
"Round 11",
"Round 12",
"Round 13",
"Round 14",
"Round 15",
"Round 16",
"Round 17",
"Round 18",
"Round 19"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
data$R0000100 <- factor(data$R0000100,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999"))
data$R1193900[0.0 <= data$R1193900 & data$R1193900 <= 139.0] <- 0.0
data$R1193900[140.0 <= data$R1193900 & data$R1193900 <= 150.0] <- 140.0
data$R1193900[151.0 <= data$R1193900 & data$R1193900 <= 160.0] <- 151.0
data$R1193900[161.0 <= data$R1193900 & data$R1193900 <= 170.0] <- 161.0
data$R1193900[171.0 <= data$R1193900 & data$R1193900 <= 180.0] <- 171.0
data$R1193900[181.0 <= data$R1193900 & data$R1193900 <= 190.0] <- 181.0
data$R1193900[191.0 <= data$R1193900 & data$R1193900 <= 200.0] <- 191.0
data$R1193900[201.0 <= data$R1193900 & data$R1193900 <= 210.0] <- 201.0
data$R1193900[211.0 <= data$R1193900 & data$R1193900 <= 220.0] <- 211.0
data$R1193900[221.0 <= data$R1193900 & data$R1193900 <= 230.0] <- 221.0
data$R1193900 <- factor(data$R1193900,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230"))
data$R1200200[1.0 <= data$R1200200 & data$R1200200 <= 4.0] <- 1.0
data$R1200200[5.0 <= data$R1200200 & data$R1200200 <= 8.0] <- 5.0
data$R1200200[9.0 <= data$R1200200 & data$R1200200 <= 14.0] <- 9.0
data$R1200200[15.0 <= data$R1200200 & data$R1200200 <= 19.0] <- 15.0
data$R1200200[20.0 <= data$R1200200 & data$R1200200 <= 24.0] <- 20.0
data$R1200200[25.0 <= data$R1200200 & data$R1200200 <= 29.0] <- 25.0
data$R1200200[30.0 <= data$R1200200 & data$R1200200 <= 34.0] <- 30.0
data$R1200200[35.0 <= data$R1200200 & data$R1200200 <= 39.0] <- 35.0
data$R1200200[40.0 <= data$R1200200 & data$R1200200 <= 44.0] <- 40.0
data$R1200200[45.0 <= data$R1200200 & data$R1200200 <= 49.0] <- 45.0
data$R1200200[50.0 <= data$R1200200 & data$R1200200 <= 9.9999999E7] <- 50.0
data$R1200200 <- factor(data$R1200200,
levels=c(0.0,1.0,5.0,9.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0),
labels=c("0",
"1 TO 4",
"5 TO 8",
"9 TO 14",
"15 TO 19",
"20 TO 24",
"25 TO 29",
"30 TO 34",
"35 TO 39",
"40 TO 44",
"45 TO 49",
"50 TO 99999999: 50+"))
data$R1204500[-999999.0 <= data$R1204500 & data$R1204500 <= -3000.0] <- -999999.0
data$R1204500[-2999.0 <= data$R1204500 & data$R1204500 <= -2000.0] <- -2999.0
data$R1204500[-1999.0 <= data$R1204500 & data$R1204500 <= -1000.0] <- -1999.0
data$R1204500[-999.0 <= data$R1204500 & data$R1204500 <= -1.0] <- -999.0
data$R1204500[1.0 <= data$R1204500 & data$R1204500 <= 1000.0] <- 1.0
data$R1204500[1001.0 <= data$R1204500 & data$R1204500 <= 2000.0] <- 1001.0
data$R1204500[2001.0 <= data$R1204500 & data$R1204500 <= 3000.0] <- 2001.0
data$R1204500[3001.0 <= data$R1204500 & data$R1204500 <= 5000.0] <- 3001.0
data$R1204500[5001.0 <= data$R1204500 & data$R1204500 <= 10000.0] <- 5001.0
data$R1204500[10001.0 <= data$R1204500 & data$R1204500 <= 20000.0] <- 10001.0
data$R1204500[20001.0 <= data$R1204500 & data$R1204500 <= 30000.0] <- 20001.0
data$R1204500[30001.0 <= data$R1204500 & data$R1204500 <= 40000.0] <- 30001.0
data$R1204500[40001.0 <= data$R1204500 & data$R1204500 <= 50000.0] <- 40001.0
data$R1204500[50001.0 <= data$R1204500 & data$R1204500 <= 65000.0] <- 50001.0
data$R1204500[65001.0 <= data$R1204500 & data$R1204500 <= 80000.0] <- 65001.0
data$R1204500[80001.0 <= data$R1204500 & data$R1204500 <= 100000.0] <- 80001.0
data$R1204500[100001.0 <= data$R1204500 & data$R1204500 <= 150000.0] <- 100001.0
data$R1204500[150001.0 <= data$R1204500 & data$R1204500 <= 200000.0] <- 150001.0
data$R1204500[200001.0 <= data$R1204500 & data$R1204500 <= 999999.0] <- 200001.0
data$R1204500 <- factor(data$R1204500,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$R1204700[-999999.0 <= data$R1204700 & data$R1204700 <= -3000.0] <- -999999.0
data$R1204700[-2999.0 <= data$R1204700 & data$R1204700 <= -2000.0] <- -2999.0
data$R1204700[-1999.0 <= data$R1204700 & data$R1204700 <= -1000.0] <- -1999.0
data$R1204700[-999.0 <= data$R1204700 & data$R1204700 <= -6.0] <- -999.0
data$R1204700[1.0 <= data$R1204700 & data$R1204700 <= 1000.0] <- 1.0
data$R1204700[1001.0 <= data$R1204700 & data$R1204700 <= 2000.0] <- 1001.0
data$R1204700[2001.0 <= data$R1204700 & data$R1204700 <= 3000.0] <- 2001.0
data$R1204700[3001.0 <= data$R1204700 & data$R1204700 <= 5000.0] <- 3001.0
data$R1204700[5001.0 <= data$R1204700 & data$R1204700 <= 10000.0] <- 5001.0
data$R1204700[10001.0 <= data$R1204700 & data$R1204700 <= 20000.0] <- 10001.0
data$R1204700[20001.0 <= data$R1204700 & data$R1204700 <= 30000.0] <- 20001.0
data$R1204700[30001.0 <= data$R1204700 & data$R1204700 <= 40000.0] <- 30001.0
data$R1204700[40001.0 <= data$R1204700 & data$R1204700 <= 50000.0] <- 40001.0
data$R1204700[50001.0 <= data$R1204700 & data$R1204700 <= 65000.0] <- 50001.0
data$R1204700[65001.0 <= data$R1204700 & data$R1204700 <= 80000.0] <- 65001.0
data$R1204700[80001.0 <= data$R1204700 & data$R1204700 <= 100000.0] <- 80001.0
data$R1204700[100001.0 <= data$R1204700 & data$R1204700 <= 150000.0] <- 100001.0
data$R1204700[150001.0 <= data$R1204700 & data$R1204700 <= 200000.0] <- 150001.0
data$R1204700[200001.0 <= data$R1204700 & data$R1204700 <= 500000.0] <- 200001.0
data$R1204700[500001.0 <= data$R1204700 & data$R1204700 <= 1000000.0] <- 500001.0
data$R1204700[1000001.0 <= data$R1204700 & data$R1204700 <= 1500000.0] <- 1000001.0
data$R1204700[1500001.0 <= data$R1204700 & data$R1204700 <= 2000000.0] <- 1500001.0
data$R1204700[2000001.0 <= data$R1204700 & data$R1204700 <= 9.99999999E8] <- 2000001.0
data$R1204700 <- factor(data$R1204700,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0,500001.0,1000001.0,1500001.0,2000001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -6",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 500000",
"500001 TO 1000000",
"1000001 TO 1500000",
"1500001 TO 2000000",
"2000001 TO 999999999: 2000001+"))
data$R1209300[0.0 <= data$R1209300 & data$R1209300 <= 139.0] <- 0.0
data$R1209300[140.0 <= data$R1209300 & data$R1209300 <= 150.0] <- 140.0
data$R1209300[151.0 <= data$R1209300 & data$R1209300 <= 160.0] <- 151.0
data$R1209300[161.0 <= data$R1209300 & data$R1209300 <= 170.0] <- 161.0
data$R1209300[171.0 <= data$R1209300 & data$R1209300 <= 180.0] <- 171.0
data$R1209300[181.0 <= data$R1209300 & data$R1209300 <= 190.0] <- 181.0
data$R1209300[191.0 <= data$R1209300 & data$R1209300 <= 200.0] <- 191.0
data$R1209300[201.0 <= data$R1209300 & data$R1209300 <= 210.0] <- 201.0
data$R1209300[211.0 <= data$R1209300 & data$R1209300 <= 220.0] <- 211.0
data$R1209300[221.0 <= data$R1209300 & data$R1209300 <= 230.0] <- 221.0
data$R1209300 <- factor(data$R1209300,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230"))
data$R1236101[30000.0 <= data$R1236101 & data$R1236101 <= 59999.0] <- 30000.0
data$R1236101[60000.0 <= data$R1236101 & data$R1236101 <= 99999.0] <- 60000.0
data$R1236101[100000.0 <= data$R1236101 & data$R1236101 <= 149999.0] <- 100000.0
data$R1236101[150000.0 <= data$R1236101 & data$R1236101 <= 199999.0] <- 150000.0
data$R1236101[200000.0 <= data$R1236101 & data$R1236101 <= 249999.0] <- 200000.0
data$R1236101[250000.0 <= data$R1236101 & data$R1236101 <= 299999.0] <- 250000.0
data$R1236101[300000.0 <= data$R1236101 & data$R1236101 <= 349999.0] <- 300000.0
data$R1236101[350000.0 <= data$R1236101 & data$R1236101 <= 399999.0] <- 350000.0
data$R1236101[400000.0 <= data$R1236101 & data$R1236101 <= 449999.0] <- 400000.0
data$R1236101[450000.0 <= data$R1236101 & data$R1236101 <= 499999.0] <- 450000.0
data$R1236101[500000.0 <= data$R1236101 & data$R1236101 <= 549999.0] <- 500000.0
data$R1236101[550000.0 <= data$R1236101 & data$R1236101 <= 599999.0] <- 550000.0
data$R1236101[600000.0 <= data$R1236101 & data$R1236101 <= 649999.0] <- 600000.0
data$R1236101[650000.0 <= data$R1236101 & data$R1236101 <= 699999.0] <- 650000.0
data$R1236101[700000.0 <= data$R1236101 & data$R1236101 <= 749999.0] <- 700000.0
data$R1236101[750000.0 <= data$R1236101 & data$R1236101 <= 799999.0] <- 750000.0
data$R1236101[800000.0 <= data$R1236101 & data$R1236101 <= 849999.0] <- 800000.0
data$R1236101[850000.0 <= data$R1236101 & data$R1236101 <= 9999999.0] <- 850000.0
data$R1236101 <- factor(data$R1236101,
levels=c(0.0,30000.0,60000.0,100000.0,150000.0,200000.0,250000.0,300000.0,350000.0,400000.0,450000.0,500000.0,550000.0,600000.0,650000.0,700000.0,750000.0,800000.0,850000.0),
labels=c("0",
"30000 TO 59999: 300.00-599.99",
"60000 TO 99999: 600.00-999.99",
"100000 TO 149999: 1000.00-1499.99",
"150000 TO 199999: 1500.00-1999.99",
"200000 TO 249999: 2000.00-2499.99",
"250000 TO 299999: 2500.00-2999.99",
"300000 TO 349999: 3000.00-3499.99",
"350000 TO 399999: 3500.00-3999.99",
"400000 TO 449999: 4000.00-4499.99",
"450000 TO 499999: 4500.00-4999.99",
"500000 TO 549999: 5000.00-5499.99",
"550000 TO 599999: 5500.00-5999.99",
"600000 TO 649999: 6000.00-6499.99",
"650000 TO 699999: 6500.00-6999.99",
"700000 TO 749999: 7000.00-7499.99",
"750000 TO 799999: 7500.00-7999.99",
"800000 TO 849999: 8000.00-8499.99",
"850000 TO 9999999: 8500.00+"))
data$R2553400[0.0 <= data$R2553400 & data$R2553400 <= 139.0] <- 0.0
data$R2553400[140.0 <= data$R2553400 & data$R2553400 <= 150.0] <- 140.0
data$R2553400[151.0 <= data$R2553400 & data$R2553400 <= 160.0] <- 151.0
data$R2553400[161.0 <= data$R2553400 & data$R2553400 <= 170.0] <- 161.0
data$R2553400[171.0 <= data$R2553400 & data$R2553400 <= 180.0] <- 171.0
data$R2553400[181.0 <= data$R2553400 & data$R2553400 <= 190.0] <- 181.0
data$R2553400[191.0 <= data$R2553400 & data$R2553400 <= 200.0] <- 191.0
data$R2553400[201.0 <= data$R2553400 & data$R2553400 <= 210.0] <- 201.0
data$R2553400[211.0 <= data$R2553400 & data$R2553400 <= 220.0] <- 211.0
data$R2553400[221.0 <= data$R2553400 & data$R2553400 <= 230.0] <- 221.0
data$R2553400[231.0 <= data$R2553400 & data$R2553400 <= 240.0] <- 231.0
data$R2553400 <- factor(data$R2553400,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240"))
data$R2563300[-999999.0 <= data$R2563300 & data$R2563300 <= -3000.0] <- -999999.0
data$R2563300[-2999.0 <= data$R2563300 & data$R2563300 <= -2000.0] <- -2999.0
data$R2563300[-1999.0 <= data$R2563300 & data$R2563300 <= -1000.0] <- -1999.0
data$R2563300[-999.0 <= data$R2563300 & data$R2563300 <= -1.0] <- -999.0
data$R2563300[1.0 <= data$R2563300 & data$R2563300 <= 1000.0] <- 1.0
data$R2563300[1001.0 <= data$R2563300 & data$R2563300 <= 2000.0] <- 1001.0
data$R2563300[2001.0 <= data$R2563300 & data$R2563300 <= 3000.0] <- 2001.0
data$R2563300[3001.0 <= data$R2563300 & data$R2563300 <= 5000.0] <- 3001.0
data$R2563300[5001.0 <= data$R2563300 & data$R2563300 <= 10000.0] <- 5001.0
data$R2563300[10001.0 <= data$R2563300 & data$R2563300 <= 20000.0] <- 10001.0
data$R2563300[20001.0 <= data$R2563300 & data$R2563300 <= 30000.0] <- 20001.0
data$R2563300[30001.0 <= data$R2563300 & data$R2563300 <= 40000.0] <- 30001.0
data$R2563300[40001.0 <= data$R2563300 & data$R2563300 <= 50000.0] <- 40001.0
data$R2563300[50001.0 <= data$R2563300 & data$R2563300 <= 65000.0] <- 50001.0
data$R2563300[65001.0 <= data$R2563300 & data$R2563300 <= 80000.0] <- 65001.0
data$R2563300[80001.0 <= data$R2563300 & data$R2563300 <= 100000.0] <- 80001.0
data$R2563300[100001.0 <= data$R2563300 & data$R2563300 <= 150000.0] <- 100001.0
data$R2563300[150001.0 <= data$R2563300 & data$R2563300 <= 200000.0] <- 150001.0
data$R2563300[200001.0 <= data$R2563300 & data$R2563300 <= 999999.0] <- 200001.0
data$R2563300 <- factor(data$R2563300,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$R2568200[0.0 <= data$R2568200 & data$R2568200 <= 139.0] <- 0.0
data$R2568200[140.0 <= data$R2568200 & data$R2568200 <= 150.0] <- 140.0
data$R2568200[151.0 <= data$R2568200 & data$R2568200 <= 160.0] <- 151.0
data$R2568200[161.0 <= data$R2568200 & data$R2568200 <= 170.0] <- 161.0
data$R2568200[171.0 <= data$R2568200 & data$R2568200 <= 180.0] <- 171.0
data$R2568200[181.0 <= data$R2568200 & data$R2568200 <= 190.0] <- 181.0
data$R2568200[191.0 <= data$R2568200 & data$R2568200 <= 200.0] <- 191.0
data$R2568200[201.0 <= data$R2568200 & data$R2568200 <= 210.0] <- 201.0
data$R2568200[211.0 <= data$R2568200 & data$R2568200 <= 220.0] <- 211.0
data$R2568200[221.0 <= data$R2568200 & data$R2568200 <= 230.0] <- 221.0
data$R2568200[231.0 <= data$R2568200 & data$R2568200 <= 240.0] <- 231.0
data$R2568200 <- factor(data$R2568200,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240"))
data$R3876200[0.0 <= data$R3876200 & data$R3876200 <= 139.0] <- 0.0
data$R3876200[140.0 <= data$R3876200 & data$R3876200 <= 150.0] <- 140.0
data$R3876200[151.0 <= data$R3876200 & data$R3876200 <= 160.0] <- 151.0
data$R3876200[161.0 <= data$R3876200 & data$R3876200 <= 170.0] <- 161.0
data$R3876200[171.0 <= data$R3876200 & data$R3876200 <= 180.0] <- 171.0
data$R3876200[181.0 <= data$R3876200 & data$R3876200 <= 190.0] <- 181.0
data$R3876200[191.0 <= data$R3876200 & data$R3876200 <= 200.0] <- 191.0
data$R3876200[201.0 <= data$R3876200 & data$R3876200 <= 210.0] <- 201.0
data$R3876200[211.0 <= data$R3876200 & data$R3876200 <= 220.0] <- 211.0
data$R3876200[221.0 <= data$R3876200 & data$R3876200 <= 230.0] <- 221.0
data$R3876200[231.0 <= data$R3876200 & data$R3876200 <= 240.0] <- 231.0
data$R3876200[241.0 <= data$R3876200 & data$R3876200 <= 250.0] <- 241.0
data$R3876200 <- factor(data$R3876200,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250"))
data$R3884900[-999999.0 <= data$R3884900 & data$R3884900 <= -3000.0] <- -999999.0
data$R3884900[-2999.0 <= data$R3884900 & data$R3884900 <= -2000.0] <- -2999.0
data$R3884900[-1999.0 <= data$R3884900 & data$R3884900 <= -1000.0] <- -1999.0
data$R3884900[-999.0 <= data$R3884900 & data$R3884900 <= -1.0] <- -999.0
data$R3884900[1.0 <= data$R3884900 & data$R3884900 <= 1000.0] <- 1.0
data$R3884900[1001.0 <= data$R3884900 & data$R3884900 <= 2000.0] <- 1001.0
data$R3884900[2001.0 <= data$R3884900 & data$R3884900 <= 3000.0] <- 2001.0
data$R3884900[3001.0 <= data$R3884900 & data$R3884900 <= 5000.0] <- 3001.0
data$R3884900[5001.0 <= data$R3884900 & data$R3884900 <= 10000.0] <- 5001.0
data$R3884900[10001.0 <= data$R3884900 & data$R3884900 <= 20000.0] <- 10001.0
data$R3884900[20001.0 <= data$R3884900 & data$R3884900 <= 30000.0] <- 20001.0
data$R3884900[30001.0 <= data$R3884900 & data$R3884900 <= 40000.0] <- 30001.0
data$R3884900[40001.0 <= data$R3884900 & data$R3884900 <= 50000.0] <- 40001.0
data$R3884900[50001.0 <= data$R3884900 & data$R3884900 <= 65000.0] <- 50001.0
data$R3884900[65001.0 <= data$R3884900 & data$R3884900 <= 80000.0] <- 65001.0
data$R3884900[80001.0 <= data$R3884900 & data$R3884900 <= 100000.0] <- 80001.0
data$R3884900[100001.0 <= data$R3884900 & data$R3884900 <= 150000.0] <- 100001.0
data$R3884900[150001.0 <= data$R3884900 & data$R3884900 <= 200000.0] <- 150001.0
data$R3884900[200001.0 <= data$R3884900 & data$R3884900 <= 999999.0] <- 200001.0
data$R3884900 <- factor(data$R3884900,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$R3890100[0.0 <= data$R3890100 & data$R3890100 <= 139.0] <- 0.0
data$R3890100[140.0 <= data$R3890100 & data$R3890100 <= 150.0] <- 140.0
data$R3890100[151.0 <= data$R3890100 & data$R3890100 <= 160.0] <- 151.0
data$R3890100[161.0 <= data$R3890100 & data$R3890100 <= 170.0] <- 161.0
data$R3890100[171.0 <= data$R3890100 & data$R3890100 <= 180.0] <- 171.0
data$R3890100[181.0 <= data$R3890100 & data$R3890100 <= 190.0] <- 181.0
data$R3890100[191.0 <= data$R3890100 & data$R3890100 <= 200.0] <- 191.0
data$R3890100[201.0 <= data$R3890100 & data$R3890100 <= 210.0] <- 201.0
data$R3890100[211.0 <= data$R3890100 & data$R3890100 <= 220.0] <- 211.0
data$R3890100[221.0 <= data$R3890100 & data$R3890100 <= 230.0] <- 221.0
data$R3890100[231.0 <= data$R3890100 & data$R3890100 <= 240.0] <- 231.0
data$R3890100[241.0 <= data$R3890100 & data$R3890100 <= 250.0] <- 241.0
data$R3890100 <- factor(data$R3890100,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250"))
data$R5453600[0.0 <= data$R5453600 & data$R5453600 <= 139.0] <- 0.0
data$R5453600[140.0 <= data$R5453600 & data$R5453600 <= 150.0] <- 140.0
data$R5453600[151.0 <= data$R5453600 & data$R5453600 <= 160.0] <- 151.0
data$R5453600[161.0 <= data$R5453600 & data$R5453600 <= 170.0] <- 161.0
data$R5453600[171.0 <= data$R5453600 & data$R5453600 <= 180.0] <- 171.0
data$R5453600[181.0 <= data$R5453600 & data$R5453600 <= 190.0] <- 181.0
data$R5453600[191.0 <= data$R5453600 & data$R5453600 <= 200.0] <- 191.0
data$R5453600[201.0 <= data$R5453600 & data$R5453600 <= 210.0] <- 201.0
data$R5453600[211.0 <= data$R5453600 & data$R5453600 <= 220.0] <- 211.0
data$R5453600[221.0 <= data$R5453600 & data$R5453600 <= 230.0] <- 221.0
data$R5453600[231.0 <= data$R5453600 & data$R5453600 <= 240.0] <- 231.0
data$R5453600[241.0 <= data$R5453600 & data$R5453600 <= 250.0] <- 241.0
data$R5453600[251.0 <= data$R5453600 & data$R5453600 <= 260.0] <- 251.0
data$R5453600 <- factor(data$R5453600,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0,251.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260"))
data$R5464100[-999999.0 <= data$R5464100 & data$R5464100 <= -3000.0] <- -999999.0
data$R5464100[-2999.0 <= data$R5464100 & data$R5464100 <= -2000.0] <- -2999.0
data$R5464100[-1999.0 <= data$R5464100 & data$R5464100 <= -1000.0] <- -1999.0
data$R5464100[-999.0 <= data$R5464100 & data$R5464100 <= -1.0] <- -999.0
data$R5464100[1.0 <= data$R5464100 & data$R5464100 <= 1000.0] <- 1.0
data$R5464100[1001.0 <= data$R5464100 & data$R5464100 <= 2000.0] <- 1001.0
data$R5464100[2001.0 <= data$R5464100 & data$R5464100 <= 3000.0] <- 2001.0
data$R5464100[3001.0 <= data$R5464100 & data$R5464100 <= 5000.0] <- 3001.0
data$R5464100[5001.0 <= data$R5464100 & data$R5464100 <= 10000.0] <- 5001.0
data$R5464100[10001.0 <= data$R5464100 & data$R5464100 <= 20000.0] <- 10001.0
data$R5464100[20001.0 <= data$R5464100 & data$R5464100 <= 30000.0] <- 20001.0
data$R5464100[30001.0 <= data$R5464100 & data$R5464100 <= 40000.0] <- 30001.0
data$R5464100[40001.0 <= data$R5464100 & data$R5464100 <= 50000.0] <- 40001.0
data$R5464100[50001.0 <= data$R5464100 & data$R5464100 <= 65000.0] <- 50001.0
data$R5464100[65001.0 <= data$R5464100 & data$R5464100 <= 80000.0] <- 65001.0
data$R5464100[80001.0 <= data$R5464100 & data$R5464100 <= 100000.0] <- 80001.0
data$R5464100[100001.0 <= data$R5464100 & data$R5464100 <= 150000.0] <- 100001.0
data$R5464100[150001.0 <= data$R5464100 & data$R5464100 <= 200000.0] <- 150001.0
data$R5464100[200001.0 <= data$R5464100 & data$R5464100 <= 999999.0] <- 200001.0
data$R5464100 <- factor(data$R5464100,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$R5472200[0.0 <= data$R5472200 & data$R5472200 <= 139.0] <- 0.0
data$R5472200[140.0 <= data$R5472200 & data$R5472200 <= 150.0] <- 140.0
data$R5472200[151.0 <= data$R5472200 & data$R5472200 <= 160.0] <- 151.0
data$R5472200[161.0 <= data$R5472200 & data$R5472200 <= 170.0] <- 161.0
data$R5472200[171.0 <= data$R5472200 & data$R5472200 <= 180.0] <- 171.0
data$R5472200[181.0 <= data$R5472200 & data$R5472200 <= 190.0] <- 181.0
data$R5472200[191.0 <= data$R5472200 & data$R5472200 <= 200.0] <- 191.0
data$R5472200[201.0 <= data$R5472200 & data$R5472200 <= 210.0] <- 201.0
data$R5472200[211.0 <= data$R5472200 & data$R5472200 <= 220.0] <- 211.0
data$R5472200[221.0 <= data$R5472200 & data$R5472200 <= 230.0] <- 221.0
data$R5472200[231.0 <= data$R5472200 & data$R5472200 <= 240.0] <- 231.0
data$R5472200[241.0 <= data$R5472200 & data$R5472200 <= 250.0] <- 241.0
data$R5472200[251.0 <= data$R5472200 & data$R5472200 <= 260.0] <- 251.0
data$R5472200 <- factor(data$R5472200,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0,251.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260"))
data$R7215900[0.0 <= data$R7215900 & data$R7215900 <= 139.0] <- 0.0
data$R7215900[140.0 <= data$R7215900 & data$R7215900 <= 150.0] <- 140.0
data$R7215900[151.0 <= data$R7215900 & data$R7215900 <= 160.0] <- 151.0
data$R7215900[161.0 <= data$R7215900 & data$R7215900 <= 170.0] <- 161.0
data$R7215900[171.0 <= data$R7215900 & data$R7215900 <= 180.0] <- 171.0
data$R7215900[181.0 <= data$R7215900 & data$R7215900 <= 190.0] <- 181.0
data$R7215900[191.0 <= data$R7215900 & data$R7215900 <= 200.0] <- 191.0
data$R7215900[201.0 <= data$R7215900 & data$R7215900 <= 210.0] <- 201.0
data$R7215900[211.0 <= data$R7215900 & data$R7215900 <= 220.0] <- 211.0
data$R7215900[221.0 <= data$R7215900 & data$R7215900 <= 230.0] <- 221.0
data$R7215900[231.0 <= data$R7215900 & data$R7215900 <= 240.0] <- 231.0
data$R7215900[241.0 <= data$R7215900 & data$R7215900 <= 250.0] <- 241.0
data$R7215900[251.0 <= data$R7215900 & data$R7215900 <= 260.0] <- 251.0
data$R7215900[261.0 <= data$R7215900 & data$R7215900 <= 270.0] <- 261.0
data$R7215900 <- factor(data$R7215900,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0,251.0,261.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260",
"261 TO 270"))
data$R7227800[-999999.0 <= data$R7227800 & data$R7227800 <= -3000.0] <- -999999.0
data$R7227800[-2999.0 <= data$R7227800 & data$R7227800 <= -2000.0] <- -2999.0
data$R7227800[-1999.0 <= data$R7227800 & data$R7227800 <= -1000.0] <- -1999.0
data$R7227800[-999.0 <= data$R7227800 & data$R7227800 <= -1.0] <- -999.0
data$R7227800[1.0 <= data$R7227800 & data$R7227800 <= 1000.0] <- 1.0
data$R7227800[1001.0 <= data$R7227800 & data$R7227800 <= 2000.0] <- 1001.0
data$R7227800[2001.0 <= data$R7227800 & data$R7227800 <= 3000.0] <- 2001.0
data$R7227800[3001.0 <= data$R7227800 & data$R7227800 <= 5000.0] <- 3001.0
data$R7227800[5001.0 <= data$R7227800 & data$R7227800 <= 10000.0] <- 5001.0
data$R7227800[10001.0 <= data$R7227800 & data$R7227800 <= 20000.0] <- 10001.0
data$R7227800[20001.0 <= data$R7227800 & data$R7227800 <= 30000.0] <- 20001.0
data$R7227800[30001.0 <= data$R7227800 & data$R7227800 <= 40000.0] <- 30001.0
data$R7227800[40001.0 <= data$R7227800 & data$R7227800 <= 50000.0] <- 40001.0
data$R7227800[50001.0 <= data$R7227800 & data$R7227800 <= 65000.0] <- 50001.0
data$R7227800[65001.0 <= data$R7227800 & data$R7227800 <= 80000.0] <- 65001.0
data$R7227800[80001.0 <= data$R7227800 & data$R7227800 <= 100000.0] <- 80001.0
data$R7227800[100001.0 <= data$R7227800 & data$R7227800 <= 150000.0] <- 100001.0
data$R7227800[150001.0 <= data$R7227800 & data$R7227800 <= 200000.0] <- 150001.0
data$R7227800[200001.0 <= data$R7227800 & data$R7227800 <= 999999.0] <- 200001.0
data$R7227800 <- factor(data$R7227800,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$R7236000[0.0 <= data$R7236000 & data$R7236000 <= 139.0] <- 0.0
data$R7236000[140.0 <= data$R7236000 & data$R7236000 <= 150.0] <- 140.0
data$R7236000[151.0 <= data$R7236000 & data$R7236000 <= 160.0] <- 151.0
data$R7236000[161.0 <= data$R7236000 & data$R7236000 <= 170.0] <- 161.0
data$R7236000[171.0 <= data$R7236000 & data$R7236000 <= 180.0] <- 171.0
data$R7236000[181.0 <= data$R7236000 & data$R7236000 <= 190.0] <- 181.0
data$R7236000[191.0 <= data$R7236000 & data$R7236000 <= 200.0] <- 191.0
data$R7236000[201.0 <= data$R7236000 & data$R7236000 <= 210.0] <- 201.0
data$R7236000[211.0 <= data$R7236000 & data$R7236000 <= 220.0] <- 211.0
data$R7236000[221.0 <= data$R7236000 & data$R7236000 <= 230.0] <- 221.0
data$R7236000[231.0 <= data$R7236000 & data$R7236000 <= 240.0] <- 231.0
data$R7236000[241.0 <= data$R7236000 & data$R7236000 <= 250.0] <- 241.0
data$R7236000[251.0 <= data$R7236000 & data$R7236000 <= 260.0] <- 251.0
data$R7236000[261.0 <= data$R7236000 & data$R7236000 <= 270.0] <- 261.0
data$R7236000 <- factor(data$R7236000,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0,251.0,261.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260",
"261 TO 270"))
data$R9792900[1.0 <= data$R9792900 & data$R9792900 <= 9.0] <- 1.0
data$R9792900[10.0 <= data$R9792900 & data$R9792900 <= 19.0] <- 10.0
data$R9792900[20.0 <= data$R9792900 & data$R9792900 <= 29.0] <- 20.0
data$R9792900[30.0 <= data$R9792900 & data$R9792900 <= 39.0] <- 30.0
data$R9792900[40.0 <= data$R9792900 & data$R9792900 <= 49.0] <- 40.0
data$R9792900[50.0 <= data$R9792900 & data$R9792900 <= 59.0] <- 50.0
data$R9792900[60.0 <= data$R9792900 & data$R9792900 <= 69.0] <- 60.0
data$R9792900[70.0 <= data$R9792900 & data$R9792900 <= 79.0] <- 70.0
data$R9792900[80.0 <= data$R9792900 & data$R9792900 <= 89.0] <- 80.0
data$R9792900[90.0 <= data$R9792900 & data$R9792900 <= 100.0] <- 90.0
data$R9792900 <- factor(data$R9792900,
levels=c(0.0,1.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0),
labels=c("0",
"1 TO 9",
"10 TO 19",
"20 TO 29",
"30 TO 39",
"40 TO 49",
"50 TO 59",
"60 TO 69",
"70 TO 79",
"80 TO 89",
"90 TO 100"))
data$R9829600[1.0 <= data$R9829600 & data$R9829600 <= 999.0] <- 1.0
data$R9829600[1000.0 <= data$R9829600 & data$R9829600 <= 19999.0] <- 1000.0
data$R9829600[20000.0 <= data$R9829600 & data$R9829600 <= 39999.0] <- 20000.0
data$R9829600[40000.0 <= data$R9829600 & data$R9829600 <= 59999.0] <- 40000.0
data$R9829600[60000.0 <= data$R9829600 & data$R9829600 <= 79999.0] <- 60000.0
data$R9829600[80000.0 <= data$R9829600 & data$R9829600 <= 100000.0] <- 80000.0
data$R9829600 <- factor(data$R9829600,
levels=c(0.0,1.0,1000.0,20000.0,40000.0,60000.0,80000.0),
labels=c("0",
"1 TO 999: .001-.999",
"1000 TO 19999: 1.000-19.999",
"20000 TO 39999: 20.000-39.999",
"40000 TO 59999: 40.000-59.999",
"60000 TO 79999: 60.000-79.999",
"80000 TO 100000: 80.000-100.000"))
data$R9871900[0.0 <= data$R9871900 & data$R9871900 <= 99.0] <- 0.0
data$R9871900[100.0 <= data$R9871900 & data$R9871900 <= 199.0] <- 100.0
data$R9871900[200.0 <= data$R9871900 & data$R9871900 <= 299.0] <- 200.0
data$R9871900[300.0 <= data$R9871900 & data$R9871900 <= 399.0] <- 300.0
data$R9871900[400.0 <= data$R9871900 & data$R9871900 <= 500.0] <- 400.0
data$R9871900 <- factor(data$R9871900,
levels=c(-9.0,-8.0,-7.0,-6.0,0.0,100.0,200.0,300.0,400.0),
labels=c("-9: No courses with valid credits and grades",
"-8: Pre-High school only",
"-7: No credits earned",
"-6: No courses taken",
"0 TO 99: 0 to .99",
"100 TO 199: 1.00 to 1.99",
"200 TO 299: 2.00 to 2.99",
"300 TO 399: 3.00 to 3.99",
"400 TO 500: 4.00 to 5.00"))
data$S1531300[0.0 <= data$S1531300 & data$S1531300 <= 139.0] <- 0.0
data$S1531300[140.0 <= data$S1531300 & data$S1531300 <= 150.0] <- 140.0
data$S1531300[151.0 <= data$S1531300 & data$S1531300 <= 160.0] <- 151.0
data$S1531300[161.0 <= data$S1531300 & data$S1531300 <= 170.0] <- 161.0
data$S1531300[171.0 <= data$S1531300 & data$S1531300 <= 180.0] <- 171.0
data$S1531300[181.0 <= data$S1531300 & data$S1531300 <= 190.0] <- 181.0
data$S1531300[191.0 <= data$S1531300 & data$S1531300 <= 200.0] <- 191.0
data$S1531300[201.0 <= data$S1531300 & data$S1531300 <= 210.0] <- 201.0
data$S1531300[211.0 <= data$S1531300 & data$S1531300 <= 220.0] <- 211.0
data$S1531300[221.0 <= data$S1531300 & data$S1531300 <= 230.0] <- 221.0
data$S1531300[231.0 <= data$S1531300 & data$S1531300 <= 240.0] <- 231.0
data$S1531300[241.0 <= data$S1531300 & data$S1531300 <= 250.0] <- 241.0
data$S1531300[251.0 <= data$S1531300 & data$S1531300 <= 260.0] <- 251.0
data$S1531300[261.0 <= data$S1531300 & data$S1531300 <= 270.0] <- 261.0
data$S1531300[271.0 <= data$S1531300 & data$S1531300 <= 280.0] <- 271.0
data$S1531300[281.0 <= data$S1531300 & data$S1531300 <= 290.0] <- 281.0
data$S1531300[291.0 <= data$S1531300 & data$S1531300 <= 300.0] <- 291.0
data$S1531300 <- factor(data$S1531300,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0,251.0,261.0,271.0,281.0,291.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260",
"261 TO 270",
"271 TO 280",
"281 TO 290",
"291 TO 300"))
data$S1541700[-999999.0 <= data$S1541700 & data$S1541700 <= -3000.0] <- -999999.0
data$S1541700[-2999.0 <= data$S1541700 & data$S1541700 <= -2000.0] <- -2999.0
data$S1541700[-1999.0 <= data$S1541700 & data$S1541700 <= -1000.0] <- -1999.0
data$S1541700[-999.0 <= data$S1541700 & data$S1541700 <= -1.0] <- -999.0
data$S1541700[1.0 <= data$S1541700 & data$S1541700 <= 1000.0] <- 1.0
data$S1541700[1001.0 <= data$S1541700 & data$S1541700 <= 2000.0] <- 1001.0
data$S1541700[2001.0 <= data$S1541700 & data$S1541700 <= 3000.0] <- 2001.0
data$S1541700[3001.0 <= data$S1541700 & data$S1541700 <= 5000.0] <- 3001.0
data$S1541700[5001.0 <= data$S1541700 & data$S1541700 <= 10000.0] <- 5001.0
data$S1541700[10001.0 <= data$S1541700 & data$S1541700 <= 20000.0] <- 10001.0
data$S1541700[20001.0 <= data$S1541700 & data$S1541700 <= 30000.0] <- 20001.0
data$S1541700[30001.0 <= data$S1541700 & data$S1541700 <= 40000.0] <- 30001.0
data$S1541700[40001.0 <= data$S1541700 & data$S1541700 <= 50000.0] <- 40001.0
data$S1541700[50001.0 <= data$S1541700 & data$S1541700 <= 65000.0] <- 50001.0
data$S1541700[65001.0 <= data$S1541700 & data$S1541700 <= 80000.0] <- 65001.0
data$S1541700[80001.0 <= data$S1541700 & data$S1541700 <= 100000.0] <- 80001.0
data$S1541700[100001.0 <= data$S1541700 & data$S1541700 <= 150000.0] <- 100001.0
data$S1541700[150001.0 <= data$S1541700 & data$S1541700 <= 200000.0] <- 150001.0
data$S1541700[200001.0 <= data$S1541700 & data$S1541700 <= 999999.0] <- 200001.0
data$S1541700 <- factor(data$S1541700,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$S1550800[0.0 <= data$S1550800 & data$S1550800 <= 139.0] <- 0.0
data$S1550800[140.0 <= data$S1550800 & data$S1550800 <= 150.0] <- 140.0
data$S1550800[151.0 <= data$S1550800 & data$S1550800 <= 160.0] <- 151.0
data$S1550800[161.0 <= data$S1550800 & data$S1550800 <= 170.0] <- 161.0
data$S1550800[171.0 <= data$S1550800 & data$S1550800 <= 180.0] <- 171.0
data$S1550800[181.0 <= data$S1550800 & data$S1550800 <= 190.0] <- 181.0
data$S1550800[191.0 <= data$S1550800 & data$S1550800 <= 200.0] <- 191.0
data$S1550800[201.0 <= data$S1550800 & data$S1550800 <= 210.0] <- 201.0
data$S1550800[211.0 <= data$S1550800 & data$S1550800 <= 220.0] <- 211.0
data$S1550800[221.0 <= data$S1550800 & data$S1550800 <= 230.0] <- 221.0
data$S1550800[231.0 <= data$S1550800 & data$S1550800 <= 240.0] <- 231.0
data$S1550800[241.0 <= data$S1550800 & data$S1550800 <= 250.0] <- 241.0
data$S1550800[251.0 <= data$S1550800 & data$S1550800 <= 260.0] <- 251.0
data$S1550800[261.0 <= data$S1550800 & data$S1550800 <= 270.0] <- 261.0
data$S1550800[271.0 <= data$S1550800 & data$S1550800 <= 280.0] <- 271.0
data$S1550800[281.0 <= data$S1550800 & data$S1550800 <= 290.0] <- 281.0
data$S1550800[291.0 <= data$S1550800 & data$S1550800 <= 300.0] <- 291.0
data$S1550800 <- factor(data$S1550800,
levels=c(0.0,140.0,151.0,161.0,171.0,181.0,191.0,201.0,211.0,221.0,231.0,241.0,251.0,261.0,271.0,281.0,291.0),
labels=c("0 TO 139: <140",
"140 TO 150",
"151 TO 160",
"161 TO 170",
"171 TO 180",
"181 TO 190",
"191 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260",
"261 TO 270",
"271 TO 280",
"281 TO 290",
"291 TO 300"))
data$S2000900[0.0 <= data$S2000900 & data$S2000900 <= 200.0] <- 0.0
data$S2000900[201.0 <= data$S2000900 & data$S2000900 <= 210.0] <- 201.0
data$S2000900[211.0 <= data$S2000900 & data$S2000900 <= 220.0] <- 211.0
data$S2000900[221.0 <= data$S2000900 & data$S2000900 <= 230.0] <- 221.0
data$S2000900[231.0 <= data$S2000900 & data$S2000900 <= 240.0] <- 231.0
data$S2000900[241.0 <= data$S2000900 & data$S2000900 <= 250.0] <- 241.0
data$S2000900[251.0 <= data$S2000900 & data$S2000900 <= 260.0] <- 251.0
data$S2000900[261.0 <= data$S2000900 & data$S2000900 <= 270.0] <- 261.0
data$S2000900[271.0 <= data$S2000900 & data$S2000900 <= 280.0] <- 271.0
data$S2000900[281.0 <= data$S2000900 & data$S2000900 <= 290.0] <- 281.0
data$S2000900[291.0 <= data$S2000900 & data$S2000900 <= 300.0] <- 291.0
data$S2000900 <- factor(data$S2000900,
levels=c(0.0,201.0,211.0,221.0,231.0,241.0,251.0,261.0,271.0,281.0,291.0),
labels=c("0 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260",
"261 TO 270",
"271 TO 280",
"281 TO 290",
"291 TO 300"))
data$S2011500[-999999.0 <= data$S2011500 & data$S2011500 <= -3000.0] <- -999999.0
data$S2011500[-2999.0 <= data$S2011500 & data$S2011500 <= -2000.0] <- -2999.0
data$S2011500[-1999.0 <= data$S2011500 & data$S2011500 <= -1000.0] <- -1999.0
data$S2011500[-999.0 <= data$S2011500 & data$S2011500 <= -1.0] <- -999.0
data$S2011500[1.0 <= data$S2011500 & data$S2011500 <= 1000.0] <- 1.0
data$S2011500[1001.0 <= data$S2011500 & data$S2011500 <= 2000.0] <- 1001.0
data$S2011500[2001.0 <= data$S2011500 & data$S2011500 <= 3000.0] <- 2001.0
data$S2011500[3001.0 <= data$S2011500 & data$S2011500 <= 5000.0] <- 3001.0
data$S2011500[5001.0 <= data$S2011500 & data$S2011500 <= 10000.0] <- 5001.0
data$S2011500[10001.0 <= data$S2011500 & data$S2011500 <= 20000.0] <- 10001.0
data$S2011500[20001.0 <= data$S2011500 & data$S2011500 <= 30000.0] <- 20001.0
data$S2011500[30001.0 <= data$S2011500 & data$S2011500 <= 40000.0] <- 30001.0
data$S2011500[40001.0 <= data$S2011500 & data$S2011500 <= 50000.0] <- 40001.0
data$S2011500[50001.0 <= data$S2011500 & data$S2011500 <= 65000.0] <- 50001.0
data$S2011500[65001.0 <= data$S2011500 & data$S2011500 <= 80000.0] <- 65001.0
data$S2011500[80001.0 <= data$S2011500 & data$S2011500 <= 100000.0] <- 80001.0
data$S2011500[100001.0 <= data$S2011500 & data$S2011500 <= 150000.0] <- 100001.0
data$S2011500[150001.0 <= data$S2011500 & data$S2011500 <= 200000.0] <- 150001.0
data$S2011500[200001.0 <= data$S2011500 & data$S2011500 <= 999999.0] <- 200001.0
data$S2011500 <- factor(data$S2011500,
levels=c(-999999.0,-2999.0,-1999.0,-999.0,0.0,1.0,1001.0,2001.0,3001.0,5001.0,10001.0,20001.0,30001.0,40001.0,50001.0,65001.0,80001.0,100001.0,150001.0,200001.0),
labels=c("-999999 TO -3000: < -2999",
"-2999 TO -2000",
"-1999 TO -1000",
"-999 TO -1",
"0",
"1 TO 1000",
"1001 TO 2000",
"2001 TO 3000",
"3001 TO 5000",
"5001 TO 10000",
"10001 TO 20000",
"20001 TO 30000",
"30001 TO 40000",
"40001 TO 50000",
"50001 TO 65000",
"65001 TO 80000",
"80001 TO 100000",
"100001 TO 150000",
"150001 TO 200000",
"200001 TO 999999: 200001+"))
data$S2020700[0.0 <= data$S2020700 & data$S2020700 <= 200.0] <- 0.0
data$S2020700[201.0 <= data$S2020700 & data$S2020700 <= 210.0] <- 201.0
data$S2020700[211.0 <= data$S2020700 & data$S2020700 <= 220.0] <- 211.0
data$S2020700[221.0 <= data$S2020700 & data$S2020700 <= 230.0] <- 221.0
data$S2020700[231.0 <= data$S2020700 & data$S2020700 <= 240.0] <- 231.0
data$S2020700[241.0 <= data$S2020700 & data$S2020700 <= 250.0] <- 241.0
data$S2020700[251.0 <= data$S2020700 & data$S2020700 <= 260.0] <- 251.0
data$S2020700[261.0 <= data$S2020700 & data$S2020700 <= 270.0] <- 261.0
data$S2020700[271.0 <= data$S2020700 & data$S2020700 <= 280.0] <- 271.0
data$S2020700[281.0 <= data$S2020700 & data$S2020700 <= 290.0] <- 281.0
data$S2020700[291.0 <= data$S2020700 & data$S2020700 <= 300.0] <- 291.0
data$S2020700 <- factor(data$S2020700,
levels=c(0.0,201.0,211.0,221.0,231.0,241.0,251.0,261.0,271.0,281.0,291.0),
labels=c("0 TO 200",
"201 TO 210",
"211 TO 220",
"221 TO 230",
"231 TO 240",
"241 TO 250",
"251 TO 260",
"261 TO 270",
"271 TO 280",
"281 TO 290",
"291 TO 300"))
data$S3801000[0.0 <= data$S3801000 & data$S3801000 <= 200.0] <- 0.0
data$S3801000[201.0 <= data$S3801000 & data$S3801000 <= 220.0] <- 201.0
data$S3801000[221.0 <= data$S3801000 & data$S3801000 <= 240.0] <- 221.0
data$S3801000[241.0 <= data$S3801000 & data$S3801000 <= 260.0] <- 241.0
data$S3801000[261.0 <= data$S3801000 & data$S3801000 <= 280.0] <- 261.0
data$S3801000[281.0 <= data$S3801000 & data$S3801000 <= 300.0] <- 281.0
data$S3801000[301.0 <= data$S3801000 & data$S3801000 <= 320.0] <- 301.0
data$S3801000[321.0 <= data$S3801000 & data$S3801000 <= 340.0] <- 321.0
data$S3801000 <- factor(data$S3801000,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340"))
data$S3821900[0.0 <= data$S3821900 & data$S3821900 <= 200.0] <- 0.0
data$S3821900[201.0 <= data$S3821900 & data$S3821900 <= 220.0] <- 201.0
data$S3821900[221.0 <= data$S3821900 & data$S3821900 <= 240.0] <- 221.0
data$S3821900[241.0 <= data$S3821900 & data$S3821900 <= 260.0] <- 241.0
data$S3821900[261.0 <= data$S3821900 & data$S3821900 <= 280.0] <- 261.0
data$S3821900[281.0 <= data$S3821900 & data$S3821900 <= 300.0] <- 281.0
data$S3821900[301.0 <= data$S3821900 & data$S3821900 <= 320.0] <- 301.0
data$S3821900[321.0 <= data$S3821900 & data$S3821900 <= 340.0] <- 321.0
data$S3821900 <- factor(data$S3821900,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340"))
data$S5400900[0.0 <= data$S5400900 & data$S5400900 <= 200.0] <- 0.0
data$S5400900[201.0 <= data$S5400900 & data$S5400900 <= 220.0] <- 201.0
data$S5400900[221.0 <= data$S5400900 & data$S5400900 <= 240.0] <- 221.0
data$S5400900[241.0 <= data$S5400900 & data$S5400900 <= 260.0] <- 241.0
data$S5400900[261.0 <= data$S5400900 & data$S5400900 <= 280.0] <- 261.0
data$S5400900[281.0 <= data$S5400900 & data$S5400900 <= 300.0] <- 281.0
data$S5400900[301.0 <= data$S5400900 & data$S5400900 <= 320.0] <- 301.0
data$S5400900[321.0 <= data$S5400900 & data$S5400900 <= 340.0] <- 321.0
data$S5400900 <- factor(data$S5400900,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340"))
data$S5421900[0.0 <= data$S5421900 & data$S5421900 <= 200.0] <- 0.0
data$S5421900[201.0 <= data$S5421900 & data$S5421900 <= 220.0] <- 201.0
data$S5421900[221.0 <= data$S5421900 & data$S5421900 <= 240.0] <- 221.0
data$S5421900[241.0 <= data$S5421900 & data$S5421900 <= 260.0] <- 241.0
data$S5421900[261.0 <= data$S5421900 & data$S5421900 <= 280.0] <- 261.0
data$S5421900[281.0 <= data$S5421900 & data$S5421900 <= 300.0] <- 281.0
data$S5421900[301.0 <= data$S5421900 & data$S5421900 <= 320.0] <- 301.0
data$S5421900[321.0 <= data$S5421900 & data$S5421900 <= 340.0] <- 321.0
data$S5421900 <- factor(data$S5421900,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340"))
data$S7501100[0.0 <= data$S7501100 & data$S7501100 <= 200.0] <- 0.0
data$S7501100[201.0 <= data$S7501100 & data$S7501100 <= 220.0] <- 201.0
data$S7501100[221.0 <= data$S7501100 & data$S7501100 <= 240.0] <- 221.0
data$S7501100[241.0 <= data$S7501100 & data$S7501100 <= 260.0] <- 241.0
data$S7501100[261.0 <= data$S7501100 & data$S7501100 <= 280.0] <- 261.0
data$S7501100[281.0 <= data$S7501100 & data$S7501100 <= 300.0] <- 281.0
data$S7501100[301.0 <= data$S7501100 & data$S7501100 <= 320.0] <- 301.0
data$S7501100[321.0 <= data$S7501100 & data$S7501100 <= 340.0] <- 321.0
data$S7501100 <- factor(data$S7501100,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340"))
data$S7524000[0.0 <= data$S7524000 & data$S7524000 <= 200.0] <- 0.0
data$S7524000[201.0 <= data$S7524000 & data$S7524000 <= 220.0] <- 201.0
data$S7524000[221.0 <= data$S7524000 & data$S7524000 <= 240.0] <- 221.0
data$S7524000[241.0 <= data$S7524000 & data$S7524000 <= 260.0] <- 241.0
data$S7524000[261.0 <= data$S7524000 & data$S7524000 <= 280.0] <- 261.0
data$S7524000[281.0 <= data$S7524000 & data$S7524000 <= 300.0] <- 281.0
data$S7524000[301.0 <= data$S7524000 & data$S7524000 <= 320.0] <- 301.0
data$S7524000[321.0 <= data$S7524000 & data$S7524000 <= 340.0] <- 321.0
data$S7524000 <- factor(data$S7524000,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340"))
data$T0008400[0.0 <= data$T0008400 & data$T0008400 <= 200.0] <- 0.0
data$T0008400[201.0 <= data$T0008400 & data$T0008400 <= 220.0] <- 201.0
data$T0008400[221.0 <= data$T0008400 & data$T0008400 <= 240.0] <- 221.0
data$T0008400[241.0 <= data$T0008400 & data$T0008400 <= 260.0] <- 241.0
data$T0008400[261.0 <= data$T0008400 & data$T0008400 <= 280.0] <- 261.0
data$T0008400[281.0 <= data$T0008400 & data$T0008400 <= 300.0] <- 281.0
data$T0008400[301.0 <= data$T0008400 & data$T0008400 <= 320.0] <- 301.0
data$T0008400[321.0 <= data$T0008400 & data$T0008400 <= 340.0] <- 321.0
data$T0008400[341.0 <= data$T0008400 & data$T0008400 <= 360.0] <- 341.0
data$T0008400 <- factor(data$T0008400,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360"))
data$T0024400[333.0 <= data$T0024400 & data$T0024400 <= 334.0] <- 333.0
data$T0024400[335.0 <= data$T0024400 & data$T0024400 <= 336.0] <- 335.0
data$T0024400[337.0 <= data$T0024400 & data$T0024400 <= 338.0] <- 337.0
data$T0024400[339.0 <= data$T0024400 & data$T0024400 <= 340.0] <- 339.0
data$T0024400[341.0 <= data$T0024400 & data$T0024400 <= 342.0] <- 341.0
data$T0024400 <- factor(data$T0024400,
levels=c(333.0,335.0,337.0,339.0,341.0),
labels=c("333 TO 334",
"335 TO 336",
"337 TO 338",
"339 TO 340",
"341 TO 342"))
data$T2011000[0.0 <= data$T2011000 & data$T2011000 <= 200.0] <- 0.0
data$T2011000[201.0 <= data$T2011000 & data$T2011000 <= 220.0] <- 201.0
data$T2011000[221.0 <= data$T2011000 & data$T2011000 <= 240.0] <- 221.0
data$T2011000[241.0 <= data$T2011000 & data$T2011000 <= 260.0] <- 241.0
data$T2011000[261.0 <= data$T2011000 & data$T2011000 <= 280.0] <- 261.0
data$T2011000[281.0 <= data$T2011000 & data$T2011000 <= 300.0] <- 281.0
data$T2011000[301.0 <= data$T2011000 & data$T2011000 <= 320.0] <- 301.0
data$T2011000[321.0 <= data$T2011000 & data$T2011000 <= 340.0] <- 321.0
data$T2011000[341.0 <= data$T2011000 & data$T2011000 <= 360.0] <- 341.0
data$T2011000 <- factor(data$T2011000,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360"))
data$T2019300[345.0 <= data$T2019300 & data$T2019300 <= 346.0] <- 345.0
data$T2019300[347.0 <= data$T2019300 & data$T2019300 <= 348.0] <- 347.0
data$T2019300[349.0 <= data$T2019300 & data$T2019300 <= 350.0] <- 349.0
data$T2019300[351.0 <= data$T2019300 & data$T2019300 <= 352.0] <- 351.0
data$T2019300[353.0 <= data$T2019300 & data$T2019300 <= 354.0] <- 353.0
data$T2019300 <- factor(data$T2019300,
levels=c(345.0,347.0,349.0,351.0,353.0),
labels=c("345 TO 346",
"347 TO 348",
"349 TO 350",
"351 TO 352",
"353 TO 354"))
data$T3601400[0.0 <= data$T3601400 & data$T3601400 <= 200.0] <- 0.0
data$T3601400[201.0 <= data$T3601400 & data$T3601400 <= 220.0] <- 201.0
data$T3601400[221.0 <= data$T3601400 & data$T3601400 <= 240.0] <- 221.0
data$T3601400[241.0 <= data$T3601400 & data$T3601400 <= 260.0] <- 241.0
data$T3601400[261.0 <= data$T3601400 & data$T3601400 <= 280.0] <- 261.0
data$T3601400[281.0 <= data$T3601400 & data$T3601400 <= 300.0] <- 281.0
data$T3601400[301.0 <= data$T3601400 & data$T3601400 <= 320.0] <- 301.0
data$T3601400[321.0 <= data$T3601400 & data$T3601400 <= 340.0] <- 321.0
data$T3601400[341.0 <= data$T3601400 & data$T3601400 <= 360.0] <- 341.0
data$T3601400[361.0 <= data$T3601400 & data$T3601400 <= 380.0] <- 361.0
data$T3601400 <- factor(data$T3601400,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0),
labels=c("0 TO 200: <200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380"))
data$T3609900[356.0 <= data$T3609900 & data$T3609900 <= 357.0] <- 356.0
data$T3609900[358.0 <= data$T3609900 & data$T3609900 <= 359.0] <- 358.0
data$T3609900[360.0 <= data$T3609900 & data$T3609900 <= 361.0] <- 360.0
data$T3609900[362.0 <= data$T3609900 & data$T3609900 <= 363.0] <- 362.0
data$T3609900[364.0 <= data$T3609900 & data$T3609900 <= 365.0] <- 364.0
data$T3609900 <- factor(data$T3609900,
levels=c(356.0,358.0,360.0,362.0,364.0),
labels=c("356 TO 357",
"358 TO 359",
"360 TO 361",
"362 TO 363",
"364 TO 365"))
data$T5201300[0.0 <= data$T5201300 & data$T5201300 <= 300.0] <- 0.0
data$T5201300[301.0 <= data$T5201300 & data$T5201300 <= 320.0] <- 301.0
data$T5201300[321.0 <= data$T5201300 & data$T5201300 <= 340.0] <- 321.0
data$T5201300[341.0 <= data$T5201300 & data$T5201300 <= 360.0] <- 341.0
data$T5201300[361.0 <= data$T5201300 & data$T5201300 <= 380.0] <- 361.0
data$T5201300 <- factor(data$T5201300,
levels=c(0.0,301.0,321.0,341.0,361.0),
labels=c("0 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380"))
data$T5210300[370.0 <= data$T5210300 & data$T5210300 <= 371.0] <- 370.0
data$T5210300[372.0 <= data$T5210300 & data$T5210300 <= 373.0] <- 372.0
data$T5210300[374.0 <= data$T5210300 & data$T5210300 <= 375.0] <- 374.0
data$T5210300[376.0 <= data$T5210300 & data$T5210300 <= 377.0] <- 376.0
data$T5210300[378.0 <= data$T5210300 & data$T5210300 <= 379.0] <- 378.0
data$T5210300 <- factor(data$T5210300,
levels=c(370.0,372.0,374.0,376.0,378.0),
labels=c("370 TO 371",
"372 TO 373",
"374 TO 375",
"376 TO 377",
"378 TO 379"))
data$T6651200[0.0 <= data$T6651200 & data$T6651200 <= 300.0] <- 0.0
data$T6651200[301.0 <= data$T6651200 & data$T6651200 <= 320.0] <- 301.0
data$T6651200[321.0 <= data$T6651200 & data$T6651200 <= 340.0] <- 321.0
data$T6651200[341.0 <= data$T6651200 & data$T6651200 <= 360.0] <- 341.0
data$T6651200[361.0 <= data$T6651200 & data$T6651200 <= 380.0] <- 361.0
data$T6651200[381.0 <= data$T6651200 & data$T6651200 <= 400.0] <- 381.0
data$T6651200 <- factor(data$T6651200,
levels=c(0.0,301.0,321.0,341.0,361.0,381.0),
labels=c("0 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400"))
data$T6661300[380.0 <= data$T6661300 & data$T6661300 <= 381.0] <- 380.0
data$T6661300[382.0 <= data$T6661300 & data$T6661300 <= 383.0] <- 382.0
data$T6661300[384.0 <= data$T6661300 & data$T6661300 <= 385.0] <- 384.0
data$T6661300[386.0 <= data$T6661300 & data$T6661300 <= 387.0] <- 386.0
data$T6661300[388.0 <= data$T6661300 & data$T6661300 <= 389.0] <- 388.0
data$T6661300[390.0 <= data$T6661300 & data$T6661300 <= 391.0] <- 390.0
data$T6661300 <- factor(data$T6661300,
levels=c(380.0,382.0,384.0,386.0,388.0,390.0),
labels=c("380 TO 381",
"382 TO 383",
"384 TO 385",
"386 TO 387",
"388 TO 389",
"390 TO 391"))
data$T8123500[0.0 <= data$T8123500 & data$T8123500 <= 340.0] <- 0.0
data$T8123500[341.0 <= data$T8123500 & data$T8123500 <= 360.0] <- 341.0
data$T8123500[361.0 <= data$T8123500 & data$T8123500 <= 380.0] <- 361.0
data$T8123500[381.0 <= data$T8123500 & data$T8123500 <= 400.0] <- 381.0
data$T8123500[401.0 <= data$T8123500 & data$T8123500 <= 420.0] <- 401.0
data$T8123500[421.0 <= data$T8123500 & data$T8123500 <= 440.0] <- 421.0
data$T8123500[441.0 <= data$T8123500 & data$T8123500 <= 460.0] <- 441.0
data$T8123500 <- factor(data$T8123500,
levels=c(0.0,341.0,361.0,381.0,401.0,421.0,441.0),
labels=c("0 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460"))
data$T8132800[407.0 <= data$T8132800 & data$T8132800 <= 408.0] <- 407.0
data$T8132800[409.0 <= data$T8132800 & data$T8132800 <= 410.0] <- 409.0
data$T8132800[411.0 <= data$T8132800 & data$T8132800 <= 412.0] <- 411.0
data$T8132800[413.0 <= data$T8132800 & data$T8132800 <= 414.0] <- 413.0
data$T8132800[415.0 <= data$T8132800 & data$T8132800 <= 416.0] <- 415.0
data$T8132800 <- factor(data$T8132800,
levels=c(407.0,409.0,411.0,413.0,415.0),
labels=c("407 TO 408",
"409 TO 410",
"411 TO 412",
"413 TO 414",
"415 TO 416"))
data$U0001700[0.0 <= data$U0001700 & data$U0001700 <= 340.0] <- 0.0
data$U0001700[341.0 <= data$U0001700 & data$U0001700 <= 360.0] <- 341.0
data$U0001700[361.0 <= data$U0001700 & data$U0001700 <= 380.0] <- 361.0
data$U0001700[381.0 <= data$U0001700 & data$U0001700 <= 400.0] <- 381.0
data$U0001700[401.0 <= data$U0001700 & data$U0001700 <= 420.0] <- 401.0
data$U0001700[421.0 <= data$U0001700 & data$U0001700 <= 440.0] <- 421.0
data$U0001700[441.0 <= data$U0001700 & data$U0001700 <= 460.0] <- 441.0
data$U0001700 <- factor(data$U0001700,
levels=c(0.0,341.0,361.0,381.0,401.0,421.0,441.0),
labels=c("0 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460"))
data$U0013100[430.0 <= data$U0013100 & data$U0013100 <= 431.0] <- 430.0
data$U0013100[432.0 <= data$U0013100 & data$U0013100 <= 433.0] <- 432.0
data$U0013100[434.0 <= data$U0013100 & data$U0013100 <= 435.0] <- 434.0
data$U0013100[436.0 <= data$U0013100 & data$U0013100 <= 437.0] <- 436.0
data$U0013100[438.0 <= data$U0013100 & data$U0013100 <= 439.0] <- 438.0
data$U0013100[440.0 <= data$U0013100 & data$U0013100 <= 441.0] <- 440.0
data$U0013100 <- factor(data$U0013100,
levels=c(430.0,432.0,434.0,436.0,438.0,440.0),
labels=c("430 TO 431",
"432 TO 433",
"434 TO 435",
"436 TO 437",
"438 TO 439",
"440 TO 441"))
data$U1838400[0.0 <= data$U1838400 & data$U1838400 <= 340.0] <- 0.0
data$U1838400[341.0 <= data$U1838400 & data$U1838400 <= 360.0] <- 341.0
data$U1838400[361.0 <= data$U1838400 & data$U1838400 <= 380.0] <- 361.0
data$U1838400[381.0 <= data$U1838400 & data$U1838400 <= 400.0] <- 381.0
data$U1838400[401.0 <= data$U1838400 & data$U1838400 <= 420.0] <- 401.0
data$U1838400[421.0 <= data$U1838400 & data$U1838400 <= 440.0] <- 421.0
data$U1838400[441.0 <= data$U1838400 & data$U1838400 <= 460.0] <- 441.0
data$U1838400[461.0 <= data$U1838400 & data$U1838400 <= 480.0] <- 461.0
data$U1838400 <- factor(data$U1838400,
levels=c(0.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0),
labels=c("0 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480"))
data$U1850600[454.0 <= data$U1850600 & data$U1850600 <= 456.0] <- 454.0
data$U1850600[457.0 <= data$U1850600 & data$U1850600 <= 459.0] <- 457.0
data$U1850600[460.0 <= data$U1850600 & data$U1850600 <= 462.0] <- 460.0
data$U1850600[463.0 <= data$U1850600 & data$U1850600 <= 466.0] <- 463.0
data$U1850600 <- factor(data$U1850600,
levels=c(454.0,457.0,460.0,463.0),
labels=c("454 TO 456",
"457 TO 459",
"460 TO 462",
"463 TO 466"))
data$U3437800[0.0 <= data$U3437800 & data$U3437800 <= 340.0] <- 0.0
data$U3437800[341.0 <= data$U3437800 & data$U3437800 <= 360.0] <- 341.0
data$U3437800[361.0 <= data$U3437800 & data$U3437800 <= 380.0] <- 361.0
data$U3437800[381.0 <= data$U3437800 & data$U3437800 <= 400.0] <- 381.0
data$U3437800[401.0 <= data$U3437800 & data$U3437800 <= 420.0] <- 401.0
data$U3437800[421.0 <= data$U3437800 & data$U3437800 <= 440.0] <- 421.0
data$U3437800[441.0 <= data$U3437800 & data$U3437800 <= 460.0] <- 441.0
data$U3437800[461.0 <= data$U3437800 & data$U3437800 <= 480.0] <- 461.0
data$U3437800[481.0 <= data$U3437800 & data$U3437800 <= 500.0] <- 481.0
data$U3437800[501.0 <= data$U3437800 & data$U3437800 <= 520.0] <- 501.0
data$U3437800 <- factor(data$U3437800,
levels=c(0.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$U3450100[477.0 <= data$U3450100 & data$U3450100 <= 480.0] <- 477.0
data$U3450100[481.0 <= data$U3450100 & data$U3450100 <= 484.0] <- 481.0
data$U3450100[485.0 <= data$U3450100 & data$U3450100 <= 488.0] <- 485.0
data$U3450100[489.0 <= data$U3450100 & data$U3450100 <= 492.0] <- 489.0
data$U3450100 <- factor(data$U3450100,
levels=c(477.0,481.0,485.0,489.0),
labels=c("477 TO 480",
"481 TO 484",
"485 TO 488",
"489 TO 492"))
data$Z9084100[0.0 <= data$Z9084100 & data$Z9084100 <= 200.0] <- 0.0
data$Z9084100[201.0 <= data$Z9084100 & data$Z9084100 <= 220.0] <- 201.0
data$Z9084100[221.0 <= data$Z9084100 & data$Z9084100 <= 240.0] <- 221.0
data$Z9084100[241.0 <= data$Z9084100 & data$Z9084100 <= 260.0] <- 241.0
data$Z9084100[261.0 <= data$Z9084100 & data$Z9084100 <= 280.0] <- 261.0
data$Z9084100[281.0 <= data$Z9084100 & data$Z9084100 <= 300.0] <- 281.0
data$Z9084100[301.0 <= data$Z9084100 & data$Z9084100 <= 320.0] <- 301.0
data$Z9084100[321.0 <= data$Z9084100 & data$Z9084100 <= 340.0] <- 321.0
data$Z9084100[341.0 <= data$Z9084100 & data$Z9084100 <= 360.0] <- 341.0
data$Z9084100[361.0 <= data$Z9084100 & data$Z9084100 <= 380.0] <- 361.0
data$Z9084100[381.0 <= data$Z9084100 & data$Z9084100 <= 400.0] <- 381.0
data$Z9084100[401.0 <= data$Z9084100 & data$Z9084100 <= 420.0] <- 401.0
data$Z9084100[421.0 <= data$Z9084100 & data$Z9084100 <= 440.0] <- 421.0
data$Z9084100[441.0 <= data$Z9084100 & data$Z9084100 <= 460.0] <- 441.0
data$Z9084100[461.0 <= data$Z9084100 & data$Z9084100 <= 480.0] <- 461.0
data$Z9084100[481.0 <= data$Z9084100 & data$Z9084100 <= 500.0] <- 481.0
data$Z9084100[501.0 <= data$Z9084100 & data$Z9084100 <= 520.0] <- 501.0
data$Z9084100 <- factor(data$Z9084100,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084200[0.0 <= data$Z9084200 & data$Z9084200 <= 200.0] <- 0.0
data$Z9084200[201.0 <= data$Z9084200 & data$Z9084200 <= 220.0] <- 201.0
data$Z9084200[221.0 <= data$Z9084200 & data$Z9084200 <= 240.0] <- 221.0
data$Z9084200[241.0 <= data$Z9084200 & data$Z9084200 <= 260.0] <- 241.0
data$Z9084200[261.0 <= data$Z9084200 & data$Z9084200 <= 280.0] <- 261.0
data$Z9084200[281.0 <= data$Z9084200 & data$Z9084200 <= 300.0] <- 281.0
data$Z9084200[301.0 <= data$Z9084200 & data$Z9084200 <= 320.0] <- 301.0
data$Z9084200[321.0 <= data$Z9084200 & data$Z9084200 <= 340.0] <- 321.0
data$Z9084200[341.0 <= data$Z9084200 & data$Z9084200 <= 360.0] <- 341.0
data$Z9084200[361.0 <= data$Z9084200 & data$Z9084200 <= 380.0] <- 361.0
data$Z9084200[381.0 <= data$Z9084200 & data$Z9084200 <= 400.0] <- 381.0
data$Z9084200[401.0 <= data$Z9084200 & data$Z9084200 <= 420.0] <- 401.0
data$Z9084200[421.0 <= data$Z9084200 & data$Z9084200 <= 440.0] <- 421.0
data$Z9084200[441.0 <= data$Z9084200 & data$Z9084200 <= 460.0] <- 441.0
data$Z9084200[461.0 <= data$Z9084200 & data$Z9084200 <= 480.0] <- 461.0
data$Z9084200[481.0 <= data$Z9084200 & data$Z9084200 <= 500.0] <- 481.0
data$Z9084200[501.0 <= data$Z9084200 & data$Z9084200 <= 520.0] <- 501.0
data$Z9084200 <- factor(data$Z9084200,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084300[0.0 <= data$Z9084300 & data$Z9084300 <= 200.0] <- 0.0
data$Z9084300[201.0 <= data$Z9084300 & data$Z9084300 <= 220.0] <- 201.0
data$Z9084300[221.0 <= data$Z9084300 & data$Z9084300 <= 240.0] <- 221.0
data$Z9084300[241.0 <= data$Z9084300 & data$Z9084300 <= 260.0] <- 241.0
data$Z9084300[261.0 <= data$Z9084300 & data$Z9084300 <= 280.0] <- 261.0
data$Z9084300[281.0 <= data$Z9084300 & data$Z9084300 <= 300.0] <- 281.0
data$Z9084300[301.0 <= data$Z9084300 & data$Z9084300 <= 320.0] <- 301.0
data$Z9084300[321.0 <= data$Z9084300 & data$Z9084300 <= 340.0] <- 321.0
data$Z9084300[341.0 <= data$Z9084300 & data$Z9084300 <= 360.0] <- 341.0
data$Z9084300[361.0 <= data$Z9084300 & data$Z9084300 <= 380.0] <- 361.0
data$Z9084300[381.0 <= data$Z9084300 & data$Z9084300 <= 400.0] <- 381.0
data$Z9084300[401.0 <= data$Z9084300 & data$Z9084300 <= 420.0] <- 401.0
data$Z9084300[421.0 <= data$Z9084300 & data$Z9084300 <= 440.0] <- 421.0
data$Z9084300[441.0 <= data$Z9084300 & data$Z9084300 <= 460.0] <- 441.0
data$Z9084300[461.0 <= data$Z9084300 & data$Z9084300 <= 480.0] <- 461.0
data$Z9084300[481.0 <= data$Z9084300 & data$Z9084300 <= 500.0] <- 481.0
data$Z9084300[501.0 <= data$Z9084300 & data$Z9084300 <= 520.0] <- 501.0
data$Z9084300 <- factor(data$Z9084300,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084400[0.0 <= data$Z9084400 & data$Z9084400 <= 200.0] <- 0.0
data$Z9084400[201.0 <= data$Z9084400 & data$Z9084400 <= 220.0] <- 201.0
data$Z9084400[221.0 <= data$Z9084400 & data$Z9084400 <= 240.0] <- 221.0
data$Z9084400[241.0 <= data$Z9084400 & data$Z9084400 <= 260.0] <- 241.0
data$Z9084400[261.0 <= data$Z9084400 & data$Z9084400 <= 280.0] <- 261.0
data$Z9084400[281.0 <= data$Z9084400 & data$Z9084400 <= 300.0] <- 281.0
data$Z9084400[301.0 <= data$Z9084400 & data$Z9084400 <= 320.0] <- 301.0
data$Z9084400[321.0 <= data$Z9084400 & data$Z9084400 <= 340.0] <- 321.0
data$Z9084400[341.0 <= data$Z9084400 & data$Z9084400 <= 360.0] <- 341.0
data$Z9084400[361.0 <= data$Z9084400 & data$Z9084400 <= 380.0] <- 361.0
data$Z9084400[381.0 <= data$Z9084400 & data$Z9084400 <= 400.0] <- 381.0
data$Z9084400[401.0 <= data$Z9084400 & data$Z9084400 <= 420.0] <- 401.0
data$Z9084400[421.0 <= data$Z9084400 & data$Z9084400 <= 440.0] <- 421.0
data$Z9084400[441.0 <= data$Z9084400 & data$Z9084400 <= 460.0] <- 441.0
data$Z9084400[461.0 <= data$Z9084400 & data$Z9084400 <= 480.0] <- 461.0
data$Z9084400[481.0 <= data$Z9084400 & data$Z9084400 <= 500.0] <- 481.0
data$Z9084400[501.0 <= data$Z9084400 & data$Z9084400 <= 520.0] <- 501.0
data$Z9084400 <- factor(data$Z9084400,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084500[0.0 <= data$Z9084500 & data$Z9084500 <= 200.0] <- 0.0
data$Z9084500[201.0 <= data$Z9084500 & data$Z9084500 <= 220.0] <- 201.0
data$Z9084500[221.0 <= data$Z9084500 & data$Z9084500 <= 240.0] <- 221.0
data$Z9084500[241.0 <= data$Z9084500 & data$Z9084500 <= 260.0] <- 241.0
data$Z9084500[261.0 <= data$Z9084500 & data$Z9084500 <= 280.0] <- 261.0
data$Z9084500[281.0 <= data$Z9084500 & data$Z9084500 <= 300.0] <- 281.0
data$Z9084500[301.0 <= data$Z9084500 & data$Z9084500 <= 320.0] <- 301.0
data$Z9084500[321.0 <= data$Z9084500 & data$Z9084500 <= 340.0] <- 321.0
data$Z9084500[341.0 <= data$Z9084500 & data$Z9084500 <= 360.0] <- 341.0
data$Z9084500[361.0 <= data$Z9084500 & data$Z9084500 <= 380.0] <- 361.0
data$Z9084500[381.0 <= data$Z9084500 & data$Z9084500 <= 400.0] <- 381.0
data$Z9084500[401.0 <= data$Z9084500 & data$Z9084500 <= 420.0] <- 401.0
data$Z9084500[421.0 <= data$Z9084500 & data$Z9084500 <= 440.0] <- 421.0
data$Z9084500[441.0 <= data$Z9084500 & data$Z9084500 <= 460.0] <- 441.0
data$Z9084500[461.0 <= data$Z9084500 & data$Z9084500 <= 480.0] <- 461.0
data$Z9084500[481.0 <= data$Z9084500 & data$Z9084500 <= 500.0] <- 481.0
data$Z9084500[501.0 <= data$Z9084500 & data$Z9084500 <= 520.0] <- 501.0
data$Z9084500 <- factor(data$Z9084500,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084600[0.0 <= data$Z9084600 & data$Z9084600 <= 200.0] <- 0.0
data$Z9084600[201.0 <= data$Z9084600 & data$Z9084600 <= 220.0] <- 201.0
data$Z9084600[221.0 <= data$Z9084600 & data$Z9084600 <= 240.0] <- 221.0
data$Z9084600[241.0 <= data$Z9084600 & data$Z9084600 <= 260.0] <- 241.0
data$Z9084600[261.0 <= data$Z9084600 & data$Z9084600 <= 280.0] <- 261.0
data$Z9084600[281.0 <= data$Z9084600 & data$Z9084600 <= 300.0] <- 281.0
data$Z9084600[301.0 <= data$Z9084600 & data$Z9084600 <= 320.0] <- 301.0
data$Z9084600[321.0 <= data$Z9084600 & data$Z9084600 <= 340.0] <- 321.0
data$Z9084600[341.0 <= data$Z9084600 & data$Z9084600 <= 360.0] <- 341.0
data$Z9084600[361.0 <= data$Z9084600 & data$Z9084600 <= 380.0] <- 361.0
data$Z9084600[381.0 <= data$Z9084600 & data$Z9084600 <= 400.0] <- 381.0
data$Z9084600[401.0 <= data$Z9084600 & data$Z9084600 <= 420.0] <- 401.0
data$Z9084600[421.0 <= data$Z9084600 & data$Z9084600 <= 440.0] <- 421.0
data$Z9084600[441.0 <= data$Z9084600 & data$Z9084600 <= 460.0] <- 441.0
data$Z9084600[461.0 <= data$Z9084600 & data$Z9084600 <= 480.0] <- 461.0
data$Z9084600[481.0 <= data$Z9084600 & data$Z9084600 <= 500.0] <- 481.0
data$Z9084600[501.0 <= data$Z9084600 & data$Z9084600 <= 520.0] <- 501.0
data$Z9084600 <- factor(data$Z9084600,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
data$Z9084700[0.0 <= data$Z9084700 & data$Z9084700 <= 200.0] <- 0.0
data$Z9084700[201.0 <= data$Z9084700 & data$Z9084700 <= 220.0] <- 201.0
data$Z9084700[221.0 <= data$Z9084700 & data$Z9084700 <= 240.0] <- 221.0
data$Z9084700[241.0 <= data$Z9084700 & data$Z9084700 <= 260.0] <- 241.0
data$Z9084700[261.0 <= data$Z9084700 & data$Z9084700 <= 280.0] <- 261.0
data$Z9084700[281.0 <= data$Z9084700 & data$Z9084700 <= 300.0] <- 281.0
data$Z9084700[301.0 <= data$Z9084700 & data$Z9084700 <= 320.0] <- 301.0
data$Z9084700[321.0 <= data$Z9084700 & data$Z9084700 <= 340.0] <- 321.0
data$Z9084700[341.0 <= data$Z9084700 & data$Z9084700 <= 360.0] <- 341.0
data$Z9084700[361.0 <= data$Z9084700 & data$Z9084700 <= 380.0] <- 361.0
data$Z9084700[381.0 <= data$Z9084700 & data$Z9084700 <= 400.0] <- 381.0
data$Z9084700[401.0 <= data$Z9084700 & data$Z9084700 <= 420.0] <- 401.0
data$Z9084700[421.0 <= data$Z9084700 & data$Z9084700 <= 440.0] <- 421.0
data$Z9084700[441.0 <= data$Z9084700 & data$Z9084700 <= 460.0] <- 441.0
data$Z9084700[461.0 <= data$Z9084700 & data$Z9084700 <= 480.0] <- 461.0
data$Z9084700[481.0 <= data$Z9084700 & data$Z9084700 <= 500.0] <- 481.0
data$Z9084700[501.0 <= data$Z9084700 & data$Z9084700 <= 520.0] <- 501.0
data$Z9084700 <- factor(data$Z9084700,
levels=c(0.0,201.0,221.0,241.0,261.0,281.0,301.0,321.0,341.0,361.0,381.0,401.0,421.0,441.0,461.0,481.0,501.0),
labels=c("0 TO 200",
"201 TO 220",
"221 TO 240",
"241 TO 260",
"261 TO 280",
"281 TO 300",
"301 TO 320",
"321 TO 340",
"341 TO 360",
"361 TO 380",
"381 TO 400",
"401 TO 420",
"421 TO 440",
"441 TO 460",
"461 TO 480",
"481 TO 500",
"501 TO 520"))
return(data)
}

varlabels <- c("1997 COLLEGE: ENROLLMENT STATUS L1",
"1997 COLLEGE: ENROLLMENT STATUS L2",
"1997 COLLEGE: ENROLLMENT STATUS L3",
"1997 COLLEGE: ENROLLMENT STATUS L4",
"1997 COLLEGE: ENROLLMENT STATUS L5",
"1997 COLLEGE: ENROLLMENT STATUS L6",
"1997 COLLEGE: ENROLLMENT STATUS L7",
"1997 COLLEGE: ENROLLMENT STATUS L8",
"1997 COLLEGE: ENROLLMENT STATUS L9",
"1997 COLLEGE: ENROLLMENT STATUS L10",
"1997 COLLEGE: ENROLLMENT STATUS L11",
"1997 COLLEGE: ENROLLMENT STATUS L12",
"1998 COLLEGE: ENROLLMENT STATUS L1",
"1998 COLLEGE: ENROLLMENT STATUS L2",
"1998 COLLEGE: ENROLLMENT STATUS L3",
"1998 COLLEGE: ENROLLMENT STATUS L4",
"1998 COLLEGE: ENROLLMENT STATUS L5",
"1998 COLLEGE: ENROLLMENT STATUS L6",
"1998 COLLEGE: ENROLLMENT STATUS L7",
"1998 COLLEGE: ENROLLMENT STATUS L8",
"1998 COLLEGE: ENROLLMENT STATUS L9",
"1998 COLLEGE: ENROLLMENT STATUS L10",
"1998 COLLEGE: ENROLLMENT STATUS L11",
"1998 COLLEGE: ENROLLMENT STATUS L12",
"1999 COLLEGE: ENROLLMENT STATUS L1",
"1999 COLLEGE: ENROLLMENT STATUS L2",
"1999 COLLEGE: ENROLLMENT STATUS L3",
"1999 COLLEGE: ENROLLMENT STATUS L4",
"1999 COLLEGE: ENROLLMENT STATUS L5",
"1999 COLLEGE: ENROLLMENT STATUS L6",
"1999 COLLEGE: ENROLLMENT STATUS L7",
"1999 COLLEGE: ENROLLMENT STATUS L8",
"1999 COLLEGE: ENROLLMENT STATUS L9",
"1999 COLLEGE: ENROLLMENT STATUS L10",
"1999 COLLEGE: ENROLLMENT STATUS L11",
"1999 COLLEGE: ENROLLMENT STATUS L12",
"2000 COLLEGE: ENROLLMENT STATUS L1",
"2000 COLLEGE: ENROLLMENT STATUS L2",
"2000 COLLEGE: ENROLLMENT STATUS L3",
"2000 COLLEGE: ENROLLMENT STATUS L4",
"2000 COLLEGE: ENROLLMENT STATUS L5",
"2000 COLLEGE: ENROLLMENT STATUS L6",
"2000 COLLEGE: ENROLLMENT STATUS L7",
"2000 COLLEGE: ENROLLMENT STATUS L8",
"2000 COLLEGE: ENROLLMENT STATUS L9",
"2000 COLLEGE: ENROLLMENT STATUS L10",
"2000 COLLEGE: ENROLLMENT STATUS L11",
"2000 COLLEGE: ENROLLMENT STATUS L12",
"2001 COLLEGE: ENROLLMENT STATUS L1",
"2001 COLLEGE: ENROLLMENT STATUS L2",
"2001 COLLEGE: ENROLLMENT STATUS L3",
"2001 COLLEGE: ENROLLMENT STATUS L4",
"2001 COLLEGE: ENROLLMENT STATUS L5",
"2001 COLLEGE: ENROLLMENT STATUS L6",
"2001 COLLEGE: ENROLLMENT STATUS L7",
"2001 COLLEGE: ENROLLMENT STATUS L8",
"2001 COLLEGE: ENROLLMENT STATUS L9",
"2001 COLLEGE: ENROLLMENT STATUS L10",
"2001 COLLEGE: ENROLLMENT STATUS L11",
"2001 COLLEGE: ENROLLMENT STATUS L12",
"2002 COLLEGE: ENROLLMENT STATUS L1",
"2002 COLLEGE: ENROLLMENT STATUS L2",
"2002 COLLEGE: ENROLLMENT STATUS L3",
"2002 COLLEGE: ENROLLMENT STATUS L4",
"2002 COLLEGE: ENROLLMENT STATUS L5",
"2002 COLLEGE: ENROLLMENT STATUS L6",
"2002 COLLEGE: ENROLLMENT STATUS L7",
"2002 COLLEGE: ENROLLMENT STATUS L8",
"2002 COLLEGE: ENROLLMENT STATUS L9",
"2002 COLLEGE: ENROLLMENT STATUS L10",
"2002 COLLEGE: ENROLLMENT STATUS L11",
"2002 COLLEGE: ENROLLMENT STATUS L12",
"2003 COLLEGE: ENROLLMENT STATUS L1",
"2003 COLLEGE: ENROLLMENT STATUS L2",
"2003 COLLEGE: ENROLLMENT STATUS L3",
"2003 COLLEGE: ENROLLMENT STATUS L4",
"2003 COLLEGE: ENROLLMENT STATUS L5",
"2003 COLLEGE: ENROLLMENT STATUS L6",
"2003 COLLEGE: ENROLLMENT STATUS L7",
"2003 COLLEGE: ENROLLMENT STATUS L8",
"2003 COLLEGE: ENROLLMENT STATUS L9",
"2003 COLLEGE: ENROLLMENT STATUS L10",
"2003 COLLEGE: ENROLLMENT STATUS L11",
"2003 COLLEGE: ENROLLMENT STATUS L12",
"2004 COLLEGE: ENROLLMENT STATUS L1",
"2004 COLLEGE: ENROLLMENT STATUS L2",
"2004 COLLEGE: ENROLLMENT STATUS L3",
"2004 COLLEGE: ENROLLMENT STATUS L4",
"2004 COLLEGE: ENROLLMENT STATUS L5",
"2004 COLLEGE: ENROLLMENT STATUS L6",
"2004 COLLEGE: ENROLLMENT STATUS L7",
"2004 COLLEGE: ENROLLMENT STATUS L8",
"2004 COLLEGE: ENROLLMENT STATUS L9",
"2004 COLLEGE: ENROLLMENT STATUS L10",
"2004 COLLEGE: ENROLLMENT STATUS L11",
"2004 COLLEGE: ENROLLMENT STATUS L12",
"2005 COLLEGE: ENROLLMENT STATUS L1",
"2005 COLLEGE: ENROLLMENT STATUS L2",
"2005 COLLEGE: ENROLLMENT STATUS L3",
"2005 COLLEGE: ENROLLMENT STATUS L4",
"2005 COLLEGE: ENROLLMENT STATUS L5",
"2005 COLLEGE: ENROLLMENT STATUS L6",
"2005 COLLEGE: ENROLLMENT STATUS L7",
"2005 COLLEGE: ENROLLMENT STATUS L8",
"2005 COLLEGE: ENROLLMENT STATUS L9",
"2005 COLLEGE: ENROLLMENT STATUS L10",
"2005 COLLEGE: ENROLLMENT STATUS L11",
"2005 COLLEGE: ENROLLMENT STATUS L12",
"2006 COLLEGE: ENROLLMENT STATUS L1",
"2006 COLLEGE: ENROLLMENT STATUS L2",
"2006 COLLEGE: ENROLLMENT STATUS L3",
"2006 COLLEGE: ENROLLMENT STATUS L4",
"2006 COLLEGE: ENROLLMENT STATUS L5",
"2006 COLLEGE: ENROLLMENT STATUS L6",
"2006 COLLEGE: ENROLLMENT STATUS L7",
"2006 COLLEGE: ENROLLMENT STATUS L8",
"2006 COLLEGE: ENROLLMENT STATUS L9",
"2006 COLLEGE: ENROLLMENT STATUS L10",
"2006 COLLEGE: ENROLLMENT STATUS L11",
"2006 COLLEGE: ENROLLMENT STATUS L12",
"2007 COLLEGE: ENROLLMENT STATUS L1",
"2007 COLLEGE: ENROLLMENT STATUS L2",
"2007 COLLEGE: ENROLLMENT STATUS L3",
"2007 COLLEGE: ENROLLMENT STATUS L4",
"2007 COLLEGE: ENROLLMENT STATUS L5",
"2007 COLLEGE: ENROLLMENT STATUS L6",
"2007 COLLEGE: ENROLLMENT STATUS L7",
"2007 COLLEGE: ENROLLMENT STATUS L8",
"2007 COLLEGE: ENROLLMENT STATUS L9",
"2007 COLLEGE: ENROLLMENT STATUS L10",
"2007 COLLEGE: ENROLLMENT STATUS L11",
"2007 COLLEGE: ENROLLMENT STATUS L12",
"2008 COLLEGE: ENROLLMENT STATUS L1",
"2008 COLLEGE: ENROLLMENT STATUS L2",
"2008 COLLEGE: ENROLLMENT STATUS L3",
"2008 COLLEGE: ENROLLMENT STATUS L4",
"2008 COLLEGE: ENROLLMENT STATUS L5",
"2008 COLLEGE: ENROLLMENT STATUS L6",
"2008 COLLEGE: ENROLLMENT STATUS L7",
"2008 COLLEGE: ENROLLMENT STATUS L8",
"2008 COLLEGE: ENROLLMENT STATUS L9",
"2008 COLLEGE: ENROLLMENT STATUS L10",
"2008 COLLEGE: ENROLLMENT STATUS L11",
"2008 COLLEGE: ENROLLMENT STATUS L12",
"2009 COLLEGE: ENROLLMENT STATUS L1",
"2009 COLLEGE: ENROLLMENT STATUS L2",
"2009 COLLEGE: ENROLLMENT STATUS L3",
"2009 COLLEGE: ENROLLMENT STATUS L4",
"2009 COLLEGE: ENROLLMENT STATUS L5",
"2009 COLLEGE: ENROLLMENT STATUS L6",
"2009 COLLEGE: ENROLLMENT STATUS L7",
"2009 COLLEGE: ENROLLMENT STATUS L8",
"2009 COLLEGE: ENROLLMENT STATUS L9",
"2009 COLLEGE: ENROLLMENT STATUS L10",
"2009 COLLEGE: ENROLLMENT STATUS L11",
"2009 COLLEGE: ENROLLMENT STATUS L12",
"2010 COLLEGE: ENROLLMENT STATUS L1",
"2010 COLLEGE: ENROLLMENT STATUS L2",
"2010 COLLEGE: ENROLLMENT STATUS L3",
"2010 COLLEGE: ENROLLMENT STATUS L4",
"2010 COLLEGE: ENROLLMENT STATUS L5",
"2010 COLLEGE: ENROLLMENT STATUS L6",
"2010 COLLEGE: ENROLLMENT STATUS L7",
"2010 COLLEGE: ENROLLMENT STATUS L8",
"2010 COLLEGE: ENROLLMENT STATUS L9",
"2010 COLLEGE: ENROLLMENT STATUS L10",
"2010 COLLEGE: ENROLLMENT STATUS L11",
"2010 COLLEGE: ENROLLMENT STATUS L12",
"2011 COLLEGE: ENROLLMENT STATUS L1",
"2011 COLLEGE: ENROLLMENT STATUS L2",
"2011 COLLEGE: ENROLLMENT STATUS L3",
"2011 COLLEGE: ENROLLMENT STATUS L4",
"2011 COLLEGE: ENROLLMENT STATUS L5",
"2011 COLLEGE: ENROLLMENT STATUS L6",
"2011 COLLEGE: ENROLLMENT STATUS L7",
"2011 COLLEGE: ENROLLMENT STATUS L8",
"2011 COLLEGE: ENROLLMENT STATUS L9",
"2011 COLLEGE: ENROLLMENT STATUS L10",
"2011 COLLEGE: ENROLLMENT STATUS L11",
"2011 COLLEGE: ENROLLMENT STATUS L12",
"2012 COLLEGE: ENROLLMENT STATUS L1",
"2012 COLLEGE: ENROLLMENT STATUS L2",
"2012 COLLEGE: ENROLLMENT STATUS L3",
"2012 COLLEGE: ENROLLMENT STATUS L4",
"2012 COLLEGE: ENROLLMENT STATUS L5",
"2012 COLLEGE: ENROLLMENT STATUS L6",
"2012 COLLEGE: ENROLLMENT STATUS L7",
"2012 COLLEGE: ENROLLMENT STATUS L8",
"2012 COLLEGE: ENROLLMENT STATUS L9",
"2012 COLLEGE: ENROLLMENT STATUS L10",
"2012 COLLEGE: ENROLLMENT STATUS L11",
"2012 COLLEGE: ENROLLMENT STATUS L12",
"2013 COLLEGE: ENROLLMENT STATUS L1",
"2013 COLLEGE: ENROLLMENT STATUS L2",
"2013 COLLEGE: ENROLLMENT STATUS L3",
"2013 COLLEGE: ENROLLMENT STATUS L4",
"2013 COLLEGE: ENROLLMENT STATUS L5",
"2013 COLLEGE: ENROLLMENT STATUS L6",
"2013 COLLEGE: ENROLLMENT STATUS L7",
"2013 COLLEGE: ENROLLMENT STATUS L8",
"2013 COLLEGE: ENROLLMENT STATUS L9",
"2013 COLLEGE: ENROLLMENT STATUS L10",
"2013 COLLEGE: ENROLLMENT STATUS L11",
"2013 COLLEGE: ENROLLMENT STATUS L12",
"2014 COLLEGE: ENROLLMENT STATUS L1",
"2014 COLLEGE: ENROLLMENT STATUS L2",
"2014 COLLEGE: ENROLLMENT STATUS L3",
"2014 COLLEGE: ENROLLMENT STATUS L4",
"2014 COLLEGE: ENROLLMENT STATUS L5",
"2014 COLLEGE: ENROLLMENT STATUS L6",
"2014 COLLEGE: ENROLLMENT STATUS L7",
"2014 COLLEGE: ENROLLMENT STATUS L8",
"2014 COLLEGE: ENROLLMENT STATUS L9",
"2014 COLLEGE: ENROLLMENT STATUS L10",
"2014 COLLEGE: ENROLLMENT STATUS L11",
"2014 COLLEGE: ENROLLMENT STATUS L12",
"2015 COLLEGE: ENROLLMENT STATUS L1",
"2015 COLLEGE: ENROLLMENT STATUS L2",
"2015 COLLEGE: ENROLLMENT STATUS L3",
"2015 COLLEGE: ENROLLMENT STATUS L4",
"2015 COLLEGE: ENROLLMENT STATUS L5",
"2015 COLLEGE: ENROLLMENT STATUS L6",
"2015 COLLEGE: ENROLLMENT STATUS L7",
"2015 COLLEGE: ENROLLMENT STATUS L8",
"2015 COLLEGE: ENROLLMENT STATUS L9",
"2015 COLLEGE: ENROLLMENT STATUS L10",
"2015 COLLEGE: ENROLLMENT STATUS L11",
"2015 COLLEGE: ENROLLMENT STATUS L12",
"2016 COLLEGE: ENROLLMENT STATUS L1",
"2016 COLLEGE: ENROLLMENT STATUS L2",
"2016 COLLEGE: ENROLLMENT STATUS L3",
"2016 COLLEGE: ENROLLMENT STATUS L4",
"2016 COLLEGE: ENROLLMENT STATUS L5",
"2016 COLLEGE: ENROLLMENT STATUS L6",
"2016 COLLEGE: ENROLLMENT STATUS L7",
"2016 COLLEGE: ENROLLMENT STATUS L8",
"2016 COLLEGE: ENROLLMENT STATUS L9",
"2016 COLLEGE: ENROLLMENT STATUS L10",
"2016 COLLEGE: ENROLLMENT STATUS L11",
"2016 COLLEGE: ENROLLMENT STATUS L12",
"2017 COLLEGE: ENROLLMENT STATUS L1",
"2017 COLLEGE: ENROLLMENT STATUS L2",
"2017 COLLEGE: ENROLLMENT STATUS L3",
"2017 COLLEGE: ENROLLMENT STATUS L4",
"2017 COLLEGE: ENROLLMENT STATUS L5",
"2017 COLLEGE: ENROLLMENT STATUS L6",
"2017 COLLEGE: ENROLLMENT STATUS L7",
"2017 COLLEGE: ENROLLMENT STATUS L8",
"2017 COLLEGE: ENROLLMENT STATUS L9",
"2017 COLLEGE: ENROLLMENT STATUS L10",
"2017 COLLEGE: ENROLLMENT STATUS L11",
"2017 COLLEGE: ENROLLMENT STATUS L12",
"2018 COLLEGE: ENROLLMENT STATUS L1",
"2018 COLLEGE: ENROLLMENT STATUS L2",
"2018 COLLEGE: ENROLLMENT STATUS L3",
"2018 COLLEGE: ENROLLMENT STATUS L4",
"2018 COLLEGE: ENROLLMENT STATUS L5",
"2018 COLLEGE: ENROLLMENT STATUS L6",
"2018 COLLEGE: ENROLLMENT STATUS L7",
"2018 COLLEGE: ENROLLMENT STATUS L8",
"2018 COLLEGE: ENROLLMENT STATUS L9",
"2018 COLLEGE: ENROLLMENT STATUS L10",
"2018 COLLEGE: ENROLLMENT STATUS L11",
"2018 COLLEGE: ENROLLMENT STATUS L12",
"2019 COLLEGE: ENROLLMENT STATUS L1",
"2019 COLLEGE: ENROLLMENT STATUS L2",
"2019 COLLEGE: ENROLLMENT STATUS L3",
"2019 COLLEGE: ENROLLMENT STATUS L4",
"2019 COLLEGE: ENROLLMENT STATUS L5",
"2019 COLLEGE: ENROLLMENT STATUS L6",
"2019 COLLEGE: ENROLLMENT STATUS L7",
"2019 COLLEGE: ENROLLMENT STATUS L8",
"2019 COLLEGE: ENROLLMENT STATUS L9",
"2019 COLLEGE: ENROLLMENT STATUS L10",
"2019 COLLEGE: ENROLLMENT STATUS L11",
"2019 COLLEGE: ENROLLMENT STATUS L12",
"2020 COLLEGE: ENROLLMENT STATUS L1",
"2020 COLLEGE: ENROLLMENT STATUS L2",
"2020 COLLEGE: ENROLLMENT STATUS L3",
"2020 COLLEGE: ENROLLMENT STATUS L4",
"2020 COLLEGE: ENROLLMENT STATUS L5",
"2020 COLLEGE: ENROLLMENT STATUS L6",
"2020 COLLEGE: ENROLLMENT STATUS L7",
"PUBID - YTH ID CODE 1997",
"KEY!SEX (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"CV_AGE(MONTHS)_INT_DATE 1997",
"CV_BIO_MOM_AGE_YOUTH 1997",
"CV_ENROLLSTAT 1997",
"CV_INCOME_GROSS_YR 1997",
"CV_HH_INCOME_SOURCE 1997",
"CV_HH_NET_WORTH_P 1997",
"CV_INTERVIEW_CMONTH 1997",
"CV_SAMPLE_TYPE 1997",
"R1 SAMPLE WEIGHT CC 1997",
"CV_HGC_BIO_DAD 1997",
"CV_HGC_BIO_MOM 1997",
"CV_HGC_RES_DAD 1997",
"CV_HGC_RES_MOM 1997",
"KEY!RACE_ETHNICITY (SYMBOL) 1997",
"CV_AGE(MONTHS)_INT_DATE 1998",
"CV_ENROLLSTAT_EDT 1998",
"CV_INCOME_GROSS_YR 1998",
"CV_INTERVIEW_CMONTH 1998",
"CV_AGE(MONTHS)_INT_DATE 1999",
"CV_ENROLLSTAT_EDT 1999",
"CV_INCOME_GROSS_YR 1999",
"CV_INTERVIEW_CMONTH 1999",
"CV_AGE(MONTHS)_INT_DATE 2000",
"CV_ENROLLSTAT_EDT 2000",
"CV_INCOME_GROSS_YR 2000",
"CV_INTERVIEW_CMONTH 2000",
"CV_AGE(MONTHS)_INT_DATE 2001",
"CV_ENROLLSTAT_EDT 2001",
"CV_INCOME_GROSS_YR 2001",
"CV_INTERVIEW_CMONTH 2001",
"TRANS_GPA HSTR",
"ASVAB_MATH_VERBAL_SCORE_PCT 1999",
"TRANS CRD GPA OVERALL HSTR",
"CV_AGE(MONTHS)_INT_DATE 2002",
"CV_ENROLLSTAT_EDT 2002",
"CV_INCOME_GROSS_YR 2002",
"CV_INTERVIEW_CMONTH 2002",
"CV_AGE(MONTHS)_INT_DATE 2003",
"CV_ENROLLSTAT_EDT 2003",
"CV_INCOME_GROSS_YR 2003",
"CV_INTERVIEW_CMONTH 2003",
"CV_AGE(MONTHS)_INT_DATE 2004",
"CV_ENROLLSTAT_EDT 2004",
"CV_INTERVIEW_CMONTH 2004",
"CV_AGE(MONTHS)_INT_DATE 2005",
"CV_ENROLLSTAT 2005",
"CV_INTERVIEW_CMONTH 2005",
"CV_AGE(MONTHS)_INT_DATE 2006",
"CV_ENROLLSTAT 2006",
"CV_INTERVIEW_CMONTH 2006",
"CV_AGE(MONTHS)_INT_DATE 2007",
"CV_ENROLLSTAT 2007",
"CV_INTERVIEW_CMONTH 2007",
"CV_AGE(MONTHS)_INT_DATE 2008",
"CV_ENROLLSTAT 2008",
"CV_INTERVIEW_CMONTH 2008",
"CV_AGE(MONTHS)_INT_DATE 2009",
"CV_ENROLLSTAT 2009",
"CV_INTERVIEW_CMONTH 2009",
"CV_AGE(MONTHS)_INT_DATE 2010",
"CV_ENROLLSTAT 2010",
"CV_INTERVIEW_CMONTH 2010",
"CV_AGE(MONTHS)_INT_DATE 2011",
"CV_ENROLLSTAT 2011",
"CV_INTERVIEW_CMONTH 2011",
"CV_AGE(MONTHS)_INT_DATE 2013",
"CV_ENROLLSTAT 2013",
"CV_INTERVIEW_CMONTH 2013",
"CV_AGE(MONTHS)_INT_DATE 2015",
"CV_ENROLLSTAT 2015",
"CV_INTERVIEW_CMONTH 2015",
"CV_AGE(MONTHS)_INT_DATE 2017",
"CV_ENROLLSTAT 2017",
"CV_INTERVIEW_CMONTH 2017",
"CV_AGE(MONTHS)_INT_DATE 2019",
"CV_ENROLLSTAT 2019",
"CV_INTERVIEW_CMONTH 2019",
"CVC_SAT_MATH_SCORE_2007",
"CVC_SAT_MATH_RND_2007",
"CVC_SAT_VERBAL_SCORE_2007",
"CVC_SAT_VERBAL_RND_2007",
"CVC_HGC_EVER",
"CVC_GED",
"CVC_HS_DIPLOMA",
"CVC_AA_DEGREE",
"CVC_BA_DEGREE",
"CVC_PROF_DEGREE",
"CVC_PHD_DEGREE",
"CVC_MA_DEGREE",
"CVC_RND"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("SCH_COLLEGE_STATUS_1997.01_XRND",
"SCH_COLLEGE_STATUS_1997.02_XRND",
"SCH_COLLEGE_STATUS_1997.03_XRND",
"SCH_COLLEGE_STATUS_1997.04_XRND",
"SCH_COLLEGE_STATUS_1997.05_XRND",
"SCH_COLLEGE_STATUS_1997.06_XRND",
"SCH_COLLEGE_STATUS_1997.07_XRND",
"SCH_COLLEGE_STATUS_1997.08_XRND",
"SCH_COLLEGE_STATUS_1997.09_XRND",
"SCH_COLLEGE_STATUS_1997.10_XRND",
"SCH_COLLEGE_STATUS_1997.11_XRND",
"SCH_COLLEGE_STATUS_1997.12_XRND",
"SCH_COLLEGE_STATUS_1998.01_XRND",
"SCH_COLLEGE_STATUS_1998.02_XRND",
"SCH_COLLEGE_STATUS_1998.03_XRND",
"SCH_COLLEGE_STATUS_1998.04_XRND",
"SCH_COLLEGE_STATUS_1998.05_XRND",
"SCH_COLLEGE_STATUS_1998.06_XRND",
"SCH_COLLEGE_STATUS_1998.07_XRND",
"SCH_COLLEGE_STATUS_1998.08_XRND",
"SCH_COLLEGE_STATUS_1998.09_XRND",
"SCH_COLLEGE_STATUS_1998.10_XRND",
"SCH_COLLEGE_STATUS_1998.11_XRND",
"SCH_COLLEGE_STATUS_1998.12_XRND",
"SCH_COLLEGE_STATUS_1999.01_XRND",
"SCH_COLLEGE_STATUS_1999.02_XRND",
"SCH_COLLEGE_STATUS_1999.03_XRND",
"SCH_COLLEGE_STATUS_1999.04_XRND",
"SCH_COLLEGE_STATUS_1999.05_XRND",
"SCH_COLLEGE_STATUS_1999.06_XRND",
"SCH_COLLEGE_STATUS_1999.07_XRND",
"SCH_COLLEGE_STATUS_1999.08_XRND",
"SCH_COLLEGE_STATUS_1999.09_XRND",
"SCH_COLLEGE_STATUS_1999.10_XRND",
"SCH_COLLEGE_STATUS_1999.11_XRND",
"SCH_COLLEGE_STATUS_1999.12_XRND",
"SCH_COLLEGE_STATUS_2000.01_XRND",
"SCH_COLLEGE_STATUS_2000.02_XRND",
"SCH_COLLEGE_STATUS_2000.03_XRND",
"SCH_COLLEGE_STATUS_2000.04_XRND",
"SCH_COLLEGE_STATUS_2000.05_XRND",
"SCH_COLLEGE_STATUS_2000.06_XRND",
"SCH_COLLEGE_STATUS_2000.07_XRND",
"SCH_COLLEGE_STATUS_2000.08_XRND",
"SCH_COLLEGE_STATUS_2000.09_XRND",
"SCH_COLLEGE_STATUS_2000.10_XRND",
"SCH_COLLEGE_STATUS_2000.11_XRND",
"SCH_COLLEGE_STATUS_2000.12_XRND",
"SCH_COLLEGE_STATUS_2001.01_XRND",
"SCH_COLLEGE_STATUS_2001.02_XRND",
"SCH_COLLEGE_STATUS_2001.03_XRND",
"SCH_COLLEGE_STATUS_2001.04_XRND",
"SCH_COLLEGE_STATUS_2001.05_XRND",
"SCH_COLLEGE_STATUS_2001.06_XRND",
"SCH_COLLEGE_STATUS_2001.07_XRND",
"SCH_COLLEGE_STATUS_2001.08_XRND",
"SCH_COLLEGE_STATUS_2001.09_XRND",
"SCH_COLLEGE_STATUS_2001.10_XRND",
"SCH_COLLEGE_STATUS_2001.11_XRND",
"SCH_COLLEGE_STATUS_2001.12_XRND",
"SCH_COLLEGE_STATUS_2002.01_XRND",
"SCH_COLLEGE_STATUS_2002.02_XRND",
"SCH_COLLEGE_STATUS_2002.03_XRND",
"SCH_COLLEGE_STATUS_2002.04_XRND",
"SCH_COLLEGE_STATUS_2002.05_XRND",
"SCH_COLLEGE_STATUS_2002.06_XRND",
"SCH_COLLEGE_STATUS_2002.07_XRND",
"SCH_COLLEGE_STATUS_2002.08_XRND",
"SCH_COLLEGE_STATUS_2002.09_XRND",
"SCH_COLLEGE_STATUS_2002.10_XRND",
"SCH_COLLEGE_STATUS_2002.11_XRND",
"SCH_COLLEGE_STATUS_2002.12_XRND",
"SCH_COLLEGE_STATUS_2003.01_XRND",
"SCH_COLLEGE_STATUS_2003.02_XRND",
"SCH_COLLEGE_STATUS_2003.03_XRND",
"SCH_COLLEGE_STATUS_2003.04_XRND",
"SCH_COLLEGE_STATUS_2003.05_XRND",
"SCH_COLLEGE_STATUS_2003.06_XRND",
"SCH_COLLEGE_STATUS_2003.07_XRND",
"SCH_COLLEGE_STATUS_2003.08_XRND",
"SCH_COLLEGE_STATUS_2003.09_XRND",
"SCH_COLLEGE_STATUS_2003.10_XRND",
"SCH_COLLEGE_STATUS_2003.11_XRND",
"SCH_COLLEGE_STATUS_2003.12_XRND",
"SCH_COLLEGE_STATUS_2004.01_XRND",
"SCH_COLLEGE_STATUS_2004.02_XRND",
"SCH_COLLEGE_STATUS_2004.03_XRND",
"SCH_COLLEGE_STATUS_2004.04_XRND",
"SCH_COLLEGE_STATUS_2004.05_XRND",
"SCH_COLLEGE_STATUS_2004.06_XRND",
"SCH_COLLEGE_STATUS_2004.07_XRND",
"SCH_COLLEGE_STATUS_2004.08_XRND",
"SCH_COLLEGE_STATUS_2004.09_XRND",
"SCH_COLLEGE_STATUS_2004.10_XRND",
"SCH_COLLEGE_STATUS_2004.11_XRND",
"SCH_COLLEGE_STATUS_2004.12_XRND",
"SCH_COLLEGE_STATUS_2005.01_XRND",
"SCH_COLLEGE_STATUS_2005.02_XRND",
"SCH_COLLEGE_STATUS_2005.03_XRND",
"SCH_COLLEGE_STATUS_2005.04_XRND",
"SCH_COLLEGE_STATUS_2005.05_XRND",
"SCH_COLLEGE_STATUS_2005.06_XRND",
"SCH_COLLEGE_STATUS_2005.07_XRND",
"SCH_COLLEGE_STATUS_2005.08_XRND",
"SCH_COLLEGE_STATUS_2005.09_XRND",
"SCH_COLLEGE_STATUS_2005.10_XRND",
"SCH_COLLEGE_STATUS_2005.11_XRND",
"SCH_COLLEGE_STATUS_2005.12_XRND",
"SCH_COLLEGE_STATUS_2006.01_XRND",
"SCH_COLLEGE_STATUS_2006.02_XRND",
"SCH_COLLEGE_STATUS_2006.03_XRND",
"SCH_COLLEGE_STATUS_2006.04_XRND",
"SCH_COLLEGE_STATUS_2006.05_XRND",
"SCH_COLLEGE_STATUS_2006.06_XRND",
"SCH_COLLEGE_STATUS_2006.07_XRND",
"SCH_COLLEGE_STATUS_2006.08_XRND",
"SCH_COLLEGE_STATUS_2006.09_XRND",
"SCH_COLLEGE_STATUS_2006.10_XRND",
"SCH_COLLEGE_STATUS_2006.11_XRND",
"SCH_COLLEGE_STATUS_2006.12_XRND",
"SCH_COLLEGE_STATUS_2007.01_XRND",
"SCH_COLLEGE_STATUS_2007.02_XRND",
"SCH_COLLEGE_STATUS_2007.03_XRND",
"SCH_COLLEGE_STATUS_2007.04_XRND",
"SCH_COLLEGE_STATUS_2007.05_XRND",
"SCH_COLLEGE_STATUS_2007.06_XRND",
"SCH_COLLEGE_STATUS_2007.07_XRND",
"SCH_COLLEGE_STATUS_2007.08_XRND",
"SCH_COLLEGE_STATUS_2007.09_XRND",
"SCH_COLLEGE_STATUS_2007.10_XRND",
"SCH_COLLEGE_STATUS_2007.11_XRND",
"SCH_COLLEGE_STATUS_2007.12_XRND",
"SCH_COLLEGE_STATUS_2008.01_XRND",
"SCH_COLLEGE_STATUS_2008.02_XRND",
"SCH_COLLEGE_STATUS_2008.03_XRND",
"SCH_COLLEGE_STATUS_2008.04_XRND",
"SCH_COLLEGE_STATUS_2008.05_XRND",
"SCH_COLLEGE_STATUS_2008.06_XRND",
"SCH_COLLEGE_STATUS_2008.07_XRND",
"SCH_COLLEGE_STATUS_2008.08_XRND",
"SCH_COLLEGE_STATUS_2008.09_XRND",
"SCH_COLLEGE_STATUS_2008.10_XRND",
"SCH_COLLEGE_STATUS_2008.11_XRND",
"SCH_COLLEGE_STATUS_2008.12_XRND",
"SCH_COLLEGE_STATUS_2009.01_XRND",
"SCH_COLLEGE_STATUS_2009.02_XRND",
"SCH_COLLEGE_STATUS_2009.03_XRND",
"SCH_COLLEGE_STATUS_2009.04_XRND",
"SCH_COLLEGE_STATUS_2009.05_XRND",
"SCH_COLLEGE_STATUS_2009.06_XRND",
"SCH_COLLEGE_STATUS_2009.07_XRND",
"SCH_COLLEGE_STATUS_2009.08_XRND",
"SCH_COLLEGE_STATUS_2009.09_XRND",
"SCH_COLLEGE_STATUS_2009.10_XRND",
"SCH_COLLEGE_STATUS_2009.11_XRND",
"SCH_COLLEGE_STATUS_2009.12_XRND",
"SCH_COLLEGE_STATUS_2010.01_XRND",
"SCH_COLLEGE_STATUS_2010.02_XRND",
"SCH_COLLEGE_STATUS_2010.03_XRND",
"SCH_COLLEGE_STATUS_2010.04_XRND",
"SCH_COLLEGE_STATUS_2010.05_XRND",
"SCH_COLLEGE_STATUS_2010.06_XRND",
"SCH_COLLEGE_STATUS_2010.07_XRND",
"SCH_COLLEGE_STATUS_2010.08_XRND",
"SCH_COLLEGE_STATUS_2010.09_XRND",
"SCH_COLLEGE_STATUS_2010.10_XRND",
"SCH_COLLEGE_STATUS_2010.11_XRND",
"SCH_COLLEGE_STATUS_2010.12_XRND",
"SCH_COLLEGE_STATUS_2011.01_XRND",
"SCH_COLLEGE_STATUS_2011.02_XRND",
"SCH_COLLEGE_STATUS_2011.03_XRND",
"SCH_COLLEGE_STATUS_2011.04_XRND",
"SCH_COLLEGE_STATUS_2011.05_XRND",
"SCH_COLLEGE_STATUS_2011.06_XRND",
"SCH_COLLEGE_STATUS_2011.07_XRND",
"SCH_COLLEGE_STATUS_2011.08_XRND",
"SCH_COLLEGE_STATUS_2011.09_XRND",
"SCH_COLLEGE_STATUS_2011.10_XRND",
"SCH_COLLEGE_STATUS_2011.11_XRND",
"SCH_COLLEGE_STATUS_2011.12_XRND",
"SCH_COLLEGE_STATUS_2012.01_XRND",
"SCH_COLLEGE_STATUS_2012.02_XRND",
"SCH_COLLEGE_STATUS_2012.03_XRND",
"SCH_COLLEGE_STATUS_2012.04_XRND",
"SCH_COLLEGE_STATUS_2012.05_XRND",
"SCH_COLLEGE_STATUS_2012.06_XRND",
"SCH_COLLEGE_STATUS_2012.07_XRND",
"SCH_COLLEGE_STATUS_2012.08_XRND",
"SCH_COLLEGE_STATUS_2012.09_XRND",
"SCH_COLLEGE_STATUS_2012.10_XRND",
"SCH_COLLEGE_STATUS_2012.11_XRND",
"SCH_COLLEGE_STATUS_2012.12_XRND",
"SCH_COLLEGE_STATUS_2013.01_XRND",
"SCH_COLLEGE_STATUS_2013.02_XRND",
"SCH_COLLEGE_STATUS_2013.03_XRND",
"SCH_COLLEGE_STATUS_2013.04_XRND",
"SCH_COLLEGE_STATUS_2013.05_XRND",
"SCH_COLLEGE_STATUS_2013.06_XRND",
"SCH_COLLEGE_STATUS_2013.07_XRND",
"SCH_COLLEGE_STATUS_2013.08_XRND",
"SCH_COLLEGE_STATUS_2013.09_XRND",
"SCH_COLLEGE_STATUS_2013.10_XRND",
"SCH_COLLEGE_STATUS_2013.11_XRND",
"SCH_COLLEGE_STATUS_2013.12_XRND",
"SCH_COLLEGE_STATUS_2014.01_XRND",
"SCH_COLLEGE_STATUS_2014.02_XRND",
"SCH_COLLEGE_STATUS_2014.03_XRND",
"SCH_COLLEGE_STATUS_2014.04_XRND",
"SCH_COLLEGE_STATUS_2014.05_XRND",
"SCH_COLLEGE_STATUS_2014.06_XRND",
"SCH_COLLEGE_STATUS_2014.07_XRND",
"SCH_COLLEGE_STATUS_2014.08_XRND",
"SCH_COLLEGE_STATUS_2014.09_XRND",
"SCH_COLLEGE_STATUS_2014.10_XRND",
"SCH_COLLEGE_STATUS_2014.11_XRND",
"SCH_COLLEGE_STATUS_2014.12_XRND",
"SCH_COLLEGE_STATUS_2015.01_XRND",
"SCH_COLLEGE_STATUS_2015.02_XRND",
"SCH_COLLEGE_STATUS_2015.03_XRND",
"SCH_COLLEGE_STATUS_2015.04_XRND",
"SCH_COLLEGE_STATUS_2015.05_XRND",
"SCH_COLLEGE_STATUS_2015.06_XRND",
"SCH_COLLEGE_STATUS_2015.07_XRND",
"SCH_COLLEGE_STATUS_2015.08_XRND",
"SCH_COLLEGE_STATUS_2015.09_XRND",
"SCH_COLLEGE_STATUS_2015.10_XRND",
"SCH_COLLEGE_STATUS_2015.11_XRND",
"SCH_COLLEGE_STATUS_2015.12_XRND",
"SCH_COLLEGE_STATUS_2016.01_XRND",
"SCH_COLLEGE_STATUS_2016.02_XRND",
"SCH_COLLEGE_STATUS_2016.03_XRND",
"SCH_COLLEGE_STATUS_2016.04_XRND",
"SCH_COLLEGE_STATUS_2016.05_XRND",
"SCH_COLLEGE_STATUS_2016.06_XRND",
"SCH_COLLEGE_STATUS_2016.07_XRND",
"SCH_COLLEGE_STATUS_2016.08_XRND",
"SCH_COLLEGE_STATUS_2016.09_XRND",
"SCH_COLLEGE_STATUS_2016.10_XRND",
"SCH_COLLEGE_STATUS_2016.11_XRND",
"SCH_COLLEGE_STATUS_2016.12_XRND",
"SCH_COLLEGE_STATUS_2017.01_XRND",
"SCH_COLLEGE_STATUS_2017.02_XRND",
"SCH_COLLEGE_STATUS_2017.03_XRND",
"SCH_COLLEGE_STATUS_2017.04_XRND",
"SCH_COLLEGE_STATUS_2017.05_XRND",
"SCH_COLLEGE_STATUS_2017.06_XRND",
"SCH_COLLEGE_STATUS_2017.07_XRND",
"SCH_COLLEGE_STATUS_2017.08_XRND",
"SCH_COLLEGE_STATUS_2017.09_XRND",
"SCH_COLLEGE_STATUS_2017.10_XRND",
"SCH_COLLEGE_STATUS_2017.11_XRND",
"SCH_COLLEGE_STATUS_2017.12_XRND",
"SCH_COLLEGE_STATUS_2018.01_XRND",
"SCH_COLLEGE_STATUS_2018.02_XRND",
"SCH_COLLEGE_STATUS_2018.03_XRND",
"SCH_COLLEGE_STATUS_2018.04_XRND",
"SCH_COLLEGE_STATUS_2018.05_XRND",
"SCH_COLLEGE_STATUS_2018.06_XRND",
"SCH_COLLEGE_STATUS_2018.07_XRND",
"SCH_COLLEGE_STATUS_2018.08_XRND",
"SCH_COLLEGE_STATUS_2018.09_XRND",
"SCH_COLLEGE_STATUS_2018.10_XRND",
"SCH_COLLEGE_STATUS_2018.11_XRND",
"SCH_COLLEGE_STATUS_2018.12_XRND",
"SCH_COLLEGE_STATUS_2019.01_XRND",
"SCH_COLLEGE_STATUS_2019.02_XRND",
"SCH_COLLEGE_STATUS_2019.03_XRND",
"SCH_COLLEGE_STATUS_2019.04_XRND",
"SCH_COLLEGE_STATUS_2019.05_XRND",
"SCH_COLLEGE_STATUS_2019.06_XRND",
"SCH_COLLEGE_STATUS_2019.07_XRND",
"SCH_COLLEGE_STATUS_2019.08_XRND",
"SCH_COLLEGE_STATUS_2019.09_XRND",
"SCH_COLLEGE_STATUS_2019.10_XRND",
"SCH_COLLEGE_STATUS_2019.11_XRND",
"SCH_COLLEGE_STATUS_2019.12_XRND",
"SCH_COLLEGE_STATUS_2020.01_XRND",
"SCH_COLLEGE_STATUS_2020.02_XRND",
"SCH_COLLEGE_STATUS_2020.03_XRND",
"SCH_COLLEGE_STATUS_2020.04_XRND",
"SCH_COLLEGE_STATUS_2020.05_XRND",
"SCH_COLLEGE_STATUS_2020.06_XRND",
"SCH_COLLEGE_STATUS_2020.07_XRND",
"PUBID_1997",
"KEY_SEX_1997",
"KEY_BDATE_M_1997",
"KEY_BDATE_Y_1997",
"CV_AGE(MONTHS)_INT_DATE_1997",
"CV_BIO_MOM_AGE_YOUTH_1997",
"CV_ENROLLSTAT_1997",
"CV_INCOME_GROSS_YR_1997",
"CV_HH_INCOME_SOURCE_1997",
"CV_HH_NET_WORTH_P_1997",
"CV_INTERVIEW_CMONTH_1997",
"CV_SAMPLE_TYPE_1997",
"SAMPLING_WEIGHT_CC_1997",
"CV_HGC_BIO_DAD_1997",
"CV_HGC_BIO_MOM_1997",
"CV_HGC_RES_DAD_1997",
"CV_HGC_RES_MOM_1997",
"KEY_RACE_ETHNICITY_1997",
"CV_AGE(MONTHS)_INT_DATE_1998",
"CV_ENROLLSTAT_EDT_1998",
"CV_INCOME_GROSS_YR_1998",
"CV_INTERVIEW_CMONTH_1998",
"CV_AGE(MONTHS)_INT_DATE_1999",
"CV_ENROLLSTAT_EDT_1999",
"CV_INCOME_GROSS_YR_1999",
"CV_INTERVIEW_CMONTH_1999",
"CV_AGE(MONTHS)_INT_DATE_2000",
"CV_ENROLLSTAT_EDT_2000",
"CV_INCOME_GROSS_YR_2000",
"CV_INTERVIEW_CMONTH_2000",
"CV_AGE(MONTHS)_INT_DATE_2001",
"CV_ENROLLSTAT_EDT_2001",
"CV_INCOME_GROSS_YR_2001",
"CV_INTERVIEW_CMONTH_2001",
"TRANS_GPA_HSTR",
"ASVAB_MATH_VERBAL_SCORE_PCT_1999",
"TRANS_CRD_GPA_OVERALL_HSTR",
"CV_AGE(MONTHS)_INT_DATE_2002",
"CV_ENROLLSTAT_EDT_2002",
"CV_INCOME_GROSS_YR_2002",
"CV_INTERVIEW_CMONTH_2002",
"CV_AGE(MONTHS)_INT_DATE_2003",
"CV_ENROLLSTAT_EDT_2003",
"CV_INCOME_GROSS_YR_2003",
"CV_INTERVIEW_CMONTH_2003",
"CV_AGE_MONTHS_INT_DATE_2004",
"CV_ENROLLSTAT_EDT_2004",
"CV_INTERVIEW_CMONTH_2004",
"CV_AGE_MONTHS_INT_DATE_2005",
"CV_ENROLLSTAT_2005",
"CV_INTERVIEW_CMONTH_2005",
"CV_AGE_MONTHS_INT_DATE_2006",
"CV_ENROLLSTAT_2006",
"CV_INTERVIEW_CMONTH_2006",
"CV_AGE_MONTHS_INT_DATE_2007",
"CV_ENROLLSTAT_2007",
"CV_INTERVIEW_CMONTH_2007",
"CV_AGE_MONTHS_INT_DATE_2008",
"CV_ENROLLSTAT_2008",
"CV_INTERVIEW_CMONTH_2008",
"CV_AGE_MONTHS_INT_DATE_2009",
"CV_ENROLLSTAT_2009",
"CV_INTERVIEW_CMONTH_2009",
"CV_AGE_MONTHS_INT_DATE_2010",
"CV_ENROLLSTAT_2010",
"CV_INTERVIEW_CMONTH_2010",
"CV_AGE_MONTHS_INT_DATE_2011",
"CV_ENROLLSTAT_2011",
"CV_INTERVIEW_CMONTH_2011",
"CV_AGE_MONTHS_INT_DATE_2013",
"CV_ENROLLSTAT_2013",
"CV_INTERVIEW_CMONTH_2013",
"CV_AGE_MONTHS_INT_DATE_2015",
"CV_ENROLLSTAT_2015",
"CV_INTERVIEW_CMONTH_2015",
"CV_AGE_MONTHS_INT_DATE_2017",
"CV_ENROLLSTAT_2017",
"CV_INTERVIEW_CMONTH_2017",
"CV_AGE_MONTHS_INT_DATE_2019",
"CV_ENROLLSTAT_2019",
"CV_INTERVIEW_CMONTH_2019",
"CVC_SAT_MATH_SCORE_2007_XRND",
"CVC_SAT_MATH_RND_2007_XRND",
"CVC_SAT_VERBAL_SCORE_2007_XRND",
"CVC_SAT_VERBAL_RND_2007_XRND",
"CVC_HGC_EVER_XRND",
"CVC_GED_XRND",
"CVC_HS_DIPLOMA_XRND",
"CVC_AA_DEGREE_XRND",
"CVC_BA_DEGREE_XRND",
"CVC_PROF_DEGREE_XRND",
"CVC_PHD_DEGREE_XRND",
"CVC_MA_DEGREE_XRND",
"CVC_RND_XRND")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
#categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
#categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
#summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
#summary(categories)

#************************************************************************************************************

