library(data.table)
d=read.csv('t2dm.all',header=T)
dt= data.table( d)

# 당뇨병성 말초혈관질환 E1[0-4]5
# 말초혈관질환 I73[89]|I771|I79[028]|Z95[89]|M6597|M66(05|13|32)
# 족부질환 L97|R02|Z89[4-9]|N057[1-5]|S80[7-9]|S81[0789]||S90[8-9]|S91[0-3|7]|T003|T043|T13[01]|TT13[3-5|89]

pad_main= dt[ MAIN_SICK %like% '(E1[0-4]5|I73[8-9]|I771|I79[028]|Z95[89]|M6597|M66(05|13|32)|L97|R02|Z89[4-9]|N057[1-5]|S80[7-9]|S81[0789]|S90[8-9]|S91[0-3|7]|T003|T043|T13[01]|TT13[3-5|89])$']
pad_sub= dt[ SUB_SICK %like% '(E1[0-4]5|I73[8-9]|I771|I79[028]|Z95[89]|M6597|M66(05|13|32)|L97|R02|Z89[4-9]|N057[1-5]|S80[7-9]|S81[0789]|S90[8-9]|S91[0-3|7]|T003|T043|T13[01]|TT13[3-5|89])$']
dm_only= dt[ MAIN_SICK %like% 'E1[0-4]$' & SUB_SICK %like% 'E1[0-4]$' ]

pad=rbind( pad_main, pad_sub )


# 환자군 평균 나이 
age_pad = pad[ !duplicated(NO), mean(age) ]
age_dm = dm_only[ !duplicated(NO), mean(age) ]

# 환자군 성별 인원
no_sex_pad = pad[ !duplicated(NO), length(NO), by=gen ]
no_sex_dm = dm_only[ !duplicated(NO), length(NO), by=gen ]


# 환자군 총 의료비 
totCost_pad = sum( pad$DMD_TRAMT)
totCost_dm = sum( dm_only$DMD_TRAMT )

# 환자군 청구건 당 평균 의료비
costPerRequest_pad = pad[ , mean(DMD_TRAMT) ]
costPerRequest_dm = dm_only[ , mean(DMD_TRAMT) ]

# 환자군 1인 당 평균 의료비
costPerPerson_pad = mean( pad[ , sum(DMD_TRAMT), by=NO ]$V1 )
costPerPerson_dm = mean( dm_only[ , sum(DMD_TRAMT), by=NO ]$V1 )

