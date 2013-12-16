##### HIRA 1% 에서 날짜 기준 patient grouping 

library( data.table)
d=read.csv('nps_20~~.csv', header=T ) # 분석할 20 table read
dt= data.table(d)

disease = 'E1[0-4]5[01]'; # 분석 대상 질병 code regular expression.

# date 칼럼 추가 
dt[ , `:=` (date, as.Date(  paste('0', RECU_FR_DT , sep=''), format='%y%m%d'  )  ) ]

# 분석 대상 질병이 발생한 각 환자마다 최초발생일 정보를 담은 FD ( data.table ) 생성
FD= dt[ MAIN_SICK %like% disease | SUB_SICK %like% disease, list(first_date=sort(date)[1]), by=NO ] 

# 원본 data에 분석 대상 질병 최초발생일 정보를 first_date 칼럼으로 붙임.
dt= merge( dt, FD, by='NO', all=TRUE )


# 최초 발생일 이후 청구 기록 selection
dt[ date >= first_date ] 


# 최초 발생일 이전과 이후 기준 평균 1인당 의료비 
dt[  , sum( as.numeric( DMD_TRAMT))/ length( unique(NO)) , by=(date>=first_date) ]

# 최초 발생일 이전 이후 기준 평균 1인당 의료비 ( 입원 / 외래구분 )
dt[  , sum( as.numeric( DMD_TRAMT))/ length( unique(NO)) , by=list(date>=first_date, dif) ]


# 위와 같고 1인 평균 환자 의료이용일수  (입원 / 외래 구분 )
dt[  , sum( as.numeric( VSCN ) )/ length( unique(NO)) , by=(date>=first_date, dif) ]



