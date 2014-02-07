##### Set environment 
setwd('E:/E_LAB/말초혈관질환')
library( data.table )

# PAD patient definition 
regex_pad1= "E1[0-4]5[01]$"
regex_pad2= "(I73[89]|I771|I79[28]|Z958|M6597|M66(05|13|32))$"
regex_pad3= "(L97|R02|Z89[4-9]|N057[1-5]|S80[789]|S81|S81[789]|S90[7-9]|S91[0-3]|S917|T0[04]3|T13[0134589])$"
regex_pad_all = "(E1[0-4]5[01]|I73[89]|I771|I79[28]|Z958|M6597|M66(05|13|32)|L97|R02|Z89[4-9]|N057[1-5]|S80[789]|S81|S81[789]|S90[7-9]|S91[0-3]|S917|T0[04]3|T13[0134589])$"


##### Data murging 

# Read HIRA
hira = read.table('t200.txt',sep='\t', header=T)
hira = data.table( hira )


# Read KNDP 
kndp=read.csv('kndp.csv', header=T )
kndp=data.table(kndp)

# Filter out patients without regi_date
kndp=kndp[ regi_date!='']  

# Read KNDP death data 
death=read.table('kndp_death_final.txt', sep='\t', header=T)

# Merge KNDP death to KNDP 
kndp=merge(kndp, death, by='JID', all=T)

# Merge HIRA , KNDP( JID, regi_date, death_date( if there is)  )
kndp_var = kndp[, list( JID, regi_date ) ]
hk = merge( hira, kndp_var, by='JID')

# PAD patient identification
fy=hk[ MAIN_SICK %like% regex_pad_all | SUB_SICK %like% regex_pad_all , list(first_PAD=sort( RECV_FR_DT )[1]), by=JID ]
hk=merge( hk, fy, by='JID', all=T )
hk[ is.na( first_PAD), `:=`(first_PAD,'2011-01-01') ]

# Filtering HIRA records before regi_date
hk_filter=hk[ as.Date(RECV_FR_DT) >= as.Date(regi_date) ]


########## 1. Cohort group summary statistics
# total patient
hk[!duplicated(JID), length(JID) ]

# total patient by sex
hk[!duplicated(JID), length(JID), by=SEX ]

# mean age 
hk[!duplicated(JID), mean(PAT_AGE) ]

# mean age by sex 
hk[!duplicated(JID), mean(PAT_AGE), by=SEX ]


########### 2. PAD & Non-PAD group summary statistics
# Mean follow-up year 
hk_filter[!duplicated(JID), as.numeric(sum( as.Date('2011-01-01')-as.Date(regi_date))) / length(JID) ]/365

# Number of claim records by group
hk_filter[ , length(JID), by=as.Date(RECV_FR_DT) >= as.Date(first_PAD) ]

### PAD group 
pad=hk_filter[ as.Date(RECV_FR_DT) >= as.Date(first_PAD) ]

# Number of PAD patients
pad[!duplicated(JID), length(JID) ]

# Mean PAD group age
pad[!duplicated(JID), mean(PAT_AGE) ]

### Non-PAD group (Exclusive grouping )
npad=hk_filter[! JID %in% pad$JID ]

# Number of Non-PAD patient
npad[!duplicated(JID), length(JID) ]

# Mean Non-PAD group age 
npad[!duplicated(JID), mean(PAT_AGE) ]

### t.test on age between two group 
t.test(pad[!duplicated(JID), PAT_AGE], npad[!duplicated(JID), PAT_AGE] )


### PAD incidence rate per person year
# Follow-up days before PAD
days_before_pad=as.numeric( hk_filter[!duplicated(JID), sum( as.Date( first_PAD) - as.Date( regi_date) ) ] )
# Number of PAD incidence
num_pad_inc = pad[!duplicated(JID), length(JID) ]
# Incidence rate per Person-Year
num_pad_inc / days_before_pad *365 


### PAD and Non-PAD group costs/hospital visits and stays per year 

# Hospital visit per person year 
pad.visit = pad[, sum( as.numeric( RECN ) ) / mean(as.numeric( as.Date('2011-01-01')-as.Date(first_PAD) ) ) * 365 , by=JID ]$V1
npad.visit = npad[, sum( as.numeric( RECN ) ) / mean(as.numeric( as.Date(first_PAD)-as.Date(regi_date) ) ) * 365 , by=JID ]$V1
t.test( pad.visit, npad.visit )

# Hospital stay per person year 
pad.stay = pad[, sum( as.numeric( VSCN ) ) / mean(as.numeric( as.Date('2011-01-01')-as.Date(first_PAD) ) ) * 365 , by=JID ]$V1
npad.stay = npad[, sum( as.numeric( VSCN ) ) / mean(as.numeric( as.Date(first_PAD)-as.Date(regi_date) ) ) * 365 , by=JID ]$V1
t.test( pad.stay, npad.stay )

# Total cost per person year

pad.cost = pad[, sum( as.numeric( EDEC_TRAMT) ) / mean(as.numeric( as.Date('2011-01-01')-as.Date(first_PAD) ) ) * 365 , by=JID ]$V1
npad.cost = npad[, sum( as.numeric( EDEC_TRAMT) ) / mean(as.numeric( as.Date(first_PAD)-as.Date(regi_date) ) ) * 365 , by=JID ]$V1
t.test( pad.cost, npad.cost )

# Total cost by 외래/입원 per PY
pad.cost_form = pad[, sum( as.numeric( EDEC_TRAMT) ) / mean(as.numeric( as.Date('2011-01-01')-as.Date(first_PAD) ) ) * 365 , by=list(JID,FORM_CD) ]
npad.cost_form = npad[, sum( as.numeric( EDEC_TRAMT) ) / mean(as.numeric( as.Date(first_PAD)-as.Date(regi_date) ) ) * 365 , by=list(JID,FORM_CD) ]



########## 3. Find PAD associated variables
# PAD patient mapping to KNDP data
pad_jid=pad[!duplicated(JID), list(JID, pad=1) ]
kndp_pad=merge(kndp, pad_jid, by='JID', all=T)
kndp_pad[ is.na( pad ), pad:=0 ]


# Function to transform factor to numeric
asnumeric=function(x) as.numeric( as.character(x))

# Multiple Logistic Regression on variables of interest between PAD and Non-PAD group
summary(glm( pad==1~asnumeric(age_curr)+Yr_0_bio_hba1c+Yr_0_bio_bmi+Yr_0_bio_fbs+asnumeric(Yr_0_bio_sys_bp)+Yr_0_bio_dia_bp+cal+Yr_0_alt_cost+ Yr_0_exer_cost , data=kndp_pad, family=binomial))

# Mean variable in PAD and Non-PAD group
kndp_pad[ , mean(asnumeric(Yr_0_bio_hba1c), na.rm=T) , by=list(pad==1)]



######### 4. Survival analysis on PAD and Non-PAD group
# Calulate dead patients' survival days
kndp_pad[ , days:=as.numeric(as.Date(death_date)-as.Date(regi_date))]
# Calucate survived patients' survival days 
kndp_pad[ is.na(days), days:=as.numeric(as.Date('2011-01-01')-as.Date(regi_date)) ]


