
# Read KNDP and HIRA

# Aggregating KNDP variables


aggre_var= function(var) {
	varframe=kndp[, grep(var, names(kndp)), with=F]
	var_mean=rowMeans( varframe, na.rm=T)
	var_mean
}

bmi=aggre_var('bmi')
kndp=cbind(kndp, bmi)



# Merge KNDP and HIRA
merge_data = merge( HIRA, KNDP, by=c('age','weight', 'fbs','bp'), all.x=T)



# Data filtering by register_date
data_after_regi=merge_data[ date >= regi_date ]


# Incidence in Person-Year
ni=data_after_regi[!duplicated(JID) & first_date < as.Date('2011-01-01'), length(JID) ]  # Number of incidence
pd=data_after_regi[!duplicated(JID)& regi_date<=first_date , sum( first_date-regi_date) ] # Person-day 
incidence = (ni/pd)*365 # Incidence in Person-Year   


# Find significant variables 

glm( regi_date<=first_date ~ age * weight * fbs * bp * hba1c, data=data_after_regi, family=binomial )


