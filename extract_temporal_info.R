# Reads a vector of dates in YYYY-MM-DD format and returns a data frame with Year, Month, Season, WeekOfMonth, and Day as columns

extract.temporal.info <- function(dates){
	season=vector(length=length(dates))
	week = vector(length=length(dates))
	for(i in 1:length(dates)){
		if(as.numeric(format(dates[i],format="%m")) == 1 || as.numeric(format(dates[i],format="%m")) == 2 || as.numeric(format(dates[i],format="%m"))== 12) season[i]="W"
		if(as.numeric(format(dates[i],format="%m"))== 3 || as.numeric(format(dates[i],format="%m"))== 4 || as.numeric(format(dates[i],format="%m"))== 5) season[i]="Sp"
		if(as.numeric(format(dates[i],format="%m"))== 6 || as.numeric(format(dates[i],format="%m"))==7 || as.numeric(format(dates[i],format="%m"))==8) season[i]="Su"
		if(as.numeric(format(dates[i],format="%m"))== 9 || as.numeric(format(dates[i],format="%m"))==10 || as.numeric(format(dates[i],format="%m"))==11) season[i]="F"
		if(as.numeric(format(dates[i],format="%d")) < 8) week[i] = 1 else{
			if(as.numeric(format(dates[i],format="%d")) < 15) week[i] = 2 else{
				if(as.numeric(format(dates[i],format="%d")) < 22) week[i] = 3 else{
					week[i] = 4
				}
			}
		}
	}

	dates.df=data.frame(years=as.numeric(format(dates,format="%Y")),months=as.numeric(format(dates,format="%m")),seasons=season, weeks=week, days=as.numeric(format(dates,format="%d")))
	return(dates.df)
}
