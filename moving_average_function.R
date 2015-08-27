# moving.average is a smoothing function. The arguments dt and dc are spatial/temporal distance and bray curtis similarities, 
# respectively. 'window' is the size of the window you want to average within (30 days, 60 days, 1 year, etc.) and 'skip' is 
# the interval you want to move the window.

moving.average<-function(dt, dc, window, skip){
	averages = data.frame(dist.bin=numeric(), mean.bray=numeric())
	bot = min(dt)
	top = max(dt)
	window = window
	skip = skip
	n.bins = 1+(top-bot-window)/skip
	for(i in 1:n.bins)
	{
		mean.bray = mean(dc[dt>bot+(i-1) * skip & dt<=(bot+(i-1) * skip + window)])
		dist.bin = median(dt[dt>bot+(i-1) * skip & dt<=(bot+(i-1) * skip + window)])
		averages[i,] = c(dist.bin, mean.bray)
	}
	return(averages)
}
