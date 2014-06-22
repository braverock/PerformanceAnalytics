inslib <-
function(x){
	x <-as.character(substitute(x))
	if(!x %in% rownames(installed.packages())) 
	{install.packages(x,repos="http://cran.stat.ucla.edu")}
	eval(parse(text=paste("library(",x,")",sep="")))}
