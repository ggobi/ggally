# attempt to make a violin plot

violin <- function(x)
{
	b <- density(x)$y
	
	t <- 1:length(b)/length(b) + min(x)

	qplot(c(t,t[length(t):1]),c(b/2,b[length(b):1]/-2),geom = "polygon") + labs(x = "X", y = "violin density")
		
}





