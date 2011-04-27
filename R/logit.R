ilogit <- function (x) 
{
    exp(x)/(1 + exp(x))
}

logit <- function(x) 
{
	log(x/(1 - x))
}
