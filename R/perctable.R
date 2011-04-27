
perctable <- function(...) 
{
	t <- table(...)
	t/sum(t) * 100
}

proptable <- function(...) 
{
	t <- table(...)
	t/sum(t) 
}

