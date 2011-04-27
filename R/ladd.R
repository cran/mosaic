ladd <- function(x,col=1,row=1) { 
	trellis.focus('panel',col,row); x ; trellis.unfocus() 
}
