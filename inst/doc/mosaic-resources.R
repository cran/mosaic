
## ----include=FALSE-------------------------------------------------------
pdfpath <- function(name) {
  return( 
    paste('http://github.com/rpruim/mosaic/blob/master/inst/doc/',
          name, "?raw=TRUE",sep="")
    )
}

anitem <- function(name, text) {
	paste("\\item[\\href{",pdfpath(name),"}{",text,"}]", sep="")
}


## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("MinimalR-vignette.pdf", "Minimal R") )


## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("Commands-vignette.pdf", "A Compendium of Commands to Teach Statistics using R") )


## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("StartTeaching-vignette.pdf", "Start Teaching Statistics Using R") )


## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("Resampling-vignette.pdf", "Resampling in R") )


## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("Modeling-vignette.pdf", "Start Modeling with R") )


## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("Calculus-vignette.pdf", "Start R in Calculus") )


