## ----include=FALSE-------------------------------------------------------
pdfpath <- function(name) {
  return( 
    paste('http://github.com/rpruim/mosaic/blob/master/vignettes/',
          name, "?raw=TRUE",sep="")
    )
}

anitem <- function(name, text) {
	paste("\\item[\\href{",pdfpath(name),"}{",text,"}]", sep="")
}

## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("01-MinimalR.pdf", "Minimal R") )

## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("02-StartTeaching.pdf", "Start Teaching Statistics Using R") )

## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("03-Commands.pdf", "A Compendium of Commands to Teach Statistics using R") )

## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("04-Modeling.pdf", "Start Modeling with R") )

## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("05-Resample.pdf", "Resampling in R") )

## ----results='asis', echo=FALSE------------------------------------------
cat( anitem("06-Calculus.pdf", "Start R in Calculus") )

