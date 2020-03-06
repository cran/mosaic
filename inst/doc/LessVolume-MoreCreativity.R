## ---- include = FALSE-------------------------------------------------------------------
require(lubridate)
require(dplyr)
require(mosaic)
require(mosaicData)
theme_set(theme_bw())
trellis.par.set(theme = col.mosaic())
require(knitr)
opts_chunk$set(
  size = 'tiny', 
  tidy = FALSE,
  fig.width = 6, 
  fig.height = 3,
  fig.align = "center",
  out.width = "70%"
)
options(width = 90)

## ---------------------------------------------------------------------------------------
library(mosaic)  # loads mosaicData and ggformula as well

## ---- eval = FALSE----------------------------------------------------------------------
#  # simpler version
#  goal(~ x, data = mydata)
#  # fancier version
#  goal(y ~ x | z , data = mydata)
#  # unified version
#  goal(formula, data = mydata)

## ---- echo=FALSE, out.width="60%", out.height="35%"-------------------------------------
gf_point(births ~ date, data = Births78) 

## ---- echo=TRUE, out.width="60%"--------------------------------------------------------
gf_point(births ~ date, data = Births78) 

## ---- echo=FALSE------------------------------------------------------------------------
gf_boxplot(age ~ substance, data = HELPrct, xlab = "substance")

## ---- echo=TRUE-------------------------------------------------------------------------
gf_boxplot(age ~ substance, data=HELPrct)

## ---- echo=FALSE------------------------------------------------------------------------
gf_boxploth(substance ~ age, data = HELPrct)

## ---- echo=TRUE-------------------------------------------------------------------------
gf_boxploth(substance ~ age, data = HELPrct)

## ---------------------------------------------------------------------------------------
gf_histogram(~ age, data = HELPrct) 

## ---- eval=FALSE, tidy=FALSE------------------------------------------------------------
#    gf_histogram( ~ age, data = HELPrct)
#      gf_density( ~ age, data = HELPrct)
#      gf_boxplot( ~ age, data = HELPrct)
#           gf_qq( ~ age, data = HELPrct)
#     gf_freqpoly( ~ age, data = HELPrct)

## ---- eval=FALSE, tidy=FALSE------------------------------------------------------------
#  gf_point(i1 ~ age,          data = HELPrct)
#  gf_boxplot(age ~ substance, data = HELPrct)

## ---- eval=FALSE------------------------------------------------------------------------
#  names(KidsFeet)    # 4th graders' feet
#  ?KidsFeet

## ---- eval=FALSE------------------------------------------------------------------------
#  names(Utilities)   # utility bill data
#  ?Utilities

## ---- eval=FALSE------------------------------------------------------------------------
#  require(NHANES)    # load package
#  names(NHANES)      # body shape, etc.
#  ?NHANES

## ---- tidy=FALSE------------------------------------------------------------------------
gf_density( ~ age | sex, data = HELPrct, fill = ~ substance)

## ---------------------------------------------------------------------------------------
library(lubridate)
Births78 <- Births78 %>%
  mutate(weekday = wday(date, label = TRUE, abbr = TRUE))
gf_line(births ~ date, color = ~ weekday, data = Births78)

## ---- fig.show='hold'-------------------------------------------------------------------
gf_histogram( ~ age, data = HELPrct)  # binwidth = 5 (or 10) might be good here
        mean( ~ age, data = HELPrct)

## ---------------------------------------------------------------------------------------
favstats( ~ age, data = HELPrct)

## ---- df-stats--------------------------------------------------------------------------
df_stats( ~ age, data = HELPrct)
df_stats( ~ age, data = HELPrct, mean, sd, median, iqr)

## ---------------------------------------------------------------------------------------
tally(~ sex, data = HELPrct)
tally(~ substance, data = HELPrct)
df_stats(~ substance, data = HELPrct, counts, props)

## ---- eval = FALSE----------------------------------------------------------------------
#  sd(age ~ substance, data = HELPrct)
#  sd(~ age | substance, data = HELPrct)
#  sd(~ age, groups = substance, data = HELPrct)
#  # note option color = ~ substance is used for graphics

## ---- echo=FALSE------------------------------------------------------------------------
sd(~ age, groups = substance, data = HELPrct)

## ---- df-stats2-------------------------------------------------------------------------
df_stats(age ~ substance, data = HELPrct, sd)  

## ---------------------------------------------------------------------------------------
tally(sex ~ substance, data = HELPrct)
tally( ~ sex + substance, data = HELPrct)
df_stats(sex ~ substance, data = HELPrct, counts)


## ---------------------------------------------------------------------------------------
tally(sex ~ substance,   data = HELPrct, format = "proportion")
tally(substance ~ sex,   data = HELPrct, format = "proportion", margins = TRUE)
tally(~ sex + substance, data = HELPrct, format = "proportion", margins = TRUE)
tally(sex ~ substance,   data = HELPrct, format = "percent")
df_stats(sex ~ substance,   data = HELPrct, props, percs)

## ---- echo=FALSE------------------------------------------------------------------------
HELPrct <- mutate(HELPrct, sex = factor(sex, labels = c('F','M')),
                     substance = factor(substance, labels = c('A', 'C', 'H')))

## ---- size='small'----------------------------------------------------------------------
mean(age ~ substance | sex, data = HELPrct)
mean(age ~ substance | sex, data = HELPrct, .format = "table")

## ---- echo=FALSE------------------------------------------------------------------------
rm(HELPrct)
data(HELPrct)

## ---- eval=FALSE------------------------------------------------------------------------
#        mean(age ~ sex, data = HELPrct)
#  gf_boxplot(age ~ sex, data = HELPrct)
#          lm(age ~ sex, data = HELPrct)

## ---- echo=FALSE------------------------------------------------------------------------
  mean(age ~ sex, data = HELPrct)
    coef(lm(age ~ sex, data = HELPrct))

## ---------------------------------------------------------------------------------------
xpnorm(700, mean = 500, sd = 100)

## ---------------------------------------------------------------------------------------
xpnorm(c(300, 700), mean = 500, sd = 100)

## ---- echo=FALSE------------------------------------------------------------------------
phs <- cbind(c(104,189),c(10933,10845))
colnames(phs) <- c("heart attack","no heart attack")
rownames(phs) <- c("aspirin","placebo")

## ---------------------------------------------------------------------------------------
xchisq.test(phs)

## ---------------------------------------------------------------------------------------
model <- lm(width ~ length * sex, 
            data = KidsFeet)
Width <- makeFun(model)
Width(length = 25, sex = "B")
Width(length = 25, sex = "G")

## ---- fig.keep='last'-------------------------------------------------------------------
gf_point(width ~ length, data = KidsFeet, 
        color = ~ sex) %>%
  gf_fun(Width(length, sex = "B") ~ length, color = ~"B") %>%
  gf_fun(Width(length, sex = "G") ~ length, color = ~"G")

## ---- include=FALSE---------------------------------------------------------------------
theme_set(theme_bw())

## ---- echo = FALSE----------------------------------------------------------------------
require(mosaic)
trellis.par.set(theme = col.mosaic())
theme_set(theme_bw())
require(knitr)
opts_chunk$set(size = 'small', cache = TRUE)
options(width = 90)
set.seed(12345)

## ---------------------------------------------------------------------------------------
rflip()

## ---------------------------------------------------------------------------------------
rflip(10)

## ---------------------------------------------------------------------------------------
do(2) * rflip(10)

## ---- ladies5000------------------------------------------------------------------------
Ladies <- do(5000) * rflip(10)
head(Ladies, 2)
gf_histogram(~ heads, data = Ladies, binwidth = 1)

## ---------------------------------------------------------------------------------------
tally( ~ (heads >= 9), data = Ladies)
tally( ~ (heads >= 9), data = Ladies, format = "prop")
 prop( ~ (heads >= 9), data = Ladies)

## ---------------------------------------------------------------------------------------
diffmean(age ~ sex, data = HELPrct)
do(1) * 
  diffmean(age ~ shuffle(sex), data = HELPrct)
Null <- do(5000) * 
  diffmean(age ~ shuffle(sex), data = HELPrct)

## ---------------------------------------------------------------------------------------
prop( ~ (abs(diffmean) > 0.7841), data = Null) 
gf_histogram( ~ diffmean, data = Null) %>%
  gf_vline(xintercept = -0.7841) 

## ---------------------------------------------------------------------------------------
Bootstrap <- do(5000) * 
  diffmean(age ~ sex, data = resample(HELPrct))

gf_histogram( ~ diffmean, data = Bootstrap) %>%
  gf_vline(xintercept = -0.7841)

## ---------------------------------------------------------------------------------------
cdata( ~ diffmean, data = Bootstrap, p = 0.95)
confint(Bootstrap, method = "quantile")
confint(Bootstrap)  # default uses bootstrap st. err.

## ---- size="tiny"-----------------------------------------------------------------------
do(1) * lm(width ~ length, data = KidsFeet)
do(3) * lm(width ~ shuffle(length), data = KidsFeet)

## ---- size="tiny"-----------------------------------------------------------------------
do(1) * 
  lm(width ~ length + sex, data = KidsFeet)
do(3) * 
  lm(width ~ length + shuffle(sex), data = KidsFeet)

## ---------------------------------------------------------------------------------------
Null <- do(5000) * 
  lm(width ~ length + shuffle(sex), 
                       data = KidsFeet)
gf_histogram( ~ sexG, data = Null, boundary = -0.2325) %>%
  gf_vline(xintercept = -0.2325)

## ---------------------------------------------------------------------------------------
gf_histogram(~ sexG, data = Null, boundary = -0.2325) %>%
  gf_vline(xintercept = -0.2325)
prop(~ (sexG <= -0.2325), data = Null)

