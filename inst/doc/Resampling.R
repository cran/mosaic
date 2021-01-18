## ----setup, include = FALSE-----------------------------------------
library(mosaic)
require(grDevices) 
require(datasets) 
require(stats) 
require(lattice)
require(grid) 
require(mosaic) 
require(mosaicData) 
trellis.par.set(theme=col.mosaic(bw=FALSE))
trellis.par.set(fontsize=list(text=9))
options(keep.blank.line=FALSE) 
options(width=70)
require(vcd)
require(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align="center",
  fig.show="hold"
)
options('mosaic:parallelMessage'=FALSE)

## ----eval=FALSE, echo=TRUE------------------------------------------
#  install.packages("mosaic")

## ----moreSetup------------------------------------------------------
require(mosaic)
options(digits = 3)

## -------------------------------------------------------------------
library(mosaic)
data(Mustangs)

## ----dot, fig.width=4, fig.height=2---------------------------------
gf_histogram( ~ Price, data = Mustangs)

## ----meanmust-------------------------------------------------------
mean( ~ Price, data = Mustangs)

## ----eval=FALSE-----------------------------------------------------
#  mean(Mustangs$Price)

## ----eval=FALSE-----------------------------------------------------
#  mean( ~ Price, data = Mustangs)

## -------------------------------------------------------------------
simple = c(1, 2, 3, 4, 5)
resample(simple)
resample(simple)
resample(simple)

## ----results="hide"-------------------------------------------------
resample(Mustangs)

## ----echo=FALSE-----------------------------------------------------
set.seed(104)
head(resample(Mustangs))
cat("... and so on")

## -------------------------------------------------------------------
mean( ~ Price, data = resample(Mustangs))

## -------------------------------------------------------------------
mean( ~ Price, data = resample(Mustangs))

## -------------------------------------------------------------------
do(5) * mean( ~ Price, data = resample(Mustangs))

## -------------------------------------------------------------------
mustangs_price_boot <- do(2000) * mean( ~ Price, data = resample(Mustangs))

## ----hist, fig.width=4, fig.height=2--------------------------------
gf_histogram( ~ mean, data = mustangs_price_boot, 
  xlab="Mean Mustang Price (in thousand dollars)")

## ----confint--------------------------------------------------------
confint(mustangs_price_boot, level = 0.90, method = "quantile")
confint(mustangs_price_boot, level = 0.90, method = "stderr")

## ----qdata----------------------------------------------------------
qdata( ~ mean, c(.05, .95), data = mustangs_price_boot)
# alternative
cdata(~ mean, 0.90, data = mustangs_price_boot)

## ----tstar----------------------------------------------------------
tstar <- qt(.95, df = 24)
zstar <- qnorm(.95)

## ----margin---------------------------------------------------------
tstar * sd( ~ mean, data = mustangs_price_boot)
zstar * sd( ~ mean, data = mustangs_price_boot)

## ----proptable------------------------------------------------------
prop( ~ rbinom(1000, prob = 0.5, size = 428) >= 240)

## ----proptable2-----------------------------------------------------
prop( ~ rbinom(1000, prob = 0.5, size = 428) >= 240)

## ----pbinom---------------------------------------------------------
xpbinom(239, prob = 0.5, size = 428)

## -------------------------------------------------------------------
binom.test(x = 240, n = 428)

## ----coinflip-------------------------------------------------------
do(1) * rflip(428)

## ----flips1, fig.width=4, fig.height=3------------------------------
nfl_null <- do(2000) * rflip(428)
prop( ~ heads >= 240, data = nfl_null)

## ----flips, fig.width=4, fig.height=2-------------------------------
gf_histogram( ~ heads, fill = ~ (heads >= 240), data = nfl_null)

## ----fetchsleep-----------------------------------------------------
data(Sleep) 

## ----obsmean--------------------------------------------------------
mean(Words ~ Group, data = Sleep)
obs <- diff(mean(Words ~ Group, data = Sleep))
obs

## ----fig.width=4, fig.height=2--------------------------------------
gf_boxplot(Words ~ Group, data = Sleep)

## -------------------------------------------------------------------
diff(mean(Words ~ shuffle(Group), data = Sleep))

## -------------------------------------------------------------------
diff(mean(Words ~ shuffle(Group), data = Sleep))

## ----setseed134, echo=FALSE-----------------------------------------
set.seed(134) # make sure the result below is "typical"

## ----sleep, tidy=FALSE, fig.width=4, fig.height=2-------------------
sleep_null <- do(2000) * diff(mean(Words ~ shuffle(Group), data = Sleep))
gf_histogram( ~ Sleep, fill = ~ (Sleep >= obs), data = sleep_null, 
  binwidth = 0.4,
  xlab = "Distribution of difference in means under the null hypothesis")

## -------------------------------------------------------------------
cor(Price ~ Miles, data = Mustangs)

## -------------------------------------------------------------------
mustangs_cor_boot <- do(2000) * cor(Price ~ Miles, data = resample(Mustangs))
quantiles <- qdata( ~ cor, c(.025, .975), data = mustangs_cor_boot)
quantiles

## ----cor, fig.width=4, fig.height=2, tidy=FALSE---------------------
mustangs_hist <- mutate(mustangs_cor_boot, 
  colorval = cut(cor, c(-Inf, quantiles, Inf),
    labels = c("Lower", "Middle", "Upper")))
gf_histogram( ~ cor, data = mustangs_hist, fill = ~ colorval, n = 50) 
confint(mustangs_cor_boot)

## ----price-mileage-graph, fig.width=4, fig.height=3-----------------
gf_point(Price ~ Miles, data = Mustangs) %>%
  gf_lm()

## ----mustangregression----------------------------------------------
lm(Price ~ Miles, data = Mustangs)

## -------------------------------------------------------------------
mean( ~ Price, data = Mustangs)

## -------------------------------------------------------------------
lm(Price ~ 1, data = Mustangs)

## -------------------------------------------------------------------
mean(Price ~ 1, data = Mustangs)

## -------------------------------------------------------------------
mean(Words ~ 1, data = Sleep)

## -------------------------------------------------------------------
mean(Words ~ Group, data = Sleep)

## -------------------------------------------------------------------
lm(Words ~ Group, data = Sleep)

## -------------------------------------------------------------------
diffmean(Words ~ Group, data = Sleep) 

## -------------------------------------------------------------------
prop(homeless ~ 1, data = HELPrct)

## -------------------------------------------------------------------
prop(homeless ~ sex, data = HELPrct)

## -------------------------------------------------------------------
diffprop(homeless ~ sex, data = HELPrct)

## -------------------------------------------------------------------
lm(homeless=="homeless" ~ 1, data = HELPrct)

## -------------------------------------------------------------------
lm(homeless=="homeless" ~ sex, data = HELPrct)

## -------------------------------------------------------------------
mustangs_lm_boot <- do(2000) * lm(Price ~ Miles, data = resample(Mustangs))
confint(mustangs_lm_boot)
# compare to large sample estimate
confint(lm(Price ~ Miles, data = Mustangs))

## -------------------------------------------------------------------
# large sample estimate
msummary(lm(age ~ sex, data = HELPrct))
# compare to permutation test
HELPrct_null <- do(2000) * lm(age ~ shuffle(sex), data = HELPrct)
prop(~ (abs(sexmale) > 0.7841), data = HELPrct_null) 

## -------------------------------------------------------------------
mustangs_boot1 <- do(2000) * lm(Price ~ Age, data = resample(Mustangs))
mustangs_boot2 <- do(2000) * lm(Price ~ Miles, data = resample(Mustangs))
mustangs_boot3 <- do(2000) * lm(Price ~ Miles + Age, data = resample(Mustangs))


## -------------------------------------------------------------------
confint(mustangs_boot1)

## -------------------------------------------------------------------
confint(mustangs_boot2)

## -------------------------------------------------------------------
confint(mustangs_boot3)

## -------------------------------------------------------------------
confint(lm(Price ~ Miles + Age, data = Mustangs))

## ----results = "asis", echo = FALSE---------------------------------
ds <- data.frame(
  y = c(234,324,231), residual = c(25,-23,-22))
set.seed(104)

ds <- ds %>%
  mutate(
    predicted = y - residual,
    resamp_residual = resample(residual),
    resamp_observed = predicted + resamp_residual) %>%
  select(y, predicted, residual, resamp_residual, resamp_observed)
knitr::kable(ds)

## -------------------------------------------------------------------
set.seed(104)

mustang_mod <- lm(Price ~ Miles + Age, data = Mustangs)
mustang_relm_boot <- do(2000) * relm(mustang_mod)

## -------------------------------------------------------------------
confint(mustang_relm_boot)
gf_histogram( ~ Miles, data = mustang_relm_boot)

## -------------------------------------------------------------------
anova(lm(Price ~ Miles + Age, data = Mustangs))

## -------------------------------------------------------------------
anova(lm(Price ~ Age + Miles, data = Mustangs))

## -------------------------------------------------------------------
do(1) * lm(Price ~ Miles, data = Mustangs)
do(1) * lm(Price ~ Miles + Age, data = Mustangs)

## -------------------------------------------------------------------
mustangs_age_boot <- do(2000) * lm(Price ~ Miles + shuffle(Age), data = Mustangs )
favstats(~ r.squared, data = mustangs_age_boot)
cdata(~ r.squared, .95, data = mustangs_age_boot)

## -------------------------------------------------------------------
mustangs_miles_boot <- do(2000) * lm(Price ~ shuffle(Miles) + Age, data = Mustangs)
favstats(~ r.squared, data = mustangs_miles_boot)
cdata(~ r.squared, .95, data = mustangs_miles_boot)

## -------------------------------------------------------------------
chisq.test(tally( ~ homeless + sex, 
                   data = HELPrct, margins = FALSE))

## -------------------------------------------------------------------
pval(chisq.test(tally( ~ homeless + sex, 
                   data = HELPrct, margins = FALSE)) )

## -------------------------------------------------------------------
pval(chisq.test(tally( ~ shuffle(homeless) + sex, 
                         data = HELPrct, margins = FALSE)))

## -------------------------------------------------------------------
chisq_null <- do(2000)* pval(chisq.test(tally( ~ shuffle(homeless) + sex, 
                         data = HELPrct, margins = FALSE)))

## ----fig.width=4, fig.height=2--------------------------------------
prop( ~ (p.value < 0.05), data = chisq_null)
gf_histogram( ~ p.value, data = chisq_null, binwidth = 0.1, center = 0.05)

## ----fig.width=4, fig.height=2--------------------------------------
qqmath( ~ p.value, data = chisq_null, dist = qunif)

## -------------------------------------------------------------------
HELP_logistic_boot <- do(2000) * 
   glm(homeless=="homeless" ~ age + sex, 
     data = resample(HELPrct), family = "binomial")
confint(HELP_logistic_boot)

