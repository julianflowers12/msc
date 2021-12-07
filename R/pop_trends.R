## poptrend  - uses `poptrend` pacakge to look at trend counts using GAMM'

if(!require("poptrend"))install.packages("poptrend")

??poptrend

data <- simTrend(30, 10)
data
fit <- ptrend(count ~ trend(year, type = "smooth") + site, data = data)
summary(fit)
change(fit, 10, 20)
checkFit(fit, residuals = T)
summary.trend(fit)
