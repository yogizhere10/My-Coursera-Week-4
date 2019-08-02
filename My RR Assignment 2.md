My Reproducible Research Week 4 Assignment
===========================================

Impact of Severe Weather Events on Population Health and Property Damage
========================================================================

Synopsis:
========

This research investigates the impact of natural events on public and economic health. The data used is the National Weather Service Storm Data collection over 61 years (from 1950 untill 2011) which contains 902297 observations and 37 variables. The purpose of this report is to show which weather related event types have the most damaging impact. The first graph shows the top eight event types which lead to the most direct peronal injuries and the second graph shows the top eight event types which lead to most economic damage. Both graphs clearly show the Tornado event type as a leading cause in both personal injuries and economic damage, with other wind or water related (e.g. flooding, flash-flooding) event types a not so close second. This research suggests taking a further look into preventing wind and water related disasters as a measure with the highest ROI.

Data Processing:
===============


```r
calamity <- read.table("calamitydata.csv", sep = ",", header = TRUE)
str(calamity)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436774 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(knitr)
```


Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
=========================================================================================================================================



```r
cal1 <- group_by(calamity, EVTYPE)
cal1 <- summarize(cal1, sum_fatality = sum(FATALITIES, na.rm = TRUE), sum_injury = sum(INJURIES, na.tm = TRUE))
cal1 <- arrange(cal1, desc(sum_fatality))
head(cal1)
```

```
## # A tibble: 6 x 3
##   EVTYPE         sum_fatality sum_injury
##   <fct>                 <dbl>      <dbl>
## 1 TORNADO                5633      91347
## 2 EXCESSIVE HEAT         1903       6526
## 3 FLASH FLOOD             978       1778
## 4 HEAT                    937       2101
## 5 LIGHTNING               816       5231
## 6 TSTM WIND               504       6958
```

```r
cal2 <- cal1[1:12,]
ggplot(data = cal2, aes(EVTYPE, sum_fatality)) + geom_bar(fill = "green", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8, color=2))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)


Across the United States, which types of events have the greatest economic consequences?
========================================================================================



```r
cal3 <- select(calamity, EVTYPE, CROPDMG, CROPDMGEXP, PROPDMG, PROPDMGEXP)
cal3 <- mutate(cal3, cropdamage = CROPDMG * case_when(
  CROPDMGEXP == "B" ~ 10^9,
  CROPDMGEXP == "k" | CROPDMGEXP == "K" ~ 10^3,
  CROPDMGEXP == "m" | CROPDMGEXP == "M" ~ 10^6,
  CROPDMGEXP == 2 ~ 2,
  TRUE ~ 0
))
cal3 <- arrange(cal3, desc(cropdamage))
head(cal3)
```

```
##              EVTYPE CROPDMG CROPDMGEXP PROPDMG PROPDMGEXP cropdamage
## 1       RIVER FLOOD    5.00          B    5.00          B 5000000000
## 2         ICE STORM    5.00          B  500.00          K 5000000000
## 3 HURRICANE/TYPHOON    1.51          B    5.88          B 1510000000
## 4           DROUGHT    1.00          B    0.00            1000000000
## 5      EXTREME COLD  596.00          M    0.00             596000000
## 6           DROUGHT  578.85          M    0.00             578850000
```

```r
cal3 <- mutate(cal3, propdamage = PROPDMG * case_when(
  PROPDMGEXP == "B" | PROPDMGEXP == "b" ~ 10^9,
  PROPDMGEXP == "H" | PROPDMGEXP == "h" ~ 10^2,
  PROPDMGEXP == "k" | PROPDMGEXP == "K" ~ 10^3,
  PROPDMGEXP == "m" | PROPDMGEXP == "M" ~ 10^6,
  PROPDMGEXP == 0 ~ 0,
  PROPDMGEXP == 1 ~ 1,
  PROPDMGEXP == 2 ~ 2,
  PROPDMGEXP == 3 ~ 3,
  PROPDMGEXP == 4 ~ 4,
  PROPDMGEXP == 5 ~ 5,
  PROPDMGEXP == 6 ~ 6,
  PROPDMGEXP == 7 ~ 7,
  PROPDMGEXP == 8 ~ 8,
  TRUE ~ 0
))
```


```r
cal3 <- arrange(cal3, desc(propdamage))
head(cal3)
```

```
##              EVTYPE CROPDMG CROPDMGEXP PROPDMG PROPDMGEXP cropdamage
## 1             FLOOD    32.5          M  115.00          B   32500000
## 2       STORM SURGE     0.0              31.30          B          0
## 3 HURRICANE/TYPHOON     0.0              16.93          B          0
## 4       STORM SURGE     0.0              11.26          B          0
## 5 HURRICANE/TYPHOON     0.0              10.00          B          0
## 6 HURRICANE/TYPHOON     0.0               7.35          B          0
##   propdamage
## 1  1.150e+11
## 2  3.130e+10
## 3  1.693e+10
## 4  1.126e+10
## 5  1.000e+10
## 6  7.350e+09
```

```r
cal4 <- select(cal3, EVTYPE, propdamage, cropdamage)
cal4 <- arrange(cal4, desc(propdamage + cropdamage))
head(cal4)
```

```
##              EVTYPE propdamage cropdamage
## 1             FLOOD  1.150e+11   3.25e+07
## 2       STORM SURGE  3.130e+10   0.00e+00
## 3 HURRICANE/TYPHOON  1.693e+10   0.00e+00
## 4       STORM SURGE  1.126e+10   0.00e+00
## 5 HURRICANE/TYPHOON  1.000e+10   0.00e+00
## 6       RIVER FLOOD  5.000e+09   5.00e+09
```

```r
cal5 <- cal4[1:10,]
cal5
```

```
##               EVTYPE propdamage cropdamage
## 1              FLOOD  1.150e+11   3.25e+07
## 2        STORM SURGE  3.130e+10   0.00e+00
## 3  HURRICANE/TYPHOON  1.693e+10   0.00e+00
## 4        STORM SURGE  1.126e+10   0.00e+00
## 5  HURRICANE/TYPHOON  1.000e+10   0.00e+00
## 6        RIVER FLOOD  5.000e+09   5.00e+09
## 7  HURRICANE/TYPHOON  5.880e+09   1.51e+09
## 8  HURRICANE/TYPHOON  7.350e+09   0.00e+00
## 9  HURRICANE/TYPHOON  5.420e+09   2.85e+08
## 10    TROPICAL STORM  5.150e+09   0.00e+00
```

```r
x <- cal5$EVTYPE
damage<- as.matrix(t(cal5[,-1]))
colnames(damage)<-x
par(mfrow = c(1,1))
barplot(damage, col = c("green", "blue"), main = "Impact of Severe Weather Events on Economic Damage")
legend("topright", c("Property","Crop"), fill = c("green", "blue"), bty = "x")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Results and Conclusions:
=======================
Tornado caused the maximum number of fatalities and injuries. It was followed by Excessive Heat for fatalities and Thunderstorm wind for injuries.

Floods caused the maximum property damage where as Drought caused the maximum crop damage. Second major events that caused the maximum damage was Hurricanes/Typhoos for property damage and Floods for crop damage.library(knitr)
> knit2html("My RR Assignment 2.Rmd")
