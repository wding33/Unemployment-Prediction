    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(tidyr)

    bridges=read.csv("https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt") %>% 
      select(COUNTY_CODE_003,YEAR_BUILT_027,ADT_029)

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec =
    ## dec, : EOF within quoted string

    year=bridges %>% 
      group_by(COUNTY_CODE_003) %>% 
      summarise(year=sum(YEAR_BUILT_027))
    traffic=bridges %>% 
      group_by(COUNTY_CODE_003) %>% 
      summarise(traffic=sum(ADT_029))
    bridges=left_join(year,traffic)

    ## Joining, by = "COUNTY_CODE_003"

    WIunemployment=read.csv('https://www.bls.gov/web/metro/laucntycur14.txt',skip = 6,sep = '|',header = F,colClasses = 'character')
    colnames(WIunemployment)=c("LAUS Area Code","State","County","Area Title","Period","Civilian_Labor_Force","Employed","Unemployed_Level","Unemployed_Rate")
    WIunemployment$State = as.numeric(WIunemployment$State)
    WIunemployment_Dec=filter(WIunemployment,State == 55 & Period=='   Dec-18  ') %>% 
      select(County,Unemployed_Level,Unemployed_Rate)
    WIunemployment_Nov=filter(WIunemployment,State == 55 & Period=='   Nov-18  ') %>% 
      select(County,pre_level=Unemployed_Level,pre_rate=Unemployed_Rate)
    WIunemployment=left_join(WIunemployment_Dec,WIunemployment_Nov)

    ## Joining, by = "County"

    WIunemployment$County=as.numeric(WIunemployment$County)
    WIunemployment$Unemployed_Level=as.numeric(gsub(",", "", WIunemployment$Unemployed_Level))
    WIunemployment$Unemployed_Rate=as.numeric(WIunemployment$Unemployed_Rate)
    WIunemployment$pre_level=as.numeric(gsub(",", "", WIunemployment$pre_level))
    WIunemployment$pre_rate=as.numeric(WIunemployment$pre_rate)

    data=left_join(bridges,WIunemployment,by = c("COUNTY_CODE_003" = "County")) %>% 
          select(COUNTY_CODE_003,year,traffic,Unemployed_Level,Unemployed_Rate,pre_level,pre_rate)
    head(data)

    ## # A tibble: 6 x 7
    ##   COUNTY_CODE_003   year traffic Unemployed_Level Unemployed_Rate pre_level
    ##             <dbl>  <int>   <dbl>            <dbl>           <dbl>     <dbl>
    ## 1               1  63673   42616              442             5.5       318
    ## 2               3 146656   57316              312             4.1       267
    ## 3               5 319548  438748              916             3.7       655
    ## 4               7 162912  142437              429             5.7       337
    ## 5               9 317930 2613530             3513             2.5      3132
    ## 6              11 381388  236227              240             3.6       162
    ## # … with 1 more variable: pre_rate <dbl>

    number=lm(Unemployed_Level~year+traffic,data = data)
    rate=lm(Unemployed_Rate~year+traffic,data = data)
    number_pre=lm(Unemployed_Level~year+traffic+pre_level,data = data)
    rate_pre=lm(Unemployed_Rate~year+traffic+pre_rate,data = data)
    summary(number)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Level ~ year + traffic, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1794.5  -777.9  -428.1   -19.8 12618.1 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  1.221e+03  3.751e+02   3.255  0.00185 **
    ## year        -2.969e-03  1.616e-03  -1.838  0.07097 . 
    ## traffic      1.925e-03  5.576e-04   3.452  0.00102 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1930 on 61 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1638, Adjusted R-squared:  0.1364 
    ## F-statistic: 5.973 on 2 and 61 DF,  p-value: 0.004275

    summary(rate)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Rate ~ year + traffic, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.43928 -0.54657 -0.05081  0.46644  2.29068 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.677e+00  1.595e-01  23.056   <2e-16 ***
    ## year        -1.436e-06  6.869e-07  -2.090   0.0408 *  
    ## traffic     -2.386e-07  2.371e-07  -1.007   0.3181    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8204 on 61 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1597, Adjusted R-squared:  0.1321 
    ## F-statistic: 5.795 on 2 and 61 DF,  p-value: 0.004965

    summary(number_pre)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Level ~ year + traffic + pre_level, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -544.31  -56.06   -8.58   45.72  274.63 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.165e+02  2.433e+01   4.790 1.13e-05 ***
    ## year        -1.061e-05  1.007e-04  -0.105    0.916    
    ## traffic      7.883e-06  3.698e-05   0.213    0.832    
    ## pre_level    1.009e+00  7.851e-03 128.479  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 117.1 on 60 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.997,  Adjusted R-squared:  0.9968 
    ## F-statistic:  6584 on 3 and 60 DF,  p-value: < 2.2e-16

    summary(rate_pre)

    ## 
    ## Call:
    ## lm(formula = Unemployed_Rate ~ year + traffic + pre_rate, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.70287 -0.23334 -0.09807  0.16943  1.02209 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.771e-01  2.417e-01   0.733  0.46642    
    ## year         2.221e-07  3.330e-07   0.667  0.50733    
    ## traffic     -2.907e-07  1.086e-07  -2.676  0.00959 ** 
    ## pre_rate     1.181e+00  7.776e-02  15.192  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3757 on 60 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.8266, Adjusted R-squared:  0.818 
    ## F-statistic: 95.35 on 3 and 60 DF,  p-value: < 2.2e-16

I select the year bubilt and the average daily traffic to predict unemployment. As suggested by the model summary, average daily traffic is significant in predicting the number of unemployed and year bubilt is significant in predicting the nuemployment rate. The number of unemployed is estimated to increase 1.93 with 1000 increase of average daily traffic. After adding the unemployment level and rate from previous month, I found the accuracy of model increase dramatically indicated by R-square.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
