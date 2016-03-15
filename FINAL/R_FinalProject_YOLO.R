#####YOLO_½²Ä£½å ÁÂ©Ó¿Ý ¿½³Õ­× ÃQ¾å±á
#####final project code

District.A2_d <- read.csv("E:/District A2_d.csv")
View(District.A2_d)

y.illness = District.A2_d$Illness.Case
y.illness_cum = District.A2_d$Illness_cum

x.rain = District.A2_d$Rain
x.spray = District.A2_d$Spraying
x.temp = District.A2_d$Temparature
x.density = District.A2_d$Density_movaverage
x.t = District.A2_d$t
x.t2 = District.A2_d$t.2


#============================================================
#model 1
#y = illness, x:{rain,spray,temp,density}
xy1 = data.frame(x.rain,x.spray,x.temp,x.density,y.illness)
model1 = lm(y.illness~x.rain+x.spray+x.temp+x.density,xy1)
summary(model1)
# R-squred : 0.3085
"""
lm(formula = y.illness ~ x.rain + x.spray + x.temp + x.density, 
   data = xy1)

Residuals:
  Min     1Q Median     3Q    Max 
-64.89 -24.12 -10.36  23.27 110.93 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -114.42473   86.61432  -1.321 0.189215    
x.rain         0.07232    0.17836   0.405 0.685913    
x.spray       13.18288    2.99282   4.405 2.47e-05 ***
  x.temp         5.64704    2.97462   1.898 0.060263 .  
x.density     -2.42312    0.65243  -3.714 0.000322 ***
  ---
  Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 38.18 on 110 degrees of freedom
Multiple R-squared:  0.3085,	Adjusted R-squared:  0.2833 
F-statistic: 12.27 on 4 and 110 DF,  p-value: 2.782e-08
"""


#============================================================
#model 2
#y = illness, x:{rain,spray,temp,density,t}
xy2 = data.frame(x.rain,x.spray,x.temp,x.density,x.t,y.illness)
model2 = lm(y.illness~x.rain+x.spray+x.temp+x.density+x.t,xy2)
summary(model2)
#R-squared:  0.4346
"""
lm(formula = y.illness ~ x.rain + x.spray + x.temp + x.density + 
x.t, data = xy2)

Residuals:
Min      1Q  Median      3Q     Max 
-70.445 -20.939  -4.112  14.191  92.707 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept) -444.1988   103.2646  -4.302 3.71e-05 ***
x.rain         0.4382     0.1782   2.459   0.0155 *  
x.spray        9.2811     2.8314   3.278   0.0014 ** 
x.temp        15.3530     3.3431   4.592 1.18e-05 ***
x.density     -0.6767     0.6904  -0.980   0.3292    
x.t            0.8096     0.1642   4.931 2.95e-06 ***
---
Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 34.68 on 109 degrees of freedom
Multiple R-squared:  0.4346,	Adjusted R-squared:  0.4086 
F-statistic: 16.75 on 5 and 109 DF,  p-value: 2.968e-12
"""


#============================================================
#model 3
#y = illness, x:{rain,spray,temp,density,t,t2}
xy3 = data.frame(x.rain,x.spray,x.temp,x.density,x.t,x.t2,y.illness)
model3 = lm(y.illness~x.rain+x.spray+x.temp+x.density+x.t+x.t2,xy3)
summary(model3)
#R-squared:  0.6253
"""
lm(formula = y.illness ~ x.rain + x.spray + x.temp + x.density + 
x.t + x.t2, data = xy3)

Residuals:
Min      1Q  Median      3Q     Max 
-51.126 -20.737  -5.895  19.820  75.625 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept) -306.98338   86.45732  -3.551 0.000571 ***
x.rain         0.04965    0.15488   0.321 0.749134    
x.spray        6.46242    2.34663   2.754 0.006911 ** 
x.temp         8.63312    2.88041   2.997 0.003381 ** 
x.density      0.65815    0.59266   1.110 0.269253    
x.t            3.49662    0.38651   9.047 6.82e-15 ***
x.t2          -0.02313    0.00312  -7.414 2.91e-11 ***
---
Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 28.36 on 108 degrees of freedom
Multiple R-squared:  0.6253,	Adjusted R-squared:  0.6045 
F-statistic: 30.04 on 6 and 108 DF,  p-value: < 2.2e-16
"""


#============================================================
#model 4
#y = illness_cum, x:{rain,spray,temp,density}
xy4 = data.frame(x.rain,x.temp,x.spray,x.density,y.illness_cum)
model4 = lm(y.illness_cum~x.rain+x.temp+x.spray+x.density,xy4)
summary(model4)
#R-squared:  0.5302
"""
lm(formula = y.illness_cum ~ x.rain + x.temp + x.spray + x.density, 
    data = xy4)

Residuals:
Min      1Q  Median      3Q     Max 
-3011.8 -1208.0   -98.8   811.1  3775.8 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept) 25605.402   3310.227   7.735 5.31e-12 ***
x.rain        -35.926      6.817  -5.270 6.85e-07 ***
x.temp       -810.346    113.684  -7.128 1.13e-10 ***
x.spray       190.273    114.380   1.664 0.099054 .  
x.density     -96.872     24.935  -3.885 0.000175 ***
---
Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 1459 on 110 degrees of freedom
Multiple R-squared:  0.5302,	Adjusted R-squared:  0.5131 
F-statistic: 31.04 on 4 and 110 DF,  p-value: < 2.2e-16
"""


#============================================================
#model 5
#y = illness_cum, x:{rain,spray,temp,density,t}
xy5 = data.frame(x.rain,x.temp,x.spray,x.density,x.t,y.illness_cum)
model5 = lm(y.illness_cum~x.rain+x.temp+x.spray+x.density+x.t,xy5)
summary(model5)
#91%


#============================================================
#model 6
#y = illness_cum, x:{rain,spray,temp,density,t,t2}
xy6 = data.frame(x.rain,x.temp,x.spray,x.density,x.t,y.illness_cum)
model6 = lm(y.illness_cum~x.rain+x.temp+x.spray+x.density+x.t+x.t2,xy6)
summary(model6)
#97%


#============================================================
#model1_log :model1 with exp
model1_l = lm(log(y.illness+1)~x.rain+x.spray+x.temp+x.density)
summary(model1_l)#43%
"""
lm(formula = log(y.illness + 1) ~ x.rain + x.spray + x.temp + 
    x.density)

Residuals:
Min      1Q  Median      3Q     Max 
-3.3224 -0.7379  0.0636  0.8495  2.6689 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.269289   2.611276   1.635 0.104920    
x.rain      -0.006159   0.005377  -1.145 0.254543    
x.spray      0.363354   0.090228   4.027 0.000104 ***
x.temp      -0.030039   0.089680  -0.335 0.738295    
x.density   -0.113529   0.019670  -5.772 7.34e-08 ***
---
Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 1.151 on 110 degrees of freedom
(69 observations deleted due to missingness)
Multiple R-squared:  0.4388,	Adjusted R-squared:  0.4184 
F-statistic:  21.5 on 4 and 110 DF,  p-value: 4.004e-13
"""

#============================================================
#model2_log :model2 with exp
model2_l = lm(log(y.illness+1)~x.rain+x.spray+x.temp+x.t+x.density)
summary(model2_l)#61%
"""
lm(formula = log(y.illness + 1) ~ x.rain + x.spray + x.temp + 
    x.t + x.density)

Residuals:
Min       1Q   Median       3Q      Max 
-2.64361 -0.55782  0.02812  0.67469  1.70661 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept) -8.874599   2.841799  -3.123  0.00229 ** 
x.rain       0.008425   0.004904   1.718  0.08866 .  
x.spray      0.207841   0.077920   2.667  0.00881 ** 
x.temp       0.356815   0.092000   3.878  0.00018 ***
x.t          0.032266   0.004518   7.141 1.09e-10 ***
x.density   -0.043923   0.019000  -2.312  0.02267 *  
---
Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 0.9543 on 109 degrees of freedom
Multiple R-squared:  0.6177,	Adjusted R-squared:  0.6001 
F-statistic: 35.22 on 5 and 109 DF,  p-value: < 2.2e-16
"""

#============================================================
#model3_log :model3 with exp
model3_l = lm(log(y.illness+1)~x.rain+x.spray+x.temp+x.t+x.t2+x.density)
summary(model3_l)
#R-squared:  0.892
"""
lm(formula = log(y.illness + 1) ~ x.rain + x.spray + x.temp + 
    x.t + x.t2 + x.density)

Residuals:
Min       1Q   Median       3Q      Max 
-2.28117 -0.23707  0.03921  0.28735  1.10155 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept) -3.367e+00  1.553e+00  -2.168   0.0324 *  
x.rain      -7.172e-03  2.782e-03  -2.578   0.0113 *  
x.spray      9.470e-02  4.216e-02   2.246   0.0267 *  
x.temp       8.708e-02  5.175e-02   1.683   0.0953 .  
x.t          1.401e-01  6.944e-03  20.180   <2e-16 ***
x.t2        -9.284e-04  5.604e-05 -16.565   <2e-16 ***
x.density    9.659e-03  1.065e-02   0.907   0.3663    
---
Signif. codes:  0 ??**??0.001 ??*??0.01 ????0.05 ????0.1 ????1

Residual standard error: 0.5095 on 108 degrees of freedom
Multiple R-squared:  0.892,	Adjusted R-squared:  0.886 
F-statistic: 148.7 on 6 and 108 DF,  p-value: < 2.2e-16
"""

#============================================================
#model4_log :model 4 with exp
model4_l = lm(log(y.illness_cum+1)~x.rain+x.spray+x.temp+x.density)
summary(model4_l)#60%


#============================================================
#model5_log :model 5 with exp
model5_l = lm(log(y.illness_cum+1)~x.rain+x.spray+x.temp+x.t+x.density)
summary(model5_l)#91%


#============================================================
#model6_log :model 6 with exp
model6_l = lm(log(y.illness_cum+1)~x.rain+x.spray+x.temp+x.t+x.t2+x.density)
summary(model6_l)#99%


#============================================================
#model1_log_s :model1 with exp without spray
model1_l_s = lm(log(y.illness+1)~x.rain+x.temp+x.density)
summary(model1_l_s)
#R-squared:  0.356
"""
lm(formula = log(y.illness + 1) ~ x.rain + x.temp + x.density)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.5531 -0.7390  0.1044  0.9130  3.0232 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  9.223177   2.456178   3.755 0.000278 ***
x.rain      -0.012380   0.005492  -2.254 0.026152 *  
x.temp      -0.183476   0.086570  -2.119 0.036286 *  
x.density   -0.136756   0.020053  -6.820    5e-10 ***
---
Signif. codes:  0 ¡¥***¡¦ 0.001 ¡¥**¡¦ 0.01 ¡¥*¡¦ 0.05 ¡¥.¡¦ 0.1 ¡¥ ¡¦ 1

Residual standard error: 1.227 on 111 degrees of freedom
Multiple R-squared:  0.356,     Adjusted R-squared:  0.3386 
F-statistic: 20.46 on 3 and 111 DF,  p-value: 1.264e-10
"""

#============================================================
#model2_log_s :model2 with exp without spray
model2_l_s = lm(log(y.illness+1)~x.rain+x.temp+x.t+x.density)
summary(model2_l_s)
#R-squared:  0.5927
"""
lm(formula = log(y.illness + 1) ~ x.rain + x.temp + x.t + x.density)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.71848 -0.57975  0.05383  0.76036  1.60974 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -7.634407   2.880376  -2.650 0.009223 ** 
x.rain       0.006666   0.004993   1.335 0.184578    
x.temp       0.316288   0.093225   3.393 0.000963 ***
x.t          0.035635   0.004457   7.995 1.41e-12 ***
x.density   -0.048905   0.019427  -2.517 0.013262 *  
---
Signif. codes:  0 ¡¥***¡¦ 0.001 ¡¥**¡¦ 0.01 ¡¥*¡¦ 0.05 ¡¥.¡¦ 0.1 ¡¥ ¡¦ 1

Residual standard error: 0.9805 on 110 degrees of freedom
Multiple R-squared:  0.5927,    Adjusted R-squared:  0.5779 
F-statistic: 40.02 on 4 and 110 DF,  p-value: < 2.2e-16
"""

#============================================================
#model3_log_s :model3 with exp without spray
model3_l_s = lm(log(y.illness+1)~x.rain+x.temp+x.t+x.t2+x.density)
summary(model3_l_s)
#R-squared:  0.887
"""
lm(formula = log(y.illness + 1) ~ x.rain + x.temp + x.t + x.t2 + 
    x.density)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.33753 -0.24030  0.04558  0.30489  1.16460 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2.696e+00  1.552e+00  -1.737   0.0853 .  
x.rain      -8.295e-03  2.788e-03  -2.976   0.0036 ** 
x.temp       6.317e-02  5.157e-02   1.225   0.2232    
x.t          1.440e-01  6.851e-03  21.017   <2e-16 ***
x.t2        -9.488e-04  5.632e-05 -16.846   <2e-16 ***
x.density    8.626e-03  1.083e-02   0.796   0.4276    
---
Signif. codes:  0 ¡¥***¡¦ 0.001 ¡¥**¡¦ 0.01 ¡¥*¡¦ 0.05 ¡¥.¡¦ 0.1 ¡¥ ¡¦ 1

Residual standard error: 0.5189 on 109 degrees of freedom
Multiple R-squared:  0.887,     Adjusted R-squared:  0.8818 
F-statistic: 171.1 on 5 and 109 DF,  p-value: < 2.2e-16
"""

#============================================================
#model4_l_s :model 4 with exp without spray
model4_l_s = lm(log(y.illness_cum+1)~x.rain+x.temp+x.density)
summary(model4_l_s) #56%


#============================================================
#model5_l_s :model 5 with exp withour spray
model5_l_s = lm(log(y.illness_cum+1)~x.rain+x.temp+x.t+x.density)
summary(model5_l_s) #91%


#============================================================
#model6_l_s : model 6 with exp withour spray
model6_l_s = lm(log(y.illness_cum+1)~x.rain+x.temp+x.t+x.t2+x.density)
summary(model6_l_s) #99%


#============================================================
#model: density
pre.den2 = data.frame(x.temp,x.rain,x.spray,x.t,x.t2,x.density)
modelpre2 = lm(x.density~x.temp+x.rain+x.spray+x.t+x.t2,pre.den2)
summary(modelpre2)

"""
lm(formula = x.density ~ x.temp + x.rain + x.spray + x.t, data = pre.den)

Residuals:
    Min      1Q  Median      3Q     Max 
-9.9884 -3.1644 -0.0185  2.4058 24.3981 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 50.30456   13.42989   3.746 0.000288 ***
x.temp      -1.30171    0.44468  -2.927 0.004156 ** 
x.rain      -0.05328    0.02408  -2.213 0.028984 *  
x.spray     -0.40314    0.38912  -1.036 0.302467    
x.t         -0.12200    0.01946  -6.268 7.33e-09 ***
---
Signif. codes:  0 ¡¥***¡¦ 0.001 ¡¥**¡¦ 0.01 ¡¥*¡¦ 0.05 ¡¥.¡¦ 0.1 ¡¥ ¡¦ 1

Residual standard error: 4.789 on 110 degrees of freedom
  (69 observations deleted due to missingness)
Multiple R-squared:  0.3534,	Adjusted R-squared:  0.3298 
F-statistic: 15.03 on 4 and 110 DF,  p-value: 7.89e-10
"""
