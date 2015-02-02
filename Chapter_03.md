# Chapter 03


# 3.4 Multiple Linear Regression Parameter Estimation


```r
SUP = read.table("All_Data//P060.txt", sep="\t", header=TRUE)
SUP.lm = lm(Y ~ X1+X2+X3+X4+X5+X6, SUP)
print(summary(SUP.lm))
```

```
## 
## Call:
## lm(formula = Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = SUP)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.9418  -4.3555   0.3158   5.5425  11.5990 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 10.78708   11.58926   0.931 0.361634    
## X1           0.61319    0.16098   3.809 0.000903 ***
## X2          -0.07305    0.13572  -0.538 0.595594    
## X3           0.32033    0.16852   1.901 0.069925 .  
## X4           0.08173    0.22148   0.369 0.715480    
## X5           0.03838    0.14700   0.261 0.796334    
## X6          -0.21706    0.17821  -1.218 0.235577    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.068 on 23 degrees of freedom
## Multiple R-squared:  0.7326,	Adjusted R-squared:  0.6628 
## F-statistic:  10.5 on 6 and 23 DF,  p-value: 1.24e-05
```

