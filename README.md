# txtplot
Text based plots in R. Some examples


```r
## basic plotting function
require(stats)
txtplot(cars[,1], cars[,2])
## can include axis labels when desired
txtplot(cars[,1], cars[,2], xlab = "speed", ylab = "distance")

## text based density plot
txtdensity(rnorm(500))

## text based plotting of functions
txtcurve(x/(x+1), 0, 4, xlab = "Emax model")

## text based acf
txtacf(rnorm(100))

## text based barchart
x <- factor(c("orange", "orange", "red", "green", "green", "red",
             "yellow", "purple", "purple", "orange"))
txtbarchart(x)
```

