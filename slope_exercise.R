obsInt = -79.03
obsSlope = 0.3860
winAvg = 50.4
winSD = 20.12

X = rnorm(25, 335, 45)



plot(NA, type='n', xlim=c(-100,100), ylim=c(-100,100))
slopes = NULL

for(i in 1:100)
{
    win = winAvg + rnorm(25, 0, 20.12)
    temp = lm(win~X)
    
    slopes = c(slopes, coef(temp)[2])
    abline(a=coef(temp[1]), b=coef(temp)[2])    
}


abline(a=obsInt, b=obsSlope, col="red", lty=2)
hist(slopes, xlim=c(-.3,.4))
abline(v=obsSlope, col=2)
sum(slopes>obsSlope) / length(slopes)

