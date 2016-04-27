## Load the data
data("mtcars")

## Take a look at the structure and a summary of the data we are using
str(mtcars)
summary(mtcars)


## Convert necessary columns to factors
mtcars$am   <- factor(mtcars$am,labels=c("Auto","Manual"))
mtcars$cyl  <- factor(mtcars$cyl)
mtcars_expl <- mtcars[,c(1,2,4,6,7,9)]

## Exploratory Data Analysis
plot(as.factor(mtcars$am),mtcars$mpg, col = (c("green","red")), 
        ylab = "mpg (Miles per Gallon)", xlab = "Transmission Type")
pairs(mtcars_expl)

## Create regression models for just am, and for all variables
ammodel <- lm(mpg ~ am, mtcars)
allmodel <- lm(mpg~., mtcars)
## Create regression models with the most applicable variables
trimmodel <- step(allmodel, direction = "both")
summary(trimmodel)

anova(ammodel, trimmodel)

confint(trimmodel, "amManual", level=0.95)

par(mfrow=c(2, 2))
plot(trimmodel)