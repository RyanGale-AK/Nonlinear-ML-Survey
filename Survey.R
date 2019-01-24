advertising = read.csv("Advertising.csv", header = TRUE)
advertising <- data.frame(advertising$TV, advertising$sales) # remove unnecessary columns

polyMSE <- 0
stepMSE <- 0
cubicMSE <- 0
naturalMSE <- 0
smoothMSE <- 0

for(i in 1:50){
  set.seed(i)
  trainset = sample(1:200, 100, replace=FALSE)
  train <- advertising[trainset, ]
  test <- advertising[-trainset, ]
  
  # POLYNOMIAL REGRESSION
  poly4=lm(advertising.sales~poly(advertising.TV,4,raw=T),data=train) #use raw polynomial x, x^2,x^3,...
  poly_preds=predict(poly4,newdata=as.list(test),se=TRUE)
  poly_err=mean((poly_preds$fit-test[,2])^2)
  polyMSE[i] <- poly_err
  
  # STEP FUNCTIONS
  table(cut(advertising[,1],4))
  step=lm(advertising[,2]~cut(advertising[,1],4),data=train)
  step_preds=predict(step,newdata=list(test[,1]),se=TRUE)
  step_err = mean((step_preds$fit-test[,2])^2)
  stepMSE[i] <- step_err
  
  # CUBIC SPLINES
  library(splines)
  cubic=lm(advertising.sales~bs(advertising.TV,df=5),data=train) 
  cubic_pred=predict(cubic,newdata=as.list(test),se=T)
  cubic_err = mean((cubic_pred$fit-test[,2])^2)
  cubicMSE[i] <- cubic_err
  
  # NATURAL CUBIC SPLINES
  natural=lm(advertising.sales~ns(advertising.TV,df=5),data=train) #fit natural spline with 5 degrees 
  natural_pred=predict(natural,newdata=as.list(test),se=T)
  natural_err = mean((natural_pred$fit-test[,2])^2)
  naturalMSE[i] <- natural_err
  
  # SMOOTHING SPLINES
  smooth=smooth.spline(advertising$advertising.sales,advertising$advertising.TV,df=5)
  smooth_pred=predict(smooth,newdata=as.list(test),se=T)
  smooth_err = mean((smooth_pred$x-test[,2])^2)
  smoothMSE[i] <- smooth_err
}
boxplot(polyMSE, stepMSE, cubicMSE, naturalMSE, smoothMSE, 
        names=c("Poly","Step",
                "Cubic","Nat","Smooth"), 
        main="Nonlinear Methods", ylab="Mean-Squared Error")

# Get prediction errors for 1st seed
polyMSE[1]
stepMSE[1]
cubicMSE[1]
naturalMSE[1]
smoothMSE[1]