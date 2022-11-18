df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

nll_lm <- function(par,data){
  
  
  beta_hat<-matrix(par[1:4],nrow  = 4)
  
  xs<- as.matrix(cbind(1,data$x1,data$x2,data$x3))
  
  -1*sum(dnorm(data$y,xs%*%beta_hat,par[5],log=TRUE))
}

output<-optim(par=c(mean(df$y),1,1,1,1),
              fn=nll_lm,data=df
)
output
sum<-summary(lm(y~x1+x2+x3,data = df))
sum$coefficients
sum$sigma
