


data<-read.csv("manpower.csv",header=FALSE)
data<-as.matrix(data)

y<-data[,7]
n<-nrow(data)
x<-matrix(0,nrow=n,ncol=6)
x[,2:6]<-data[,2:6]
x[,1]<-1

(beta<-solve(t(x)%*%x)%*%t(x)%*%y)   

(h<-x%*%solve(t(x)%*%x)%*%t(x))      # HAT MATRIX
(yhat<-h%*%y)                        

(Q<-t((y-x%*%beta))%*%(y-x%*%beta))  # SSE
(sigma2<-Q/(n-6))                    # MSE

sigma2<-as.integer(sigma2)           
(covbeta<-sigma2*(solve(t(x)%*%x)))

# 多自变量回归
manpower<-read.csv("manpower.csv",header=FALSE)

fit1<-lm(V7~1, data=manpower)
result1<-summary(fit1)

fit2 <- lm(V7~1+V2+V3+V4, data=manpower)
result2<-summary(fit2)


(result3<-anova(fit1,fit2))




