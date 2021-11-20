X<-c(0,50,100,200,250,300)

Y<-c(5.1,7.5,9.2,9.8,9.6,9.7)

plot(X,Y, col="red",ylim=c(0,11), pch=19)

#Quadratic model
# Y= a + b*X + c*X^2

X2<-X*X

Mod_Q<-lm(Y~X+X2)
summary(Mod_Q)

dose<-0:300
Rdt<-5.367+4.439e-02*dose+-1.037e-04*dose^2
Rdt<-coef(Mod_Q)[1]+coef(Mod_Q)[2]*dose+coef(Mod_Q)[3]*dose^2

lines(dose,Rdt, lwd=2,col="blue")



LP<-function(X, T1, T2, T3) {
  rdt<-T1+T3*(X-T2)
  rdt[X>=T2]<-T1
  return(rdt)	
}

Mod_LP<-nls(Y~LP(X,T1,T2,T3), start=list(T1=9, T2=100, T3=0.01), trace=T)
summary(Mod_LP)

lines(0:300,LP(0:300,coef(Mod_LP)[1],coef(Mod_LP)[2],coef(Mod_LP)[3]),col="brown", lwd=2)

QP<-function(d, Theta0, Theta1, Theta2) {
  Y<-Theta0+Theta1*(d-Theta2)^2
  Y[d>=Theta2]<-Theta0
  return(Y)
}  

Fit<-nls(Y~QP(X, Theta0, Theta1, Theta2), start=list(Theta0=9, Theta1=-0.004, Theta2=100))

summary(Fit)

Parameters<-coef(Fit)

Pred<-Parameters[1]+Parameters[2]*(0:300-Parameters[3])^2
Pred[0:300>Parameters[3]]<-Parameters[1]

lines(0:300, Pred, lwd=2, col="darkgreen")

