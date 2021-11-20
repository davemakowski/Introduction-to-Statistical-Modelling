x<-c(100, 200)

y<-c(0.83, 0.95)

plot(x,y, pch=19, xlim=c(0,250), ylim=c(0,1))

TAB<-data.frame(x,y)
print(TAB)

#Our model is: Y= 1- exp(-Theta*x)

Fit<-nls(y~1-exp(-Theta*x), data=TAB, start=list(Theta=0.01), trace=T)

print(summary(Fit))

X.vec<-0:250

Y.vec<-1-exp(-coef(Fit)[1]*X.vec)

#Y.vec<-1-exp(-0.1733636)*X.vec
dev.new()
plot(x,y, xlim=c(0, 250), pch=19, cex=2, ylim=c(0,1))
lines(X.vec, Y.vec, lwd=2)

X=50
Y1=1-exp(-(coef(Fit)[1]+2*0.001)*X)
Y2=1-exp(-(coef(Fit)[1]-2*0.001)*X)
print(Y1)
print(Y2)
