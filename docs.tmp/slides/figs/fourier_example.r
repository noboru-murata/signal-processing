def.par <- par(no.readonly = TRUE) # layout前の環境を保存
lns <- function(x) lcm(x * par("csi") * 2.54)
layout(rbind(c(1,1,1,1),
             c(2,3,4,5),
             c(6,7,8,9)),
       height = c(1,1,1),
       width = c(1,1,1,1))
## layout の確認
## layout.show(3)
## box("outer", lty = "dotted")
f <- function(x) {
  if (all(Im(z <- zapsmall(x))==0)) as.numeric(z) else x
}
par(mar=c(2,1,2,.5))
an <- function(n){ifelse(n!=0,
                         (1i)^(n+1)/(2*pi*n)*((-1)^n-1),
                         1/2)}
n <- -8:8
plot(n,sapply(n,an),type="h",col="blue",lwd=2,xlab="",ylab="",xaxt="n",yaxt="n")
axis(1,at=n,labels=n)
x <- seq(-1.5,1.5,length=20*6+1)
y <- rep(an(0),length=length(x))
for(k in 1:8){
  plot(x,y,type="l",lwd=2,col="orange",xlab="",ylab="",xaxt="n",yaxt="n",
       main=paste0("|n|<",k))
  axis(1,at=-1:1,labels=c(expression(-pi),0,expression(pi)))
  y <- f(y + an(k)*exp(pi*k*1i*x) + an(-k)*exp(-pi*k*1i*x))
}
par(def.par)
