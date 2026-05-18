### 1
st1_c(33,37,40,46,42,42,38,34,45,36,38,42)
st2_c(32,34,54,58,40,45,47,43,35,45,46,53,43,32,43)

library("stepfun")
Fn1_ecdf(st1)
Fn2_ecdf(st2)

r_range(c(st1,st2))

postscript("Figs/hw03.st.ps",height=7,width=10)
par(las=1)
plot(Fn1,xlim=r,verticals=TRUE,col.01line="white",main="",pch=16)
plot(Fn2,xlim=r,verticals=TRUE,col.01line="white",lty=2,
     main="Comparing two samples",add=T)
text(40,0.9,"A")
text(48,0.6,"B")
dev.off()

t.test(st1,st2)

ks.test(st1,st2)


### 2

cd4_scan("cd4.txt")

r_range(log(cd4))
br_seq(r[1],r[2],length=51)

postscript("Figs/hw03.cd4.ps",horiz=TRUE,height=7,width=10)
par(mfrow=c(1,2),las=1,mar=c(4.1,3.1,5.1,1.1))
hist(log(cd4),breaks=br,main="Logarithm of CD4 counts",xlab="",ylab="")
qqnorm(log(cd4),main="QQ-plot",ylab=""); qqline(log(cd4))
dev.off()


mu_mean(log(cd4))
sd_sqrt(var(log(cd4)))

ks.test(log(cd4),"pnorm",mu,sd) 

