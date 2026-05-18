Gtest_function(x){
ff_c(9,3,3,1)/16*sum(x)
G_sum((x-ff)^2/ff)
return(G)}

val.sim_matrix(ncol=4,nrow=10000)
for (j in 1:10000) val.sim[j,]_rmultinom(5,f)
ref.dist0_apply(val.sim,1,Gtest)

val.sim_matrix(ncol=4,nrow=10000)
for (j in 1:10000) val.sim[j,]_rmultinom(10,f)
ref.dist1_apply(val.sim,1,Gtest)

val.sim_matrix(ncol=4,nrow=10000)
for (j in 1:10000) val.sim[j,]_rmultinom(20,f)
ref.dist2_apply(val.sim,1,Gtest)

val.sim_matrix(ncol=4,nrow=10000)
for (j in 1:10000) val.sim[j,]_rmultinom(40,f)
ref.dist3_apply(val.sim,1,Gtest)

val.sim_matrix(ncol=4,nrow=10000)
for (j in 1:10000) val.sim[j,]_rmultinom(80,f)
ref.dist4_apply(val.sim,1,Gtest)

val.sim_matrix(ncol=4,nrow=10000)
for (j in 1:10000) val.sim[j,]_rmultinom(160,f)
ref.dist5_apply(val.sim,1,Gtest)

r_range(c(ref.dist0,ref.dist1,ref.dist2,ref.dist3,ref.dist4,ref.dist5),na.rm=T)
br_seq(r[1],r[2],length=51) 
zz_seq(0,15,length=101)

postscript("Figs/hwrandom.ps",horiz=TRUE,height=7.5,width=10)
par(mfrow=c(2,3),las=1,mar=c(4.1,3.1,5.1,1.1))
hist(ref.dist0,breaks=br,xlab="",ylab="",prob=T,
     xlim=c(0,15),ylim=c(0,0.5),col="gray65",
     main="n = 5")
lines(zz,dchisq(zz,3),col="green")
hist(ref.dist1,breaks=br,xlab="",ylab="",prob=T,
     xlim=c(0,15),ylim=c(0,0.5),col="gray65",
     main="n = 10")
lines(zz,dchisq(zz,3),col="green")
hist(ref.dist2,breaks=br,xlab="",ylab="",prob=T,
     xlim=c(0,15),ylim=c(0,0.5),col="gray65",
     main="n = 20")
lines(zz,dchisq(zz,3),col="green")
hist(ref.dist3,breaks=br,xlab="",ylab="",prob=T,
     xlim=c(0,15),ylim=c(0,0.5),col="gray65",
     main="n = 40")
lines(zz,dchisq(zz,3),col="green")
hist(ref.dist4,breaks=br,xlab="",ylab="",prob=T,
     xlim=c(0,15),ylim=c(0,0.5),col="gray65",
     main="n = 80")
lines(zz,dchisq(zz,3),col="green")
hist(ref.dist5,breaks=br,xlab="",ylab="",prob=T,
     xlim=c(0,15),ylim=c(0,0.5),col="gray65",
     main="n = 160")
lines(zz,dchisq(zz,3),col="green")
dev.off()


