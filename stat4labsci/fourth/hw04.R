### 1

x_matrix(c(142,199858,56,199944),ncol=2,byrow=T)
dimnames(x)_list(c("Placebo","Vaccine"),c("Polio","No Polio"))

y_matrix(ncol=2,nrow=2)

for (i in 1:2){
  for (j in 1:2){
    y[i,j]_x1[i]*x2[j]/xx}}

tst_(x-y)^2/y
round(tst,3)

X2_sum(tst)
round(X2,2)

p.value_1-pchisq(X2,1)

chisq.test(x,correct=F)


### 2

x_matrix(scan("treatments.txt"),ncol=6,byrow=T)
dimnames(x)_list(c("no    improvement","some  improvement",
                   "great improvement"),LETTERS[1:6])

chisq.test(x)

chisq.test(x[,c(1,3)])
chisq.test(x[,c(2,4,5,6)])

# from lecture 4 computing notes:
mct_function(x){
n_dim(x)[1];k_dim(x)[2]
x1_apply(x,1,sum);x2_apply(x,2,sum);xx_sum(x)
y_x
for (i in 1:n){
for (j in 1:k){
y[i,j]_x1[i]*x2[j]/xx}}
tst_(x-y)^2/y
X2_sum(tst)
p.value_1-pchisq(X2,(n-1)*(k-1))
return(y,tst,X2,p.value)}

test_mct(x)
round(x-test$y,1)

apply(x[,c(1,3)],1,sum)/2
apply(x[,c(2,4,5,6)],1,sum)/4

pvals_matrix(nrow=6,ncol=6)
for (j in 1:5){
  for (k in (j+1):6){
    check_chisq.test(x[,c(j,k)])
    pvals[j,k]_check$p.value}}
round(pvals,3)

