#Q1
PTF.p0=function(a,b,g){ #return p0 value
  p0=0
  if(a==0){
    p0=(1-g)^b
  }
  else{
    p0=exp(b*((1-g)^a-1)/a)
  }
  return(p0)
}
PTF.p1=function(a,b,g){ #return p1 value
  p0=PTF.p0(a,b,g)
  p1=b*g*p0
  return(p1)
}
PTF.r1=function(a,g){  #return r1 value
  (1-a)*g
}
PTF=function(k,a,b,g){
  if(k==0){                #K=0, return p0
    return(PTF.p0(a,b,g))
  }
  if(k==1){
    return(PTF.p1(a,b,g)) #k=1, return p1
  }
  else{   
    p=PTF.p1(a,b,g) #call for p1 value, and PTF.p1 function will call PTF.p0 value
    p[1]=p
    r=PTF.r1(a,g)   #call for r1 value
    r[1]=r
    for(i in 2:k){  #because pk start at p2
      j=c(1:(i-1))    
      pn=(1/i)*((b*g*p[i-1])+sum(j*rev(r)*p)) #if i=2 , j=1 => j[]=1 , r[]=1 , p[]=1   
      p[i]=pn[1]                              #if i=3 , j=1,2 => j[]=2, r[]=2 , p[]=2
     
      rn=(i-2+a)/i*g*r[i-1]  #using rn change rn+1 formula, and start cacluate r[2] for i =3 p[3]
      r[i]=rn[1]
    }
    return(tail(p,n=1)) #the result is an array. store many times results
    #return(p)
  }
}
PTF(9,-3,2,0.5)
#answer 0.04235393

#Q2
#(b)

bernoulli.MLE = function(n){
  sim = sample(c(0,1), size = n, replace = TRUE , prob = c(0.5,0.5))
  mle = sum(sim)/n 
  return(mle-0.5)
}

bernoulli.MLE(50)   #answer is 0.08
bernoulli.MLE(5000) #answer is 0.003
bernoulli.MLE(500000) #answer is 0.000362

#(d)
exponential.MLE = function(n){
  sim = rexp(n,0.5) 
  mle = n/sum(sim)
  return(mle-0.5)
}
exponential.MLE(50)     #answer is 0.01947052
exponential.MLE(5000)   #answer is 0.01026765
exponential.MLE(500000) #answer is 0.0004299465

#Q3
#(a)
car.accident = c(rep(0,109),rep(1,65),rep(2,22),rep(3,3),rep(4,1))

#(b)
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

fitdistr(car.accident, densfun = "Poisson")$estimate # lambda = 0.61 
fitdistr(car.accident, densfun = "Poisson")$loglik   # loglik = -206.1067

#(c)
car.poisson=function(lambda){
  cat("Possion(Lambda =", lambda,")\n" )
  frequence=c()
  for(i in 0:4){
    x = dpois(i,lambda = lambda) 
    frequence[i+1] = 200*x
  }
  y = 1 - ppois(4,lambda = lambda) # minus cdf ~ 4
  frequence[6] = 200*y
  for(j in 1:6){
    if(j==6){
      cat("200*P(X> 4 |lambda)=",frequence[j],"\n")
    }
    else{
      cat("200*P(X=",j-1,"|lambda)=",frequence[j],"\n")
    }
  }
}
car.poisson(0.61)

#200*P(X= 0 |lambda)= 108.6702 
#200*P(X= 1 |lambda)= 66.28881 
#200*P(X= 2 |lambda)= 20.21809 
#200*P(X= 3 |lambda)= 4.111011 
#200*P(X= 4 |lambda)= 0.6269291 
#200*P(X> 4 |lambda)= 0.08499439 

#(d)
car.binomial = function(n=200, lambda = 0.61){
  p = lambda/200
  frequence= c()
  for(i in 0:4){
    x = dbinom(i,size = n, prob = p)
    frequence[i+1] = 200*x
  }
  y = 1- pbinom(4,size = n,prob = p)
  frequence[6] = 200*y 
  for(j in 1:6){
    if(j==6){
      cat("200*P(X> 4 |n,p)=",frequence[j],"\n")
    }
    else{
      cat("200*P(X=",j-1,"|n,p)=",frequence[j],"\n")
    }
  }
}
car.binomial()
#200*P(X= 0 |n,p)= 108.5689 
#200*P(X= 1 |n,p)= 66.42965 
#200*P(X= 2 |n,p)= 20.22141 
#200*P(X= 3 |n,p)= 4.083024 
#200*P(X= 4 |n,p)= 0.6151976 
#200*P(X> 4 |n,p)= 0.08178472 

#Q4
#(a)
x=c()
y=c()
z=c()
for(i in 0:8){
  x[i+1]=dbinom(i,size = 20,prob = 0.2)
  y[i+1]=dpois(i,lambda = 4)
  z[i+1]=dnorm(i,mean = 4, sd = 2)  #var = 4
  cat("X=",i,"Binomial=",x[i+1],";Poisson =",y[i+1],";Normal =",z[i+1],"\n")
}

#(b)

a=c(0:8)
plot(c(0,8),c(0,0.25),xlab = "P(X=x)", ylab = "probability" , type = "n" ) #n for no typing
lines(a,x,col="red",lwd=1)
lines(a,y,col="green",lwd = 3)
lines(a,z,col="blue", lwd= 5)
legend(5,0.23,c("Binomial","Poisson","Normal"),lty=c(1,1,1),lwd=c(1,3,5),bty="n",cex=1.1,col=c('red','green','blue'))

#5
#(a)
runi.congru = function(N,A,B,m,seed){
  u=c()
  for(i in 1:N){ 
    #set.seed(seed) 
    #X(n+1) = (AX(n)+B) mod m, x1=seed
    x = (A*seed+B)%%m # %% is mod
    u[i] = x/m
    seed= x #update seed for next loop
  }
  return(u)
}

u=c()
u=runi.congru(5,1217,0,32767,1)
# 0.03714103, 0.20062868, 0.16510514, 0.93295083, 0.40116581

#(b)
rbinom.invtran = function(N,n,p,uni){
  k=c()
  for(i in 1:N){
    x=0
    while(pbinom( x , size = n, prob = p) < uni[i]){
      x = x+1
      #cat(i,"次,",x,"\n")
    }
    k[i] = x
  }
  return(k)
}

rbinom.invtran(length(u),3,0.5,u)  
# answer is  0 1 1 3 1

#(c)
U=c()
U=runi.congru(50,1217,0,32767,2)
U
#7.428205e-02 4.012574e-01 3.302103e-01 8.659017e-01 8.023316e-01 4.375744e-01 5.280313e-01 6.140324e-01 2.774438e-01
#6.490677e-01 9.153417e-01 9.707938e-01 4.560381e-01 9.983520e-01 9.943846e-01 1.660512e-01 8.432264e-02 6.206549e-01
#3.370464e-01 1.854915e-01 7.431562e-01 4.211249e-01 5.090182e-01 4.751732e-01 2.857753e-01 7.885678e-01 6.869716e-01
#4.449599e-02 1.516160e-01 5.166173e-01 7.232887e-01 2.423170e-01 8.997467e-01 9.917295e-01 9.347819e-01 6.296274e-01
#2.565081e-01 1.703238e-01 2.840663e-01 7.086703e-01 4.517655e-01 7.986084e-01 9.063692e-01 5.133213e-02 4.712058e-01
#4.574419e-01 7.068392e-01 2.233039e-01 7.608875e-01 6.103702e-05

#(d)
dztpois = function(lambda,x){       #cdf function
  (lambda^x*exp(-lambda))/(factorial(x)*(1-exp(-lambda)))
}

pztpois = function(lambda,x){      #pdf function, use recursive calculate
  if(x>1){
    return (dztpois(lambda,x)+pztpois(lambda,x-1))
  }
  else{  # don't know if x=0 ...
    return(dztpois(lambda,1))
    #return(0) # will output error
  }
}


rztpois.invtran = function(N, lambda, uni){
  k=c()
  for(i in 1:N){
    x=1 #because x > 0 
    #while( ((lambda^x*exp(-lambda))/factorial(x)*(1-exp(-lambda))) < uni[i] ){
    while(pztpois(lambda,x)<uni[i]){ 
      x=x+1
    }
    k[i]=x
  }
  return(k)
}

k = rztpois.invtran(length(U),4,U)
#1  3  3  6  6  4  4  4  3  5  7  8  4 11 10  2  2  4  3  2  5  3  4  4  3  6  5  1  2  4  5  3  7 10  7
#5  3  2  3  5  4  6  7  1  4  4  5  2  5  1

table(k)
# 1  2  3  4  5  6  7  8 10 11 
# 4  6  9 11  8  4  4  1  2  1 


#(e)
customer = rztpois.invtran(50,4,U) #Q5(4) answer, I guess...
table.two = length(which(customer<=2))                    #10 group
table.four = length(which(customer<=4&customer>2))        # 20 group
table.six = length(which(customer<=6&customer>4))         # 12 group
privateroom = length(which(customer>6))                   #8 group

cat("two-people tables customers:",table.two,"\n")
cat("four-people tables customers:",table.four,"\n")
cat("six-people tables customers:",table.six,"\n")
cat("private room tables customers:",privateroom,"\n")   

