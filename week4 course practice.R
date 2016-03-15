#at lecture 1 end
#use attach function to catch the title
attach(results)

#stem is one of the graph 分類個數
stem(prog1)

#hist長條圖
hist(prog1)

#pairs 用來比較兩軸出來的圖(對稱)
pairs(results[,2:5])
ncol(results)
nrow(results)

#end of lecture 1

#start lecture 2

#        xxxxx          = function(xxxx)  {
#   (finction name)         (input of the function)

#tasks to do for the function

#}

double.num = function(x){
  x*2
}

double.num(2)
double.num(c(1,2,3))

annuityAmt = function(r,i,n){
  r*((1+i)^n-1)/i
}
annuityAmt(400,0.05,10)

toCheck = 1
if(toCheck==1){
  print("hello")
}

check.bool = function(x){
  if(x==1)
    print("Hello")
  else
    print("goodbye")
  
}
check.bool(0)
check.bool(1)

#if else(a,b,c)
#a= condition to be accessed TRUE or FALSE
#b= output of TRUE
#c= output of FALSE

ifelse(1==1,"YES","NO")
toTest=c(1,1,0,1,0,1)
ifelse(toTest==1,toTest*3,"ZERO")

use.switch = function(x){
  switch(x,"a"="fist","z"="last","other")
}
use.switch("a")
use.switch("z")
use.switch(3)

#for loop 
#for(i in 1:10)
#i = input of the looping process
#1:10 = the range for the index to go

for(i in 1:10){
  print(i)
}

oddcount= function(x){
  k=0
  for(n in 1:length(x)){
    if(x[n]%%2==1){
      # %% = reminder operater
      k=k+1
    }
  }
  return(k)
}
oddcount(0:100)

fruit=c("apple","orange","watermelon")
fruit
fruitL=rep(NA,length(fruit))
names(fruitL)=fruit
for (i in 1:length(fruit)){
  fruitL[i]=nchar(fruit[i])  
}
fruitL

#In-class exercise: Hilbert Matrix
library(Matrix)
H3=Hilbert(3)

Hibert_own=function(k){
  result = matrix(NA,nrow = k, ncol = k)
  for(i in 1:k){
    for(j in 1:k)
      result[i,j]=1/(i+j-1)
  }
  return(result)
}

Hilbert_inv=function(k){
  k1 = Hibert_own(k)
  k1
  for(i in 1:k)
    for(j in 1:k)
      k1[i,j] = (-1)^(i+j)*(i+j-1)*choose(k+i-1,k-j)*choose(k+j-1,k-i)*choose(i+j-2,i-1)^2
  return(k1)
}

h =  Hibert_own(3)
hI=  Hilbert_inv(3)
h
hI
h%*%hI
solve(h)
h%*%solve(h) 


Hibert_teacher=function(k){
  Hi=c()
  for(i in 1:k){
    Hi = rbind(Hi,1/(i+(1:k)-1))
  }
  return(Hi)
}

Hibert_teacherinv=function(k){
  Hk_inv=matrix(NA,ncol=k,nrow=k)
  for(i in 1:k){
    for(j in 1:k){
      Hk_inv[i,j]=(-1)^(i+j)*(i+j-1)*choose(k+i-1,k-j)*choose(k+j-1,k-i)*choose(i+j-2,i-1)^2
    }
  }
  return (Hk_inv)
}
a=Hibert_teacher(3)
b=Hibert_teacherinv(3)
a
b
a%*%b

#Newton's method
#f(x) = ax+ bx^2 + cx^3 + dx^4 ..... 
#Goal is find x' such that f'(x) ~= 0 
#If f'(x)=df(x)/dx exists
#If not, update x0->x1, check f(x0) ~= 0?
#If not, update x1->x2, check f(x1) ~= 0?
#....
#xk => f(xk) ~= 0

#x1 = x0 - adjustment; adjustment=f(x0)/f'(x0)
#x2 = x1 - adjustment; adjustment=f(x1)/f'(x1)

#tolerance must be a very small number ~=0

x=0
f=x^3+2*x^2-7
tolerance=0.000001
while(abs(f) < tolerance){
  f.prime= 3*x^2+4*x 
  x= x- f/f.prime 
  f=x^3+2*x^2-7
} 
x
