#########R_HW1_YOLO

#Q1

#(a)
downtime=c(0,1,2,12,12,14,18,21,21,23,24,25,28,29,30,30,30,33,36,44,45,47,51)

#(b)
mean(downtime)
median(downtime)
min(downtime)
max(downtime)
range(downtime)

#(c)
sd(downtime)
quantile(downtime, 0.05)
quantile(downtime, 0.95)

#(d)
table(downtime)
#mode is 30

#(e)
#mode is 30
subset(downtime,downtime==30)

#####################################################################

#Q2

#(a)
a=c(rep(0,5),rep(1,5),rep(2,5),rep(3,5),rep(4,5))

#(b)
b=rep(seq(1,5),5)

#####################################################################

#Q3

#(a)
x=c(61,175,111,124)
y=c(13,21,24,23)
z=c(4,18,14,18)
A=matrix(c(x,y,z),nrow=4,ncol=3)
colnames(A)=c("x","y","z")
A

#(b)
A[1,3]

#####################################################################

#Q4

compare=function(N){
	
#calculate sigma() equotation
	sigma=0
	for(i in 1:N){
		sigma=sigma+1/i
	}

#compare with log() equotation
	k=sigma-(log(N)+0.6)
	print(sigma)
	print("the sigma equotation - the log equotation = ")	
	return(k)
}

compare(500)
compare(2000)
compare(8000)

#####################################################################

#Q5

x = 0
f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
tolerance=0.000001
i=0
while(abs(f)>tolerance){
	f.prime=7*x^6+60000*x^5+5.3*x^4+42400*x^3+0.1815*x^2+1210*x+0.0005
	x=x-f/f.prime
	f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
	i=i+1
}
x
i

#####################################################################

#Q6
results=read.table("E:/results.txt",header=T)
attach(results)
dev.new(width=13, height=10)
par(mar=c(4, 4, 4, 4))
par(mfrow=c(2, 2))
par(las=1)
boxplot(arch1~gender, main="Architecture Semester 1")
boxplot(arch2~gender, main="Architecture Semester 2")
boxplot(prog1~gender, main="Programming Semester 1")
boxplot(prog2~gender, main="Programming Semester 2")

#####################################################################

#Q7

#(a)
factorial(4)
factorial(50)
factorial(5000)

#(b)
choose(4,2)
choose(50,20)
choose(5000,2000)

#(c)
log_factorial = function(x){
  total=c()
  for(n in 1:x){
    total[n]=log(n)  
  }
  return(sum(total))
}
#factorial(5000)
log_factorial(5000) 
#choose(5000,2000)
log_factorial(5000)-(log_factorial(3000)+log_factorial(2000)) 

#####################################################################

#Q8

#(a)(b)(c)
fib=function(x){
	a=1
	b=1
	k=a+b
	n=3
	while(k<x){
		a=b
		b=k
		k=k+a
		n=n+1
	}
	
	a=1
	b=1
	c=a+b
	all=c(a,b)
	for(i in 4:n){
		a=b
		b=c
		c=c+a
		all=c(all,c)	
	}
	
	cat("n=",n)
 	cat("\n")
	cat("k=",k) 
	cat("\n")
	return(all)
}
fib(100)

#####################################################################

#Q9

prime.number=function(n){
	x=c()
	for(i in 2:n){
		for(j in 2:i){
			if(i%%j==0)	break
		}
		if(i==j) 
			x=c(x,i)
	}
	return(x)
}
prime.number(100)

#####################################################################

#Q10

functionY = function(x){
  result=c()
  num=1
  for(n in 1:length(x)){
    if(x[n]<=0){
      result[num] = (-1)*(x[n]^3)
      num = num+1
    }
    else if(x[n]>0 && x[n]<=1){
      result[num] = (x[n]^2)
      num = num+1
    }
    else if(x[n]>1){
      result[num] = sqrt(x[n])
      num = num+1
    }
  }
  return(result)
}
x=seq(-2, 2, 0.1)

dev.new(width=10, height=10)
par(mar=c(5,5,5,5))
par(las=1)
plot(x,functionY(x),ann = F,type="l")
mtext("f(x)", side=2, line=3)
mtext("(x)", side=1, line=3)