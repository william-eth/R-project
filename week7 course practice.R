#lecture 3 
#page 12
#Bernoulli distribution

#not using function to create
p=0.2
sample(c(0,1),100,prob = c(1-p,p),replace = TRUE)

qbinom(0.95, size=20, prob=0.2)

#Simulating IC defect inspection processes
#rbinom(how many times you try(simulation times), how many chips you have, defect chips prob)
defect = rbinom(50,20,0.2)

defect

sum(defect<=7)
sum(defect<=7)/50

#sum(x-0 ~ 7) (x=20)*p*(1-p)   p=0.2
#P(X<=7) = 0.9678573
pbinom(7,size = 20 , 0.2) #exact answer

#Exponential distribution 
#(a)
pexp(0.5, 4)
#(b)
1- pexp(20,4)
#(c)
qexp(0.95,4)

curve(dnorm(x, 1.5, .5), from=0, to=3, ylab="f(x)")
