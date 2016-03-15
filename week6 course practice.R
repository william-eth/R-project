#lecture 3 of middle 

##Draw three poker cards
(26/52)*(25/51)*(24/50)
(26/52)^3 #draw and back the card

##Simulation of drawing three poker cards  
#Black:1 Red:0
deck=c(rep(1,26),rep(0,26))
#
x=c() #empty deck to store simulation results
y=c()
#
S=50000 #simulation 50000 times
for(s in 1:S){
  x[s]=sum(sample(deck,3,replace=F)) #取出不放回
  y[s]=sum(sample(deck,3,replace=T)) #取出放回
}
sum(x==3)/S
sum(y==3)/S 


##Intel example
error=1/9000000000
noerror=1-error
noerror^1000000000      
noerror^2000000000   

##Attacks to computing center example
attack.data=c(rep(0,9),rep(1,14),rep(2,13),rep(3,9),rep(4,4),rep(5,2),rep(6,1))

table(attack.data)
attacks=c(9,14,13,9,4,2,1)       
probability=attacks/sum(attacks) #P(X=x)  = ... 
probdens=round(probability,2)

Attacks=0:6
plot(Attacks,probdens,xlab="attacks/week",ylab="f(x)",type='h')

cumprob=cumsum(probdens)
plot(Attacks,cumprob,xlab="attacks/week",ylab="F(x)",type='S')


x=0:6
probability=c(9/52, 14/52, 13/52, 9/52, 4/52, 2/52, 1/52)
mean=sum(x*probability)
mean #nu
sum((x-mean)^2*probability) #var

##Simulating the impact of sample size on E(X) and Var(X) estimates
attacks_100=sample(c(0:6), 100, replace=T, prob=probability)
attacks_1000=sample(c(0:6), 1000, replace=T, prob=probability)
attacks_100000=sample(c(0:6), 100000, replace=T, prob=probability)

mean(attacks_100)
mean(attacks_1000)
mean(attacks_100000)
var(attacks_100)
var(attacks_1000)
var(attacks_100000)

