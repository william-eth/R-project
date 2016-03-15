##Lecture 4 

##Simulating a game of chance
win=sample(c(-1,1),size=50,replace=T)
cum.win=cumsum(win)
cum.win
sum(win)

##
win=sample(c(-1,1),size=50,replace=T)
sum(win)    #last total sum result
cumsum(win) #every time sum result 

#
snoopy=function(n=50){win=sample(c(-1,1),size=n,replace=T)
sum(win)}
#
F=replicate(10000,snoopy())
#replicte like for loop
#replicate(# of replications, function)

table(F)
plot(table(F))
length(F) # check how many times replications
sum(F==0)/10000 

dbinom(25,size=50,prob=0.5)

##
snoopy=function(n=50){
  win=sample(c(-1,1),size=n,replace=T)
  cum.win=cumsum(win)
  c(F=sum(win),L=sum(cum.win>0),M=max(cum.win))}
  #F: final fortune at the end of game 
  #L: # of times Snoopy "leads" during the game
  #M: "MAX" cumlative earning during the game

S=replicate(1000,snoopy())

##
S
mean(S["L",])
c(quantile(S["L",],0.05),quantile(S["L",],0.95))
mean(S["M",])
sum(S["M",]>10)/1000

###
##Service time simulation
ServiceT= replicate(1000,sum(rexp(10,3)))
mean(ServiceT)
quantile(ServiceT,0.975) #97.5% CI for total ServiceTime of 10 costumers   <= ??? mins

########


x=rnorm(10000)
x=x[(0<=x)&(x<=3)]
hist(x,probability=T)
lines(density(x,from=0,to=3),col='red',lty=2,lwd=2)
curve(dnorm(x),add=TRUE,col='green')