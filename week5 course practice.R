library(CMPControl)

#Z(lamda,nu) 
#val=Z(lamda,nu)|j=N (N<max)
#val.last=Z(lamda,nu)|j=N-1
#stop if |val-val.last| < tolerence

CMPZ=function(lam,nu,max=1000,tol=1e-10){
  j=0
  val.last=-Inf #beacuse val.last j=N-1 , but if N=0, we set Val.last=-Inf
  val=lam^j*factorial(j)^-nu
  while(abs(val.last-val)>tol & j<max){
    val.last=val
    j=j+1
    val=val.last+lam^j*factorial(j)^-nu
  }
  return(val)
}

computez.lambdaest(5,1,5000)
CMPZ(5,1,max=5000)
system.time(computez.lambdaest(5,1,5000))
system.time(CMPZ(5,1,max=5000))

#======lecture 3 ===========================
#E: event
#E1:spade (13 card) 
#E2:Ace  (4 card)
#P(E1 union E2) = 16/52
#P(E1 intersction E2) = 1/52


#sample space (S) = the space of all possible events
#P(E) >= 0 , non-negativity

#expeeriment : data gerenating procees


#Toss two coins and look for at least one head.
#Exp:Toss two coins
#S=(H,H)(T,H)(H,T)(T,T)
#E=(H,T)(T,H)(H,H)
#P(E)=n(E)/n(S) = 3/4

#Inspect IC chips from a manufacturing process repeatedly
#Exp: check IC chips until the 1st Defective is foun
#S: d, gd, ggd, gggd,......   <-- infinite

#E: d, gd, ggd  
#n(E) = Inf
#n(S) = 3
#P(E)= n(E)/n(s) 

#Experimrnt : USA V.S. Taiwan
#S:{USA win, Taiwan win}
#E:{Taiwan win} 
#P(E)=n(E)/n(S) = 1/2 = 0.5

#Game 1,2,3,....,n (n repetitions)
#Nn(E) : 0,1,2,3,....,n?
#P(E) = Nn(E)/n  = ?! 

# group of four IC chips consists of two good chips and two defective chips. 
#The experiment consists of selecting three chips randomly from this group. 
#Write down S and E. What is the probability that two are defective?

#S:{(g1,g2,d1),(g1,g2,d2),(d1,d2,g1),(d2,g1,g2)}
#E:{(g1,d1,d2),(g2,d1,d2)}
#P(E) = n(E)/n(S) = 2/4



#p(E) = lim(n=Inf) Nn(E)/n

##Simulating coin-tossing experiments
x=sample(c("H","T"),10,replace=TRUE)
table(x)
table(x)/10
#
x=sample(c("H","T"),100,replace=TRUE)
table(x)/100
#
x=sample(c("H","T"),10000,replace=TRUE)
table(x)/10000 

#difference approximation error 
#ideal P(E) and using simulation data 

##Snoopy & Charlie Brown
#sample((H,T), simulation 10 times, prob(H,T) , replace = T)
x=sample(c(1,-1),50,prob=c(0.6,0.4),replace=TRUE)
table(x)/50 

pocket=c()
pocket[1]=x[1]
for(i in 2:50){
  pocket[i]=pocket[i-1]+x[i]
  #pocket[i] = current money after ith gamble
  #pocket[i-1]= previous money after (i-1)th gamble
  #outcome of ith gamble
}

num=1:50
plot(num,pocket,type="o",xlab="",ylab="$")
plot(num,pocket,type="l",xlab="",ylab="$") 


#page 4 bottom
# n
#P   = n*(n-1)*......*n(n-(k-1))
# k
#n elements : generate ? samples of size k
#P : prod(n:n-k+1)

#(a)
#E: all processors are occupied
#:P(E)= n(E)/n(S) = 101/10^10 = prod(10:1)/10^10 
prod(10:1)/10^10
#n(S): 10^10
#in jobs, n processors total ways of assignments is n^m

#processors: 1,2,3,...,10
#jobs : 1(10 possible choices) -> 2(9 possible choices) -> ... -> 10(1 choice)

#Prob(E: At least one processors receive 2+jobs)
# = 1 - Prob(No processer receives 2 + jobs) 
#= 1 - 10!/10^10


#Prob(All jobs go to different processors)
#=p(n,10) / n^10 >= 0.9
n=100
prod(n:10)/n^10
