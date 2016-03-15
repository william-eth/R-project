#Q1(e)
x=sample(1:6,1000,replace=TRUE)
y=sample(1:6,1000,replace=TRUE)
#Intersection
count=0
for(i in 1:1000){
  if(x[i]+y[i]>=5 && x[i]==4) 
    count=count+1
}
count/1000

#Union
count=0
for(i in 1:1000){
  if(x[i]>y[i] || x[i]==4) 
    count=count+1
}
count/1000

#Union,intersection
count=0
for(i in 1:1000){
  if(x[i]+y[i]>=5 && (x[i]>y[i]||x[i]==4)) 
    count=count+1
}
count/1000
#0.458

#####################################################################

###Q2
##(c)
choose(4,1)*choose(6,1)*choose(3,1) #=72

##(d)
choose(5,1)*choose(2,1)*choose(3,1)*choose(2,1)*choose(8,1)/choose(12,5) #=0.606
#error answer

choose(5,1)*choose(2,1)*choose(3,1)*choose(2,1)*choose(4,1)/choose(12,5) #=0.303 <-- correct
member = c(rep("chi",5), rep(("AS"),3),rep("AA",2), rep("Cau",2))
sample(member,5)
selectsim=replicate(100000,sample(member,5))

res=0
for(i in 1:ncol(selectsim)){
  res=(length(unique(selectsim[,i]))==4)+res
}

res
res/100000

##(e)
(choose(7,2)+choose(8,2)+choose(9,2))/choose(24,2) #=0.308
choose(7,2)/choose(24,2) #=0.076

#####################################################################

###Q3
##(a)
choose(4,1)*choose(16,1)/choose(52,2) #=0.048


##(b)
blackjack=function(times){
  win = 0
  for(i in 1:times){
    gameresult = sample(52,2,replace = FALSE)
    ace = any(gameresult<=4)
    flowercard = any(gameresult>=37&gameresult<=52)
    if(ace&flowercard){
      win = win + 1
    }
  }
  return (win)
}

simblackjack = blackjack(5000)/5000
print(simblackjack) 
##0.0498

#####################################################################

###Q5
##(a)
1-prod(c(10000:9901)/10000) #=0.391
##(b)
same=function(times)
{
  count=0
  for(i in 1:times)
  {
    x=sample(0:9999,100,replace=TRUE)
    if(length(unique(x))<100) count=count+1
  }
  return(count)
}
print(same(5000)/5000)
#0.3872

##(c)
least=function(times)
{
  count=0
  for(i in 100:500)
  {
    for(j in 1:times)
    {
      x=sample(0:9999,i,replace=TRUE)
      if(length(unique(x))<i)
        count=count+1
    }
    if((count/times) >0.5) {
      numbers=i
      break
    }
    else count=0
    
  }
  return(numbers)
}
print(least(5000))
#119
