#using-half-to-even ===> nearest even number
round(1.5)
round(2.5)

#call txt file
#file name = read.table(“files address” , header = T)
#attach(data set)

#call matrix of the row
results$arch1[5]

#use attach to using data set
attach(results)
arch1

#-------------------------

x=10:1
y=-4:5
q=c("H","F","B","C","R","L","B","T","C","S")
#let x,y&q combine to matrix

theDF=data.frame(First=x,Second=y,Sport=q)
theDF

#some function can use
nrow(theDF)
ncol(theDF)
dim(theDF)
colnames(theDF)
rownames(theDF)
theDF$Sport

head(theDF,5)
tail(theDF,5)

theDF[3,2:3]
theDF[,3]
theDF[c(3,5),1]

#----using list function --------
list(1,2,3)
list(c(1,2,3))

Alist= list(1,2,3)
Alist
Alist[[1]]

#list(data.frame mix of matrix $ charatters, vector , list) you can store this type

list5=list(theDF, 1:10, list(c(1, 2, 3), 3:7))
list5

list5[[1]]
list5[[1]][ , 2]
#the original attribute of the object will be presented 
list5[[1]][ , 2, drop=FALSE]

#array(elements/objects to be stored, dimensions)
#dim=(row,column,third dimension)

#for example if dim 
#dim(2,3,2)
#2*3 matrix[]
#2*3 matrix[]

theArray=array(1:12, dim=c(2, 3, 2))
theArray
theArray[1, , ]
theArray[1, ,1]
theArray[ , , 1]

#matrix(objects to be stored , ncol=? or nrow=?)
A=matrix(1:10, nrow=5)
B=matrix(21:30, nrow=5)

A
B
A+B
A*B

#A[5:2] * B[5:2] = C[5:2]
# but it's mathematically incorrect 
#matrix multiplication need transpose B matrix
#so we using A%*% t(B)
#that's mathematically correct 
A%*% t(B) 


#cbind => combine by columns
#rbind => combine by rows
#using this function should same rows or same columns
cbind(A,B)
rbind(A,B)

#apply(matrix, 1:row 2:column, the function to be applied to each row[1] or column[2])
row.means=apply(B, 1, mean)
col.means=apply(A, 2, mean)
row.means
col.means

#A[]*A^-1[] = I 
#[1,0,0]
#[0,1,0]
#[0,0,1]

D=matrix(sample(1:100,25),ncol=5)
D
solve(D)
D%*%solve(D)  

#sample(numbers to be randomly , how many random numbers to be generate, replace = T or F)
#replace = T   means sample with replacement
#replace = F   means sample without replacement 
#sample may let you look each objects how many times you take from list pool
sample(1:100,100,T)
table(sample(1:100,100,T))
sample(1:100,100,replace = F)

#you can't do this 
#sample(1:10,100,F)

x=sample(x=1:100, size=100, replace=TRUE)
x
mean(x)
y=x
y[sample(x=1:100, size=20, replace=FALSE)]=NA
y
mean(y)
mean(y, na.rm=TRUE)

#weights
# X  Weights 
# x1  w1
# x2  w2
# .   .
# x100 w100

weights=sample(1:100, 100, replace=FALSE)/sum(1:100)
weighted.mean(x, w=weights)

var(x)
#var also can use under function
sum((x-mean(x))^2)/(length(x)-1) 

#sd = var^-2
sd(x)
median(x)

max(x)
min(x)
max(x)-min(x)

#see all the detail of the matrix
summary(x)

#Tukey’s five number summary
fivenum(x)

#show the pie chart
groupsizes=c(18, 30, 32, 10, 10)
labels=c("A", "B", "C", "D", "E")
pie(groupsizes, labels, col=c("grey","yellow","blue","green","red"))

#Scatter plot
x=sample(1:100, 20, replace=TRUE)
y=sample(1:100, 20, replace=TRUE)
plot(x, y)
#lwd = line width
#lty = line type
lines(sort(x), sort(y), type="l" ,col="red" , lwd=2.5 , lty=2)

#abline(h horizontal line or v vertical line, ....) 
abline(h=40,col="green",lwd=2,lty=3)
abline(v=80,col="pink",lwd=2,lty=4)

#-------------
attach(results)
names(results)

#show the boxplot graph  
#xlab : graph name
boxplot(prog1, xlab="Programming Semester 1")

boxplot(arch1~gender, xlab="gender", ylab="Marks(%)", main="Architecture Semester 1")

stem(prog1)
pairs(results[,2:5])

bins=c(0, 20,40, 60, 80, 100)
#show hist graph
hist(arch1, breaks=bins, xlab="Marks(%)", ylab="Number of Students", main="Architecture Semester 1")

dev.new(4,3)
#mar: margin how you choose  down left up right 
#par(mar=c(3, 4, 1, 1))
#to show 2*2 block  (rows*cols)
par(mfrow=c(2, 2))

#ylim = y limit
hist(arch1, xlab="Architecture", main="Semester 1", ylim=c(0, 35))
hist(arch2, xlab="Architecture", main="Semester 2", ylim=c(0, 35))
hist(prog1, xlab="Programming", main="", ylim=c(0, 35))
hist(prog2, xlab="Programming", main="", ylim=c(0, 35))