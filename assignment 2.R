#I want to create three qualitative and three quantitative data
#The data below is data of all those working in Dassyham group of company
set.seed(1)
Age=sample(21:60, 120, replace=TRUE) #quantitative data
print(Age)
Height=round(runif(120, 4, 6), 2) #quantitative data
print(Height)
No_of_cars=sample(0:3, 120, replace=TRUE) #quantitative data
print(No_of_cars)
mf=c("M","F")
Gender=sample(mf, 120, replace =TRUE) #qualitative data
print(Gender)
u=c("Cleaners","IT students","Secretary","Typist","Accountant","Data analyst","Gate keeper","Drivers","Factory worker","HR team","Finance team","Tech team","Engineers","Security")
Units=sample(u, 120, replace=TRUE) #qualitative data
print(Units)
q=c("SSCE","OND","HND","Bsc/Beng","Msc")
Qualification=sample(q, 120, replace=TRUE) #qualitative data
print(Qualification)
df=data.frame(Age,Height,No_of_cars,Gender,Units,Qualification)
print(df)

#i want to export the file as csv to python 
write.csv(df,"C:\\Users\\MOJISOLA\\Desktop\\Data Analysis\\170806005 R projects\\assignment 2.csv",row.names=FALSE)

head(df)
str(df)
tail(df)
names(df)



frequnit=table(df$Units)
print("modified frequency table")
print(frequnit)

pie(frequnit)

freqage=table(df$Age)
print("modified frequency table")
print(freqage)

pie(freqage)

freqqual=table(df$Qualification)
print("modified frequency table")
print(freqqual)

pie(freqqual)

freqgender=table(df$Gender)
print("modified frequency table")
print(freqgender)

pie(freqgender)

freqheight=table(df$Height)
print("modified frequency table")
print(freqheight)

pie(freqheight)

freqcars=table(df$No_of_cars)
print("modified frequency table")
print(freqcars)

summary(df$Age)
summary(df$Height)
summary(df$No_of_cars)





IQR(df$Age)
IQR(df$Height)
IQR(df$No_of_cars)

chisq.test(df$Age)
chisq.test(df$Height)
chisq.test(df$No_of_cars)

sd(df$Age)
sd(df$Height)
sd(df$No_of_cars)

var(df$Age)
var(df$Height)
var(df$No_of_cars)



library(ggplot2)


as=df$Units
print(as)
gt=data.frame(col1=1:120,as)
print(gt)
ggplot(gt,aes(x=as))+
  geom_bar()

as=df$Units
print(as)
gt=data.frame(col1=1:120,as)
print(gt)
ggplot(gt,aes(x=as,y=col1))+
  geom_point(size=5)

as=df$Units
print(as)
gt=data.frame(col1=1:120,as)
print(gt)
ggplot(gt,aes(x=as,y=col1))+
  geom_line(size=5 ,colour="purple")

as=df$Units
print(as)
gt=data.frame(col1=1:120,as)
print(gt)
ggplot(gt,aes(x=as,y=col1))+
  geom_boxplot()



plot(density(df$Age)) #to know distribution shape of data


frequ=table(df$Units,df$Qualification)
print("modified frequency table")
print(frequ)

cumsum=cumsum(frequ)
print('cummulative frequency table')
print(cumsum)

prob=prop.table(frequ)
print("relative frequency table")
print(prob)


freque=table(df$Age,df$Height)
print("modified frequency table")
print(freque)

pie(freque)
cumsum=cumsum(freque)
print('cummulative frequency table')
print(cumsum)

prob=prop.table(freque)
print("relative frequency table")
print(prob)


hist(df$Height)
hist(df$Age)
hist(df$No_of_cars)




library(ggplot2)


x=df$Age
y=df$Height
plot(x,y,main="AGE & HEIGHTS", xlab="AGE",ylab="HEIGHT",pch=19,frame=FALSE)

J=data.frame(df$Age,df$Height)
print(J)
plot(J,main=" FABAB SCATTER PLOT", xlab="AGE",ylab = "HEIGHT")
abline(lm(y~x,data=J),col="red")


ggplot(J,aes(x=df$Age))+
  geom_bar()


ggplot(J,aes(x=df$Age,y=df$Height))+
  geom_point(size=5)

ggplot(J,aes(x=df$Age,y=df$Height))+
  geom_line(size=2 ,colour="purple")


ggplot(J,aes(x=df$Age,y=df$Height))+
  geom_boxplot()

das=head(df$Age)
ham=head(df$Height)
mosaicplot(das,ham)

boxplot(df$Age,df$Height)


freque=table(df$Age,df$Units)
print("modified frequency table")
print(freque)

cumsum=cumsum(freque)
print('cummulative frequency table')
print(cumsum)

prob=prop.table(freque)
print("relative frequency table")
print(prob)

pie(df$Age,labels=Units,main="pie chart",shadow=TRUE,col=rainbow(length(Units)))


das=head(df$Age)
ham=head(df$Units)
pie(das,labels=ham,main="pie chart",shadow=TRUE,col=rainbow(length(Units)))

e=head(df$Age)
p=head(df$Units)
pie(e,p,shadow=TRUE,col=rainbow(length(p)))
legend("topright",p,cex=0.5,fill=rainbow(length(b)))


scatterplot3d::scatterplot3d(Age,Height,No_of_cars)

#HALLELUYAH

