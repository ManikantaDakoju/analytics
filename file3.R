#day2:21Feb20
library(dplyr)
mtcars
?mtcars
table(mtcars$cyl)
summary(mtcars$cyl)
mtcars %>% group_by(cyl) %>% tally()
mtcars %>% group_by(cyl) %>% summarise(COUNT = n())
?xtabs

xtabs(~ cyl, data=mtcars)
ftable(mtcars$cyl)
?ftable

mtcars %>% group_by(cyl, gear) %>% tally()
mtcars %>% group_by(cyl, gear) %>% summarise(COUNT=n())
table(mtcars$cyl, mtcars$gear)

table(mtcars$cyl, mtcars$gear, mtcars$am, dnn=c('cyl' , 'Gear' , 'AutoManual'))
df = mtcars
head(df)
tail(df)
df$am= ifelse(df$am==0, 'Auto' , 'Manual')
df
?ifelse
?mutate
mtcars %>% mutate(TxType = ifelse(am==0, 'Auto', 'Manual')) %>% group_by(TxType) %>% summarise(Count = n())
mtcars
df = mtcars
df$mpg
df[,'mpg']
df
head(df)
df = df %>% mutate(TxType = ifelse(am==0, 'Auto' , 'Manual'))
head(df)
#increase mileage by 10%
df$mpg * 1.1
#add mpg+wt in to new column MPGT
df$mpg + df$wt
df$MPGWT = df$mpg * 1.1 + df$wt
head(df)
df
#top 2 carsfrom mpg from each gear type : use group_by & top_n
df %>% group_by(gear) %>% top_n(n=2, wt=mpg) %>% select(gear , mpg)
df %>% group_by(gear) %>% arrange(-mpg) %>% select(gear, mpg)
df %>% group_by(gear) %>% top_n(n=2, wt=-mpg) %>% select(gear , mpg)

#list out details of any 2 cars picked randomly; then do 25% of the cars
df %>% sample_n(2)
df %>% sample_frac(0.25)

#sort on mpg
df %>% sample_n(0.25) %>% arrange(mpg)
#ascending gear , descending mpg
df %>% sample_frac(0.25) %>% arrange(gear, desc(mpg))

# mean of mileage, Hp, 
df %>% group_by(gear) %>% summarise_at(c('mpg' , 'wt' , 'hp' , 'disp'),c(mean))
df %>% group_by(gear) %>% summarise_at(c('mpg' , 'wt' , 'hp' , 'disp'),c(mean,median))
df %>% select(gear, mpg, hp, wt, disp) %>% group_by(gear) %>% summarise_all(mean)

#find min an max of wt for each gear type
df %>% select(mpg, wt, gear) %>% group_by(gear) %>% summarise_each(c(min, max))


#graphs----
hist(df$mpg)
barplot(table(df$gear), col=1:5)
pie(table(df$gear))
plot(df$wt, df$mpg)

library(ggplot2)
library(reshape2)



(rollno = paste('IIM' ,1:10, sep='_'))
(name = paste('SName' ,1:10, sep='_'))
(gender = sample(c('M','F'), size=10, replace=T))
(marketing = trunc(rnorm(10, mean=60, sd=10)))
(program = sample(c('BBA','MBA'), size=10, replace = T))
(finance = trunc(rnorm(10, mean=55, sd=12)))
(Operations = trunc(rnorm(10, mean=60, sd=11)))
students <- data.frame(rollno, name, gender, marketing, program, finance, Operations, stringsAsFactors = F)
students
head(students)

(meltsum1 <- melt(students, id.vars=c('rollno','name','gender','program'), variable.name = 'subject', value.name = 'marks'))
meltsum1
head(meltsum1)
sum2 <- meltsum1 %>% group_by(program, gender, subject) %>% summarise(MeanMarks = mean(marks))
head(sum2)
ggplot(data=sum2, aes)


students
head(students, n=2)
students %>% group_by(gender) %>% summarise(count = n())
#Stacked bar
ggplot(students %>% group_by(program, gender) %>% summarise(count = n()), aes(x=gender, y=count, fill=program)) + geom_bar(stat= 'identity') + geom_text(aes(label=count)) + labs(title='Gender Wise - program Count')
#side by side
ggplot(students %>% group_by(program, gender) %>% summarise(count = n()), aes(x=gender, y=count, fill=program)) + geom_bar(stat= 'identity',position = position_dodge2(.7)) + geom_text(aes(label=count), position = position_dodge2(.7)) + labs(title='Gender Wise - program Count')
#subject - program - gender- Mean marks
names(students)
names(meltsum1)
head(meltsum1)
ggplot(meltsum1 %>% group_by(program, gender, subject) %>% summarise(meanMarks = round(mean(marks))), aes(x=gender, y=meanMarks, fill=program)) + geom_bar(stat= 'identity',position = position_dodge2(.7)) + geom_text(aes(label=meanMarks), position = position_dodge2(.7)) + labs(title='Gender Wise - program Count') + facet_grid(~ subject)

ggplot(students, aes(x=gender, y=..count..)) + geom_bar(stat='count')

ggplot(mtcars, aes(x=wt, y=mpg, size=hp, color=factor(gear), shape=factor(am))) + geom_point()

#
ggplot(students, aes(x=gender, y=..count..)) + geom_bar(stat='count')


