student<-read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/student.txt', header=TRUE)

names(data)
nrow(data)
ncol(data)
dim(data)

# 2

heights<-student$height
mean(heights)
var(heights)
sd(heights)
mean(heights)
tapply(student$height, student$gender, mean)
mean(student$height[student$gender == 'male'])
mean(student$height[student$gender == 'female'])

student.new <- data.frame(student$height, student$shoesize)
cov(student.new)
cor(student.new)
smean <- colMeans(student.new)
smean
sweep(student.new, 2, smean, '-')

# 3

maleStudents<-student[student$gender == 'male',]
femaleStudents<-student[student$gender == 'female',]
nrow(maleStudents)
nrow(femaleStudents)
nrow(maleStudents[maleStudents$population == 'kuopio',])
nrow(maleStudents[maleStudents$population == 'tampere',])
table(student$gender)
table(student$gender, student$population)

# 4

hist(student$height)
boxplot(student$height)
boxplot(student$height ~ student$gender)
boxplot(student$height ~ student$population)
plot(student$height, student$shoesize, xlab='height', ylab='shoesize', main='pituus vs kengankoko')

# 5

sym <- ifelse(student$gender == 'male', 'M', 'F')
summary(sym)
plot(student$height, student$shoesize, pch = sym)

cols <- ifelse(student$population == 'kuopio', 'Blue', 'Red')
cols
plot(student$height, student$shoesize, pch = sym, col = cols)

# 6

nstudent <- data.frame(student$height, student$gender, student$population)
summary(nstudent)
names(nstudent) <- c('height', 'gender', 'population')
# nstudent <- student[,c('height', 'gender', 'population')]

nstudent$gender <- as.character(nstudent$gender)
nstudent$gender[nstudent$gender == 'male'] <- 'M'
nstudent$gender[nstudent$gender == 'female'] <- 'F'

nstudent$population <- as.character(nstudent$population)
nstudent$population[nstudent$population == 'kuopio'] <- 1
nstudent$gender[nstudent$population == 'tampere'] <- 2
nstudent

# 7

index <- which(student$gender == 'male')
student.male <- student[index,]
colnames(student.male)[4] <- 'populationM'
student.male

aboveMedian<-student[student$height > median(student$height),]
belowMedian<-student[student$height < median(student$height),]
aboveMedian
belowMedian
splitted<-split(student, student$height > median(student$height))
splitted
unsplit(splitted, student$height > median(student$height))

# 8

student$heightLog <- log(student$height)