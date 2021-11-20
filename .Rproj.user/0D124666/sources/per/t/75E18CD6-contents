marks <- read.table("marks.csv", header=TRUE, sep=",")

result <- t.test(Mark~Gender, data=marks)

if(result[3] <= 0.05)
{
  print("The test result was significant")
  print("Congratulations")
}else
{
  print("No significant rest results")
  print("Retry time")
}

count <- nrow(marks)
GENDER <- 1
EXAM <- 2

for(i in 1:count)
{
  x <- marks[i, EXAM]
  if(x < 10)
  {
    cat("Student NO ", i , "FAILED", "\n")
  }else
  {
    cat("Student NO ", i, "PASSED", "\n")
  }
}

#Find the % of pass and fail
Total <- 0
Pass <- 0
Fail <- 0
for(i in 1:count)
{
  x <- marks[i, EXAM]
  if(x < 10)
  {
    Fail = Fail + 1
  }else
  {
    Pass = Pass + 1
  }
  Total = Total + 1
}
Passed <- (Pass/Total) * 100
Failed <- (Fail/Total) * 100
cat("The percentage of passes is ", Passed, "\n")
cat("The percentage of fails is ", Failed, "\n")

#Switch example
print("Enter a number between 1 to 6")
user <- scan(what=double(0), nmax=1, multi.line=FALSE)

switch (user,
        print("You entered 1"),
        print("You entered 2"),
        print("You entered 3"),
        print("You entered 4"),
        print("You entered 5"),
        print("You entered 6")
)

#Vectors(1 dimensional data)
x <- c(10,20,30) #Creates a vector with 3 elements
length(x)
x[3]
#Insert 99 between 20 and 30
x <- c(x[1:2], 99, x[3])
x[1] <- 100 #change existing value
x <- x + 3 #add 3 to all elements
y <- c(10, 20, 30, 40)
z = x + y

#Matrices example, using smoking and genders
#Performing a chi-square testcount 
count <- matrix(c(80, 20, 15, 85), nrow=2)

chisq.test(count)

colnames(count) <- c("Males", "Females")
rownames(count) <- c("Smoker", "Nonsmoker")

#Lists
lecturer <- list()
lecturer[[1]] <- list(name="Mcgarry", title="Senior Lecturer", papers=73,
                      citations = 843)
lecturer[[2]] <- list(name="Davies", title="Professor", papers=60,
                      citations = 1112)
lecturer[[3]] <- list(name="Doink", title="Intern", papers=6,
                      citations = 11)
lecturer[[4]] <- list(name="Borkus", title="Senior Lecturer", papers=54,
                      citations = 1001)
lecturer[[5]] <- list(name="Hyaah", title="Tenured Professor", papers=102,
                      citations = 1337)
str(lecturer)

#Data frame
mydata <- data.frame(lapply(data.frame(t(sapply(lecturer,'['))),
                            unlist), stringsAsFactors = FALSE)

write.csv(mydata, file="Lecturer Data.csv", row.names = TRUE)