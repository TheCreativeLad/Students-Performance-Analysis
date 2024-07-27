# link to the dataset: https://www.kaggle.com/datasets/waqi786/student-performance-dataset

stud <- read.csv("~/Datasets/student_performance_data.csv")
attach(stud)
names(stud)

library(dplyr)
library(ggplot2)

str(stud)

## DATA CLEANING

# Turning the Gender, PartTimeJob, and ExtraCurricularActivities to a factor
stud$Gender <- as.factor(stud$Gender)
stud$PartTimeJob <- as.factor(stud$PartTimeJob)
stud$ExtraCurricularActivities <- as.factor(stud$ExtraCurricularActivities)

## Getting the summary statistics of the data

summary(stud)

# getting the major courses
major_courses <- data.frame(table(stud$Major))
course <- major_courses$Var1
value <- major_courses$Freq

## SUMMARY ON GENDER
# we can see that from this dataset, we have more female students than the male students, having 256 females and 244 males

## SUMMARY ON AGE
# The average age of the students is 21 years, where the minimum and maximum is 18 and 24 years respectively


## SUMMARY ON THE STUDY HOURS PER WEEK
# The average study hours of the student per week is approximately 20 hours, while the minimum and maximum study hours per week is 1 and 39 respectively

## SUMMARY ON ATTENDANCE RATE
# The average attendance rate of the students is approximately 75% while the minimum and the maximum is approximately50% and 100% respectively

## SUMMARY ON THE GPA
# The average GPA of the students is 2.985 while the minimum and maximum is 2.00 and 3.99 respectively

## SUMMARY OF THE MAJOR COURSES
# From the given data, we can see that there 5 major courses, which are: Arts, Business, Education, Engineering, and Science
# We can also see that students take Business course more than the other courses, Engineering, Arts, Education and Science in that order

## SUMMARY ON THE PART TIME JOB
# The number of student doing part time job (268) is more than the students who are not doing part Time Job (232)

## SUMMARY ON EXTRA CURRIICULAR ACTIVITIES
# The number fo students engaging in extra curricular activities (240) is less than the students who are not (240)


### DATA VISUALIZATION
# Visualizing the proportion of gender using barplot
ggplot(stud, aes(Gender, StudentID)) + geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Visualization of the Students according to their gender using Bar Chart") + theme_update()


dat <- data.frame(table(stud$Gender))
Sex <- dat$Var1

# Visualizing the proportion of gender using pie chart
ggplot(dat, aes(x = "", y = Freq, fill = Sex)) + geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + theme_void() + labs(title = "Visualization of the Students according to their Gender using Pie Chart")

# Visualization of the Major Courses
ggplot(major_courses, aes(course, value, fill = course)) + geom_bar(stat = "identity") + 
  labs(title = "Visualization of the students according to the Major Courses registered", y = "Number of Students")

# Visualizatiom of Students according to Part Time Jobs
pt_job <- data.frame(table(stud$PartTimeJob))
job <- pt_job$Var1
cnt <- pt_job$Freq

ggplot(pt_job, aes(job, cnt, fill = job)) + geom_bar(stat = "identity") +
  labs(title = "Visualization of the students according to Part Time Jobs Engagements", 
       y = "Number of Students", x = "Part Time Job Engagement")

# Visualizatiom of Students according to Extra Curricular Activities
extra <- data.frame(table(stud$ExtraCurricularActivities))
extras <- extra$Var1
cnt <- extra$Freq

ggplot(extra, aes(extras, cnt, fill = extras)) + geom_bar(stat = "identity") +
  labs(title = "Visualization of the students according to Extra Curricular Activities Engagements", 
       y = "Number of Students", x = "Extra Curricular Activities Engagement")



## ANALYSIS OF THE IMPACTS OF EACH VARIABLES ON THE PERFORMANCE OF THE STUDENTS

# The performance of each gender of the students
gpa_gender <- stud %>% 
  group_by(Gender) %>% 
  summarise(cnt = mean(GPA))
gpa_gender

ggplot(gpa_gender, aes(Gender, cnt, fill = Gender)) + geom_bar(stat = "identity") +
  labs(title = "Visualization of the performance of each gender of students", 
       x = "Gender", y = "Students Performance")
# From the above result, we can see that there is no much difference in the performance of the male and female, wit the male having a average GPA of 2.97 and the female having an average GPA of 3.00

# The performance of each Age of the students
gpa_age <- stud %>% 
  group_by(Age) %>% 
  summarise(cnt = mean(GPA))
gpa_age

ggplot(gpa_age, aes(Age, cnt, fill = Age)) + geom_bar(stat = "identity", width = 0.8) +
  labs(title = "Visualization of the performance of each Age of students", 
       x = "Age", y = "Students Performance") +
  geom_text(aes(label = round(cnt, 2)), vjust = 5, color = "white")
# From the above result, we can see that the students of age 23 with 3.2 performs more better than the others, and the students of age 18 has the least performance of 2.87


## Looking for the relationship between the student performance and the Study Hours per Week
ggplot(stud, aes(StudyHoursPerWeek, GPA)) + geom_point(stat = "identity", position = "identity") + 
  geom_smooth(method = "lm") + labs(title = "Relationship between the Study Hours of the Students and their Performance")
# From the result above, we can see that there is no significant relationship between the Study Hours per week and the students' performance

# performing a correlation test to check further
cor.test(x = StudyHoursPerWeek, y = GPA)
# From the result of the correlation test, since our p value = 0.0404, is less than 0.05, we do not reject our null hypothesis and hereby conclude that the correlation is significantly different from zero.
# Moreover, from the value of our correlation coefficient being 0.0917, we can see that there is only a little correlation of about 9% between the study hours per week of the students and their performance.


# Looking for the relationship between thw Attendance rate and the performance rate of the students.
ggplot(stud, aes(AttendanceRate, GPA)) + geom_point(stat = "identity", position = "identity") +
  geom_smooth(method = "lm") + labs(title = "Relationship between the Attendance Rate of the Students and their Performance")

# performing a correlation test to check further
cor.test(x = AttendanceRate, y = GPA)
# From the result of the correlation test, since our p value = 0.1745, is greater than 0.05, we reject our null hypothesis and hereby conclude that the correlation is not significantly different from zero.
# Moreover, from the value of our correlation coefficient being 0.0608, we can see that there is only a little correlation of about 6% between the Attendance Rates of the students and their performance.


## Checking for the impact of the Major Course registered on the performance of the Students
gpa_major <- stud %>% 
  group_by(Major) %>% 
  summarise(cnt = mean(GPA))
gpa_major

ggplot(gpa_major, aes(Major, cnt, fill = Major)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(cnt, 2)), vjust = 5, color = "black") +
  labs(title = "Visualization of the students' performance based on the Major Course offered")
# From the result above, we can see that the students doing Business as their major course performs more while the students doing Science as their Major Course had the least performance. But there is no much difference in their performance.


## Checking for the impact of doing a Part Time Job on the performance of the Students
gpa_job <- stud %>% 
  group_by(PartTimeJob) %>% 
  summarise(cnt = mean(GPA))
gpa_job

ggplot(gpa_job, aes(PartTimeJob, cnt, fill = PartTimeJob)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(cnt, 2)), vjust = 5, color = "black") +
  labs(title = "Visualization of the students' performance based on the Major Course offered")
# From the result above, we can see that the students doing Part Time Job (3.01) performs more than the students who are not doing Part Time Job(2.96). But there is no much difference in their performance.


## Checking for the impact of doing Extra Curricular Activities on the performance of the Students
gpa_extra <- stud %>% 
  group_by(ExtraCurricularActivities) %>% 
  summarise(cnt = mean(GPA))
gpa_extra

ggplot(gpa_extra, aes(ExtraCurricularActivities, cnt, fill = ExtraCurricularActivities)) +
  geom_bar(stat = "identity") + geom_text(aes(label = round(cnt, 2)), vjust = 5, color = "black") +
  labs(title = "Visualization of the students' performance based on the Extra Curricular Activities")
# From the result above, we can see that the students doing Extra Curricular Activities (3.01) performs more than the students who are not doing Extra Curricular Activities(2.96). But there is no much difference in their performance.
