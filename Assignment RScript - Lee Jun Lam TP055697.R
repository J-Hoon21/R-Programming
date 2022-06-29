#LEE JUN LAM 
#TP055697

#DATA IMPORT
#Read data from csv file
CSVdata = read.csv("C:\\R-4.1.2\\Placement_Data_Full_Class.csv",header = TRUE)
View(CSVdata)

#install packages and load packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotrix")
install.packages("plotly")
library(ggplot2)
library(dplyr)
library(plotrix)
library(plotly)


#DATA EXPLORATION
#show data stored
class(CSVdata)    # data type
str(CSVdata)      # all the headers and some data display
dim(CSVdata)      # number of rows and columns

head(CSVdata)     # first 6 lines
tail(CSVdata)     # last 6 lines

#Data PRE-PROCESSING
#Round up Secondary School Education Percentage
CSVdata$ssc_p = as.integer(format(round(CSVdata$ssc_p, 0)))
class(CSVdata$ssc_p)

#Round up Higher Secondary Education Percentage
CSVdata$hsc_p = as.integer(format(round(CSVdata$hsc_p, 0)))
class(CSVdata$hsc_p)

#Round up Degree Percentage
CSVdata$degree_p = as.integer(format(round(CSVdata$degree_p, 0)))
class(CSVdata$degree_p)

#Round up Employability test percentage
CSVdata$etest_p = as.integer(format(round(CSVdata$etest_p, 0)))
class(CSVdata$etest_p)
#-------------------------------------------------------------------------------
#Question 1: Why certain students can receive placement while certain students did not?
#Analysis 1.1: Find the number of students who receive placement and not receive placement
placed=nrow(CSVdata[CSVdata$status=="Placed",])
placed
notplaced=nrow(CSVdata[CSVdata$status=="Not Placed",])
notplaced
a=c(placed,notplaced)
pie3D(a,labels=a,explode=0.5, main="Placement Status",col=c("green","red"))


#Analysis 1.2: Find the number of students who receive and not receive placement 
#according to students' Post Graduation (MBA) Specialisation.
ggplot(CSVdata, aes(specialisation)) + geom_bar()+ facet_wrap(~status) + geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+
  labs(title="Relationship between Students' Post Graduation (MBA) Specialisation and Placement Status",
       y="Number of Students",
       x="Post Graduation (MBA) Specialisation")

#Analysis 1.3: Find the number of students who receive and not receive placement according to students' age
ggplot(CSVdata, aes(age)) + geom_bar()+ facet_wrap(~status) + geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Age and Placement Status",
       y="Number of Students",
       x="Age")

#Analysis 1:4: Find the number of students who receive and not receive placement according to students' gender
ggplot(CSVdata, aes(gender)) + geom_bar()+ facet_wrap(~status) + 
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Gender and Placement Status",
       y="Number of Students",
       x="Gender")

#Analysis 1.5: Find the number of students who receive and not receive placement 
#according to students' age,gender and Post Graduation (MBA) Specialisation = "Mkt&Fin"
CSVdata1.5 <- mutate(CSVdata,Analysis_1.5 =
                     case_when(specialisation=="Mkt&Fin"&age=="18"&gender=="M" ~ "18M", 
                               specialisation=="Mkt&Fin"&age=="18"&gender=="F" ~ "18F",
                               
                               specialisation=="Mkt&Fin"&age=="19"&gender=="M" ~ "19M", 
                               specialisation=="Mkt&Fin"&age=="19"&gender=="F" ~ "19F",
                               
                               specialisation=="Mkt&Fin"&age=="20"&gender=="M" ~ "20M", 
                               specialisation=="Mkt&Fin"&age=="20"&gender=="F" ~ "20F",
                               
                               specialisation=="Mkt&Fin"&age=="21"&gender=="M" ~ "21M", 
                               specialisation=="Mkt&Fin"&age=="21"&gender=="F" ~ "21F",
                               
                               specialisation=="Mkt&Fin"&age=="22"&gender=="M" ~ "22M", 
                               specialisation=="Mkt&Fin"&age=="22"&gender=="F" ~ "22F",
                               
                               specialisation=="Mkt&Fin"&age=="23"&gender=="M" ~ "23M", 
                               specialisation=="Mkt&Fin"&age=="23"&gender=="F" ~ "23F",)
)
View(CSVdata1.5)
CSVdata1.5 = subset(CSVdata1.5, select = -salary )
CSVdata1.5 <- na.omit(CSVdata1.5) 
CSVdata1.5
ggplot(CSVdata1.5, aes(Analysis_1.5)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Age , Gender, Post Graduation (MBA) Specialisation = Mkt&Fin and Placement Status",
       y="Number of Students",
       x="Students' Age,Gender,Post Graduation (MBA) Specialisation = Mkt&Fin")

#Analysis 1.6: Find the number of students who receive and not receive placement 
#according to students' age,gender and Post Graduation (MBA) Specialisation = "Mkt&HR"
CSVdata1.6 <- mutate(CSVdata,Analysis_1.6 =
                     case_when(specialisation=="Mkt&HR"&age=="18"&gender=="M" ~ "18M", 
                               specialisation=="Mkt&HR"&age=="18"&gender=="F" ~ "18F",
                               
                               specialisation=="Mkt&HR"&age=="19"&gender=="M" ~ "19M", 
                               specialisation=="Mkt&HR"&age=="19"&gender=="F" ~ "19F",
                               
                               specialisation=="Mkt&HR"&age=="20"&gender=="M" ~ "20M", 
                               specialisation=="Mkt&HR"&age=="20"&gender=="F" ~ "20F",
                               
                               specialisation=="Mkt&HR"&age=="21"&gender=="M" ~ "21M", 
                               specialisation=="Mkt&HR"&age=="21"&gender=="F" ~ "21F",
                               
                               specialisation=="Mkt&HR"&age=="22"&gender=="M" ~ "22M", 
                               specialisation=="Mkt&HR"&age=="22"&gender=="F" ~ "22F",
                               
                               specialisation=="Mkt&HR"&age=="23"&gender=="M" ~ "23M", 
                               specialisation=="Mkt&HR"&age=="23"&gender=="F" ~ "23F",)
)
View(CSVdata1.6)
CSVdata1.6 = subset(CSVdata1.6, select = -salary )
CSVdata1.6 <- na.omit(CSVdata1.6) 
CSVdata1.6
ggplot(CSVdata1.6, aes(Analysis_1.6)) + geom_bar()+ facet_wrap(~status) +geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Age , Gender, Post Graduation (MBA) Specialisation = Mkt&HR and Placement Status",
       y="Number of Students",
       x="Students' Age , Gender, Post Graduation (MBA) Specialisation = Mkt&HR")
 
#-------------------------------------------------------------------------------
#Question 2: Does Student's University Education Records Affect Student's Placement Status?
#Analysis 2.1: Find the relationship between students' Post Graduation (MBA) Percentage with Specialisation ="Mkt&Fin" and Placement Status
min(CSVdata$mba_p)
max(CSVdata$mba_p)
CSVdata2.1 <- mutate(CSVdata,Analysis_2.1 =
                     case_when(specialisation=="Mkt&Fin"&mba_p >= 90 ~ "90:100", 
                               specialisation=="Mkt&Fin"&mba_p %in% (80:89) ~ "80:89",
                               specialisation=="Mkt&Fin"&mba_p %in% (70:79) ~ "70:79", 
                               specialisation=="Mkt&Fin"&mba_p %in% (60:69) ~ "60:69",
                               specialisation=="Mkt&Fin"&mba_p %in% (50:59) ~ "50:59",)
)
View(CSVdata2.1)
CSVdata2.1 = subset(CSVdata2.1, select = -salary)
CSVdata2.1 <- na.omit(CSVdata2.1) 
CSVdata2.1
ggplot(CSVdata2.1, aes(Analysis_2.1)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Post Graduation (MBA) Percentage with Specialisation = Mkt&Fin and Placement Status",
       y="Number of Students",
       x="Students' Post Graduation (MBA) Percentage with Specialisation = Mkt&Fin")

#Analysis 2.2: Find the relationship between students' Post Graduation (MBA) Percentage with Specialisation ="Mkt&HR" and Placement Status
CSVdata2.2 <- mutate(CSVdata,Analysis_2.2 =
                     case_when(specialisation=="Mkt&HR"&mba_p >= 90 ~ "90:100", 
                               specialisation=="Mkt&HR"&mba_p %in% (80:89) ~ "80:89",
                               specialisation=="Mkt&HR"&mba_p %in% (70:79) ~ "70:79", 
                               specialisation=="Mkt&HR"&mba_p %in% (60:69) ~ "60:69",
                               specialisation=="Mkt&HR"&mba_p %in% (50:59) ~ "50:59",)
)
View(CSVdata2.2)
CSVdata2.2 = subset(CSVdata2.2, select = -salary)
CSVdata2.2 <- na.omit(CSVdata2.2) 
CSVdata2.2
ggplot(CSVdata2.2, aes(Analysis_2.2)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Post Graduation (MBA) Percentage with Specialisation = Mkt&HR and Placement Status",
       y="Number of Students",
       x="Students' Post Graduation (MBA) Percentage with Specialisation = Mkt&HR")


#Analysis 2.3: Find the relationship between students' Employability Test Percentage and Placement Status
min(CSVdata$etest_p)
max(CSVdata$etest_p)
CSVdata2.3 <- mutate(CSVdata,Analysis_2.3 =
                     case_when(etest_p >= 90 ~ "90:100", 
                               etest_p %in% (80:89) ~ "80:89",
                               etest_p %in% (70:79) ~ "70:79", 
                               etest_p %in% (60:69) ~ "60:69",
                               etest_p %in% (50:59) ~ "50:59",)
)
View(CSVdata2.3)
ggplot(CSVdata2.3, aes(Analysis_2.3)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Employability Test Percentage and Placement Status",
       y="Number of Students",
       x="Employability Test Percentage")


# Analysis 2.4: Find the relationship between students' Field of degree education and Placement Status
ggplot(CSVdata, aes(degree_t)) + geom_bar()+ facet_wrap(~status) +geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Field of degree education and Placement Status",
       y="Number of Students",
       x="Field of degree education")

# Analysis 2.5: Find the relationship between Degree Percentage "Comm&Mgmt" and Placement Status
min(CSVdata$degree_p)
max(CSVdata$degree_p)
Comm_Mgmt <- sample_frac(CSVdata, 1) %>% filter(degree_t == "Comm&Mgmt")
Comm_Mgmt <- mutate(Comm_Mgmt,Analysis_2.5 =
                     case_when(degree_p >= 90 ~ "90:100", 
                               degree_p %in% (80:89) ~ "80:89",
                               degree_p %in% (70:79) ~ "70:79", 
                               degree_p %in% (60:69) ~ "60:69",
                               degree_p %in% (50:59) ~ "50:59",)
                   
)
View(Comm_Mgmt)
ggplot(Comm_Mgmt, aes(Analysis_2.5)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Degree Percentage = Comm&Mgmt and Placement Status",
       y="Number of Students",
       x="Degree Percentage = Comm&Mgmt")

# Analysis 2.6: Find the relationship between Degree Percentage "Other" and Placement Status
Others <- sample_frac(CSVdata, 1) %>% filter(degree_t == "Others")
Others <- mutate(Others,Analysis_2.6 =
                     case_when(degree_p >= 90 ~ "90:100", 
                               degree_p %in% (80:89) ~ "80:89",
                               degree_p %in% (70:79) ~ "70:79", 
                               degree_p %in% (60:69) ~ "60:69",
                               degree_p %in% (50:59) ~ "50:59",)
)
View(Others)
ggplot(Others, aes(Analysis_2.6)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Degree Percentage = Other and Placement Status",
       y="Number of Students",
       x="Degree Percentage = Other")

# Analysis 2.7: Find the relationship between Degree Percentage "Sci&Tech" and Placement Status
Sci_Tech <- sample_frac(CSVdata, 1) %>% filter(degree_t == "Sci&Tech")
Sci_Tech <- mutate(Sci_Tech,Analysis_2.7 =
                     case_when(degree_p >= 90 ~ "90:100", 
                               degree_p %in% (80:89) ~ "80:89",
                               degree_p %in% (70:79) ~ "70:79", 
                               degree_p %in% (60:69) ~ "60:69",
                               degree_p %in% (50:59) ~ "50:59",)
)
View(Sci_Tech)
ggplot(Sci_Tech, aes(Analysis_2.7)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Degree Percentage = Sci&Tech and Placement Status",
       y="Number of Students",
       x="Degree Marks = Sci&Tech")


# Conclusion 2.1
ggplot(CSVdata, aes(x = mba_p, y = specialisation)) + geom_violin() + geom_boxplot(width=0.1) + facet_wrap(~status)+
  labs(title="Relationship between MBA Specialisation and MBA Percentage",
       y="MBA Specialisation",
       x="MBA Percentage")

# Conclusion 2.2
ggplot(CSVdata, aes(x = degree_p, y = degree_t)) + geom_violin()+ geom_boxplot(width=0.1)+ facet_wrap(~status) +
  labs(title="Relationship between Field of Degree Education and Degree Percentage",
       y="Field of Degree Education",
       x="Degree Percentage")

#-------------------------------------------------------------------------------
#Question 3: Does Student's Past Education Record Affect Student's Placement Status?
# Analysis 3.1: Find the relationship between Specialization in Higher Secondary Education and Placement Status?
ggplot(CSVdata, aes(hsc_s)) + geom_bar()+ facet_wrap(~status) +geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Specialization in Higher Secondary Education and Placement Status",
       y="Number of Students",
       x="Specialization in Higher Secondary Education")

# Analysis 3.2: Find the relationship between students' Higher Secondary Education Percentage = "Arts" and Placement Status
max(CSVdata$hsc_p)
min(CSVdata$hsc_p)
Art <- sample_frac(CSVdata, 1) %>% filter(hsc_s == "Arts")
Art <- mutate(Art,Analysis_3.2 =
                     case_when(hsc_p >= 90 ~ "90:100", 
                               hsc_p %in% (80:89) ~ "80:89",
                               hsc_p %in% (70:79) ~ "70:79", 
                               hsc_p %in% (60:69) ~ "60:69",
                               hsc_p %in% (50:59) ~ "50:59",
                               hsc_p %in% (40:49) ~ "40:49",
                               hsc_p %in% (35:39) ~ "30:39",)
                   
)
View(Art)
ggplot(Art, aes(Analysis_3.2)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Higher Secondary Education Percentage = Arts and Placement Status",
       y="Number of Students",
       x="Higher Secondary Education Percentage = Arts")

# Analysis 3.3: Find the relationship between students' Higher Secondary Education Percentage = "Commerce" and Placement Status
Commerce <- sample_frac(CSVdata, 1) %>% filter(hsc_s == "Commerce")
Commerce <- mutate(Commerce,Analysis_3.3 =
                case_when(hsc_p >= 90 ~ "90:100", 
                          hsc_p %in% (80:89) ~ "80:89",
                          hsc_p %in% (70:79) ~ "70:79", 
                          hsc_p %in% (60:69) ~ "60:69",
                          hsc_p %in% (50:59) ~ "50:59",
                          hsc_p %in% (40:49) ~ "40:49",)
              
)
View(Commerce)
ggplot(Commerce, aes(Analysis_3.3)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Higher Secondary Education Percentage = Commerce and Placement Status",
       y="Number of Students",
       x="High School Percentage = Commerce")

# Analysis 3.4: Find the relationship between students' Higher Secondary Education Percentage = "Science" and Placement Status
Science <- sample_frac(CSVdata, 1) %>% filter(hsc_s == "Science")
Science <- mutate(Science,Analysis_3.4 =
                     case_when(hsc_p >= 90 ~ "90:100", 
                               hsc_p %in% (80:89) ~ "80:89",
                               hsc_p %in% (70:79) ~ "70:79", 
                               hsc_p %in% (60:69) ~ "60:69",
                               hsc_p %in% (50:59) ~ "50:59",
                               hsc_p %in% (40:49) ~ "40:49",
                               hsc_p %in% (30:39) ~ "30:39",)
                   
)
View(Science)
ggplot(Science, aes(Analysis_3.4)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Higher Secondary Education Percentage = Science and Placement Status",
       y="Number of Students",
       x="Higher Secondary Education Percentage = Science")

# Analysis 3.5: Find the relationship between students' Secondary School Education Percentage and Placement Status
max(CSVdata$ssc_p)
min(CSVdata$ssc_p)
SSM <- sample_frac(CSVdata, 1)
SSM <- mutate(SSM,Analysis_3.5 =
                     case_when(ssc_p >= 90 ~ "90:100", 
                               ssc_p %in% (80:89) ~ "80:89",
                               ssc_p %in% (70:79) ~ "70:79", 
                               ssc_p %in% (60:69) ~ "60:69",
                               ssc_p %in% (50:59) ~ "50:59",
                               ssc_p %in% (40:49) ~ "40:49")
)
View(SSM)
ggplot(SSM, aes(Analysis_3.5)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Students' Secondary School Education Percentage and Placement Status",
       y="Number of Students",
       x="Secondary School Education Percentage")

# Analysis 3.6: Does Board of Education (High School) affect placement status?
ggplot(CSVdata, aes(hsc_b)) + geom_bar()+ facet_wrap(~status) +geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Board of Education (High School) and Placement Status",
       y="Number of Students",
       x="Board of Education (High School)")

# Analysis 3.7: Does Board of Education (Secondary School) affect Placement Status?
ggplot(CSVdata, aes(ssc_b)) + geom_bar()+ facet_wrap(~status) +geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Board of Education (Secondary School) and Placement Status",
       y="Number of Students",
       x="Board of Education (Secondary School)")

# Analysis 3.8: Does Board of Education (Both) affect Placement Status?
Both <- sample_frac(CSVdata,1)
Both <- mutate(Both, Analysis_3.8 = paste(Both$ssc_b, Both$hsc_b))
View(Both)
ggplot(Both, aes(Analysis_3.8)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Board of Education (Both) and Placement Status",
       y="Number of Students",
       x="Board of Education (Both)")


# Conclusion 3.1: Higher Secondary Education Specialization and Higher Secondary Education Percentage
ggplot(CSVdata, aes(x = hsc_p, y = hsc_s)) + geom_violin()+ geom_boxplot(width=0.1)+ facet_wrap(~status) +
  labs(title="Relationship between Higher Secondary Education Specialization and Higher Secondary Education Percentage",
       y="Higher Secondary Education Specialization",
       x="Higher Secondary Education Percentage")

# Conclusion 3.2: Board of Education (Higher Secondary Education) and Higher Secondary Education Percentage 
ggplot(CSVdata, aes(x = hsc_p, y = hsc_b)) + geom_violin()+ geom_boxplot(width=0.1)+ facet_wrap(~status) +
  labs(title="Relationship between Board of Education (Higher Secondary Education) and Higher Secondary Education Percentage",
       y="Higher Secondary Education Specialization",
       x="Higher Secondary Education Percentage")

# Conclusion 3.3: Board of Education (Secondary School Education) and Secondary School Percentage 
ggplot(CSVdata, aes(x =ssc_p, y = ssc_b)) + geom_violin()+ geom_boxplot(width=0.1)+ facet_wrap(~status) +
  labs(title="Relationship between Board of Education (Secondary School Education) and Secondary School Percentage",
       y="Higher Secondary Education Specialization",
       x="Higher Secondary Education Percentage")

#-------------------------------------------------------------------------------
#Question 4: Does Students' Family Background contribute support to Student's Placement Status?
#Analysis 4.1: Analyzing the relationship between Mother's Education & Mother's Job and Placement Status
ggplot(CSVdata, aes(x=Mjob, fill=factor(Medu))) + geom_bar(position=position_dodge()) + 
  facet_wrap(~status) + scale_fill_brewer(palette="Purples")+ theme_minimal() +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5, position=position_dodge(width=1))+ 
  labs(title ="Relationship Between Mother's Education Level, Mother's Job and Placement Status",  
       y ="Number of Students",
       x = "Mother's Job") 

#Analysis 4.2: Analyzing the relationship between Father's Education & Father's Job and Placement Status
ggplot(CSVdata, aes(x=Fjob, fill=factor(Fedu))) + geom_bar(position=position_dodge()) + 
  facet_wrap(~status) + scale_fill_brewer(palette="Blues")+ theme_minimal() +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5, position=position_dodge(width=1))+ 
  labs(title ="Relationship Between Father's Education Level, Father's Job and Placement Status",  
       y ="Number of Students",
       x = "Father's Job") 

#Analysis 4.3: Analyzing the relationship between Family Educational Support and Placement Status
ggplot(CSVdata, aes(famsup)) + geom_bar()+ facet_wrap(~status) + geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Family Educational Support and Placement Status",
       y="Number of Students",
       x="Family Educational Support")

#Analysis 4.4: Analyzing the relationship between Extra paid classes within the course subject and Placement Status
ggplot(CSVdata, aes(paid)) + geom_bar()+ facet_wrap(~status) + geom_text(stat="count",aes(label=stat(count)), vjust=-0.5)+ 
  labs(title="Relationship between Extra Paid Classes Within The Course Subject and Placement Status",
       y="Number of Students",
       x="Extra Paid Classes")

#Analysis 4.5 Analyzing the relationship between Both Family Educational Support & Extra paid classes within the 
#course subject and Placement Status
CSVdata4.5 <- mutate(CSVdata,Analysis_4.5 =
                     case_when(famsup=="yes"&paid =="yes" ~ "Both Yes", 
                               famsup=="yes"&paid =="no" ~ "Famsup Only",
                               famsup=="no"&paid =="yes" ~ "Paid Only", 
                               famsup=="no"&paid =="no" ~ "Both No",)
)
View(CSVdata4.5)
ggplot(CSVdata4.5, aes(Analysis_4.5)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Both Family Educational Support&Extra paid classes within the course subject and Placement Status",
       y="Number of Students",
       x="Both Family Educational Support and Extra paid classes within the course subject")

#-------------------------------------------------------------------------------
#Question 5: Does Students' Personal Environment and Experiences affect the placement status?
#Analysis 5.1: Analyzing the relationship between Students' Address and Placement Status
ggplot(CSVdata, aes(address)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",
            aes(label=stat(count)),
            vjust=-0.5)+ 
  labs(title="Relationship between Student's Address and Placement Status",
       y="Number of Students",
       x="Students' Address")

#Analysis 5.2: Analyzing the relationship between Internet access and Placement Status
ggplot(CSVdata, aes(internet)) + geom_bar()+ facet_wrap(~status) +geom_text(stat="count",aes(label=stat(count)), vjust=-0.5)+ 
  labs(title="Relationship between Internet access and Placement Status",
       y="Number of Students",
       x="Internet access")

#Analysis 5.3: Analyzing the relationship between Participation in extra-curricular activities and Placement Status
ggplot(CSVdata, aes(activities)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",
            aes(label=stat(count)),
            vjust=-0.5)+ 
  labs(title="Relationship between Participation in extra-curricular activities and Placement Status",
       y="Number of Students",
       x="Participation in extra-curricular activities")

#Analysis 5.4: Analyzing the relationship between Student's Working Experience and Placement Status
ggplot(CSVdata, aes(workex)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",
            aes(label=stat(count)),
            vjust=-0.5)+ 
  labs(title="Relationship between Student's Working Experience and Placement Status",
       y="Number of Students",
       x="Student's Working Experience")

#Analysis 5.5: Analyzing the relationship between Both Participation in extra-curricular activities and Work experience 
#and Placement Status
CSVdata5.5 <- mutate(CSVdata,Analysis_5.5 =
                     case_when(activities=="yes"&workex =="Yes" ~ "Both Yes", 
                               activities=="yes"&workex =="No" ~ "Activities Only",
                               activities=="no"&workex =="Yes" ~ "Workex Only", 
                               activities=="no"&workex =="No" ~ "Both No",)
)
View(CSVdata5.5)
ggplot(CSVdata5.5, aes(Analysis_5.5)) + geom_bar()+ facet_wrap(~status) +
  geom_text(stat="count",aes(label=stat(count)),vjust=-0.5)+ 
  labs(title="Relationship between Both Participation in extra-curricular activities and Work experience and Placement Status",
       y="Number of Students",
       x="Participation in extra-curricular activities and Work experience")
#-------------------------------------------------------------------------------
#Question 6: Does students' overall records (combination of factors) affect Student's Placement Status and Salary Amount?
#Analysis 6.1: Combination of Mean Scores and Placement Status
Mean <-mutate(CSVdata, MeanScore1 = (ssc_p+hsc_p)/2, MeanScore2 = (degree_p+mba_p+etest_p)/3)
Mean$MeanScore1 <- round(Mean$MeanScore1)
Mean$MeanScore2 <- round(Mean$MeanScore2)
Mean <-mutate(Mean, MeanScore3 = MeanScore1 + MeanScore2)
View(Mean)
Histo_1 <- ggplot(Mean, aes(x=MeanScore3, fill=factor(status))) + geom_histogram()+
           scale_fill_brewer(palette="RdYlBu")+ theme_minimal() +
           labs(title="Relationship between Combination of Mean Scores and Placement Status",
           y="Number of Students",
           x="Combination of Mean Scores")
Histo_1 <- ggplotly(Histo_1)
Histo_1

#Analysis 6.2: Combination of Mean Scores and Salary Amount
Mean2 <- sample_frac(Mean,1) %>% filter(status=="Placed")
View(Mean2)
Histo_2 <- ggplot(Mean2, aes(x=MeanScore3, fill=factor(salary))) + geom_histogram() + 
           scale_fill_brewer(palette="RdYlBu")+ theme_minimal() +
           labs(title="Relationship between Combination of Mean Scores and Salary Amount",
           x="Number of Students",
           y="Combination of Mean Scores")
Histo_2 <- ggplotly(Histo_2)
Histo_2

#Conclusion 6
Conclusion6 <- sample_frac(Mean,1)
          ggplot(Conclusion6, aes(MeanScore3, fill=factor(status))) + geom_histogram(aes(x=MeanScore3,y=..density..)) + 
          geom_density(color="blue")+
          scale_fill_brewer(palette="PiYG")+ theme_minimal() +
          stat_function(fun=dnorm, args = list(mean=mean(Conclusion6$MeanScore3), sd=sd(Conclusion6$MeanScore3)), color="red")+
          labs(title="Normal Distribution Graph",
          x="Combination of Mean Scores",
          y="Number of Students")
            