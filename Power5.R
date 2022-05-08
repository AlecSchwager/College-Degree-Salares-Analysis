rm(list=ls())

#College type data file 

college_type<-read.csv("salaries-by-college-type.csv", na.strings = "N/A") 
#View(college_type) 
#str(college_type) 

#Conversion process for college type 

college_type$Starting.Median.Salary<-as.numeric(gsub('[$,]','',college_type$Starting.Median.Salary)) 
college_type$Mid.Career.Median.Salary<-as.numeric(gsub('[$,]','',college_type$Mid.Career.Median.Salary)) 
college_type$Mid.Career.10th.Percentile.Salary<-as.numeric(gsub('[$,]','',college_type$Mid.Career.10th.Percentile.Salary)) 
college_type$Mid.Career.25th.Percentile.Salary<-as.numeric(gsub('[$,]','',college_type$Mid.Career.25th.Percentile.Salary)) 
college_type$Mid.Career.75th.Percentile.Salary<-as.numeric(gsub('[$,]','',college_type$Mid.Career.75th.Percentile.Salary)) 
college_type$Mid.Career.90th.Percentile.Salary<-as.numeric(gsub('[$,]','',college_type$Mid.Career.90th.Percentile.Salary)) 
college_type<-college_type[college_type$School.Type != "Party", ]
college_type$School.Type<- as.factor(college_type$School.Type)
college_type$School.Name<-trimws(college_type$School.Name)
#str(college_type) 


#School region data file 

school_region<-read.csv("salaries-by-region.csv", na.strings = "N/A") 
#View(school_region) 
#str(school_region) 

#Conversion for region 

school_region$Starting.Median.Salary<-as.numeric(gsub('[$,]','',school_region$Starting.Median.Salary)) 
school_region$Mid.Career.Median.Salary<-as.numeric(gsub('[$,]','',school_region$Mid.Career.Median.Salary)) 
school_region$Mid.Career.10th.Percentile.Salary<-as.numeric(gsub('[$,]','',school_region$Mid.Career.10th.Percentile.Salary)) 
school_region$Mid.Career.25th.Percentile.Salary<-as.numeric(gsub('[$,]','',school_region$Mid.Career.25th.Percentile.Salary)) 
school_region$Mid.Career.75th.Percentile.Salary<-as.numeric(gsub('[$,]','',school_region$Mid.Career.75th.Percentile.Salary)) 
school_region$Mid.Career.90th.Percentile.Salary<-as.numeric(gsub('[$,]','',school_region$Mid.Career.90th.Percentile.Salary)) 
school_region$Region<-as.factor(school_region$Region)
school_region$School.Name<-trimws(school_region$School.Name)

#str(school_region) 

school_region[school_region==""]<-NA
college_type[college_type==""]<-NA
all_schools <- merge(school_region, college_type, all.x = TRUE, all.y = TRUE) 
#View(all_schools)



#Power 5
library(xml2)

page<-read_html("https://en.wikipedia.org/wiki/Power_Five_conferences")
ACC<-xml_text(xml_find_all(page,"//table[@class='multicol']//table[@class='wikitable']//a[@title='Atlantic Coast Conference']"))
BigTen<-xml_text(xml_find_all(page,"//table[@class='multicol']//table[@class='wikitable']//a[@title='Big Ten Conference']"))
Big12<-xml_text(xml_find_all(page,"//table[@class='multicol']//table[@class='wikitable']//a[@title='Big 12 Conference']"))
Pac12<-xml_text(xml_find_all(page,"//table[@class='multicol']//table[@class='wikitable']//a[@title='Pac-12 Conference']"))
SEC<-xml_text(xml_find_all(page,"//table[@class='multicol']//table[@class='wikitable']//a[@title='Southeastern Conference']"))

Power5List<-xml_text(xml_find_all(page,"//table[@class='multicol']//table[@class='wikitable']/tbody/tr/td"))
Power5List<-Power5List[1:71]
ACC_Teams<-Power5List[seq(1,length(Power5List),5)]
BigTen_Teams<-Power5List[seq(2,length(Power5List),5)]
Big12_Teams<-Power5List[seq(3,length(Power5List),5)]
Pac12_Teams<-Power5List[seq(4,length(Power5List),5)]
SEC_Teams<-Power5List[seq(5,length(Power5List),5)]

ACCDF<-data.frame(School.Name=ACC_Teams,Conference=ACC)
BigTenDF<-data.frame(School.Name=BigTen_Teams,Conference=BigTen)
Big12DF<-data.frame(School.Name=Big12_Teams,Conference=Big12)
Pac12DF<-data.frame(School.Name=Pac12_Teams,Conference=Pac12)
SECDF<-data.frame(School.Name=SEC_Teams,Conference=SEC)

Power5<-rbind(ACCDF,BigTenDF,Big12DF,Pac12DF,SECDF)
Power5$School.Name<- gsub("[\n]","",Power5$School.Name)
Power5[Power5==""]<-NA
Power5<-Power5[complete.cases(Power5),]
rm(ACC, BigTen, Big12, Pac12, SEC, Power5List, ACC_Teams, BigTen_Teams, Big12_Teams, Pac12_Teams, SEC_Teams, ACCDF, BigTenDF, Big12DF, Pac12DF,SECDF,page)
row.names(Power5)<-NULL
#View(Power5)

Power5$School.Name<-gsub("Alabama","University of Alabama, Tuscaloosa",Power5$School.Name)
Power5$School.Name[40]<-gsub("Arizona","University of Arizona",Power5$School.Name[40])
Power5$School.Name<-gsub("Arizona State","Arizona State University (ASU)",Power5$School.Name)
Power5$School.Name<-gsub("Arkansas","University of Arkansas",Power5$School.Name)
Power5$School.Name<-gsub("Auburn","Auburn University",Power5$School.Name)
Power5$School.Name<-gsub("Baylor","Baylor University",Power5$School.Name)
Power5$School.Name<-gsub("California","University of California, Berkeley",Power5$School.Name)
Power5$School.Name<-gsub("Clemson","Clemson University",Power5$School.Name)
Power5$School.Name<-gsub("Colorado","University of Colorado - Boulder (UCB)",Power5$School.Name)
Power5$School.Name<-gsub("Duke","Duke University",Power5$School.Name)
Power5$School.Name[55]<-gsub("Florida","University of Florida (UF)",Power5$School.Name[55])
Power5$School.Name<-gsub("Florida State","Florida State University (FSU)",Power5$School.Name)
Power5$School.Name[56]<-gsub("Georgia","University of Georgia (UGA)",Power5$School.Name[56])
Power5$School.Name<-gsub("Georgia Tech","Georgia Institute of Technology",Power5$School.Name)
Power5$School.Name<-gsub("Illinois","University of Illinois at Urbana-Champaign (UIUC)",Power5$School.Name)
Power5$School.Name<-gsub("Indiana","Indiana University (IU), Bloomington",Power5$School.Name)
Power5$School.Name[18]<-gsub("Iowa","University of Iowa (UI)",Power5$School.Name[18])
Power5$School.Name<-gsub("Iowa State","Iowa State University",Power5$School.Name)
Power5$School.Name[32]<-gsub("Kansas","University of Kansas",Power5$School.Name[32])
Power5$School.Name<-gsub("Kansas State","Kansas State University (KSU)",Power5$School.Name)
Power5$School.Name<-gsub("Kentucky","University of Kentucky (UK)",Power5$School.Name)
Power5$School.Name<-gsub("LSU","Louisiana State University (LSU)",Power5$School.Name)
Power5$School.Name<-gsub("Maryland","University of Maryland, College Park",Power5$School.Name)
Power5$School.Name[20]<-gsub("Michigan","University of Michigan",Power5$School.Name[20])
Power5$School.Name<-gsub("Michigan State","Michigan State University (MSU)",Power5$School.Name)
Power5$School.Name<-gsub("Minnesota","University of Minnesota",Power5$School.Name)
Power5$School.Name<-gsub("Mississippi State","Mississippi State University (MSU)",Power5$School.Name)
Power5$School.Name<-gsub("Missouri","University of Missouri - Columbia",Power5$School.Name)
Power5$School.Name<-gsub("NC State","North Carolina State University (NCSU)",Power5$School.Name)
Power5$School.Name<-gsub("Nebraska","University of Nebraska",Power5$School.Name)
Power5$School.Name[8]<-gsub("North Carolina","University of North Carolina at Chapel Hill (UNCH)",Power5$School.Name[8])
Power5$School.Name<-gsub("Northwestern","Northwestern University",Power5$School.Name)
Power5$School.Name<-gsub("Notre Dame","University of Notre Dame",Power5$School.Name)
Power5$School.Name<-gsub("Ohio State","Ohio State University (OSU)",Power5$School.Name)
Power5$School.Name[34]<-gsub("Oklahoma","University of Oklahoma",Power5$School.Name[34])
Power5$School.Name<-gsub("Oklahoma State","Oklahoma State University",Power5$School.Name)
Power5$School.Name<-gsub("Ole Miss","University of Mississippi",Power5$School.Name)
Power5$School.Name[45]<-gsub("Oregon","University of Oregon",Power5$School.Name[45])
Power5$School.Name<-gsub("Oregon State","Oregon State University (OSU)",Power5$School.Name)
Power5$School.Name<-gsub("Penn State","Pennsylvania State University (PSU)",Power5$School.Name)
Power5$School.Name<-gsub("Purdue","Purdue University",Power5$School.Name)
Power5$School.Name<-gsub("Rutgers","Rutgers University",Power5$School.Name)
Power5$School.Name<-gsub("South Carolina","University of South Carolina",Power5$School.Name)
Power5$School.Name<-gsub("Stanford","Stanford University",Power5$School.Name)
Power5$School.Name<-gsub("Syracuse","Syracuse University",Power5$School.Name)
Power5$School.Name<-gsub("TCU","Texas Christian University (TCU)",Power5$School.Name)
Power5$School.Name<-gsub("Tennessee","University of Tennessee",Power5$School.Name)
Power5$School.Name[37]<-gsub("Texas","University of Texas (UT) - Austin",Power5$School.Name[37])
Power5$School.Name<-gsub("Texas A&M","Texas A&M University",Power5$School.Name)
Power5$School.Name<-gsub("UCLA","University of California at Los Angeles (UCLA)",Power5$School.Name)
Power5$School.Name<-gsub("USC","University of Southern California (USC)",Power5$School.Name)
Power5$School.Name<-gsub("Utah","University of Utah",Power5$School.Name)
Power5$School.Name<-gsub("Vanderbilt","Vanderbilt University",Power5$School.Name)
Power5$School.Name[12]<-gsub("Virginia","University of Virginia (UVA)",Power5$School.Name[12])
all_schools$School.Name<-gsub("Virginia Polytechnic Institute and State University (Virginia Tech)","Virginia Tech",all_schools$School.Name)
Power5$School.Name[50]<-gsub("Washington","University of Washington (UW)",Power5$School.Name[50])
Power5$School.Name<-gsub("Washington State","Washington State University (WSU)",Power5$School.Name)
Power5$School.Name<-gsub("West Virginia","West Virginia University (WVU)",Power5$School.Name)
Power5$School.Name<-gsub("Wisconsin","University of Wisconsin (UW) - Madison",Power5$School.Name)

SchoolsByConference<-merge(Power5,all_schools, by="School.Name", all.x = TRUE)
SchoolsByConference[SchoolsByConference==""]<-NA
SchoolsByConference$Conference<-as.factor(SchoolsByConference$Conference)
#View(SchoolsByConference)

#1  
library(dplyr) 
library(ggplot2)  

mediansalary<-aggregate(list("AvgMedianSalary"=SchoolsByConference$Starting.Median.Salary), by=list("Region"=SchoolsByConference$Region),FUN=mean, na.rm=TRUE) 

mediansalary 

salaryanova<- aov(Starting.Median.Salary~Region, data=SchoolsByConference) 

summary(salaryanova) 

mediansalary<-aggregate(list("AvgMedianSalary"=SchoolsByConference$Starting.Median.Salary), by=list("Region"=SchoolsByConference$Region),FUN=mean, na.rm=TRUE)  

mediansalary$AvgMedianSalary <- round(mediansalary$AvgMedianSalary, digits = 7) 

mediansalary$Region <- factor(mediansalary$Region, levels = mediansalary$Region[order(mediansalary$AvgMedianSalary)]) 

g <- ggplot(data = mediansalary, aes(x = Region, y = AvgMedianSalary)) + 
geom_bar(stat = "identity", fill="3300FF", colour="black") + ggtitle("Schools in California obtain Highest Average Starting Median Salary")
g + labs(y= "Average Starting Median Salary", x="Region") + theme_update(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels=scales::dollar_format())

#2  
careergrowthdol<-aggregate(list("Growth"=SchoolsByConference$Mid.Career.90th.Percentile.Salary-SchoolsByConference$Mid.Career.10th.Percentile.Salary), by=list("Conference"=SchoolsByConference$Conference),FUN=mean, na.rm=TRUE) 

careergrowthdol

careergrowthperc<-aggregate(list("Percent Growth"=((SchoolsByConference$Mid.Career.90th.Percentile.Salary-SchoolsByConference$Mid.Career.10th.Percentile.Salary)/SchoolsByConference$Mid.Career.10th.Percentile.Salary)*100),
                            by=list("Conference"=SchoolsByConference$Conference),FUN=mean, na.rm=TRUE) 
careergrowthgroup<-group_by(SchoolsByConference, Conference)
careergrowthsummary<-summarize(careergrowthgroup,
                               Avg.Starting = mean(Starting.Median.Salary, na.rm=TRUE),
                               Avg.Mid.Career = mean(Mid.Career.Median.Salary, na.rm=TRUE))
careergrowthsummary
groupedbar2<- data.frame(CareerStatus=rep(c("Starting","Mid-Career"),each=5),
                           Conference=rep(c("ACC","Big 12","Big Ten","Pac-12","SEC"),2),
                           AverageMedianSalary=c(49878,44978,48921,50542,44571,91967,83056,88414,93692,83950)) 

ggplot(groupedbar2,aes(x=Conference,y=AverageMedianSalary, fill=reorder(CareerStatus, AverageMedianSalary))) +
        geom_bar(stat="identity", width=0.7,position=position_dodge()) +
        ggtitle("Salary Growth Consistent Across Conferences")+
        guides(fill=guide_legend(title="Career Status"))+
        scale_y_continuous(labels=scales::dollar_format())

careergrowthperc

#3
group <- group_by(SchoolsByConference, Conference) 
summary_group <- summarise(group, Max=max((Starting.Median.Salary),na.rm=TRUE), Min=min((Starting.Median.Salary),na.rm=TRUE)) 

groupedbar3<- data.frame(SalaryType=rep(c("Max","Min"),each=5),
                         Conference=rep(c("ACC","Big 12","Big Ten","Pac-12","SEC"),2),
                        Salary=c(58900,49700,52900,70400,51200,42100,42400,44700,42200,40000)) 

#View(groupedbar3) 

ggplot(groupedbar3,aes(x=Conference,y=Salary, fill=SalaryType))+
        geom_bar(stat="identity", width=0.7,position=position_dodge())+
        ggtitle("ACC and Pac-12: Largest Difference in Min and Max Starting Salaries")+
        scale_y_continuous(labels=scales::dollar_format())

#4
regression <- lm(Starting.Median.Salary~Conference+Region,data=SchoolsByConference) 
regression <- lm(Starting.Median.Salary~Conference+Region,data=SchoolsByConference)  
qqnorm(regression$residuals) 

qqline(regression$residuals, col="red") 

plot(Starting.Median.Salary~Region, data=SchoolsByConference) 

plot(regression$fitted.values, regression$residuals, 
     
     xlab="Fitted Values", ylab="Residuals") 

summary(regression) 

write.csv(SchoolsByConference,file = "Salaries_Merged.csv")
