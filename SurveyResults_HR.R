library(tidyverse)
library(readxl)



Results <- read_excel("C:/Users/sthomaz/OneDrive - City of Fort Collins/Desktop/Work Product/MiscellaneousProjects/HR Survey/2023_Alchemer_Results.xlsx")

# Remove unwanted variables
r2 <- subset(Results, select = -c(`IP Address`, Country, City, Postal, Comments, `State/Region`))

r2 <- unite(r2, Department, c(3:13) , na.rm = TRUE)
r2 <- unite(r2, Gender, c(8:13), na.rm = TRUE)
r2 <- unite(r2, Race, c(9:17), na.rm = TRUE)


r2$ServiceArea <- r2$`Which service area do you work in?`
r2$Remote <- r2$`What are your current working conditions?`
r2$Classification <- r2$`What is your classification?`
r2$Generation <- r2$`What generation are you?`
r2$Tenure <- r2$`How long have you been a city employee?`
r2$CommunicationQuality <- r2$`The employee communications that I receive are informative and clear.`
r2$LeadershipDecisions <- r2$`I feel confident the decisions being made will ensure the organization's long-term and short-term future.`
r2$ConnedtoTeam <- r2$`I feel connected to my team (or colleagues).`
r2$CheckIns <- r2$`I have sufficient check-ins with my manager to remain connected and aligned.`
r2$DeptReview <- r2$`Did your department review the 2022 Status Check Survey results?`
r2$SexualOrientation <- r2$`Sexual Orientation:`
r2$SexualOrientation <- ifelse(is.na(r2$SexualOrientation), "Decline to specify", r2$SexualOrientation)
r2$SexualOrientation <- ifelse(r2$SexualOrientation == "Prefer to self-identify:", "Prefer to self-identify", r2$SexualOrientation)


r2 <- subset(r2, select = -c( `Which service area do you work in?`, 
                              `Sexual Orientation:`,
                              `What are your current working conditions?`, 
                              `What is your classification?`, 
                              `What generation are you?`,
                              `How long have you been a city employee?`,
                              `The employee communications that I receive are informative and clear.`,
                              `I feel confident the decisions being made will ensure the organization's long-term and short-term future.`,
                              `I feel connected to my team (or colleagues).`,
                              `I have sufficient check-ins with my manager to remain connected and aligned.`,
                              `Did your department review the 2022 Status Check Survey results?`))

rm(Results)
# See the unique options
 # unique(r2$ServiceArea)
 # unique(r2$Department)
 # unique(r2$Gender)
 # unique(r2$Race)
 # unique(r2$`Sexual Orientation:`)
 # unique(r2$Remote)
 # unique(r2$Classification)
 # unique(r2$Generation)
 # unique(r2$Tenure)
 # unique(r2$CommunicationQuality)
 # unique(r2$LeadershipDecisions)
 # unique(r2$ConnedtTeam)
 # unique(r2$CheckIns)
 # unique(r2$DeptReview)



# Ammend Race results to categorize the results more cleanly
r2$Race <- replace(r2$Race, r2$Race == "", "Decline to specify") 
r2$Race <- replace(r2$Race, r2$Race == "Prefer to self-identify_Why?", "Decline to specify") 
r2$Race <- replace(r2$Race, r2$Race == "American Indian/Alaska Native_White", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African American/Black_White", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African American/Black_White_Prefer to self-identify_Mixed Race", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African_African American/Black", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African_White", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "Prefer to self-identify_Human", "Prefer to self-identify") 

# Ammend Gender results to categorize the results more cleanly
r2$Gender <- replace(r2$Gender, r2$Gender == "", "Decline to Specify") 
r2$Gender <- replace(r2$Gender, r2$Gender == "I decline to answer this. there are only 2 genders.", "Decline to Specify") 
r2$Gender <- replace(r2$Gender, r2$Gender == "Why?", "Decline to Specify") 
r2$Gender <- replace(r2$Gender, r2$Gender == "Man_I am a Man!", "Man")
r2$Gender <- replace(r2$Gender, r2$Gender == "Man_Transgender", "Transgender")
r2$Gender <- replace(r2$Gender, r2$Gender == "Woman_Transgender", "Transgender") 
r2$Gender <- replace(r2$Gender, r2$Gender == "They them", "Other/Multiple")
r2$Gender <- replace(r2$Gender, r2$Gender == "Nonbinary_Transgender", "Other/Multiple") 
r2$Gender <- replace(r2$Gender, r2$Gender == "Nonbinary_Woman", "Other/Multiple") 
r2$Gender <- replace(r2$Gender, r2$Gender == "Woman_Two-Spirit", "Other/Multiple") 


###### Counts of each outcome #####
unique(r2$Gender)
sum(r2$Gender == "Man")
sum(r2$Gender == "Woman")
sum(r2$Gender == "Nonbinary")
sum(r2$Gender == "Two-Spirit")
sum(r2$Gender == "Other/Multiple")
sum(r2$Gender == "Transgender")
sum(r2$Gender == "Decline to Specify")

unique(r2$Race)
sum(r2$Race == "White")
sum(r2$Race == "Middle Eastern/North African")
sum(r2$Race == "American Indian/Alaska Native")
sum(r2$Race == "two+")
sum(r2$Race == "African")
sum(r2$Race == "African American/Black")
sum(r2$Race == "Prefer to self-identify")
sum(r2$Race == "Decline to specify")

unique(r2$Classification)
sum(r2$Classification == "Classified")
sum(r2$Classification == "Contractual")
sum(r2$Classification == "Hourly")
sum(r2$Classification == "Unclassified Management")

unique(r2$Tenure)
sum(r2$Tenure == "1 - 6 months")
sum(r2$Tenure == "7 - 12 months")
sum(r2$Tenure == "13 - 18 months")
sum(r2$Tenure == "19 months - 2 years")
sum(r2$Tenure == "3 - 4 years")
sum(r2$Tenure == "5 - 9 years")
sum(r2$Tenure == "10 - 14 years")
sum(r2$Tenure == "15 + years")

unique(r2$Generation)
sum(r2$Generation == "Silent (1928 - 1945)")
sum(r2$Generation == "Boomer (1946 - 1964)")
sum(r2$Generation == "Gen X (1965 - 1980)")
sum(r2$Generation == "Millennials (1981-1996)")
sum(r2$Generation == "Generation Z (1997-2012)")

unique(r2$ServiceArea)
sum(r2$ServiceArea == "Broadband")
sum(r2$ServiceArea == "Community Services")
sum(r2$ServiceArea == "Executive Services")
sum(r2$ServiceArea == "Financial Services")
sum(r2$ServiceArea == "Information & Employee Services")
sum(r2$ServiceArea == "Judicial Services")
sum(r2$ServiceArea == "Legal Services")
sum(r2$ServiceArea == "Planning, Dev & Transportation")
sum(r2$ServiceArea == "Police Services")
sum(r2$ServiceArea == "Sustainability Services")
sum(r2$ServiceArea == "Utility Services")

unique(r2$SexualOrientation)

r2$count <- 1
SexualityFrequencyTable<- r2 %>%
  group_by(SexualOrientation) %>%
  summarise(Frequency = sum(count))

WorkLocationFrequencyTable<- r2 %>%
  group_by(Remote) %>%
  summarise(Frequency = sum(count))

write.table(WorkLocationFrequencyTable, "clipboard", sep="\t")

########The employee communications that I receive are informative and clear#####
# 
# Write in responses
unique(r2$`Other - Write In:The employee communications that I receive are informative and clear.`)


r2 %>%
  mutate(CommunicationQuality = fct_relevel(CommunicationQuality, 
                                            "Strongly Disagree", "Disagree", "Neutral", 
                                            "Agree", "Strongly Agree")) %>%
  ggplot(aes(CommunicationQuality, fill = SexualOrientation)) + geom_bar() + coord_flip()+
  ggtitle("Communication ") + 
  #geom_text(stat='count', aes(label=..count.., hjust = ifelse(CommunicationQuality != "Agree", -0.1, 1.1)), color = "#5196C6")+
  xlab("Agreement") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        #legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )
unique(r2$ServiceArea)
r2 <- r2[r2$ServiceArea == "Utility Services",]

######Response Table Communication by Service Area######
ComByArea <- matrix(c(
  sum(r2$ServiceArea == "Broadband"),
  sum(r2$ServiceArea == "Community Services"),
  sum(r2$ServiceArea == "Executive Services"),
  sum(r2$ServiceArea == "Financial Services"),
  sum(r2$ServiceArea == "Information & Employee Services"),
  sum(r2$ServiceArea == "Judicial Services"),
  sum(r2$ServiceArea == "Legal Services"),
  sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  sum(r2$ServiceArea == "Police Services"),
  sum(r2$ServiceArea == "Sustainability Services"),
  sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CommunicationQuality == "Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CommunicationQuality == "Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CommunicationQuality == "Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CommunicationQuality == "Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CommunicationQuality == "Neutral")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CommunicationQuality == "Neutral")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CommunicationQuality == "Neutral")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CommunicationQuality == "Neutral")/ sum(r2$ServiceArea == "Utility Services"),
  
 
  100*sum(r2$ServiceArea == "Broadband" & r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CommunicationQuality == "Disagree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CommunicationQuality == "Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CommunicationQuality == "Disagree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CommunicationQuality == "Disagree")/ sum(r2$ServiceArea == "Utility Services"),
  

  100*sum(r2$ServiceArea == "Broadband"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$ServiceArea == "Utility Services")
  
),ncol=11, byrow=TRUE)
colnames(ComByArea) <- c("Broadband","Community Services","Executive Servicess","Financial Services","Information & Employee Services", 
                         "Judicial Services", "Legal Services", "Planning, Dev & Transportation", "Police Services", "Sustainability Services", "Utility Services")
rownames(ComByArea) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByArea[3,] + ComByArea[2,]
ComByArea <- rbind(ComByArea, Agreement)
Disagreement <- ComByArea[5,] + ComByArea[6,]
ComByArea <- rbind(ComByArea, Disagreement)


write.table(ComByArea, "clipboard", sep="\t")

######Response Table Communication by Generation######
ComByGen <- matrix(c(
  sum(r2$Generation == "Silent (1928 - 1945)"),
  sum(r2$Generation == "Boomer (1946 - 1964)"),
  sum(r2$Generation == "Gen X (1965 - 1980)"),
  sum(r2$Generation == "Millennials (1981-1996)"),
  sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CommunicationQuality == "Agree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CommunicationQuality == "Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CommunicationQuality == "Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CommunicationQuality == "Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CommunicationQuality == "Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CommunicationQuality == "Neutral")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CommunicationQuality == "Neutral")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CommunicationQuality == "Neutral")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CommunicationQuality == "Neutral")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CommunicationQuality == "Neutral")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CommunicationQuality == "Disagree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CommunicationQuality == "Disagree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CommunicationQuality == "Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CommunicationQuality == "Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CommunicationQuality == "Disagree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Generation == "Generation Z (1997-2012)")
),ncol=5, byrow=TRUE)
colnames(ComByGen) <- c("Silent (1928 - 1945)","Boomer (1946 - 1964)","Gen X (1965 - 1980)s","Millennials (1981-1996)","Generation Z (1997-2012)")
rownames(ComByGen) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByGen[3,] + ComByGen[2,]
ComByGen <- rbind(ComByGen, Agreement)
Disagreement <- ComByGen[5,] + ComByGen[6,]
ComByGen <- rbind(ComByGen, Disagreement)


write.table(ComByGen, "clipboard", sep="\t")

######Response Table Communication by Tenure######
ComByTenure <- matrix(c(
  sum(r2$Tenure == "1 - 6 months"),
  sum(r2$Tenure == "7 - 12 months"),
  sum(r2$Tenure == "13 - 18 months"),
  sum(r2$Tenure == "19 months - 2 years"),
  sum(r2$Tenure == "3 - 4 years"),
  sum(r2$Tenure == "5 - 9 years"),
  sum(r2$Tenure == "10 - 14 years"),
  sum(r2$Tenure == "15 + years"),
  
100*sum(r2$Tenure == "1 - 6 months" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Tenure == "1 - 6 months"),
100*sum(r2$Tenure == "7 - 12 months"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Tenure == "7 - 12 months"),
100*sum(r2$Tenure == "13 - 18 months"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Tenure == "13 - 18 months"),
100*sum(r2$Tenure == "19 months - 2 years"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Tenure == "19 months - 2 years"),
100*sum(r2$Tenure == "3 - 4 years"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Tenure == "3 - 4 years"),
100*sum(r2$Tenure == "5 - 9 years"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Tenure == "5 - 9 years"),
100*sum(r2$Tenure == "10 - 14 years"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Tenure == "10 - 14 years"),
100*sum(r2$Tenure == "15 + years"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Tenure == "15 + years"),

100*sum(r2$Tenure == "1 - 6 months" & r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "1 - 6 months"),
100*sum(r2$Tenure == "7 - 12 months"& r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "7 - 12 months"),
100*sum(r2$Tenure == "13 - 18 months"& r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "13 - 18 months"),
100*sum(r2$Tenure == "19 months - 2 years"& r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "19 months - 2 years"),
100*sum(r2$Tenure == "3 - 4 years"& r2$CommunicationQuality == "Agree")/ sum(r2$Tenure == "3 - 4 years"),
100*sum(r2$Tenure == "5 - 9 years"& r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "5 - 9 years"),
100*sum(r2$Tenure == "10 - 14 years"& r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "10 - 14 years"),
100*sum(r2$Tenure == "15 + years"& r2$CommunicationQuality == "Agree")/sum(r2$Tenure == "15 + years"),

100*sum(r2$Tenure == "1 - 6 months" & r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "1 - 6 months"),
100*sum(r2$Tenure == "7 - 12 months"& r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "7 - 12 months"),
100*sum(r2$Tenure == "13 - 18 months"& r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "13 - 18 months"),
100*sum(r2$Tenure == "19 months - 2 years"& r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "19 months - 2 years"),
100*sum(r2$Tenure == "3 - 4 years"& r2$CommunicationQuality == "Neutral")/ sum(r2$Tenure == "3 - 4 years"),
100*sum(r2$Tenure == "5 - 9 years"& r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "5 - 9 years"),
100*sum(r2$Tenure == "10 - 14 years"& r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "10 - 14 years"),
100*sum(r2$Tenure == "15 + years"& r2$CommunicationQuality == "Neutral")/sum(r2$Tenure == "15 + years"),

100*sum(r2$Tenure == "1 - 6 months" & r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "1 - 6 months"),
100*sum(r2$Tenure == "7 - 12 months"& r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "7 - 12 months"),
100*sum(r2$Tenure == "13 - 18 months"& r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "13 - 18 months"),
100*sum(r2$Tenure == "19 months - 2 years"& r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "19 months - 2 years"),
100*sum(r2$Tenure == "3 - 4 years"& r2$CommunicationQuality == "Disagree")/ sum(r2$Tenure == "3 - 4 years"),
100*sum(r2$Tenure == "5 - 9 years"& r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "5 - 9 years"),
100*sum(r2$Tenure == "10 - 14 years"& r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "10 - 14 years"),
100*sum(r2$Tenure == "15 + years"& r2$CommunicationQuality == "Disagree")/sum(r2$Tenure == "15 + years"),

100*sum(r2$Tenure == "1 - 6 months" & r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "1 - 6 months"),
100*sum(r2$Tenure == "7 - 12 months"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "7 - 12 months"),
100*sum(r2$Tenure == "13 - 18 months"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "13 - 18 months"),
100*sum(r2$Tenure == "19 months - 2 years"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "19 months - 2 years"),
100*sum(r2$Tenure == "3 - 4 years"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Tenure == "3 - 4 years"),
100*sum(r2$Tenure == "5 - 9 years"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "5 - 9 years"),
100*sum(r2$Tenure == "10 - 14 years"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "10 - 14 years"),
100*sum(r2$Tenure == "15 + years"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Tenure == "15 + years")
),ncol=8,byrow=TRUE)
colnames(ComByTenure) <- c("1 - 6 months","7 - 12 months","13 - 18 months","19 months - 2 years","3 - 4 years","5 - 9 years","10 - 14 years","15 + years")
rownames(ComByTenure) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByTenure[3,] + ComByTenure[2,]
ComByTenure <- rbind(ComByTenure, Agreement)
Disagreement <- ComByTenure[5,] + ComByTenure[6,]
ComByTenure <- rbind(ComByTenure, Disagreement)


write.table(ComByTenure, "clipboard", sep="\t")

         


######Response Table Communication by Gender######
ComByGen <- matrix(c(
  sum(r2$ServiceArea == "Man"),
  sum(r2$ServiceArea == "Woman"),
  sum(r2$ServiceArea == "Decline to Specify"),
  sum(r2$ServiceArea == "Nonbinary"),
  sum(r2$ServiceArea == "Transgender"),
  sum(r2$ServiceArea == "Two-Spirit"),
  sum(r2$ServiceArea == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Other/Multiple")
),ncol=7, byrow=TRUE)
colnames(ComByGen) <- c("Man","Woman","Decline to Specify","Nonbinary","Transgender", "Two-Spirit", "Other/Multiple" )
rownames(ComByGen) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByGen[3,] + ComByGen[2,]
ComByGen <- rbind(ComByGen, Agreement)
Disagreement <- ComByGen[5,] + ComByGen[6,]
ComByGen <- rbind(ComByGen, Disagreement)


write.table(ComByGen, "clipboard", sep="\t")


######Response Table Communication by Race######
ComByRace <- matrix(c(
  sum(r2$Race == "White"),
  sum(r2$Race == "African"),
  sum(r2$Race == "African American/Black"),
  sum(r2$Race == "American Indian/Alaska Native"),
  sum(r2$Race == "Middle Eastern/North African"),
  sum(r2$Race == "two+"),
  sum(r2$Race == "Prefer to self-identify"),
  sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CommunicationQuality == "Agree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CommunicationQuality == "Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CommunicationQuality == "Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CommunicationQuality == "Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CommunicationQuality == "Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CommunicationQuality == "Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CommunicationQuality == "Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CommunicationQuality == "Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CommunicationQuality == "Neutral")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CommunicationQuality == "Neutral")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CommunicationQuality == "Neutral")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CommunicationQuality == "Neutral")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CommunicationQuality == "Neutral")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CommunicationQuality == "Neutral")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CommunicationQuality == "Neutral")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CommunicationQuality == "Neutral")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CommunicationQuality == "Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CommunicationQuality == "Disagree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CommunicationQuality == "Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CommunicationQuality == "Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CommunicationQuality == "Disagree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CommunicationQuality == "Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CommunicationQuality == "Disagree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CommunicationQuality == "Disagree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Race == "Decline to specify")
),ncol=8, byrow=TRUE)
colnames(ComByRace) <- c("White","African","African American/Black","American Indian/Alaska Native","Middle Eastern/North African", "two+", "Prefer to self-identify", "Decline to specify")
rownames(ComByRace) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByRace[3,] + ComByRace[2,]
ComByRace <- rbind(ComByRace, Agreement)
Disagreement <- ComByRace[5,] + ComByRace[6,]
ComByRace <- rbind(ComByRace, Disagreement)


write.table(ComByRace, "clipboard", sep="\t")

######Response Table Communication by Remote######
ComByRemote <- matrix(c(
  sum(r2$Remote == "100% Working Remotely"),
  sum(r2$Remote == "Both remote and City Site"),
  sum(r2$Remote == "Other"),
  sum(r2$Remote == "Working off-site at designated location"),
  sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CommunicationQuality == "Agree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CommunicationQuality == "Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CommunicationQuality == "Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CommunicationQuality == "Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CommunicationQuality == "Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CommunicationQuality == "Neutral")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CommunicationQuality == "Neutral")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CommunicationQuality == "Neutral")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CommunicationQuality == "Neutral")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CommunicationQuality == "Neutral")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CommunicationQuality == "Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CommunicationQuality == "Disagree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CommunicationQuality == "Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CommunicationQuality == "Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CommunicationQuality == "Disagree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Remote == "Working on City Site")
),ncol=5, byrow=TRUE)
colnames(ComByRemote) <- c("100% Working Remotely","Both remote and City Site","Others","Working off-site at designated location","Working on City Site")
rownames(ComByRemote) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByRemote[3,] + ComByRemote[2,]
ComByRemote <- rbind(ComByRemote, Agreement)
Disagreement <- ComByRemote[5,] + ComByRemote[6,]
ComByRemote <- rbind(ComByRemote, Disagreement)


write.table(ComByRemote, "clipboard", sep="\t")

######Response Table Communication by Classification######
ComByClass <- matrix(c(
  sum(r2$Classification == "Classified"),
  sum(r2$Classification == "Contractual"),
  sum(r2$Classification == "Hourly"),
  sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CommunicationQuality == "Agree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CommunicationQuality == "Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CommunicationQuality == "Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CommunicationQuality == "Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CommunicationQuality == "Neutral")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CommunicationQuality == "Neutral")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CommunicationQuality == "Neutral")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CommunicationQuality == "Neutral")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CommunicationQuality == "Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CommunicationQuality == "Disagree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CommunicationQuality == "Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CommunicationQuality == "Disagree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Classification == "Unclassified Management")
  
),ncol=4, byrow=TRUE)
colnames(ComByClass) <- c("Classified","Contractual","Hourly","Unclassified Management")
rownames(ComByClass) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComByClass[3,] + ComByClass[2,]
ComByClass <- rbind(ComByClass, Agreement)
Disagreement <- ComByClass[5,] + ComByClass[6,]
ComByClass <- rbind(ComByClass, Disagreement)


write.table(ComByClass, "clipboard", sep="\t")
######Response Table Communication by Sexual Orientation######
ComBySex <- matrix(c(
  sum(r2$Gender == "Asexual"),
  sum(r2$Gender == "Bisexual"),
  sum(r2$Gender == "Decline to Specify"),
  sum(r2$Gender == "Heterosexual"),
  sum(r2$Gender == "Lexbian or Gay"),
  sum(r2$Gender == "Pansexual"),
  sum(r2$Gender == "Prefer to self-identify"),
  sum(r2$Gender == "Queer"),
  sum(r2$Gender == "NA"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Other/Multiple")
  ),ncol=7, byrow=TRUE)
colnames(ComBySex) <- c("Man","Woman","Decline to Specify","Nonbinary","Transgender", "Two-Spirit", "Other/Multiple" )
rownames(ComBySex) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ComBySex[3,] + ComBySex[2,]
ComBySex <- rbind(ComBySex, Agreement)
Disagreement <- ComBySex[5,] + ComBySex[6,]
ComBySex <- rbind(ComBySex, Disagreement)


write.table(ComBySex, "clipboard", sep="\t")









# Leadership



######I feel confident the decisions being made will ensure the organization's long-term and short-term future.######
r2 %>%
  mutate(LeadershipDecisions = fct_relevel(LeadershipDecisions, 
                                            "Strongly Disagree", "Disagree", "Neutral", 
                                            "Agree", "Strongly Agree")) %>%
  ggplot(aes(LeadershipDecisions, fill = SexualOrientation)) + geom_bar( ) + coord_flip()+
  ggtitle("Opinion on Leadership Decisions ") + 
#  geom_text(stat='count', aes(label=..count.., hjust = ifelse(LeadershipDecisions != "Agree", -0.1, 1.1)), color = "#5196C6")+
  xlab("Agreement") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        #legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )


######Response Table Leadership by Service Area######
LeadByArea <- matrix(c(
  sum(r2$ServiceArea == "Broadband"),
  sum(r2$ServiceArea == "Community Services"),
  sum(r2$ServiceArea == "Executive Services"),
  sum(r2$ServiceArea == "Financial Services"),
  sum(r2$ServiceArea == "Information & Employee Services"),
  sum(r2$ServiceArea == "Judicial Services"),
  sum(r2$ServiceArea == "Legal Services"),
  sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  sum(r2$ServiceArea == "Police Services"),
  sum(r2$ServiceArea == "Sustainability Services"),
  sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$LeadershipDecisions == "Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$LeadershipDecisions == "Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$LeadershipDecisions == "Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$LeadershipDecisions == "Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$LeadershipDecisions == "Neutral")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$LeadershipDecisions == "Neutral")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$LeadershipDecisions == "Neutral")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$LeadershipDecisions == "Neutral")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$LeadershipDecisions == "Disagree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$LeadershipDecisions == "Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$LeadershipDecisions == "Disagree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$LeadershipDecisions == "Disagree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$ServiceArea == "Utility Services")
  
),ncol=11, byrow=TRUE)
colnames(LeadByArea) <- c("Broadband","Community Services","Executive Servicess","Financial Services","Information & Employee Services", 
                         "Judicial Services", "Legal Services", "Planning, Dev & Transportation", "Police Services", "Sustainability Services", "Utility Services")
rownames(LeadByArea) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByArea[3,] + LeadByArea[2,]
LeadByArea <- rbind(LeadByArea, Agreement)
Disagreement <- LeadByArea[5,] + LeadByArea[6,]
LeadByArea <- rbind(LeadByArea, Disagreement)


write.table(LeadByArea, "clipboard", sep="\t")

######Response Table Leadership by Generation######
LeadByGen <- matrix(c(
  sum(r2$Generation == "Silent (1928 - 1945)"),
  sum(r2$Generation == "Boomer (1946 - 1964)"),
  sum(r2$Generation == "Gen X (1965 - 1980)"),
  sum(r2$Generation == "Millennials (1981-1996)"),
  sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$LeadershipDecisions == "Agree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$LeadershipDecisions == "Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$LeadershipDecisions == "Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$LeadershipDecisions == "Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$LeadershipDecisions == "Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$LeadershipDecisions == "Neutral")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$LeadershipDecisions == "Neutral")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$LeadershipDecisions == "Neutral")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$LeadershipDecisions == "Neutral")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$LeadershipDecisions == "Neutral")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$LeadershipDecisions == "Disagree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$LeadershipDecisions == "Disagree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$LeadershipDecisions == "Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$LeadershipDecisions == "Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$LeadershipDecisions == "Disagree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Generation == "Generation Z (1997-2012)")
),ncol=5, byrow=TRUE)
colnames(LeadByGen) <- c("Silent (1928 - 1945)","Boomer (1946 - 1964)","Gen X (1965 - 1980)s","Millennials (1981-1996)","Generation Z (1997-2012)")
rownames(LeadByGen) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByGen[3,] + LeadByGen[2,]
LeadByGen <- rbind(LeadByGen, Agreement)
Disagreement <- LeadByGen[5,] + LeadByGen[6,]
LeadByGen <- rbind(LeadByGen, Disagreement)


write.table(LeadByGen, "clipboard", sep="\t")

######Response Table Leadership by Tenure######
LeadByTenure <- matrix(c(
  sum(r2$Tenure == "1 - 6 months"),
  sum(r2$Tenure == "7 - 12 months"),
  sum(r2$Tenure == "13 - 18 months"),
  sum(r2$Tenure == "19 months - 2 years"),
  sum(r2$Tenure == "3 - 4 years"),
  sum(r2$Tenure == "5 - 9 years"),
  sum(r2$Tenure == "10 - 14 years"),
  sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$LeadershipDecisions == "Agree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$LeadershipDecisions == "Agree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$LeadershipDecisions == "Neutral")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$LeadershipDecisions == "Neutral")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$LeadershipDecisions == "Disagree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$LeadershipDecisions == "Disagree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Tenure == "15 + years")
),ncol=8,byrow=TRUE)
colnames(LeadByTenure) <- c("1 - 6 months","7 - 12 months","13 - 18 months","19 months - 2 years","3 - 4 years","5 - 9 years","10 - 14 years","15 + years")
rownames(LeadByTenure) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByTenure[3,] + LeadByTenure[2,]
LeadByTenure <- rbind(LeadByTenure, Agreement)
Disagreement <- LeadByTenure[5,] + LeadByTenure[6,]
LeadByTenure <- rbind(LeadByTenure, Disagreement)


write.table(LeadByTenure, "clipboard", sep="\t")

######Response Table Leadership by Gender######
LeadByGender <- matrix(c(
  sum(r2$Gender == "Man"),
  sum(r2$Gender == "Woman"),
  sum(r2$Gender == "Decline to Specify"),
  sum(r2$Gender == "Nonbinary"),
  sum(r2$Gender == "Transgender"),
  sum(r2$Gender == "Two-Spirit"),
  sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Other/Multiple")
),ncol=7, byrow=TRUE)
colnames(LeadByGender) <- c("Man","Woman","Decline to Specify","Nonbinary","Transgender", "Two-Spirit", "Other/Multiple" )
rownames(LeadByGender) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByGender[3,] + LeadByGender[2,]
LeadByGender <- rbind(LeadByGender, Agreement)
Disagreement <- LeadByGender[5,] + LeadByGender[6,]
LeadByGender <- rbind(LeadByGender, Disagreement)


write.table(LeadByGender, "clipboard", sep="\t")

######Response Table Leadership by Race######
LeadByRace <- matrix(c(
  sum(r2$Race == "White"),
  sum(r2$Race == "African"),
  sum(r2$Race == "African American/Black"),
  sum(r2$Race == "American Indian/Alaska Native"),
  sum(r2$Race == "Middle Eastern/North African"),
  sum(r2$Race == "two+"),
  sum(r2$Race == "Prefer to self-identify"),
  sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$LeadershipDecisions == "Agree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$LeadershipDecisions == "Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$LeadershipDecisions == "Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$LeadershipDecisions == "Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$LeadershipDecisions == "Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$LeadershipDecisions == "Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$LeadershipDecisions == "Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$LeadershipDecisions == "Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$LeadershipDecisions == "Neutral")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$LeadershipDecisions == "Neutral")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$LeadershipDecisions == "Neutral")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$LeadershipDecisions == "Neutral")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$LeadershipDecisions == "Neutral")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$LeadershipDecisions == "Neutral")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$LeadershipDecisions == "Neutral")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$LeadershipDecisions == "Neutral")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$LeadershipDecisions == "Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$LeadershipDecisions == "Disagree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$LeadershipDecisions == "Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$LeadershipDecisions == "Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$LeadershipDecisions == "Disagree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$LeadershipDecisions == "Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$LeadershipDecisions == "Disagree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$LeadershipDecisions == "Disagree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Race == "Decline to specify")
),ncol=8, byrow=TRUE)
colnames(LeadByRace) <- c("White","African","African American/Black","American Indian/Alaska Native","Middle Eastern/North African", "two+", "Prefer to self-identify", "Decline to specify")
rownames(LeadByRace) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByRace[3,] + LeadByRace[2,]
LeadByRace <- rbind(LeadByRace, Agreement)
Disagreement <- LeadByRace[5,] + LeadByRace[6,]
LeadByRace <- rbind(LeadByRace, Disagreement)


write.table(LeadByRace, "clipboard", sep="\t")

######Response Table Leadership by Remote######
LeadByRemote <- matrix(c(
  sum(r2$Remote == "100% Working Remotely"),
  sum(r2$Remote == "Both remote and City Site"),
  sum(r2$Remote == "Other"),
  sum(r2$Remote == "Working off-site at designated location"),
  sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$LeadershipDecisions == "Agree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$LeadershipDecisions == "Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$LeadershipDecisions == "Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$LeadershipDecisions == "Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$LeadershipDecisions == "Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$LeadershipDecisions == "Neutral")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$LeadershipDecisions == "Neutral")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$LeadershipDecisions == "Neutral")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$LeadershipDecisions == "Neutral")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$LeadershipDecisions == "Neutral")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$LeadershipDecisions == "Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$LeadershipDecisions == "Disagree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$LeadershipDecisions == "Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$LeadershipDecisions == "Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$LeadershipDecisions == "Disagree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Remote == "Working on City Site")
),ncol=5, byrow=TRUE)
colnames(LeadByRemote) <- c("100% Working Remotely","Both remote and City Site","Others","Working off-site at designated location","Working on City Site")
rownames(LeadByRemote) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByRemote[3,] + LeadByRemote[2,]
LeadByRemote <- rbind(LeadByRemote, Agreement)
Disagreement <- LeadByRemote[5,] + LeadByRemote[6,]
LeadByRemote <- rbind(LeadByRemote, Disagreement)


write.table(LeadByRemote, "clipboard", sep="\t")

######Response Table Leadership by Classification######
LeadByClass <- matrix(c(
  sum(r2$Classification == "Classified"),
  sum(r2$Classification == "Contractual"),
  sum(r2$Classification == "Hourly"),
  sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$LeadershipDecisions == "Agree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$LeadershipDecisions == "Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$LeadershipDecisions == "Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$LeadershipDecisions == "Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$LeadershipDecisions == "Neutral")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$LeadershipDecisions == "Neutral")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$LeadershipDecisions == "Neutral")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$LeadershipDecisions == "Neutral")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$LeadershipDecisions == "Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$LeadershipDecisions == "Disagree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$LeadershipDecisions == "Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$LeadershipDecisions == "Disagree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$Classification == "Unclassified Management")
  
),ncol=4, byrow=TRUE)
colnames(LeadByClass) <- c("Classified","Contractual","Hourly","Unclassified Management")
rownames(LeadByClass) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadByClass[3,] + LeadByClass[2,]
LeadByClass <- rbind(LeadByClass, Agreement)
Disagreement <- LeadByClass[5,] + LeadByClass[6,]
LeadByClass <- rbind(LeadByClass, Disagreement)


write.table(LeadByClass, "clipboard", sep="\t")



######



LeadBySexO <- matrix(c(
  sum(r2$SexualOrientation == "Heterosexual"),
  sum(r2$SexualOrientation == "Decline to specify"),
  sum(r2$SexualOrientation == "Bisexual"),
  sum(r2$SexualOrientation == "Lesbian or Gay"),
  sum(r2$SexualOrientation == "Asexual"),
  sum(r2$SexualOrientation == "Queer"),
  sum(r2$SexualOrientation == "Pansexual"),
  sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$LeadershipDecisions == "Strongly Agree")/ sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$LeadershipDecisions == "Strongly Agree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$LeadershipDecisions == "Agree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$LeadershipDecisions == "Agree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$LeadershipDecisions == "Neutral")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$LeadershipDecisions == "Neutral")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$LeadershipDecisions == "Disagree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$LeadershipDecisions == "Disagree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$LeadershipDecisions == "Strongly Disagree")/ sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$LeadershipDecisions == "Strongly Disagree")/sum(r2$SexualOrientation == "Prefer to self-identify")
),ncol=8, byrow=TRUE)
colnames(LeadBySexO) <- c("Heterosexual","Decline to specify","Bisexuals","Lesbian or Gay","Asexual", "Queer", "Pansexual", "Prefer to self-identify")
rownames(LeadBySexO) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- LeadBySexO[3,] + LeadBySexO[2,]
LeadBySexO <- rbind(LeadBySexO, Agreement)
Disagreement <- LeadBySexO[5,] + LeadBySexO[6,]
LeadBySexO <- rbind(LeadBySexO, Disagreement)


write.table(LeadBySexO, "clipboard", sep="\t")









######I feel connected to my team (or colleagues).######
r2 %>%
  mutate(CheckIns = fct_relevel(CheckIns, 
                                           "Strongly Disagree", "Disagree", "Neutral", 
                                           "Agree", "Strongly Agree")) %>%
  ggplot(aes(CheckIns)) + geom_bar(fill = "#00477F") + coord_flip()+
  ggtitle("Sufficient Checkins") + 
  geom_text(stat='count', aes(label=..count.., hjust = ifelse(CheckIns != "Strongly Agree", -0.2, 1.3)), color = "#5196C6")+
  xlab("Agreement") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        #legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

######Response Table Leadership by Service Area######
ConnectedByArea <- matrix(c(
  sum(r2$ServiceArea == "Broadband"),
  sum(r2$ServiceArea == "Community Services"),
  sum(r2$ServiceArea == "Executive Services"),
  sum(r2$ServiceArea == "Financial Services"),
  sum(r2$ServiceArea == "Information & Employee Services"),
  sum(r2$ServiceArea == "Judicial Services"),
  sum(r2$ServiceArea == "Legal Services"),
  sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  sum(r2$ServiceArea == "Police Services"),
  sum(r2$ServiceArea == "Sustainability Services"),
  sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Neutral")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Neutral")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Neutral")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Disagree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Disagree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Disagree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Strongly Disagree")/ sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Strongly Disagree")/ sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Strongly Disagree")/ sum(r2$ServiceArea == "Utility Services")
  
),ncol=11, byrow=TRUE)
colnames(ConnectedByArea) <- c("Broadband","Community Services","Executive Servicess","Financial Services","Information & Employee Services", 
                          "Judicial Services", "Legal Services", "Planning, Dev & Transportation", "Police Services", "Sustainability Services", "Utility Services")
rownames(ConnectedByArea) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByArea[3,] + ConnectedByArea[2,]
ConnectedByArea <- rbind(ConnectedByArea, Agreement)
Disagreement <- ConnectedByArea[5,] + ConnectedByArea[6,]
ConnectedByArea <- rbind(ConnectedByArea, Disagreement)


write.table(ConnectedByArea, "clipboard", sep="\t")

######Response Table Leadership by Generation######
ConnectedByGen <- matrix(c(
  sum(r2$Generation == "Silent (1928 - 1945)"),
  sum(r2$Generation == "Boomer (1946 - 1964)"),
  sum(r2$Generation == "Gen X (1965 - 1980)"),
  sum(r2$Generation == "Millennials (1981-1996)"),
  sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Strongly Agree")/ sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Strongly Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Strongly Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Strongly Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Strongly Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Agree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Neutral")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Neutral")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Neutral")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Neutral")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Neutral")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Disagree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Disagree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Disagree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Generation Z (1997-2012)")
),ncol=5, byrow=TRUE)
colnames(ConnectedByGen) <- c("Silent (1928 - 1945)","Boomer (1946 - 1964)","Gen X (1965 - 1980)s","Millennials (1981-1996)","Generation Z (1997-2012)")
rownames(ConnectedByGen) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByGen[3,] + ConnectedByGen[2,]
ConnectedByGen <- rbind(ConnectedByGen, Agreement)
Disagreement <- ConnectedByGen[5,] + ConnectedByGen[6,]
ConnectedByGen <- rbind(ConnectedByGen, Disagreement)


write.table(ConnectedByGen, "clipboard", sep="\t")

######Response Table Leadership by Tenure######
ConnectedByTenure <- matrix(c(
  sum(r2$Tenure == "1 - 6 months"),
  sum(r2$Tenure == "7 - 12 months"),
  sum(r2$Tenure == "13 - 18 months"),
  sum(r2$Tenure == "19 months - 2 years"),
  sum(r2$Tenure == "3 - 4 years"),
  sum(r2$Tenure == "5 - 9 years"),
  sum(r2$Tenure == "10 - 14 years"),
  sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Strongly Agree")/ sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Strongly Agree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Agree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Agree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Agree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Agree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Neutral")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Neutral")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Disagree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Disagree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "15 + years")
),ncol=8,byrow=TRUE)
colnames(ConnectedByTenure) <- c("1 - 6 months","7 - 12 months","13 - 18 months","19 months - 2 years","3 - 4 years","5 - 9 years","10 - 14 years","15 + years")
rownames(ConnectedByTenure) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByTenure[3,] + ConnectedByTenure[2,]
ConnectedByTenure <- rbind(ConnectedByTenure, Agreement)
Disagreement <- ConnectedByTenure[5,] + ConnectedByTenure[6,]
ConnectedByTenure <- rbind(ConnectedByTenure, Disagreement)


write.table(ConnectedByTenure, "clipboard", sep="\t")

######Response Table Leadership by Gender######
ConnectedByGender <- matrix(c(
  sum(r2$Gender == "Man"),
  sum(r2$Gender == "Woman"),
  sum(r2$Gender == "Decline to Specify"),
  sum(r2$Gender == "Nonbinary"),
  sum(r2$Gender == "Transgender"),
  sum(r2$Gender == "Two-Spirit"),
  sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Other/Multiple")
),ncol=7, byrow=TRUE)
colnames(ConnectedByGender) <- c("Man","Woman","Decline to Specify","Nonbinary","Transgender", "Two-Spirit", "Other/Multiple" )
rownames(ConnectedByGender) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByGender[3,] + ConnectedByGender[2,]
ConnectedByGender <- rbind(ConnectedByGender, Agreement)
Disagreement <- ConnectedByGender[5,] + ConnectedByGender[6,]
ConnectedByGender <- rbind(ConnectedByGender, Disagreement)


write.table(ConnectedByGender, "clipboard", sep="\t")

######Response Table Leadership by Race######
ConnectedByRace <- matrix(c(
  sum(r2$Race == "White"),
  sum(r2$Race == "African"),
  sum(r2$Race == "African American/Black"),
  sum(r2$Race == "American Indian/Alaska Native"),
  sum(r2$Race == "Middle Eastern/North African"),
  sum(r2$Race == "two+"),
  sum(r2$Race == "Prefer to self-identify"),
  sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Agree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Neutral")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Neutral")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Neutral")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Neutral")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Neutral")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Neutral")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Neutral")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Neutral")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Disagree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Disagree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Disagree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Disagree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "Decline to specify")
),ncol=8, byrow=TRUE)
colnames(ConnectedByRace) <- c("White","African","African American/Black","American Indian/Alaska Native","Middle Eastern/North African", "two+", "Prefer to self-identify", "Decline to specify")
rownames(ConnectedByRace) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByRace[3,] + ConnectedByRace[2,]
ConnectedByRace <- rbind(ConnectedByRace, Agreement)
Disagreement <- ConnectedByRace[5,] + ConnectedByRace[6,]
ConnectedByRace <- rbind(ConnectedByRace, Disagreement)


write.table(ConnectedByRace, "clipboard", sep="\t")

######Response Table Leadership by Remote######
ConnectedByRemote <- matrix(c(
  sum(r2$Remote == "100% Working Remotely"),
  sum(r2$Remote == "Both remote and City Site"),
  sum(r2$Remote == "Other"),
  sum(r2$Remote == "Working off-site at designated location"),
  sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Strongly Agree")/ sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Strongly Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Strongly Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Strongly Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Strongly Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Agree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Neutral")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Neutral")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Neutral")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Neutral")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Neutral")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Disagree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Disagree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "Working on City Site")
),ncol=5, byrow=TRUE)
colnames(ConnectedByRemote) <- c("100% Working Remotely","Both remote and City Site","Others","Working off-site at designated location","Working on City Site")
rownames(ConnectedByRemote) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByRemote[3,] + ConnectedByRemote[2,]
ConnectedByRemote <- rbind(ConnectedByRemote, Agreement)
Disagreement <- ConnectedByRemote[5,] + ConnectedByRemote[6,]
ConnectedByRemote <- rbind(ConnectedByRemote, Disagreement)


write.table(ConnectedByRemote, "clipboard", sep="\t")

######Response Table Leadership by Classification######
ConnectedByClass <- matrix(c(
  sum(r2$Classification == "Classified"),
  sum(r2$Classification == "Contractual"),
  sum(r2$Classification == "Hourly"),
  sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Strongly Agree")/ sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Strongly Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Strongly Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Strongly Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Agree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Neutral")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Neutral")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Neutral")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Neutral")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Disagree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Disagree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified"& r2$CheckIns == "Strongly Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Strongly Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Strongly Disagree")/sum(r2$Classification == "Unclassified Management")
  
),ncol=4, byrow=TRUE)
colnames(ConnectedByClass) <- c("Classified","Contractual","Hourly","Unclassified Management")
rownames(ConnectedByClass) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByClass[3,] + ConnectedByClass[2,]
ConnectedByClass <- rbind(ConnectedByClass, Agreement)
Disagreement <- ConnectedByClass[5,] + ConnectedByClass[6,]
ConnectedByClass <- rbind(ConnectedByClass, Disagreement)


write.table(ConnectedByClass, "clipboard", sep="\t")



######



ConnectedBySexO <- matrix(c(
  sum(r2$SexualOrientation == "Heterosexual"),
  sum(r2$SexualOrientation == "Decline to specify"),
  sum(r2$SexualOrientation == "Bisexual"),
  sum(r2$SexualOrientation == "Lesbian or Gay"),
  sum(r2$SexualOrientation == "Asexual"),
  sum(r2$SexualOrientation == "Queer"),
  sum(r2$SexualOrientation == "Pansexual"),
  sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Strongly Agree")/ sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Strongly Agree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Strongly Agree")/ sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Agree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Neutral")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Disagree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Strongly Disagree")/ sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Prefer to self-identify")
),ncol=8, byrow=TRUE)
colnames(ConnectedBySexO) <- c("Heterosexual","Decline to specify","Bisexuals","Lesbian or Gay","Asexual", "Queer", "Pansexual", "Prefer to self-identify")
rownames(ConnectedBySexO) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedBySexO[3,] + ConnectedBySexO[2,]
ConnectedBySexO <- rbind(ConnectedBySexO, Agreement)
Disagreement <- ConnectedBySexO[5,] + ConnectedBySexO[6,]
ConnectedBySexO <- rbind(ConnectedBySexO, Disagreement)


write.table(ConnectedBySexO, "clipboard", sep="\t")





######I have sufficient check-ins with my manager to remain connected and aligned######
r2 %>%
  mutate(CheckIns = fct_relevel(CheckIns, 
                                    "Strongly Disagree", "Disagree", "Neutral", 
                                    "Agree", "Strongly Agree")) %>%
  ggplot(aes(CheckIns, fill = SexualOrientation)) + geom_bar() + coord_flip()+
  ggtitle("Sufficient Check-ins") + 
  #geom_text(stat='count', aes(label=..count.., hjust = ifelse(CheckIns != "Agree", -0.1, 1.1)), color = "#5196C6")+
  xlab("Agreement") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        #legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

######Response Table Leadership by Service Area######
ConnectedByArea <- matrix(c(
  sum(r2$ServiceArea == "Broadband"),
  sum(r2$ServiceArea == "Community Services"),
  sum(r2$ServiceArea == "Executive Services"),
  sum(r2$ServiceArea == "Financial Services"),
  sum(r2$ServiceArea == "Information & Employee Services"),
  sum(r2$ServiceArea == "Judicial Services"),
  sum(r2$ServiceArea == "Legal Services"),
  sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  sum(r2$ServiceArea == "Police Services"),
  sum(r2$ServiceArea == "Sustainability Services"),
  sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Strongly Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Strongly Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Agree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Agree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Agree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Agree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Neutral")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services" & r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Neutral")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Neutral")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Neutral")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband" & r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Disagree")/ sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Disagree")/ sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Disagree")/ sum(r2$ServiceArea == "Utility Services"),
  
  
  100*sum(r2$ServiceArea == "Broadband"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Broadband"),
  100*sum(r2$ServiceArea == "Community Services"& r2$CheckIns == "Strongly Disagree")/ sum(r2$ServiceArea == "Community Services"),
  100*sum(r2$ServiceArea == "Executive Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Executive Services"),
  100*sum(r2$ServiceArea == "Financial Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Financial Services"),
  100*sum(r2$ServiceArea == "Information & Employee Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Information & Employee Services"),
  100*sum(r2$ServiceArea == "Judicial Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Judicial Services"),
  100*sum(r2$ServiceArea == "Legal Services"& r2$CheckIns == "Strongly Disagree")/ sum(r2$ServiceArea == "Legal Services"),
  100*sum(r2$ServiceArea == "Planning, Dev & Transportation"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Planning, Dev & Transportation"),
  100*sum(r2$ServiceArea == "Police Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Police Services"),
  100*sum(r2$ServiceArea == "Sustainability Services"& r2$CheckIns == "Strongly Disagree")/sum(r2$ServiceArea == "Sustainability Services"),
  100*sum(r2$ServiceArea == "Utility Services"& r2$CheckIns == "Strongly Disagree")/ sum(r2$ServiceArea == "Utility Services")
  
),ncol=11, byrow=TRUE)
colnames(ConnectedByArea) <- c("Broadband","Community Services","Executive Servicess","Financial Services","Information & Employee Services", 
                               "Judicial Services", "Legal Services", "Planning, Dev & Transportation", "Police Services", "Sustainability Services", "Utility Services")
rownames(ConnectedByArea) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByArea[3,] + ConnectedByArea[2,]
ConnectedByArea <- rbind(ConnectedByArea, Agreement)
Disagreement <- ConnectedByArea[5,] + ConnectedByArea[6,]
ConnectedByArea <- rbind(ConnectedByArea, Disagreement)


write.table(ConnectedByArea, "clipboard", sep="\t")

######Response Table Leadership by Generation######
ConnectedByGen <- matrix(c(
  sum(r2$Generation == "Silent (1928 - 1945)"),
  sum(r2$Generation == "Boomer (1946 - 1964)"),
  sum(r2$Generation == "Gen X (1965 - 1980)"),
  sum(r2$Generation == "Millennials (1981-1996)"),
  sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Strongly Agree")/ sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Strongly Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Strongly Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Strongly Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Strongly Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Agree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Agree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Agree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Agree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Agree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Neutral")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Neutral")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Neutral")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Neutral")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Neutral")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)" & r2$CheckIns == "Disagree")/sum(r2$Generation == "Silent (1928 - 1945)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Disagree")/sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Disagree")/ sum(r2$Generation == "Generation Z (1997-2012)"),
  
  100*sum(r2$Generation == "Silent (1928 - 1945)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Boomer (1946 - 1964)"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Generation == "Boomer (1946 - 1964)"),
  100*sum(r2$Generation == "Gen X (1965 - 1980)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Gen X (1965 - 1980)"),
  100*sum(r2$Generation == "Millennials (1981-1996)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Millennials (1981-1996)"),
  100*sum(r2$Generation == "Generation Z (1997-2012)"& r2$CheckIns == "Strongly Disagree")/sum(r2$Generation == "Generation Z (1997-2012)")
),ncol=5, byrow=TRUE)
colnames(ConnectedByGen) <- c("Silent (1928 - 1945)","Boomer (1946 - 1964)","Gen X (1965 - 1980)s","Millennials (1981-1996)","Generation Z (1997-2012)")
rownames(ConnectedByGen) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByGen[3,] + ConnectedByGen[2,]
ConnectedByGen <- rbind(ConnectedByGen, Agreement)
Disagreement <- ConnectedByGen[5,] + ConnectedByGen[6,]
ConnectedByGen <- rbind(ConnectedByGen, Disagreement)


write.table(ConnectedByGen, "clipboard", sep="\t")

######Response Table Leadership by Tenure######
ConnectedByTenure <- matrix(c(
  sum(r2$Tenure == "1 - 6 months"),
  sum(r2$Tenure == "7 - 12 months"),
  sum(r2$Tenure == "13 - 18 months"),
  sum(r2$Tenure == "19 months - 2 years"),
  sum(r2$Tenure == "3 - 4 years"),
  sum(r2$Tenure == "5 - 9 years"),
  sum(r2$Tenure == "10 - 14 years"),
  sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Strongly Agree")/ sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Strongly Agree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Strongly Agree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Agree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Agree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Agree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Agree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Agree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Neutral")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Neutral")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Neutral")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Disagree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Disagree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Disagree")/sum(r2$Tenure == "15 + years"),
  
  100*sum(r2$Tenure == "1 - 6 months" & r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "1 - 6 months"),
  100*sum(r2$Tenure == "7 - 12 months"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "7 - 12 months"),
  100*sum(r2$Tenure == "13 - 18 months"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "13 - 18 months"),
  100*sum(r2$Tenure == "19 months - 2 years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "19 months - 2 years"),
  100*sum(r2$Tenure == "3 - 4 years"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Tenure == "3 - 4 years"),
  100*sum(r2$Tenure == "5 - 9 years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "5 - 9 years"),
  100*sum(r2$Tenure == "10 - 14 years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "10 - 14 years"),
  100*sum(r2$Tenure == "15 + years"& r2$CheckIns == "Strongly Disagree")/sum(r2$Tenure == "15 + years")
),ncol=8,byrow=TRUE)
colnames(ConnectedByTenure) <- c("1 - 6 months","7 - 12 months","13 - 18 months","19 months - 2 years","3 - 4 years","5 - 9 years","10 - 14 years","15 + years")
rownames(ConnectedByTenure) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByTenure[3,] + ConnectedByTenure[2,]
ConnectedByTenure <- rbind(ConnectedByTenure, Agreement)
Disagreement <- ConnectedByTenure[5,] + ConnectedByTenure[6,]
ConnectedByTenure <- rbind(ConnectedByTenure, Disagreement)


write.table(ConnectedByTenure, "clipboard", sep="\t")


######Response Table Leadership by Gender######
ConnectedByGender <- matrix(c(
  sum(r2$Gender == "Man"),
  sum(r2$Gender == "Woman"),
  sum(r2$Gender == "Decline to Specify"),
  sum(r2$Gender == "Nonbinary"),
  sum(r2$Gender == "Transgender"),
  sum(r2$Gender == "Two-Spirit"),
  sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Agree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Agree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Neutral")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Neutral")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man" & r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Disagree")/ sum(r2$Gender == "Other/Multiple"),
  
  100*sum(r2$Gender == "Man"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Man"),
  100*sum(r2$Gender == "Woman"& r2$CommunicationQuality == "Strongly Disagree")/ sum(r2$Gender == "Woman"),
  100*sum(r2$Gender == "Decline to Specify"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Decline to Specify"),
  100*sum(r2$Gender == "Nonbinary"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Nonbinary"),
  100*sum(r2$Gender == "Transgender"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Transgender"),
  100*sum(r2$Gender == "Two-Spirit"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Two-Spirit"),
  100*sum(r2$Gender == "Other/Multiple"& r2$CommunicationQuality == "Strongly Disagree")/sum(r2$Gender == "Other/Multiple")
),ncol=7, byrow=TRUE)
colnames(ConnectedByGender) <- c("Man","Woman","Decline to Specify","Nonbinary","Transgender", "Two-Spirit", "Other/Multiple" )
rownames(ConnectedByGender) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByGender[3,] + ConnectedByGender[2,]
ConnectedByGender <- rbind(ConnectedByGender, Agreement)
Disagreement <- ConnectedByGender[5,] + ConnectedByGender[6,]
ConnectedByGender <- rbind(ConnectedByGender, Disagreement)


write.table(ConnectedByGender, "clipboard", sep="\t")

######Response Table Leadership by Race######
ConnectedByRace <- matrix(c(
  sum(r2$Race == "White"),
  sum(r2$Race == "African"),
  sum(r2$Race == "African American/Black"),
  sum(r2$Race == "American Indian/Alaska Native"),
  sum(r2$Race == "Middle Eastern/North African"),
  sum(r2$Race == "two+"),
  sum(r2$Race == "Prefer to self-identify"),
  sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Strongly Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Strongly Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Agree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Agree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Agree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Agree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Agree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Agree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Agree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Agree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Neutral")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Neutral")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Neutral")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Neutral")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Neutral")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Neutral")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Neutral")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Neutral")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White" & r2$CheckIns == "Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Disagree")/sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Disagree")/ sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Disagree")/ sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Disagree")/ sum(r2$Race == "Decline to specify"),
  
  100*sum(r2$Race == "White"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "White"),
  100*sum(r2$Race == "African"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Race == "African"),
  100*sum(r2$Race == "African American/Black"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "African American/Black"),
  100*sum(r2$Race == "American Indian/Alaska Native"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "American Indian/Alaska Native"),
  100*sum(r2$Race == "Middle Eastern/North African"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "Middle Eastern/North African"),
  100*sum(r2$Race == "two+"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "two+"),
  100*sum(r2$Race == "Prefer to self-identify"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "Prefer to self-identify"),
  100*sum(r2$Race == "Decline to specify"& r2$CheckIns == "Strongly Disagree")/sum(r2$Race == "Decline to specify")
),ncol=8, byrow=TRUE)
colnames(ConnectedByRace) <- c("White","African","African American/Black","American Indian/Alaska Native","Middle Eastern/North African", "two+", "Prefer to self-identify", "Decline to specify")
rownames(ConnectedByRace) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByRace[3,] + ConnectedByRace[2,]
ConnectedByRace <- rbind(ConnectedByRace, Agreement)
Disagreement <- ConnectedByRace[5,] + ConnectedByRace[6,]
ConnectedByRace <- rbind(ConnectedByRace, Disagreement)


write.table(ConnectedByRace, "clipboard", sep="\t")

######Response Table Leadership by Remote######
ConnectedByRemote <- matrix(c(
  sum(r2$Remote == "100% Working Remotely"),
  sum(r2$Remote == "Both remote and City Site"),
  sum(r2$Remote == "Other"),
  sum(r2$Remote == "Working off-site at designated location"),
  sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Strongly Agree")/ sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Strongly Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Strongly Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Strongly Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Strongly Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Agree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Agree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Agree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Agree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Agree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Neutral")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Neutral")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Neutral")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Neutral")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Neutral")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely" & r2$CheckIns == "Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Disagree")/sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Disagree")/ sum(r2$Remote == "Working on City Site"),
  
  100*sum(r2$Remote == "100% Working Remotely"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "100% Working Remotely"),
  100*sum(r2$Remote == "Both remote and City Site"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Remote == "Both remote and City Site"),
  100*sum(r2$Remote == "Other"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "Other"),
  100*sum(r2$Remote == "Working off-site at designated location"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "Working off-site at designated location"),
  100*sum(r2$Remote == "Working on City Site"& r2$CheckIns == "Strongly Disagree")/sum(r2$Remote == "Working on City Site")
),ncol=5, byrow=TRUE)
colnames(ConnectedByRemote) <- c("100% Working Remotely","Both remote and City Site","Others","Working off-site at designated location","Working on City Site")
rownames(ConnectedByRemote) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByRemote[3,] + ConnectedByRemote[2,]
ConnectedByRemote <- rbind(ConnectedByRemote, Agreement)
Disagreement <- ConnectedByRemote[5,] + ConnectedByRemote[6,]
ConnectedByRemote <- rbind(ConnectedByRemote, Disagreement)


write.table(ConnectedByRemote, "clipboard", sep="\t")

######Response Table Leadership by Classification######
ConnectedByClass <- matrix(c(
  sum(r2$Classification == "Classified"),
  sum(r2$Classification == "Contractual"),
  sum(r2$Classification == "Hourly"),
  sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Strongly Agree")/ sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Strongly Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Strongly Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Strongly Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Agree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Agree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Agree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Agree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Neutral")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Neutral")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Neutral")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Neutral")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified" & r2$CheckIns == "Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Disagree")/sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Disagree")/sum(r2$Classification == "Unclassified Management"),
  
  100*sum(r2$Classification == "Classified"& r2$CheckIns == "Strongly Disagree")/sum(r2$Classification == "Classified"),
  100*sum(r2$Classification == "Contractual"& r2$CheckIns == "Strongly Disagree")/ sum(r2$Classification == "Contractual"),
  100*sum(r2$Classification == "Hourly"& r2$CheckIns == "Strongly Disagree")/sum(r2$Classification == "Hourly"),
  100*sum(r2$Classification == "Unclassified Management"& r2$CheckIns == "Strongly Disagree")/sum(r2$Classification == "Unclassified Management")
  
),ncol=4, byrow=TRUE)
colnames(ConnectedByClass) <- c("Classified","Contractual","Hourly","Unclassified Management")
rownames(ConnectedByClass) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedByClass[3,] + ConnectedByClass[2,]
ConnectedByClass <- rbind(ConnectedByClass, Agreement)
Disagreement <- ConnectedByClass[5,] + ConnectedByClass[6,]
ConnectedByClass <- rbind(ConnectedByClass, Disagreement)


write.table(ConnectedByClass, "clipboard", sep="\t")



######



ConnectedBySexO <- matrix(c(
  sum(r2$SexualOrientation == "Heterosexual"),
  sum(r2$SexualOrientation == "Decline to specify"),
  sum(r2$SexualOrientation == "Bisexual"),
  sum(r2$SexualOrientation == "Lesbian or Gay"),
  sum(r2$SexualOrientation == "Asexual"),
  sum(r2$SexualOrientation == "Queer"),
  sum(r2$SexualOrientation == "Pansexual"),
  sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Strongly Agree")/ sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Strongly Agree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Strongly Agree")/ sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Strongly Agree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Agree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Agree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Neutral")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Neutral")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual" & r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Disagree")/ sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer" & r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Disagree")/sum(r2$SexualOrientation == "Prefer to self-identify"),
  
  100*sum(r2$SexualOrientation == "Heterosexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Heterosexual"),
  100*sum(r2$SexualOrientation == "Decline to specify"& r2$CheckIns == "Strongly Disagree")/ sum(r2$SexualOrientation == "Decline to specify"),
  100*sum(r2$SexualOrientation == "Bisexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Bisexual"),
  100*sum(r2$SexualOrientation == "Lesbian or Gay"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Lesbian or Gay"),
  100*sum(r2$SexualOrientation == "Asexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Asexual"),
  100*sum(r2$SexualOrientation == "Queer"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Queer"),
  100*sum(r2$SexualOrientation == "Pansexual"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Pansexual"),
  100*sum(r2$SexualOrientation == "Prefer to self-identify"& r2$CheckIns == "Strongly Disagree")/sum(r2$SexualOrientation == "Prefer to self-identify")
),ncol=8, byrow=TRUE)
colnames(ConnectedBySexO) <- c("Heterosexual","Decline to specify","Bisexuals","Lesbian or Gay","Asexual", "Queer", "Pansexual", "Prefer to self-identify")
rownames(ConnectedBySexO) <- c("Total", "Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")


Agreement <- ConnectedBySexO[3,] + ConnectedBySexO[2,]
ConnectedBySexO <- rbind(ConnectedBySexO, Agreement)
Disagreement <- ConnectedBySexO[5,] + ConnectedBySexO[6,]
ConnectedBySexO <- rbind(ConnectedBySexO, Disagreement)


write.table(ConnectedBySexO, "clipboard", sep="\t")


#####Department Eval######


factor(r2$Department)
factor(r2$ServiceArea)

r2$count <- 1
DepartmentReviews <- r2 %>%
  group_by(Department, DeptReview) %>%
  summarise(Frequency = sum(count))
tenure <- subset(CouncilNow, select = c(DIST, owner_occ, renter_occ))
data_long <- gather(tenure, "tenure", "total", 2:3)

DeptWide <- pivot_wider(DepartmentReviews, names_from = DeptReview, values_from = Frequency )
DeptWide$Yes <- ifelse(is.na(DeptWide$Yes), 0, DeptWide$Yes)
DeptWide$No <- ifelse(is.na(DeptWide$No), 0, DeptWide$No)

write.table(DeptWide, "clipboard", sep="\t")





SAReview <- r2 %>%
  group_by(ServiceArea, DeptReview) %>%
  summarise(Frequency = sum(count))

SAWide <- pivot_wider(SAReview, names_from = DeptReview, values_from = Frequency )
SAWide$Yes <- ifelse(is.na(SAWide$Yes), 0, SAWide$Yes)
SAWide$No <- ifelse(is.na(SAWide$No), 0, SAWide$No)
SAWide$Percent_No_Response <- SAWide$No/ (SAWide$No+SAWide$Yes)

write.table(SAWide, "clipboard", sep="\t")


####### Final Graphics#####

ggplot(r2, aes(fct_rev(fct_infreq(Classification)))) + geom_bar( fill = "#00467F") + coord_flip()+
  ggtitle("Respondent Classification") + 
  xlab("Classification") + 
  ylab("Frequency of Response")+
  geom_text(aes(label = after_stat(count)),stat="count", hjust = -0.1, color = "black")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

# Ammend Race results to categorize the results more cleanly
r2$Race <- replace(r2$Race, r2$Race == "", "Decline to specify") 
r2$Race <- replace(r2$Race, r2$Race == "Prefer to self-identify_Why?", "Decline to specify") 
r2$Race <- replace(r2$Race, r2$Race == "American Indian/Alaska Native_White", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African American/Black_White", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African American/Black_White_Prefer to self-identify_Mixed Race", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African_African American/Black", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "African_White", "two+") 
r2$Race <- replace(r2$Race, r2$Race == "Prefer to self-identify_Human", "Prefer to self-identify") 




ggplot(r2, aes(fct_rev(fct_infreq(Remote)))) + geom_bar( fill = "#00467F") + coord_flip()+
  ggtitle("Respondent Work Location") + 
  geom_text(aes(label = after_stat(count)),stat="count", hjust = -0.1, color = "black")+
  xlab("Work Location") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )


palette2 = rep("grey", times = 5)

r2 %>%
  mutate(Generation = fct_relevel(Generation, 
                                  "Generation Z (1997-2012)", "Millennials (1981-1996)",
                                  "Gen X (1965 - 1980)", "Boomer (1946 - 1964)", 
                                  "Silent (1928 - 1945)")) %>%
  ggplot(aes(Generation)) + geom_bar(fill = "#00467F") + coord_flip()+
  ggtitle("Generation") + 
  xlab("Generation") +  
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )


r2 %>%
  mutate(Tenure = fct_relevel(Tenure, "15 + years", "10 - 14 years", "5 - 9 years", 
                              "3 - 4 years", "19 months - 2 years", "13 - 18 months",
                              "7 - 12 months", "1 - 6 months")) %>%
  ggplot(aes(Tenure)) +  geom_bar(fill = "#00467F") + coord_flip()+
  ggtitle("Tenure") + 
  xlab("Time Range") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )


r2 %>%
  mutate(LeadershipDecisions = fct_relevel(LeadershipDecisions, 
                                           "Strongly Strongly Disagree", "Strongly Disagree", "Neutral", 
                                           "Agree", "Strongly Agree")) %>%
  ggplot(aes(LeadershipDecisions, fill = ServiceArea)) + geom_bar() + coord_flip()+
  ggtitle("Agreement with Leadership Decisions") + 
  xlab("Agreement Scale") + 
  ylab("Frequency of Response")

theme(legend.position = "none")



r2 %>%
  mutate(ConnedtTeam = fct_relevel(ConnedtTeam, 
                                   "Strongly Strongly Disagree", "Strongly Disagree", "Neutral", 
                                   "Agree", "Strongly Agree")) %>%
  ggplot(aes(ConnedtTeam, fill = `CheckIns`)) + geom_bar() + coord_flip() +
  ggtitle("Team Connection") + 
  xlab("Did Dept Review?") + 
  ylab("Frequency of Response")

r2 %>%
  mutate(CheckIns = fct_relevel(CheckIns, 
                                "Strongly Strongly Disagree", "Strongly Disagree", "Neutral", 
                                "Agree", "Strongly Agree")) %>%
  ggplot(aes(CheckIns, fill=ConnedtTeam)) + geom_bar() + coord_flip() +
  ggtitle("Freqent Enough Checkin w/ Manager") + 
  xlab("Are Checkins Frequent?") + 
  ylab("Frequency of Response")

ggplot(r2, aes(DeptReview,  fill = Department)) + geom_bar() + coord_flip() +
  ggtitle("Department Review Responses") + 
  xlab("Did Dept Review?") + 
  ylab("Frequency of Response")

# Service Area
# ordered by frequency
#ggplot(r2, aes(fct_reorder(ServiceArea, desc(factor(ServiceArea))), fill = ServiceArea)) + geom_bar() + coord_flip()+

# ordered by Service Area Name

ggplot(r2, aes(fct_rev(fct_infreq(ServiceArea)))) + geom_bar(fill = "#00467F")  + coord_flip()+
  ggtitle("Service Areas of Responders") + 
  geom_text(aes(label = after_stat(count)),stat="count", hjust = -0.3, color = "black")+
  xlab("Service Area") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )


ggplot(r2, aes(fct_rev(fct_infreq(Department)))) + geom_bar(fill = "#00467F") + coord_flip()+
  ggtitle("Department of Responders") + 
  geom_text(aes(label = after_stat(count)),stat="count", hjust = -0.3, color = "black")+
  xlab("Department") + 
  ylab("Frequency of Response")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )



# gendercheck <- subset(r2, Gender != "Woman")
# gendercheck <- subset(gendercheck, Gender != "Man")
# gendercheck <- subset(gendercheck, Gender != "")
# rm(gendercheck)
ggplot(r2, aes(fct_rev(fct_infreq(Gender)))) + geom_bar(fill = "#00467F") + coord_flip()+
  ggtitle("Respondent Gender") + 
  xlab("Gender") + 
  ylab("Frequency of Response") +
  geom_text(aes(label = after_stat(count)),stat="count", hjust = -0.3, color = "black")+
  theme(text=element_text(size=16,  family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )



#####Decline to Specify#####
unique(r2$Gender)
unique(r2$Race)
unique(r2$SexualOrientation)

DTS <- matrix(c(
  100*sum(r2$Gender == "Decline to Specify")/904,
  100*sum(r2$Race == "Decline to specify")/904,
  100*sum(r2$SexualOrientation == "Decline to specify")/904,
  "",
  100*sum(r2$Gender == "Decline to Specify" & r2$Race == "Decline to specify")/904,
  100*sum(r2$Gender == "Decline to Specify"& r2$SexualOrientation == "Decline to specify")/904,
  "",
  "",
  100*sum(r2$Race == "Decline to specify"& r2$SexualOrientation == "Decline to specify")/904,
  100*sum(r2$Gender == "Decline to Specify" & r2$Race == "Decline to specify"& r2$SexualOrientation == "Decline to specify")/904,
  "",
  ""
),ncol=3, byrow=TRUE)
colnames(DTS) <- c("Gender","Race","Sexual Orientation")
rownames(DTS) <- c("","Gender","Race", "all")

write.table(DTS, "clipboard", sep="\t")

DTS_nums <- matrix(c(
  sum(r2$Gender == "Decline to Specify"),
  sum(r2$Race == "Decline to specify"),
  sum(r2$SexualOrientation == "Decline to specify"),
  "",
  sum(r2$Gender == "Decline to Specify" & r2$Race == "Decline to specify"),
  sum(r2$Gender == "Decline to Specify"& r2$SexualOrientation == "Decline to specify"),
  "",
  "",
  sum(r2$Race == "Decline to specify"& r2$SexualOrientation == "Decline to specify"),
  sum(r2$Gender == "Decline to Specify" & r2$Race == "Decline to specify"& r2$SexualOrientation == "Decline to specify"),
  "",
  ""
),ncol=3, byrow=TRUE)
colnames(DTS_nums) <- c("Gender","Race","Sexual Orientation")
rownames(DTS_nums) <- c("","Gender","Race", "all")

write.table(DTS_nums, "clipboard", sep="\t")
