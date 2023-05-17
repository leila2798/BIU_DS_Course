############################################################################
###### Titanic - Analyzing the titanic data
############################################################################
library(dplyr)
library(ggplot2)
library(tidyr)

### Import the titanic dataset
path <- "C:/Users/sofnl/Documents/BIU_DS/BIU_DS_Course/R/HW"

titanic <- read.csv(paste0(path,"/titanic.csv"))
head(titanic)

View(Titanic)

##################################################
### Number of passangers and how many survived (n, %)
##################################################
titanic %>% summarise(cnt = n(), 
                      survived = sum(titanic$Survived), 
                      s_percent = (sum(titanic$Survived)/ n() * 100))
#plot 
titanic%>%ggplot()+
  geom_bar(aes(x=as.factor(Survived)), color = "darkblue", fill = "lightblue", 
           width = 0.6, alpha = 0.4) + 
  scale_x_discrete(name = "Survival", limits=c("1","0"), 
                   labels = c("Survived","Drowned")) +
  labs(title =  "The survival rate")

###################################################
#### Missing values
## How many missing values are in the dataset?
## We can use summary to see the ammount of NA's on each variable
###################################################

titanic$Cabin <- factor(ifelse(titanic$Cabin == "", NA, titanic$Cabin))


res <- NULL
for (i in names(titanic)) {
  res <- rbind(res, cbind(i, sum(is.na(titanic[[i]]))))
}
res

res <- NULL
for (i in names(titanic)) {
  res <- rbind(res, cbind(i, length(unique(titanic[[i]]))))
}
res

summary(titanic)
table(titanic$Sex)

##############################################
## Gender distribution and survival
## How distribute the survivors by gender?
##############################################

titanic %>% 
  group_by(Sex) %>% summarise(cnt = n(),
                              sum = sum(Survived, na.rm = T),
                              mean = mean(Survived, na.rm = T),
                              min = min(Survived, na.rm = T),
                              max=max(Survived, na.rm = T),
                              median = median(Survived, na.rm = T))

## Age distribution and survival
## Plot the Age frequencies of the passangers. (use an histogram)

age_f <- data.frame(titanic %>%
  select(Age) %>% filter(is.na(Age) == F))

#plot
age_f %>% 
  ggplot(aes(Age)) + 
  geom_histogram(binwidth = 4, aes(alpha=0.3), color="darkblue", fill="lightblue")+
  geom_vline(xintercept = mean(age_f$Age), color = "red", linetype="dashed")+
  geom_density(aes(y=4*..count..), colour="black", adjust=4)+
  labs(title="Age frequencies of the passangers",x="Age", y = "Frequency")
  

############################################
### Create a new variable that will divide the passangers in four age categories:
# Babies: 0-5
# Children: 6-12 years old
# Young: 13-17 years old
# Adult: 18-59 years old
# Older: 60+ years old
### How many (number and percent) survived on each age group?
############################################

#creating a new column (Ordinal!)
titanic <- titanic %>% 
  mutate(AgeGroup = factor(case_when(
    is.na(Age) ~ "18-59",
    (Age >= 0 & Age < 6) ~ "0-5",
    (Age >= 6 & Age < 13) ~ "6-12",
    (Age >= 13 & Age < 18) ~ "13-17",
    (Age >= 18 & Age < 60) ~ "18-59",
    (Age >= 60) ~ "60+"), 
    ordered = TRUE, levels = c("0-5", "6-12", "13-17", "18-59", "60+"))
  )

#survived by age group
titanic %>% group_by(AgeGroup) %>% 
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
            )

# Age-Gender Survival
# Were there differences on survival by age group and gender?

titanic %>%
  group_by(Sex, AgeGroup) %>%
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
            )

#Summarizing plot is given in the Paid Fare section

#####################################
## Passenger Class and Survival
## Was there any difference in the survival among passangers 
##by their ticket class (Pclass) ?
#####################################
titanic %>%
  group_by(Pclass) %>%
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
  )


######################################
## Traveling alone vs with family
## Who survived more, individuals that traveled alone or those who traveled with their families?
######################################

titanic <- titanic %>% 
  mutate(TravelAlone = ifelse(SibSp>0, F, ifelse(Parch>0, F, T))) 

titanic %>%  
  group_by(TravelAlone) %>%
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
  )

#plot

titanic %>% ggplot() + 
  geom_bar(aes(x=as.factor(Survived), fill=TravelAlone), position = "dodge", width = 0.7)+
  scale_x_discrete(name = "Survival", limits=c("1","0"), 
                   labels = c("Survived","Drowned")) +
  labs(title="Survival alone vs. with family")

#######################################
## Embarking port and survival
## Was there any difference in survival related to the embarking port?
#######################################

titanic %>% 
  group_by(Embarked) %>%
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
  ) %>% filter(Embarked != "")

#######################################
## Paid fair and survival
## What was the fare range paid by the passangers?
#######################################

titanic %>% 
  summarise(count=n(),
            min.fare=min(Fare),
            max.fare = max(Fare),
            mean.fare=mean(Fare, na.rm=T),
            #median.fare = median(Fare),     ##=fare50
            sd.fare = sd(Fare, na.rm = T),
            fare25 = quantile(Fare, 0.25),
            fare50 = quantile(Fare, 0.5),
            fare75 = quantile(Fare, 0.75)
  )
  
#plot
titanic %>%    
  ggplot(aes(Fare)) + 
      geom_histogram(binwidth = 10, aes(alpha=0.3), color="darkblue", fill = "lightblue")+
      geom_vline(xintercept = mean(titanic$Fare), color = "red", linetype="dashed")+
      geom_density(aes(y=10*..count..), colour="black", adjust=4)+
      labs(title="Fare of the passangers",x="Fare", y = "Frequency")+
  theme_bw()

#plot
#Summarizing effect of fare and age group on survival by sex
ggplot(titanic, aes(x=as.factor(Survived),y=Fare, color = AgeGroup)) + 
  geom_boxplot(outlier.color="black")+
  scale_x_discrete(name = "Survival", limits=c("1","0"), 
                   labels = c("Survived","Drowned")) +
  labs(title="Survival by fare rates", y = "Fare")+
  ylim(c(0,300))+
  facet_grid(~Sex)

#######################################
## How many individuals didn't paid for their ticket?
#######################################

titanic <- titanic %>% 
  mutate(Gratis = ifelse(Fare==0, T, F)) 
titanic %>% summarise(gratis=sum(Gratis))
#table(titanic$Gratis)

########################################
## Does this affected their survival?
########################################

titanic %>% 
  group_by(Gratis) %>%
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
  )
#Looks like yes; statistical test is necessary

#########################################
## Were there differences in fare rates among the same ticket classes?
## If the answer was yes, does those differences affected the survival of individuals?
#########################################
titanic <- merge(titanic,
(titanic%>%group_by(Pclass)%>%
  summarize(fare25 = quantile(Fare, 0.25),
            fare50 = quantile(Fare, 0.5),
            fare75 = quantile(Fare, 0.75))), by="Pclass")
titanic <- titanic%>%
  mutate(FareGroup = ifelse(Fare<fare25, "underpaid", ifelse(Fare>fare75, "overpaid","normal")))

titanic%>%group_by(Pclass, FareGroup) %>%
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
  )

###plot
titanic%>%ggplot()+
  geom_bar(aes(x=Pclass, fill=FareGroup), position = "fill") +
  facet_grid(~Survived)+
  labs(title = "Fare rates effect on survival",  y="Percentage") 

###########################################
## The title passanger had and survival
## Which were the five most common titles passanger had? (Sir, Mr, Mrs, etc)?
## For this part we will take the 'Name' column and will split all the words in the name 
##   by the white space.Then we will join all the words and calculate the frequency of 
##  appearance of each word in descending order. We will take the five most common words 
##  (must be titles), and with them we will create a new column. Then we will procede as 
##  we did in the other analyses.
############################################

words = titanic$Name

## we can use the :punct: opperand from the re (regular expression) package:

words = gsub('[[:punct:] ]+',' ',words)
print(words)

##############################################################################
#my solution

#make a single str
words <- paste(unlist(titanic$Name), collapse = " ")
## or we can use the function removePunctuation from the tm package 
# install.packages("tm")
library(tm)
words<-removePunctuation(words)
print(words)

## now we split the words into a vector.
## strsplit generates a list with one element that is a vector of strings
## we are only interested on the vector, so we get it adding the [[1]]

wordlist = strsplit(words," ")[[1]]
class(wordlist)
length(wordlist)
wordlist

wordcnt <- data.frame(table(wordlist))

wordcnt %>% arrange(desc(Freq)) %>% top_n(15)

## We have shown here that the most common titles were Mr (521), Miss (182), Mrs (129) 
## and Master (40). This totals 872 out of 891 passengers (97.9%).
## Now we will create a new variable with those titles and check for differences on 
## survival among them.

titanic <- titanic %>% 
  mutate(Title = case_when(
    is.na(Name) ~ "",
    grepl("Mrs", Name) ~ "Mrs",
    grepl("Mr", Name) ~ "Mr",
    grepl("Miss", Name) ~ "Miss",
    grepl("Master", Name) ~ "Master",
    TRUE ~ "Other"
  ))

#survived by title
titanic %>% group_by(Title) %>% 
  summarise(count=n(),
            sum(Survived),
            mean=mean(Survived, na.rm=T)            
  )

#plot
titanic%>%ggplot()+
  geom_bar(aes(x=as.factor(Survived), fill=Title), position = "fill", width = 0.7) +
  scale_x_discrete(name = "Survival", limits=c("1","0"), 
                   labels = c("Survived","Drowned")) +
  labs(title =  "The connection between title and survival", y="Percentage")
