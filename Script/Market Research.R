# Setup
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

options(digits = 3)
options(scipen = 999)

# Reading the Data
courses <- read_xlsx(file.path(getwd(),"Raw_Data","Market Research.xlsx"),col_names = TRUE, sheet = "Competitor Courses") %>% filter(!is.na(Centre))

#histograms
## Centre/Skill
courses %>% group_by(Centre) %>% mutate(count = n()) %>% 
  group_by(Centre,`Industry/Skill`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(Centre = reorder(Centre,desc(count))) %>%
  ggplot(aes(Centre,compound_count,fill = `Industry/Skill`)) + geom_bar(stat = "identity",color = "black") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_manual(values = c(brewer.pal(12,"Set3"),brewer.pal(9,"Set1"),brewer.pal(8,"Accent"),brewer.pal(8,"Set2"))) + 
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Count")

## Skill/Centre
courses %>% group_by(`Industry/Skill`) %>% mutate(count = n()) %>% 
  group_by(Centre,`Industry/Skill`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(count))) %>%
  ggplot(aes(`Industry/Skill`,compound_count,fill = Centre)) + geom_bar(stat = "identity",color = "black") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_manual(values = c(brewer.pal(12,"Set3"),brewer.pal(9,"Set1"),brewer.pal(8,"Accent"),brewer.pal(8,"Set2"))) + 
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(y = "Count", x = "Categories")

##Centre/Event Type
courses %>% group_by(Centre) %>% mutate(count = n()) %>% 
  group_by(Centre,`Event Type`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(Centre = reorder(Centre,desc(count))) %>%
  ggplot(aes(Centre,compound_count,fill = `Event Type`)) + geom_bar(stat = "identity",color = "black",position = position_stack(reverse = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  ylab("Count")

##Skill/Event Type
courses %>% group_by(`Industry/Skill`) %>% mutate(count = n()) %>% 
  group_by(`Industry/Skill`,`Event Type`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(count))) %>%
  ggplot(aes(`Industry/Skill`,compound_count,fill = `Event Type`)) + geom_bar(stat = "identity",color = "black",position = position_stack(reverse = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  ylab("Count")

##Centre/Exam Certified
courses %>% group_by(Centre) %>% mutate(count = n()) %>% 
  group_by(Centre,`Exam Certified`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(Centre = reorder(Centre,desc(count)),`Exam Certified` = relevel(as_factor(`Exam Certified`),"No")) %>%
  ggplot(aes(Centre,compound_count,fill = `Exam Certified`)) + geom_bar(stat = "identity",color = "black") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_brewer(palette = "Paired",direction = -1) +
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  ylab("Count")

##Skill/Exam Certified
courses %>% group_by(`Industry/Skill`) %>% mutate(count = n()) %>% 
  group_by(`Industry/Skill`,`Exam Certified`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(count)),`Exam Certified` = relevel(as_factor(`Exam Certified`),"No")) %>%
  ggplot(aes(`Industry/Skill`,compound_count,fill = `Exam Certified`)) + geom_bar(stat = "identity",color = "black") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_brewer(palette = "Paired",direction = -1) +
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  ylab("Count")

# Distributions

## Cost distribution
courses %>% ggplot(aes(Cost)) + geom_histogram(color = "black", binwidth = 500) + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(breaks = seq(0,50000,2000), expand = c(0,0)) + 
  ggtitle("Distribution of Course Pricing across the market")

## Cost Distribution by skill
courses %>% filter(!is.na(Cost)) %>% ggplot(aes(Cost)) + geom_histogram(color = "black", binwidth = 500) + 
  scale_x_continuous(breaks = seq(0,50000,2000), limits = c(0,NA)) +
  scale_y_continuous(breaks = 1:10) + 
  facet_wrap(~`Industry/Skill`,scales = "free") + 
  ggtitle("Distribution of Course Pricing across Categories")

## Cost Distribution by centre
courses %>% filter(!is.na(Cost)) %>% ggplot(aes(Cost)) + geom_histogram(color = "black", binwidth = 500) + 
  scale_x_continuous(breaks = seq(0,50000,1000), limits = c(0,NA)) +
  scale_y_continuous(breaks = 1:10) + 
  facet_wrap(~Centre,scales = "free") +
  ggtitle("Distribution of Course Pricing across Centres")

##Cost Distribution by certification
courses %>% filter(!is.na(Cost)) %>% ggplot(aes(Cost)) + geom_histogram(color = "black", binwidth = 500) + 
  scale_x_continuous(breaks = seq(0,50000,1000), limits = c(0,NA)) +
  scale_y_continuous(breaks = 1:30) + 
  facet_wrap(~`Exam Certified`, ncol = 2, scales = "free") +
  ggtitle("Distribution of Course Pricing By Certification Status")

# Random Sampling

courses %>% filter(!is.na(Cost), `Exam Certified` == "Yes") %>% 
  group_by(`Industry/Skill`) %>% sample_n(1) %>% ungroup() %>%
  mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(Cost))) %>%
  ggplot(aes(`Industry/Skill`,Cost)) + geom_bar(stat ="identity")

#Summary Statistics
## Average and Median
courses %>% summarize(average = mean(Cost,na.rm = TRUE), median = median(Cost,na.rm = TRUE)) %>% knitr::kable()
courses %>% group_by(Centre) %>% summarize(average = mean(Cost,na.rm = TRUE), median = median(Cost,na.rm = TRUE)) %>% arrange(desc(median)) %>% knitr::kable()
courses %>% group_by(`Industry/Skill`) %>% summarize(average = mean(Cost,na.rm = TRUE), median = median(Cost,na.rm = TRUE)) %>% arrange(desc(median)) %>% knitr::kable()
courses %>% group_by(`Exam Certified`) %>% summarize(average = mean(Cost,na.rm = TRUE), median = median(Cost,na.rm = TRUE)) %>% arrange(desc(median)) %>% knitr::kable()
courses %>% group_by(`Event Type`) %>% summarize(average = mean(Cost,na.rm = TRUE), median = median(Cost,na.rm = TRUE)) %>% arrange(desc(median)) %>% knitr::kable()

##Counts
courses %>% summarize(count = n()) %>% knitr::kable()
courses %>% group_by(Centre) %>% summarize(count = n()) %>% arrange(desc(count)) %>% knitr::kable()
courses %>% group_by(`Industry/Skill`) %>% summarize(count = n()) %>% arrange(desc(count)) %>% knitr::kable()
courses %>% group_by(`Exam Certified`) %>% summarize(count = n()) %>% arrange(desc(count)) %>% knitr::kable()
courses %>% group_by(`Event Type`) %>% summarize(count = n()) %>% arrange(desc(count)) %>% knitr::kable()

## Missing Data
courses %>% group_by(`Industry/Skill`) %>% 
  summarise(Percent_NA = mean(is.na(Cost))*100) %>% 
  filter(Percent_NA != 100) %>%
  arrange(desc(Percent_NA)) %>% top_n(10) %>%
  knitr::kable(col.names = c("Categories","% of missing Data"))

courses %>% group_by(`Exam Certified`) %>% 
  summarise(Percentage_NA = mean(is.na(Cost)) * 100) %>%  
  kable(col.names = c("Certification","% of missing Data"))

## health and safety Courses

courses %>% filter(`Industry/Skill` == "Health and Safety") %>% 
  group_by(Centre) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  kable(col.names = c("Centre","# of Health and Safety Courses"))

courses %>% filter(`Industry/Skill` == "Health and Safety") %>% 
  group_by(`Exam Certified`) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  kable(col.names = c("Certified","# of Health and Safety Courses"))

## Medical
courses %>% filter(`Industry/Skill` == "Medical") %>% 
  group_by(Centre) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  kable(col.names = c("Centre","# of Health and Safety Courses"))

# keyword search
keywords <- c("sustain","enviroment","logistic","supply","procure","health","safety","project","marketing","ICDL","sell","sales","socail"
              ,"الصحة","السلامة","لوجست","امداد","مشتريات","مستدام ","بيئة","تدريب","مشروع","مشاريع","تسويق","مبيعات"
              )

map_df(keywords,function(keywords){
  result <- str_subset(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  ind <- str_which(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  tibble(Event_Name = result, Index = ind) 
}) %>% distinct(Index,.keep_all = TRUE) %>% pull(Event_Name)

## First Aid
keywords <- c("First Aid",
              "اسعافات"
)

map_df(keywords,function(keywords){
  result <- str_subset(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  ind <- str_which(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  tibble(Event_Name = result, Index = ind) 
}) %>% distinct(Index,.keep_all = TRUE) %>% pull(Event_Name)

## Nursing
keywords <- c("Nurs",
              "تمريض","ممرض"
)

map_df(keywords,function(keywords){
  result <- str_subset(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  ind <- str_which(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  tibble(Event_Name = result, Index = ind) 
}) %>% distinct(Index,.keep_all = TRUE) %>% pull(Event_Name)

## PMI

keywords <- c("PMP","PMI","P6")

map_df(keywords,function(keywords){
  result <- str_subset(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  ind <- str_which(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  tibble(Event_Name = result, Index = ind) 
}) %>% distinct(Index,.keep_all = TRUE) %>% pull(Event_Name) 

map_df(keywords,function(keywords){
  result <- str_subset(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  ind <- str_which(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  tibble(Event_Name = result, Index = ind) 
}) %>% distinct(Index,.keep_all = TRUE) %>% select(Event_Name) %>% kable(col.names = "Course Name")

courses %>% filter(`Industry/Skill` == "Project Management", `Exam Certified` %in% c("Yes","External Exam")) %>%
  select(`Event Name`) %>% kable(col.names = "Course Name")

## six sigma agile
keywords <- c("Lean","Agile")

map_df(keywords,function(keywords){
  result <- str_subset(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  ind <- str_which(courses$`Event Name`,regex(keywords, ignore_case = TRUE))
  tibble(Event_Name = result, Index = ind) 
}) %>% distinct(Index,.keep_all = TRUE) %>% pull(Event_Name) 

# Deep Dive

##Certification

courses %>% filter(`Exam Certified` %in% c("Yes","External Exam")) %>% group_by(`Industry/Skill`) %>% mutate(count = n()) %>% 
  group_by(Centre,`Industry/Skill`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(count))) %>%
  ggplot(aes(`Industry/Skill`,compound_count,fill = Centre)) + geom_bar(stat = "identity",color = "black") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_manual(values = c(brewer.pal(12,"Set3"),brewer.pal(9,"Set1"),brewer.pal(8,"Accent"),brewer.pal(8,"Set2"))) + 
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Count")

courses %>% filter(`Exam Certified` == "No") %>% group_by(`Industry/Skill`) %>% mutate(count = n()) %>% 
  group_by(Centre,`Industry/Skill`) %>% summarise(compound_count = n(), count = min(count)) %>%  ungroup() %>%
  mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(count))) %>%
  ggplot(aes(`Industry/Skill`,compound_count,fill = Centre)) + geom_bar(stat = "identity",color = "black") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_manual(values = c(brewer.pal(12,"Set3"),brewer.pal(9,"Set1"),brewer.pal(8,"Accent"),brewer.pal(8,"Set2"))) + 
  scale_y_continuous(breaks = 0:40, expand = c(0,0)) + 
  theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Count")

.## Health and Safety

courses %>% filter(`Industry/Skill` == "Health and Safety") %>% 
  select(Centre,`Event Type`,`Exam Certified`,Cost)

# Market Segmentation

## > 6000 SDG

courses %>% filter(Cost > 6000) %>% group_by(`Industry/Skill`) %>% 
  summarize(count = n()) %>% mutate(`Industry/Skill` = reorder(`Industry/Skill`,desc(count))) %>%
  ggplot(aes(`Industry/Skill`, count)) + geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = 1:5, expand = c(0,0))

courses %>% filter(Cost > 6000) %>% group_by(`Exam Certified`) %>% summarize(Proportion = (n()/ nrow(.))*100) %>% kable(col.names = c("Exam Certified","Percentage of Courses"))

courses %>% filter(`Industry/Skill` == "HR") %>% select(`Event Name`,`Exam Certified`, Cost)

## < 6000 SDG
courses %>% filter(Cost < 6000) %>% group_by(`Industry/Skill`) %>% 
  summarize(count = n()) %>% mutate(`Industry/Skill` = reorder(`Industry/Skill`, desc(count))) %>%
  ggplot(aes(`Industry/Skill`, count)) + geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = 1:30, expand = c(0,0))

courses %>% filter(Cost < 6000) %>% group_by(`Exam Certified`) %>% summarize(Proportion = (n()/ nrow(.))*100) %>% kable(col.names = c("Exam Certified","Percentage of Courses"))

# Segmentation via Machine Learning
library(caret)
library(rpart)
library(rpart.plot)

colnames(courses) <- make.names(colnames(courses))

fit_rpart <- courses %>% select(Centre,Industry.Skill,Location,Languages,Event.Type,Exam.Certified,Cost) %>% 
  train(Cost ~. , method = "rpart", na.action = na.omit, data = .)
summary(fit_rpart)
varImp(fit_rpart)$importance %>% as_tibble(rownames = "Variable") %>% arrange(desc(Overall)) %>% top_n(3) %>% 
  mutate(Variable = c("Exam.Certified.Yes","Exam.Certified.no","Location")) %>% kable()
plot(fit_rpart)
rpart.plot(fit_rpart$finalModel)

