data=read.csv("multipleChoiceResponses.csv")
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tibble)

# fread = fast import of a data set 
dataset <- as.tibble(suppressWarnings(fread("multipleChoiceResponses.csv")))

# Changing the job titles to readable names

dataset$CurrentJobTitleSelect[dataset$CurrentJobTitleSelect == "Software Developer/Software Engineer"] <- "Software Engineer"
dataset$CurrentJobTitleSelect[dataset$CurrentJobTitleSelect == "Operations Research Practitioner"] <- "Operations Research"
dataset$MajorSelect[dataset$MajorSelect == "Engineering (non-computer focused)"] <- "Engineering"
dataset$MajorSelect[dataset$MajorSelect == "Information technology, networking, or system administration"] <- "Information technology"
dataset$MajorSelect[dataset$MajorSelect == "Management information systems"] <- "Information systems"

dataset_names <- names(dataset)
dataset_names[dataset_names == "WorkMethodsFrequencyA/B"] <- "WorkMethodsFrequencyABTesting"
dataset_names[dataset_names == "WorkMethodsFrequencyCross-Validation"] <- "WorkMethodsFrequencyCrossValidation"
names(dataset) <- dataset_names

dataset$JobSatisfaction <- suppressWarnings(as.numeric(substr(dataset$JobSatisfaction, start = 1, stop = 2)))


## Frequency of both languages amongst different jobs 

dataset %>%
  rename(Language = LanguageRecommendationSelect, title = CurrentJobTitleSelect) %>%
  filter(Language == "R" | Language == "Python") %>%
  filter(title != "") %>%
  group_by(title, Language) %>%
  count() %>%
  ggplot(aes(title, n, color = Language)) +
  ggtitle("R VS Python Frequency") +
  labs(x = "Job Title", y = "Count") +
  geom_point(size = 6) +

  theme(
    axis.text.x = element_text(angle = 330, hjust = 0),
    axis.title.x = element_text(margin = margin(t = 12))
  )

## Major VS Job Title
# For each profession, show the distribution of the majors 
# within the market. Distribution showed as percentages within the graph.

dataset %>%
  rename(Major = MajorSelect, title = CurrentJobTitleSelect) %>%
  filter(title != "", Major != "") %>%
  group_by(title, Major) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  ggplot(aes(x = title, y = freq, fill = Major, label = ifelse(freq > 8, round(freq), ""))) +
  ggtitle("Major vs. Job Title") +
  labs(x = "Job Title", y = "Frequency (%)") +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.text.x = element_text(angle = 320, hjust = 0)
  )


# Get all column names that begin with "LearningPlatformUsefulness"
# How effective every method of learning, on a scale of 0 to 1. 

study=grep("^LearningPlatformUsefulness", names(dataset), value=T) # all the possible learning study

names <- c()
popularities <- c()
scores <- c()

for (platform in study) {
  usefulness <- dataset %>%
    group_by_(platform) %>%
    count()
  
  # Popularity = the number of people who responded to this question
  popularity <- usefulness[[2]][2] + usefulness[[2]][3] + usefulness[[2]][4]
  
  # Usefulness = a weighted average determining the usefulness of this platform
  score <- (usefulness[[2]][2] * 0 + usefulness[[2]][3] * 0.5 + usefulness[[2]][4] * 1) / popularity
  
  names <- c(names, gsub("LearningPlatformUsefulness", "", platform))
  popularities <- c(popularities, popularity)
  scores <- c(scores, score)
}

scores_df <- data.frame(
  Popularity = popularities,
  Usefulness = scores,
  Name = names
)

ggplot(scores_df, aes(x = Usefulness, y = Popularity)) +
  ggtitle("Effectiveness of Learning Methods") +
  geom_point() +
  geom_text(aes(label = Name, family = "Helvetica"), nudge_y = 200)


