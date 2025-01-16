# Homework 1 --------------------------------------------------------------

#Step 1: clean the environment
rm(list = ls()) # Remove all objects from memory

#Step 2: load and retrieve data
location = 'https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file = 'eduwa.rda'
link = paste0(location,file)

#get the data TABLE from the file in the cloud:
load(file = url(link))

#inspect data
dim(eduwa) #check dimensions
names(eduwa) #check column names
table(eduwa$LocaleType) #check different amount of locale types

#Step 3: filter data by locale type
suburbEduwa = eduwa[eduwa$LocaleType=='Suburb',] #choose suburb locale
suburbEduwa$LocaleSub = droplevels(suburbEduwa$LocaleSub)#drop unused levels
table(suburbEduwa$LocaleSub) #check filtered data

#Step 4: prepare to plot
absoluteT = table(suburbEduwa$LocaleSub, #get absolute values
                  exclude = 'nothing')
names(absoluteT)[4] = 'Unknown' #label missing values unknown
absoluteT

propT = prop.table(absoluteT) * 100 #make absolute count into percentages
propT

(tableFreq = as.data.frame(absoluteT)) #turn frequency table into data frame
names(tableFreq) = c('LocaleSub', 'Count') #specify column names
tableFreq$Percent = as.vector(propT) #add percentages
tableFreq

LABELS = paste0(round(tableFreq$Percent, 2), '%') #define labels for percentages

#Step 5: plot suburb data
library(ggplot2) #load necessary libraries
library(scales)

base = ggplot(data = tableFreq, #create base plot
              aes(x = reorder(LocaleSub, Percent), #horizontal axis label
                  y = Percent)) + theme_classic() #vertical axis label and simplify theme


plot = base +
  geom_bar(stat = 'identity', fill = 'gray', color = 'black') +
  labs(
    title = 'What is the Distribution of Public Schools in Suburban Areas?',
    subtitle = 'Based on Suburb Sizes (Small, Midsize, Large, or Unknown) of Washington State in 2019',
    x = NULL, #remove x and y axis labels due to redundancy 
    y = NULL,
    caption = 'Source: US Department of Education. The majority of public schools within suburbs are hosted by large suburbs.'
  ) +
  scale_y_continuous(labels = unit_format(suffix = '%')) + #add percentages
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 0.5, face = 'bold'), #bold title
    plot.subtitle = element_text(hjust = 0.5, vjust = 2), #adjust subtitle
    axis.text.x = element_text(angle = 45, hjust = 1), #adjust x axis label angle
    axis.title.x = element_text(size = 15), #adjust x axis title size
    axis.title.y = element_text(size = 15), #adjust y axis title size 
  )

newPlot = plot +
  geom_text(aes(label = LABELS), vjust = -0.5, size = 4) #make percentage labels higher on plot
print(newPlot)

saveRDS(list(plot = newPlot, labels = LABELS), file = 'newPlot.rds') #save plot as rds file

# Homework 2: Arrest Type-Age Plot --------------------------------------------------------------

#Step 1: clean the environment
rm(list = ls()) # Remove all objects from memory

#Step 2: load and retrieve data
linkMass = "https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"
library(rio) 
library(dplyr)
library(ggplot2)
library(janitor) #load necessary libraries
arrests = rio::import(linkMass, which = 1)

#Step 3: filter and merge data 
sheet_2 = rio::import(linkMass, which = 2, skip = 74) %>% #load sheet 2, skip first 74 rows
  clean_names() %>% #standardize names
  select(code, arrest_type) #select relevant columns

arrests = arrests %>%
  filter(!is.na(`Arrest Type`) & !is.na(Age)) %>% #remove missing values
  left_join(sheet_2, by = c('Arrest Type' = 'code')) #join arrest type description with code
  
head(arrests) #inspect data
summary(arrests)

#Step 4: prepare data
arrest_summary = arrests %>%
  group_by(arrest_type, Age) %>% #group by arrest type and age
  summarise(count = n(), .groups = 'drop') #count arrests by type and age

arrest_summary$arrest_type = factor(
  arrest_summary$arrest_type,
  levels = c('Felony', 'Misdemeanor', 'Warrant', 'Other')) #order by arrest type weight

#Step 5: create plot
plot2 = ggplot(arrest_summary, aes(x = Age, y = count, fill = arrest_type)) + 
  geom_bar(stat = 'identity', color = 'black', fill = 'grey', alpha = 0.7) + 
  facet_wrap(~arrest_type, scales = 'free_y') + #create facet plot
  scale_x_continuous(
    breaks = seq(0, max(arrest_summary$Age), by = 10),  #break every 10 years
    labels = seq(0, max(arrest_summary$Age), by = 10)   #label every 10 years
  ) +
  labs( #create plot labels
    title = 'Young Adults Face Higher Arrest Rates Across Arrest Type Categories in Massachusetts',
    subtitle = 'Among younger age groups, Felony and Misdemeanors are more frequent, Warrants have a moderate frequency and Others is less frequent.',
    caption = 'Source: Massachusetts State Police from the Field Services and Investigative Divisions from January 2019 to March 2020',
    x = 'Age',
    y = 'Number of Arrests',
    fill = 'Arrest Type'
  ) +
  theme_minimal() + #choose minimal theme
  theme(
    strip.text = element_text(size = 10, face = 'bold'),
    strip.background = element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(color = 'black'), #resize and recolor x and y axes texts and titles
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(color = 'black'),
    axis.title.y = element_text(size = 12)
  )

saveRDS(plot2, file = 'plot2.rds') #save plot as rds file
