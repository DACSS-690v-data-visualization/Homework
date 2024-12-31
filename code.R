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
