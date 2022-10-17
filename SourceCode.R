#load in libs
library(plotly)
library(tidyverse)
library(prob)
library(sampling)
library(dplyr)

#read in file
Forbes_Data <- read.csv("C:\\Users\\James Bloor\\Desktop\\BU\\CS544\\Project\\Forbes_Highest_Paid_Athletes_1990_2020.csv")

#shave out columns that I will not need
Forbes_Data <- Forbes_Data[c(-1)] 
Forbes_Data <- as.data.frame(Forbes_Data)

#Rename sports that have duplicate names
#Auto Racing
Forbes_Data[Forbes_Data=="auto racing" | Forbes_Data=="Auto racing" | 
              Forbes_Data=="Auto Racing (Nascar)" | Forbes_Data=="F1 racing" | 
              Forbes_Data=="F1 Motorsports" | Forbes_Data=="F1"|
              Forbes_Data=="NASCAR" | Forbes_Data=="motorcycle gp"] <- "Auto Racing"
#Forbes_Data[Forbes_Data=="Auto racing"] <- "Auto Racing"

#Baseball
Forbes_Data[Forbes_Data=="baseball"] <- "Baseball"

#Basketball
Forbes_Data[Forbes_Data=="basketball" | Forbes_Data=="NBA"] <- "Basketball"

#Boxing
Forbes_Data[Forbes_Data=="boxing"] <- "Boxing"

#F1
Forbes_Data[Forbes_Data=="F1 racing" | Forbes_Data=="F1 Motorsports" | Forbes_Data=="F1"] <- "F1"

#Golf
Forbes_Data[Forbes_Data=="golf"] <- "Golf"

#Ice Hockey
Forbes_Data[Forbes_Data=="ice hockey" | Forbes_Data=="Hockey"] <- "Ice Hockey"

#Soccer
Forbes_Data[Forbes_Data=="soccer"] <- "Soccer"

#Tennis
Forbes_Data[Forbes_Data=="tennis"] <- "Tennis"

#Cycling
Forbes_Data[Forbes_Data=="cycling"] <- "Cycling"

#American Football
Forbes_Data[Forbes_Data=="NFL"] <- "American Football"

#scatter plot earning 
#need to get ride of legend
earning_scatter_plot <- plot_ly(type = "scatter",data = Forbes_Data, x = ~Year, y = ~earnings....million.,color = ~Year,colors = "Set1",text = ~Name)
earning_scatter_plot <- earning_scatter_plot %>% hide_colorbar()
earning_scatter_plot


#Count on how many times a sport has been represented
#need to edit y axis
SportCount <- Forbes_Data[c(5)]
SportCount$Count <- 1
SportCount <- SportCount %>% group_by(Sport) %>% summarise_all(list(sum))
SportCount <- SportCount[order(-SportCount$Count),]
SportCount_plot <- plot_ly(
  SportCount,
  name = "Sport Count",
  type = "bar",
  x= ~Sport,
  y= ~Count,
  color = ~Sport,
  colors = "Set1",
  showlegend = FALSE)
SportCount_plot<- SportCount_plot %>% layout(yaxis = list(title = 'Title for Y....',
                                        range = c(0, 100)))
SportCount_plot

#Count how many sports have been represented but only one count per athlete
#need to edit y axis
UniqueSportCount <- Forbes_Data[c(1,5)]
UniqueSportCount <- unique(UniqueSportCount)
UniqueSportCount <- UniqueSportCount[c(-1)]
UniqueSportCount$Count <- 1
UniqueSportCount <- UniqueSportCount %>% group_by(Sport) %>% summarise_all(list(sum))
UniqueSportCount <- UniqueSportCount[order(-UniqueSportCount$Count),]
UniqueSportCount_plot <- plot_ly(
  UniqueSportCount,
  name = "Sport Count",
  type = "bar",
  x= ~Sport,
  y= ~Count,color = ~Sport,
  colors = "Set1",showlegend = FALSE
)
UniqueSportCount_plot


#Count how many times a country has been represented
#need to edit y axis
#bars are too tiny
CountryCount <- Forbes_Data[c(2)]
CountryCount$Count <- 1
CountryCount <- CountryCount %>% group_by(Sport) %>% summarise_all(list(sum))
CountryCount <- CountryCount[order(-CountryCount$Count),]
CountryCount_plot <- plot_ly(
  CountryCount,
  name = "Nationality Count",
  type = "bar",
  x= ~Nationality,
  y= ~Count,
  color = ~Nationality,
  colors = "Set1",showlegend = FALSE,
  width = .7,
  bargap = 0.4
)
CountryCount_plot






#count how many countries have been represented but only one count per athlete
#need to edit y axis
UniqueCountryCount <- Forbes_Data[c(1,2)]
UniqueCountryCount <- unique(UniqueCountryCount)
UniqueCountryCount <- UniqueCountryCount[c(-1)]
UniqueCountryCount$Count <- 1
UniqueCountryCount <- UniqueCountryCount %>% group_by(Nationality) %>% summarise_all(list(sum))
UniqueCountryCount <- UniqueCountryCount[order(-UniqueCountryCount$Count),]
UniqueCountryCount_plot <- plot_ly(
  UniqueCountryCount,
  name = "Nationality Count",
  type = "bar",
  x= ~Nationality,
  y= ~Count,
  color = ~Nationality,
  colors = "Set1",showlegend = FALSE
)
UniqueCountryCount_plot

#lets do a summary of who has made the most and a count on haw many times they have appeared in the top 10 
Athlete_Earnings_Count <- Forbes_Data[c(1,2,5,7)]
Athlete_Earnings_Count$Count <- 1
Athlete_Earnings_Count <- Athlete_Earnings_Count %>% group_by(Name,Nationality,Sport) %>% summarise_all(list(sum))




#with the data above we can do a scatter plot
# Athlete_Earnings_Count_scatter_plot <- plot_ly(type = "scatter",data = Athlete_Earnings_Count,
# x = ~Count, y = ~earnings....million.,text = ~Name, color = ~Nationality, colors = "Set1")
# Athlete_Earnings_Count_scatter_plot

Athlete_Earnings_Count_scatter_plot <- plot_ly(type = "scatter",data = Athlete_Earnings_Count, 
                                               x = ~Count, y = ~earnings....million.,text = ~Name, color = ~Sport, colors = "Set1")
Athlete_Earnings_Count_scatter_plot


#box plot
#Pick one variable with numerical data and examine the distribution of the data. 
earning_box_plot <- plot_ly(Forbes_Data , 
                            x = ~earnings....million., type="box", 
                            name = 'Forbes Earnings 1990 - 2020', boxpoints = "all",text = ~Name)
earning_box_plot


#box plot, every sport has a box plot and they go horizontally 
#now lest filter down the data to only count sports that have more than 10 data points
Sports_Over_10_count <- SportCount$Count > 10
getdata(SportCount, Sports_Over_10_count)
p <- plot_ly(Forbes_Data, x = ~earnings....million.[which(Sport == "Basketball")], type="box", name = 'Basketball')
p <- add_trace(p, x = ~earnings....million.[which(Sport == "Boxing")], type="box", name = 'Boxing')
p <- add_trace(p, x = ~earnings....million.[which(Sport == "Golf")], type="box", name = 'Golf')
p <- add_trace(p, x = ~earnings....million.[which(Sport == "Soccer")], type="box", name = 'Soccer')
p <- add_trace(p, x = ~earnings....million.[which(Sport == "Auto Racing")], type="box", name = 'Auto Racing')
p <- add_trace(p, x = ~earnings....million.[which(Sport == "Tennis")], type="box", name = 'Tennis')
p <- add_trace(p, x = ~earnings....million.[which(Sport == "American Football")], type="box", name = 'American Football')
p <- layout(p, yaxis = list(title = 'Top 7 Sports'))
p

#-----------------------------------------------------------CLT---------------------------------------------------------
set.seed(100)
samples <- 150
xbar <- numeric(samples)
par(mfrow = c(2,2))

for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(Forbes_Data$earnings....million., size, replace = FALSE))
  }

  hist(xbar,
       breaks = 20, xlim=c(20,80), ylim = c(0, 40),
       main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}
par(mfrow = c(1,1))


#plot_ly(Forbes_Data, x = ~earnings....million.)


#-----------------------------------sampling examples -- seeing how a sample could be if we were predicting the 100 next spots----------------------
# srswor
set.seed(100)
s <- srswor(50, nrow(Forbes_Data))

sample.1 <- Forbes_Data[s != 0, ]
head(sample.1)

table(sample.1$Sport)

#Systematic Sampling
set.seed(100)
N <- nrow(Forbes_Data)
n <- 50

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

# select every kth item
set.seed(100)

s <- seq(r, by = k, length = n)

sample.2 <- Forbes_Data[s, ]
head(sample.2)

table(sample.2$Sport)


#display these 2 tables are bar graphs now


#sample 1
sample_1_Count <- sample.1[c(5)]
sample_1_Count$Count <- 1
sample_1_Count <- sample_1_Count %>% group_by(Sport) %>% summarise_all(list(sum))
sample_1_Count <- sample_1_Count[order(-sample_1_Count$Count),]
sample_1_Count_plot <- plot_ly(
  sample_1_Count,
  type = "bar",
  x= ~Sport,
  y= ~Count,
  name = "SRSWOR",
  marker = list(color = 'Red'),showlegend = TRUE
)
sample_1_Count_plot


#sample 2
sample_2_Count <- sample.2[c(5)]
sample_2_Count$Count <- 1
sample_2_Count <- sample_2_Count %>% group_by(Sport) %>% summarise_all(list(sum))
sample_2_Count <- sample_2_Count[order(-sample_2_Count$Count),]
sample_2_Count_plot <- plot_ly(
  sample_2_Count,
  type = "bar",
  x= ~Sport,
  y= ~Count,
  name = "Systematic Sampling",
  marker = list(color = 'Green'),showlegend = TRUE
)
sample_2_Count_plot
sample_2_Count_plot <- sample_2_Count_plot %>% layout(title = 'Sample Size: 50')
sample_2_Count_plot

fig <- subplot(sample_1_Count_plot,sample_2_Count_plot)
fig <- fig %>% layout(title = 'Sample Size: 50')
fig




#sampling examples -- seeing how a sample could be if we were predicting the 10 next spots for next year
# srswor
set.seed(100)
s <- srswor(10, nrow(Forbes_Data))

sample.1 <- Forbes_Data[s != 0, ]
head(sample.1)

table(sample.1$Sport)


#Systematic Sampling
set.seed(100)
N <- nrow(Forbes_Data)
n <- 10

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

# select every kth item
set.seed(100)
s <- seq(r, by = k, length = n)

sample.2 <- Forbes_Data[s, ]
head(sample.2)

table(sample.2$Sport)





#sample 1
sample_1_Count <- sample.1[c(5)]
sample_1_Count$Count <- 1
sample_1_Count <- sample_1_Count %>% group_by(Sport) %>% summarise_all(list(sum))
sample_1_Count <- sample_1_Count[order(-sample_1_Count$Count),]
sample_1_Count_plot <- plot_ly(
  sample_1_Count,
  name = "SRSWOR",
  type = "bar",
  x= ~Sport,
  y= ~Count,
  marker = list(color = 'Red'),showlegend = TRUE
)
#sample_1_Count_plot <- sample_1_Count_plot %>% layout(title = 'SRSWOR')
#sample_1_Count_plot


#sample 2
sample_2_Count <- sample.2[c(5)]
sample_2_Count$Count <- 1
sample_2_Count <- sample_2_Count %>% group_by(Sport) %>% summarise_all(list(sum))
sample_2_Count <- sample_2_Count[order(-sample_2_Count$Count),]
sample_2_Count_plot <- plot_ly(
  sample_2_Count,
  type = "bar",
  x= ~Sport,
  y= ~Count,
  name = "Systematic Sampling",
  marker = list(color = 'Green'),showlegend = TRUE
)
sample_2_Count_plot
sample_2_Count_plot <- sample_2_Count_plot %>% layout(title = 'Sample Size: 50')
sample_2_Count_plot

fig <- subplot(sample_1_Count_plot,sample_2_Count_plot)
fig <- fig %>% layout(title = 'Sample Size: 10')
fig
#sample_2_Count_plot <- sample_2_Count_plot %>% layout(title = 'Systematic Sampling')
#sample_2_Count_plot









