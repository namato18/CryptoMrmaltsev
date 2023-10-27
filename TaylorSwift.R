library(lubridate)
library(dplyr)
library(xgboost)
library(ggplot2)


# Create Inputs -----------------------------------------------
names = c("Joe Jonas","Taylor Lautner","John Mayer",
          "Jake Gyllenhaal","Connor Kennedy","Harry Styles",
          "Calvin Harris","Tom Hiddleston","Joe Alwyn","Matty Healy"
          )
dates.start = c("July 2008","August 2009","November 2009",
                "October 2010","July 2012", "December 2012",
                "March 2015","June 2016","September 2016",
                "May 2023")
dates.ended = c("October 2008","November 2009","February 2010",
                "December 2010","October 2012","January 2013",
                "June 2016","September 2016","March 2023","June 2023")
t.swift.age = rep(33, 10)
t.swift.height = rep(70, 10)

boyfriend.age = c(34, 31, 46, 42, 29, 29, 39, 42, 32, 34)
height.inches = c(67, 68.5, 74.8, 71.65, 72, 72, 77.5, 73.6, 72.8, 68.9)

height.difference = round(height.inches - 70,2)

musician = c(1,0,1,0,0,1,1,0,0,1)
actor = c(0,1,0,1,0,0,0,1,0,0)

US = c(1,1,1,1,1,0,0,0,0,0)

df.t.swift = data.frame(cbind(names,dates.start,dates.ended,t.swift.age,boyfriend.age,height.difference, musician, actor, US))

df.t.swift$age.difference = boyfriend.age - t.swift.age

df.t.swift$dates.start = parse_date_time(dates.start, "%m-%Y")
df.t.swift$dates.ended = parse_date_time(dates.ended, "%m-%Y")

df.t.swift$duration.days = df.t.swift$dates.ended - df.t.swift$dates.start

df.t.swift$taylor.older = 0
df.t.swift$taylor.older[df.t.swift$t.swift.age > df.t.swift$boyfriend.age] = 1

df.t.swift$taylor.taller = 0
df.t.swift$taylor.taller[df.t.swift$height.difference < 0] = 1

df.t.swift$duration.days = as.numeric(df.t.swift$duration.days)

# -----------------------------------------------

# Create training DF and outputs -----------------------------------------------

df.train = df.t.swift %>%
  select(height.difference, musician, actor, US, age.difference, taylor.older, taylor.taller)

df.train$height.difference = as.numeric(df.train$height.difference)
df.train$musician = as.numeric(df.train$musician)
df.train$actor = as.numeric(df.train$actor)
df.train$US = as.numeric(df.train$US)

df.train.m = df.train %>%
  as.matrix()
outcomes = df.t.swift$duration.days


bst = xgboost(data = df.train.m,
              label = outcomes,
              objective = "reg:squarederror",
              max.depth = 10,
              nrounds = 100,
              verbose = FALSE)

prediction.df = data.frame(height.difference = -2,
                           musician = 1,
                           actor = 1,
                           US = 1,
                           age.difference = 6,
                           taylor.older = 0,
                           taylor.taller = 1)


prediction.df = as.matrix(prediction.df)

# -----------------------------------------------

print(paste0(round(predict(bst, prediction.df),3)," days"))

######################################################################

plot.df = data.frame(cbind(df.t.swift$names, df.t.swift$duration.days, df.t.swift$dates.ended))
colnames(plot.df) = c("Name","Duration (days)","Ended")
plot.df$`Duration (days)` = as.numeric(plot.df$`Duration (days)`)

plot.df[11,] = NA

plot.df$Name[11] = "Travis Kelce"
plot.df$`Duration (days)`[11] = round(predict(bst, prediction.df),3)

ggplot(plot.df, aes(x = Ended,y = `Duration (days)`, fill = Name)) + geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 1)) +
  scale_x_discrete(breaks = plot.df$Ended, labels = plot.df$Name) +
  theme(legend.position = "none") +
  ggtitle("Taylor Swift Boyfriends") +
  xlab("Boyfriend Name")

