library(lubridate)

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

boyfriend.age = c(34, 31, 46, 42, 29, 29, 39, 42, 32, 34)

df.t.swift = data.frame(cbind(names,dates.start,dates.ended,t.swift.age,boyfriend.age))

df.t.swift$age.difference = boyfriend.age - t.swift.age

df.t.swift$dates.start = parse_date_time(dates.start, "%m-%Y")
df.t.swift$dates.ended = parse_date_time(dates.ended, "%m-%Y")

df.t.swift$duration.months = df.t.swift$dates.ended - df.t.swift$dates.start

df.t.swift$taylor.older = 0
df.t.swift$taylor.older[df.t.swift$t.swift.age > df.t.swift$boyfriend.age] = 1
