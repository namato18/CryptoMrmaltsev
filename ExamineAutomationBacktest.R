reef.30min = read.csv("df.examine.reefusdt.30min.csv")
reef.1hr = read.csv("df.examine.reefusdt.1hr.csv")
reef.4hr = read.csv('df.examine.reefusdt.csv')
reef.4hr.360day = read.csv('df.examine.reefusdt.4hour.360day.csv')
reef.4hr.720day = read.csv('df.examine.reefusdt.4hour.720day.csv')

ind = which(reef.4hr.360day$Confidence == 0.1 & reef.4hr.360day$Target == 1.2 & reef.4hr.360day$TakeProfit == 2.6)
reef.4hr.360day$PL[ind]

