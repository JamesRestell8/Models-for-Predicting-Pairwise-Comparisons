fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  
  return(rbind(d1, d2))
}

data1 <- read.csv("seasons/1516.csv")
data2 <- read.csv("seasons/1617.csv")
data3 <- read.csv("seasons/1718.csv")
data4 <- read.csv("seasons/1819.csv")
data5 <- read.csv("seasons/1920.csv")
data6 <- read.csv("seasons/2021.csv")
data7 <- read.csv("seasons/2122.csv")
data8 <- read.csv("seasons/2223.csv")
data9 <- read.csv("seasons/2324.csv")


data <- fastmerge(data1, data2)
data <- fastmerge(data, data3)
data <- fastmerge(data, data4)
data <- fastmerge(data, data5)
data <- fastmerge(data, data6)
data <- fastmerge(data, data7)
data <- fastmerge(data, data8)
data <- fastmerge(data, data9)

data.c