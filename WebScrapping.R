install.packages('rvest')

# Loading the rvest package
library('rvest')
# Specify the url for desired website to be scrapped
url <- 'http://www.espn.com/nfl/statistics/player/_/stat/passing/sort/passingYards/year/2016/seasontype/2'
# Reading the HTML code from the website
webpage <- read_html(url)

# Using CSS selectors to scrape the table
# Or values <- html_nodes(webpage, ".tablehead") %>% html_text()
values <- html_table(webpage)

# Convert from a list to a data frame
  # ncol is the # of columns (could use nrow for # of rows)
table <- data.frame(matrix(unlist(values), ncol=14, byrow=F))

# Check the data type of the values in the 1st column
#### sapply(table[1,], class) ####
# Convert data
####
# Set the column names for a table
#### table <- setNames(test, c("RK", "Player", "Team", "Comp", "Att", "Pct", "Yds", "Yds/a", "Long", "TD", "Int", "Sack", "Rate", "Yds/g")) ####

# Set the column names to be the 1st row (as character/string)
colnames(table) = lapply(table[1, ], as.character)

# Get rid of the rows where the variable RK == "RK"
### table <- subset(table, RK != "RK")

# Get rid of duplicate rows
table <- unique(table)
# Remove the 1st row (it's the column name, not actual values)
table <- table[-1,]

View(table)




