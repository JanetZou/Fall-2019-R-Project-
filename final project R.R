#Miscellaneous
rm(list = ls())
#url<-""
#download.file(url,"Video_Games_Sales_as_at_22_Dec_2016.csv")

#library(readr)
#library(dplyr)
#library(ggplot2)
#library(ggpubr)

#Data Cleaning
my_col_types <-
  cols(
    Name = col_character(),
    Platform = col_factor(),
    Year_of_Release = col_date(format="%Y"),
    Genre = col_factor(),
    Publisher = col_factor(),
    NA_Sales =col_double(),
    EU_Sales =col_double(),
    JP_Sales =col_double(),
    Other_Sales =col_double(),
    Global_Sales =col_double(),
    Critic_Score = col_integer(),
    Critic_Count = col_integer(),
    User_Score = col_double(),
    User_Count = col_integer(),
    Developer = col_factor(),
    Rating = col_factor()
  )
my_na = c("", "NA", "N/A")
df <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv", col_types = my_col_types, na = my_na)
df
str(df)

# Analysis Preparation 
df$NOT_NA_Sales<-df$Global_Sales-df$NA_Sales


#Data Analysis & Visulization 
# 1 Grace. Which video game platform is most successful, in terms of the game bases and NA sales?  (bar(uptodtae sum total))

suppressPackageStartupMessages(library(ggplot2))
library(dplyr)
library(reshape2)


df1 <- group_by(df, Platform)
summ <- summarize(df1, sum_sales = sum(NA_Sales))
summ
summ$sum_sales<-as.numeric(summ$sum_sales)
qplot(Platform, data = summ, geom = "bar")

# 2 Grace. And which platform would we recommend based on the trend? （timeseries line chart; looking for trends） 



# 3 How is genre preference in North America vs global preference  
p3.1<-qplot(Genre,NA_Sales, data = subset(df, !is.na(Genre)), geom = "boxplot", log = "y")
p3.2<-qplot(Genre,NOT_NA_Sales, data = subset(df, !is.na(Genre)), geom = "boxplot", log = "y")
ggsave(filename = "p3.1.jpg", plot = p3.1, width = 12, height = 8,
       units = "in")
ggsave(filename = "p3.2.jpg", plot = p3.2, width = 12, height = 8,
       units = "in")

# 4. What are the top 10 video game publisher (intelligent property: based on ratings and sales) in the North America market (x: publisher , y: sales; bar chart)



# 5 Relationship between us sales and the reset of the world
p5.1<-ggscatter(df, x = "NA_Sales", y = "NOT_NA_Sales", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
p5.2<-ggscatter(subset(df,NA_Sales<=2 & NOT_NA_Sales<=2), x = "NA_Sales", y = "NOT_NA_Sales", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggsave(filename = "p5.1.jpg", plot = p5.1, width = 12, height = 8,
       units = "in")
ggsave(filename = "p5.2.jpg", plot = p5.2, width = 12, height = 8,
       units = "in")

# 6. How does Genre / platform change in different time period in different region (x: platform y: genre; heat: sale; 4 different region heatmap)



# 7. How does the sales relates to Gener and publishers (heatmap)





