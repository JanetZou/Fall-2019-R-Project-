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
library(scales)


df1 <- group_by(df, Platform)
summ <- summarize(df1, sum_sales = sum(NA_Sales))
summ

q1<- qplot(num_game, sum_sales, data = summ, geom = "point",color = Platform)
q1 <- q1 + scale_color_hue(name = "Platform")
q1 <- q1 + scale_x_continuous(name = "Number of Games")
q1 <- q1 + scale_y_continuous(name = "NA Sales (Millions)", labels = dollar)
q1 <- q1 + ggtitle("Platform Success")
q1 <- q1 + theme(plot.title = element_text(size = 24, face = "bold", hjust=0.5),
                 axis.title = element_text(size = 14))
q1
ggsave(filename = "Q1.jpg", plot = q1, width = 12, height = 8, units = "in")

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

df7 <- select(df, Publisher, Genre, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales)

# Select and keep only top 10 publishers
top_10_publishers <- c("Nintendo","Electronic Arts","Activision","Sony Computer Entertainment","Ubisoft",
                       "Take-Two Interactive","THQ","Microsoft Game Studios","Atari","Sega")

df7 <- filter(df7, Publisher == top_10_publishers)

df7 <- df7[complete.cases(df7), ]
df7 <- group_by(df7, Publisher, Genre)
summ7 <- summarize(df7, 
                   sum_NA_Sales = sum(NA_Sales), 
                   #sum_EU_Sales = sum(EU_Sales), 
                   #sum_JP_Sales = sum(JP_Sales), 
                   #sum_Other_Sales = sum(Other_Sales), 
                   sum_Global_Sales = sum(Global_Sales)
                   )

p7.1 <- qplot(Publisher, Genre, data = summ7, geom = "bin2d", fill = sum_NA_Sales)
p7.1 <- p7.1 + theme(axis.text.x = element_text(angle = 15))
p7.1 <- p7.1 + theme(panel.grid.major.y = element_blank()) + theme(panel.grid.major.x = element_blank())
ggsave(filename = "p7.1.jpg", plot = p7.1, width = 12, height = 8,
       units = "in")

p7.2 <- qplot(Publisher, Genre, data = summ7, geom = "bin2d", fill = sum_Global_Sales)
p7.2 <- p7.2 + theme(axis.text.x = element_text(angle = 15))
p7.2 <- p7.2 + theme(panel.grid.major.y = element_blank()) + theme(panel.grid.major.x = element_blank())
ggsave(filename = "p7.2.jpg", plot = p7.2, width = 12, height = 8,
       units = "in")



