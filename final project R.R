
rm(list = ls())
#url<-""
#download.file(url,"Video_Games_Sales_as_at_22_Dec_2016.csv")

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



# 1 Grace. Which video game platform is most successful, in terms of the game bases and NA sales?  (bar(uptodtae sum total))



# 2 Grace. And which platform would we recommend based on the trend? （timeseries line chart; looking for trends） 



# 3 Andy. How is genre preference (user_score) in North America (x: genre; y: avg user score; bar/heatmap) ?  



# 4. What are the top 10 video game publisher (intelligent property: based on ratings and sales) in the North America market (x: publisher , y: sales; bar chart)



# 5 Andy. Relationship between us sales and global sales (scatter plot)



# 6. How does Genre / platform change in different time period in different region (x: platform y: genre; heat: sale; 4 different region heatmap)



# 7. How does the sales relates to Gener and publishers (heatmap)





