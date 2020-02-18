## Quality Arroy Team 7 ## 

install.packages("ggplot2")
library(ggplot2)

#Import excelfile

library(readxl)
mydf <- "Web Analytics Case Student Spreadsheet.xls"
sheets <- excel_sheets(mydf)

#Setting dataframe by sheet
mydf_visit <- read_excel(mydf, skip = 4, sheet = "Weekly Visits") 
mydf_fin <- read_excel(mydf, skip = 4, sheet = "Financials")
mydf_Lbs <- read_excel(mydf, skip = 4, sheet = "Lbs. Sold")
mydf_day <- read_excel(mydf, skip = 4, sheet = "Daily Visits")

#Adjustment
mydf_visit$`Week (2008-2009)` <- as.character(mydf_visit$`Week (2008-2009)`)
mydf_fin$`Week (2008-2009)` <- as.character(mydf_fin$`Week (2008-2009)`)

#Merging data (Weekly Visits + Financials)
mydf_m <- merge(mydf_visit, mydf_fin, by ="Week (2008-2009)", sort = FALSE)
head(mydf_m) # check the result

#Descriptive Statistics - Bar Chart

#1) Unique visits per week

barplot(mydf_visit$`Unique Visits`, names.arg = mydf_visit$`Week (2008-2009)`, main = '1. Unique visits per week')
mydf_visit$`Week (2008-2009)` <- factor(mydf_visit$`Week (2008-2009)`, levels = mydf_visit$`Week (2008-2009)`)

#2) Revenue per week

barplot(mydf_fin$Revenue, names.arg = mydf_fin$`Week (2008-2009)`, main = '2. Revenue per week')
mydf_fin$`Week (2008-2009)` <- factor(mydf_fin$`Week (2008-2009)`, levels = mydf_fin$`Week (2008-2009)`)

#3) Profit per week

barplot(mydf_fin$Profit, names.arg = mydf_fin$`Week (2008-2009)`, main = '3. Profit per week')

#4) Lbs. sold per week

barplot(mydf_fin$'Lbs. Sold', names.arg = mydf_fin$`Week (2008-2009)`, main = '4. Lbs sold per week')

#Data by the period

#Creat dataframe for each period
initial <- mydf_m[1:14, ]
pre_promotion <- mydf_m[15:35, ]
promotion <- mydf_m[36:52, ]
post_promotion <- mydf_m[53:66, ]

#Difined function
my_func <- function(x){
  my_mean <- mean(x)
  my_median <- median(x)
  my_sd <- sd(x)
  my_min <- min(x)
  my_max <- max(x)
  my_summary_measure <- c(my_mean, my_median, my_sd, my_min, my_max)
  return(my_summary_measure)
  }

#Initial period
initial_vis <- my_func(x = initial$Visits) # Visits
initial_uniq <- my_func(x = initial$'Unique Visits') # Unique Visits
initial_rev <- my_func(x = initial$Revenue) # Revenue
initial_pro <- my_func(x = initial$Profit) # Profit
initial_sold <- my_func(x = initial$'Lbs. Sold') # Lbs. Sold

#Setting dataframe
initial_summary <- data.frame("Visits"=initial_vis, "Unique Visits"=initial_uniq, "Revenue"=initial_rev, "Profit"=initial_pro, "Lbs.Sold"=initial_sold)
rownames(initial_summary) <- c("mean", "median", "std. dev.", "minimum", "maximum")
initial_summary

#Pre-promotion period
pre_vis <- my_func(x = pre_promotion$Visits) # Visits
pre_uniq <- my_func(x = pre_promotion$'Unique Visits') # Unique Visits
pre_rev <- my_func(x = pre_promotion$Revenue) # Revenue
pre_pro <- my_func(x = pre_promotion$Profit) # Profit
pre_sold <- my_func(x = pre_promotion$'Lbs. Sold') # Lbs. Sold

#Setting dataframe
pre_summary <- data.frame("Visits"=pre_vis, "Unique Visits"=pre_uniq, "Revenue"=pre_rev, "Profit"=pre_pro, "Lbs.Sold"=pre_sold)
rownames(pre_summary) <- c("mean", "median", "std. dev.", "minimum", "maximum")
pre_summary

#Promotion period
promo_vis <- my_func(x = promotion$Visits) # Visits
promo_uniq <- my_func(x = promotion$'Unique Visits') # Unique Visits
promo_rev <- my_func(x = promotion$Revenue) # Revenue
promo_pro <- my_func(x = promotion$Profit) # Profit
promo_sold <- my_func(x = promotion$'Lbs. Sold') # Lbs. Sold

#Setting dataframe
promo_summary <- data.frame("Visits"=promo_vis, "Unique Visits"=promo_uniq, "Revenue"=promo_rev, "Profit"=promo_pro, "Lbs.Sold"=promo_sold)
rownames(promo_summary) <- c("mean", "median", "std. dev.", "minimum", "maximum")
promo_summary

#Post-Promotion period
post_vis <- my_func(x = post_promotion$Visits) # Visits
post_uniq <- my_func(x = post_promotion$'Unique Visits') # Unique Visits
post_rev <- my_func(x = post_promotion$Revenue) # Revenue
post_pro <- my_func(x = post_promotion$Profit) # Profit
post_sold <- my_func(x = post_promotion$'Lbs. Sold') # Lbs. Sold

#Setting dataframe
post_summary <- data.frame("Visits"=post_vis, "Unique Visits"=post_uniq, "Revenue"=post_rev, "Profit"=post_pro, "Lbs.Sold"=post_sold)
rownames(post_summary) <- c("mean", "median", "std. dev.", "minimum", "maximum")
post_summary

#Mean values for each periods

#Visits
mean_vis <- c(initial_summary[1,1],pre_summary[1,1],promo_summary[1,1],post_summary[1,1])
#Unique Visits
mean_uniq <- c(initial_summary[1,2],pre_summary[1,2],promo_summary[1,2],post_summary[1,2])
#Revenue
mean_rev <- c(initial_summary[1,3],pre_summary[1,3],promo_summary[1,3],post_summary[1,3])
#Profit
mean_pro <- c(initial_summary[1,4],pre_summary[1,4],promo_summary[1,4],post_summary[1,4])
#Lbs. Sold
mean_sold <- c(initial_summary[1,5],pre_summary[1,5],promo_summary[1,5],post_summary[1,5])


# Dataframe
means <- data.frame("Visits"=mean_vis, "Unique Visits"=mean_uniq, "Revenue"=mean_rev, "Profit"=mean_pro, "Lbs.Sold"=mean_sold)
rownames(means) <- c("Initial", "Pre-Promo", "Promotion", "Post-Promo")
means


# Relationship (1)

# Scatter plot - revenue vs ponds sold
ggplot(mydf_m, aes(x=mydf_m$'Lbs. Sold',y=mydf_m$Revenue))+
  geom_point()+
  labs(title="Revenue VS Lbs. Sold", x="Lbs. Sold", y="Revenue")+
  geom_smooth()+
  theme_bw()

# Correlation coefficient 
cor_df1 <- cor.test(mydf_m$Revenue,mydf_m$`Lbs. Sold`)
cor_df1

# Relationship (2)

# Scatter revenue vs visits
ggplot(mydf_m, aes(x=mydf_m$Visits,y=mydf_m$Revenue))+
  geom_point()+
  labs(title="Revenue VS Visits", x="Visits", y="Revenue")+
  geom_smooth()+
  theme_bw()

# Correlation coefficient 
cor_df2 <- cor.test(mydf_m$Revenue,mydf_m$Visits)
cor_df2

# Details of Lbs. Sold

# 1) Summary(Mean, Median, SD, Min. Max.)
lbs_summary <- my_func(x = mydf_Lbs$'Lbs. Sold')
lbs_summary

# 2) Shape of Lbs Sold - Bell shape? 
ggplot(mydf_Lbs, aes(x=mydf_Lbs$'Lbs. Sold'))+
  geom_histogram(bins = 30)+
  labs(title="Lbs. Sold(2005-2010)", x="Ponds of Material sold")

# 3) Statistics by empirical rule

# 3)-1. Theoritical Lbs.Sold
interval_1 <- sum(mydf_Lbs$`Lbs. Sold`)* 0.68 # Intetval_1
interval_1
interval_2 <- sum(mydf_Lbs$`Lbs. Sold`)* 0.95 # Interval_2
interval_2
interval_3 <- sum(mydf_Lbs$`Lbs. Sold`)* 0.99 # Interval_3
interval_3

# 3)-2. Actual Lbs.Sold

# Input Zvalue in a new column
mydf_Lbs$z_value <- c()
for(i in 1:length(mydf_Lbs$`Lbs. Sold`)){
  mydf_Lbs$z_value[i] <- (mydf_Lbs$`Lbs. Sold`[i]-mean(mydf_Lbs$`Lbs. Sold`))/sd(mydf_Lbs$`Lbs. Sold`) 
}
mydf_Lbs

interval_1_act <- mydf_Lbs[mydf_Lbs$z_value<=1 & mydf_Lbs$z_value>=-1, ] # Intetval_1
sum(interval_1_act$`Lbs. Sold`)
interval_2_act <- mydf_Lbs[mydf_Lbs$z_value<=2 & mydf_Lbs$z_value>=-2, ] # Intetval_2
sum(interval_2_act$`Lbs. Sold`)
interval_3_act <- mydf_Lbs[mydf_Lbs$z_value<=3 & mydf_Lbs$z_value>=-3, ] # Intetval_3
sum(interval_3_act$`Lbs. Sold`)


