# plot a scatter plot between height and weight of women 
women
scatter<-ggplot(women, aes(height, weight))
scatter +geom_point() +geom_smooth(method="lm", colour = "Red") +labs(x="Weight (kg)", y="Height (cm)")

# Use data set mtcars and plot Boxplot of mpg split by number of cylinders (cyl.f)
mtcars
mtcarsBoxplot<- ggplot(mtcars, aes(cyl, mpg))
mtcarsBoxplot + geom_boxplot() +labs(x="Cylinders", y="mpg")

# Bar chart for one independent variable
# Use dataset SpiderLong.dat shared in module 3 and plot a bar of Anxiety (y-axis) for each Group (x-axis)
spiderlong<-read.delim("SpiderLong.dat", header=TRUE)
head(spiderlong)
bar<-ggplot(spiderlong, aes(Group, Anxiety))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Group", y = "Anxiety")
