#ggplot lessons
#Written by Arthur Cousson
#Based on ggplot2, the book
#Thoses datasets and functions are not mine.
#This script is just my synthesis work on existing things.

install.packages("directlabels")
library(ggplot2)

#Key components ----
#In this script we will only work with ggplot2 own datasets. 
#They are useful because owere used in scientific articles
#Plus, when you are looking for ggplot scripts, they always are prepared with these datasets
#The following function will detauil the dataset
#MPG stands for MilesPerGallon (le même genre de données que les litres au 100)
?mpg

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, colour = class))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = drv))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = cyl))+
  geom_point()

ggplot(mpg, aes(displ, cty, colour = class)) +
  geom_point()

#Grammar importance ----

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = "blue"))

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(colour = "blue")

#Facetting ----
#There are two types of facetting : grid and wrapped. Wrapped is the most useful
#To facet a plot you simply add a facetting specification with face_wrap(), which takes the name of the variable preceded by ~

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  facet_wrap(~class)

#Styles ----

#geom_smooth() fit a smoother to the data, display it and add standard error

#geom_boxplot() produces a box and whisker to summarise distribution

#geom_histogram() and geom_freqpoly() show the distribution of continuous variables

#geom_bar() shows the distribution of categorical variables

#geom_path() and geom_line() draw lines between data points. 
#A line plot is constrained to produce lines that travel from left to right while paths can go in any direction
#Lines are typically used to explore how things change over time

#Smoother (Regression) ----

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth(method = "loess")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth(span = 0.2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth(span = 1)


#Loess dos not work well for large datasets, so an alternative smoothing algorithm is used when "n" is greater than 1000
#method = "gam" fits a generalised additive model provided by the mgcv package 

library(mgcv)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x))


#Linear model ----

#method = "lm" fits a linear model, giving the best fit

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth(method = "lm")

#method = "rlm" works like lm() but uses a robust fitting algorithm so that outliers don't affect the fit that much.
#It's part of the MASS package so load it before using it

library(MASS)

#Boxplots - Jittered points----

ggplot(mpg, aes(drv, hwy)) +
  geom_point()

#Because a lots of points are on the same position, it's difficult to understand values distributution.

#Jittering, geom_jitter() adds a little random noise to the data which can help avoid overplotting
#Boxplots, geom_boxplots() summarise the shape of the distribution with a handful of summary statistics.
#Violin plots, geom_violin(), show a compact representation of the "density" of the distribution
#Highlighting the areas where more points are found

ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()

#Histograms  - Frequency polygons ----

ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly()

#You can control the bin width of the bins with the binwidth argument.
#This is VERY IMPORTANT to experiment with the bin width. 
#The default just splits your data in 30 bins, which is unlikely to be the best choice.

ggplot(mpg, aes(hwy)) + 
  geom_freqpoly(binwidth = 2.5)

ggplot(mpg, aes(hwy)) + 
  geom_freqpoly(binwidth = 1)

#Comparing distributions ----

ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~drv, ncol = 1)

#Bar charts ----
#Discrete analog of histogram is bar chart
ggplot(mpg, aes(manufacturer)) +
  geom_bar()
#To fix labels check 8.4.2


drugs <- data.frame(
  drug = c("a", "b", "c"),
  effect = c(4.2, 9.7, 6.1)
)

ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")
ggplot(drugs, aes(drug, effect)) + geom_point()

#Times series ----
#Line and path plots are typically used for time series data.
#Line plots usually have time in x-axis, showing how a single variable has changed over time
#Path plots show how two variables have simultaneously changed over time, 
#with time encoded in the way that observations are connected


#The figure below shows two plots of unemployment over time, both produced using geom_lines().
#The first shows the unemployment rate while the second shows the median number of weeks unemployed.
#We can already see some differences in these two variables, particularly in the last peak,
#where the unemployment percentage is lower than it was un the preceeding peaks, but the length of unemployment is high

ggplot(economics, aes(date, unemploy / pop))+
  geom_line()

ggplot(economics, aes(date, uempmed))+
  geom_line()

#To examine this relationship in greater detail, we would like to draw both time series on the same plot
#We could draw a scatterplot of unempoyment rate vs length of unemployment, 
#but then we could no longer see the evolution over time.
#The solution is to join points adjacent in time with line segments, forming a path plot.

#Below, we plot unemployment rate vs length of unemployment and join the individuals observations with a path.
#Because of the many lines crossing the direction in which time flows isn' easy to see in the first plot.
#In the second plot, we colour the points to make it easier to see the direction of time

ggplot(economics, aes(unemploy / pop, uempmed)) +
  geom_path() + 
  geom_point()

year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path(color = "grey50") +
  geom_point(aes(colour = year(date)))

#We can see that unemployment rate and length of unemployment are highly correlated, but in recent years,
#the length of unemployment has been increasing relative to the unemployment rate

#With longitudinal data, you often want to display multiple time series on each plot, 
#each series representing one individual. 
#To do this you need to map the group aesthetic to a variable encoding the group membership of each observation.

#Exercice

#what's the problem with plot created here ?
ggplot(mpg, aes(cty,  hwy)) + geom_point()
#Which geom is the most effective at remedying the problem ? 
#Trys
ggplot(mpg, aes(cty,  hwy)) + geom_path() + geom_point()
ggplot(mpg, aes(cty,  hwy)) + geom_line() + geom_point()
ggplot(mpg, aes(cty,  hwy)) + geom_path(colour = "grey50") + geom_point(aes(colour = model))
ggplot(mpg, aes(cty,  hwy)) + geom_freqpoly() + geom_point()
ggplot(mpg, aes(cty,  hwy)) + geom_point() + geom_smooth()
ggplot(mpg, aes(cty,  hwy)) + geom_point() + geom_boxplot()
ggplot(mpg, aes(cty,  hwy)) + geom_point() + geom_violin()
ggplot(mpg, aes(cty,  hwy)) + geom_point() + geom_jitter()
ggplot(mpg, aes(cty,  hwy)) + geom_histogram() 
ggplot(mpg, aes(cty,  hwy)) + geom_bar(stat = cty) 

#ANswer, I guess
ggplot(mpg, aes(cty,  hwy)) + geom_point() + geom_jitter()
#Le problème vient du fauit qu'un grand nombre de points sont superposés. 
#On perd ainsi quantité d'information sur la graphique.
#On retrouve cette information avec geom_jitter()

#Ordering class

ggplot(mpg, aes(class, hwy)) + geom_boxplot() #ALphabetically, bad
ggplot(mpg, aes(reorder(class, hwy), hwy)) + geom_boxplot()
?reorder()

#Modifying axes ----

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3)

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) +
  xlab("City driving (mpg)") +
  ylab("Highway driving (mpg)")

#Remove axis label with NULL
ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) +
  xlab(NULL) +
  ylab(NULL)
#xlim and ylim
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25)

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25) +
  xlim("f", "r") +
  ylim(20,30)

#For continuous scales, use NA to set only one limit
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25, na.rm = TRUE) +
  ylim(NA, 30)

#Ouputs - saving plots ----

p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl)))+
  geom_point()
#save it to the disk
ggsave("plot.png", width = 5, height = 5)
saveRDS(p, "plot.rds")
q <- readRDS("plot.rds")

#Quick plots ----
#Allows you to quickly plot data without beothering knowing how to plot it

qplot(displ, hwy, data = mpg)
qplot(displ, data = mpg)

qplot(displ, hwy, data = mpg, colour = "blue")      
qplot(displ, hwy, data = mpg, colour = I("blue"))      

#Toolbox----

#Basic plot types ----

df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a", "b", "c")
)

p <- ggplot(df, aes(x, y, label = label)) +
  labs(x = NULL, y= NULL) + #Hide label axis
  theme(plot.title = element_text(size = 12)) #Shrink plot title

p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = "identity") + ggtitle("bar")
p + geom_raster() + ggtitle("raster")
p + geom_line() + ggtitle("line")
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path")
p + geom_polygon() + ggtitle("polygon")


#Labels----

df <- data.frame(x = 1,
                 y = 3:1,
                 family = c("sans", "serif", "mono"))
ggplot(df, aes(x, y)) +
  geom_text(aes(label = family, family = family))

df <- data.frame(x = 1,
                 y = 3:1,
                 face = c("plain", "bold", "italic"))
ggplot(df, aes(x, y)) +
  geom_text(aes(label = face, fontface = face))

df <- data.frame(x = c(1, 1, 2, 2, 1.5),
                 y = c(1, 2, 1 ,2, 1.5),
                 text = c("bottom-left", "bottom-right", 
                          "top-left", "top-right", "center"))

#The next graphic causes problems because you can't read the labels properly
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text))
#So here, you just move them not to be getting outside
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text), vjust = "inward", hjust = "inward")


df <- data.frame(trt = c("a", "b", "c"), resp = c(1.2, 3.4, 2.5))
ggplot(df, aes(resp, trt)) + 
  geom_point() +
  geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.25) + 
  xlim(1, 3.6)

#Here, you can't read neither, too much text
ggplot(mpg, aes(displ, hwy)) + 
  geom_text(aes(label = model)) + 
  xlim(1, 8)

#A brutal and not very useful way to get over it
ggplot(mpg, aes(displ, hwy)) + 
  geom_text(aes(label = model), check_overlap = TRUE) + 
  xlim(1, 8)

#A variation on geom_text() is geom_label() : it draws a rounded rectangle behind the text

label <- data.frame(
  waiting = c(55, 80),
  eruptions = c(2, 4.3),
  label = c("peak one", "peak two")
)

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_tile(aes(fill = density)) + 
  geom_label(data = label, aes(label = label))

#Direct labels----
#Text labels can be an alternative to legend. This usually makes the plot easier to read
#The package directlabels is great for that purpose

ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point()

library(directlabels)
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(show.legend = FALSE) + 
  geom_dl(aes(label = class), method = "smart.grid")

#Annotations----
#Annotations add metadata to your plot. But metadata is just data, so you can use
#geom_text() to add text descritpions
#geom_rect() to highlight interesting rectangular regions of the plot
#geom_rect() has aestethics xmin, xmax, ymin, ymax
#geom_line(), geom_path(), geom_segment() to add lines. All these geoms have an arrow parameter,
#which allows you to place an arrowhead on the line. Create arrowheads with arrow(), which has argument angle, length ends and type
#geom_vline(), geom_hline() and geom_abline() allow you to add reference lines (sometimes called rules)

#To show the basic idea, here is a plot

ggplot(economics, aes(date, unemploy)) + 
  geom_line()

#We can annotate this plot with which president was in power at the time.
#Note that Inf and -Inf refer to the top and bottom (or left and right) limits of the plot

presidential <- subset(presidential, start > economics$date[1])
ggplot(economics) +
  geom_rect(
    aes(xmin = start, xmax = end, fill = party), 
    ymin = -Inf, ymax = Inf, alpha = 0.2,
    data = presidential
  ) +
  geom_vline(
    aes(xintercept = as.numeric(start)),
    data = presidential, 
    colour = "grey50", alpha = 0.5
  ) + 
  geom_text(
    aes(x = start, y = 2500, label = name),
    data = presidential,
    size = 3, vjust = 0, hjust = 0, nudge_x = 50
  ) +
  geom_line(aes(date, unemploy)) +
  scale_fill_manual(values = c("blue", "red"))

#You can use the same technique to add a single annotation to a plot, 
#but it's a bit fiddly because you have to create a one row dataframe

yrng <- range(economics$unemploy) 
xrng <- range(economics$date)  
caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years", 40 ), collapse= "\n")

ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  geom_text(
    aes(x, y, label = caption), 
    data = data.frame(x = xrng[1], y = yrng[2], caption = caption),
    hjust = 0, vjust = 1, size = 4
  )


#It's easier to use the annotate() function which creates the dataframe for you

ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  annotate("text", x = xrng[1], y = yrng[2], label = caption, hjust = 0, vjust = 1, size = 4
  )

#Annotations, particularly reference lines, are also useful when comparing groups accross facets. 
#In the following plot, it's much easier to see the subtle differences if we add a reference line

ggplot(diamonds, aes(log10(carat), log10(price))) + 
  geom_bin2d() +
  facet_wrap(~cut, nrow = 1)

mod_coef <- coef(lm(log10(price) ~ log10(carat), data = diamonds))
ggplot(diamonds, aes(log10(carat), log10(price))) + 
  geom_bin2d() + 
  geom_abline(intercept = mod_coef[1], slope = mod_coef[2],
              colour = "white", size = 1) +
  facet_wrap(~cut, nrow = 1)


#Collective geoms -----
#Geoms can be divided into individual and collective geoms.
#AN individual geom draws a distinct graphical object for each observation (row)
#A collective geom displays multiple observations with one geometric object. 
#This may be a result of a statistical summary, like a boxplot, 
#or may be fundamental to the display of the geom, like a polygon. 
#Lines and paths fall somewhere in between: each line is composed of a set of straight segments,
#but each segment represent two points.

#How do we control the assignment of observations to graphical elements ? 
#This is the job of the "group" aesthetic.

data("Oxboys")
head(Oxboys)

#Multiple groups one aesthetic ----
#In many situations, you want to separate your data into groups, but render them in the same way. 
#In other words, you want to be able to distinguish individual subjects, but not identify them. 
#This is common in longitudinal studies with many subjects, 
#where the plots are often descriptively called "spaghetti plots"
#For example, the following plot shows the growth trajectory for each boy. 

ggplot(Oxboys, aes(age, height, group = Subject)) +
  geom_point() +
  geom_line()

#IMPORTANT !!!!!!
#If you incorectly specify the grouping variable, you'll get a characteristic sawtooth appearance

ggplot(Oxboys, aes(age, height)) +
  geom_point() +
  geom_line()


#If a group isn't defined by a single variable, but instead by a combination of multiple variables, 
#Use interaction() to combine them, 
#e.g aes(group = interaction(school_id, student_id)

#Different groups on different layers----
#Suppose we want to  add a single smooth line, showing teh overall trend for all boys.
#If we use the same grouping in both layers, we get one smooth per boy. 

ggplot(Oxboys, aes(age, height, group = Subject)) +
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE)
#That's clearly not what you want
#Instead, what you want is 

ggplot(Oxboys, aes(age, height)) +
  geom_line(aes(group = Subject)) + 
  geom_smooth(method = "lm", size = 2, se = FALSE)

#Overriding the default grouping----
#Some plots have a discrete x axis, but you still want to draw lines connecting accross groups.
#This is a strategyused in interaction plotsn profile plots and parallel coordinate plots, among others.
#For example, imagine we've drawn boxplots of height at each measurement occasion. 

ggplot(Oxboys, aes(Occasion, height)) + 
  geom_boxplot() 

#C'est par exemple le cas dans lequel on  se place pour représenter 
#les paramètres physico-chimiques au cours du temps d'écantillonage

#There is one discrete variable in this plot, Occasion, so we get one boxplot for each unique x value.
#Now we want to overlay lines that connect each individual boy.
#Simply adding geom_line() does not work : The lines are drawn within each observations
#Not accross subject

ggplot(Oxboys, aes(Occasion, height)) + 
  geom_boxplot() +
  geom_line(colour = "#3366FF", alpha = 0.5)

#To get the plot we want, we need to override the grouping to say we want one line per boy.

ggplot(Oxboys, aes(Occasion, height)) + 
  geom_boxplot() +
  geom_line(aes(group = Subject), colour = "#3366FF", alpha = 0.5)

#Matching Aesthetics to Graphic objects----


#A faire



#Statistical summaries ----

ggplot(diamonds, aes(color)) +
  geom_bar()

ggplot(diamonds, aes(color, price)) +
  geom_bar(stat = "summary_bin", fun = mean)

ggplot(diamonds, aes(table, depth)) +
  geom_bin2d(binwidth = 1, na.rm = TRUE) +
  xlim(50, 70) +
  ylim(50, 70)

ggplot(diamonds, aes(table, depth, z = price)) +
  geom_raster(binwidth = 1, stat = "summary_2d", fun = mean, na.rm = TRUE) +
  xlim(50, 70) +
  ylim(50, 70)
