#' ---
#' title: "Week 3: Visualization Lab"
#' author: "Angela Abraham (PID A18245850)"
#' date: "October 19, 2025"
#' ---

# Week 3 Data Visualization Lab

# Install the package ggplot2
#install.packages("ggplot2")

# Any time I want to use this 
# package I need to load it
library(ggplot2)

View(cars)

# A quick base R plot - this is not ggplot
plot(cars)

# Our first ggplot
# we need data + aes + geoms
ggplot(data=cars)+
  aes(x=speed, y=dist) + 
  geom_point()

p<-ggplot(data=cars)+
  aes(x=speed, y=dist) + 
  geom_point()

# Add a line geom with geom_line()
p + geom_line()

# Add a trend line close to the data
p + geom_smooth()

p + geom_smooth(method="lm")

#---------------------#

#Read in our drug expression data
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# Q. how many genes are in this dataset
nrow(genes)

# Q. how many columns did you find?
colnames(genes)
ncol(genes)

# Q. how many 'up' regulated genes
table(genes$State)

# Q. What fraction of total genes is up-regulated?
round( (table(genes$State) / nrow(genes)) * 100, 2)


# Let's make a first plot attempt
g <- ggplot(data=genes) +
  aes(x=Condition1, y=Condition2,
      col=State) + 
  geom_point()

g
# Add some color
g + scale_color_manual(values=c("blue","grey","red"))+
  labs(title="Gene Expression Changes Upon Drug Treatments",
         x="Control (no drug)", y="Drug Treatment") +
        theme_bw()
