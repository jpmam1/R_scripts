#install.packages("leaflet")
library(leaflet)
library(tidyverse)
leaflet(width = "100%") %>% addProviderTiles("CartoDB.Positron") %>%
  setView(lat = -37.93, lng = 145.425, 14) %>% 
  addMarkers(lat = -37.921363, lng = 145.431046, label = "Telopea Trail") %>% 
  addMarkers(lat = -37.932101, lng = 145.446549, label = "Pepi's Land")

#################
library(tidyverse)
library(ggpubr)
carnames <- c("bmw","renault","mercedes","seat")
carcolors <- c("red","white","blue","green")
datavals <- round(rnorm(16, mean=100, sd=60),1)
car_data <- data.frame(Car = rep(carnames,4),
                       Color = rep(carcolors, c(4,4,4,4) ),
                       Value=datavals )
ggballoonplot(car_data, fill = "Value") +
  scale_fill_gradientn(colors = carcolors) +
  guides(size = FALSE) +
  theme_bw() +
  scale_y_discrete(name = "changed y axis title",
                   labels = c("y_new4", "y_new3", "y_new2", "y_new1")) +
  scale_x_discrete(name = "changed x axis title",
                   labels = c("x_new1", "x_new2", "x_new3", "x_new4")) +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 22),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12))

#################
library(tidyverse)
library(xgboost)
# https://github.com/dmlc/xgboost/blob/master/R-package/R/xgb.Booster.R
 test_df <- iris[0:105,]
 num_class <- 3
 set.seed(11)
 dtrain <- xgb.DMatrix(data = as.matrix(test_df[, -5]), label = as.numeric(test_df$Species) - 1)
 bst <- xgboost(dtrain, max_depth = 2, eta = 0.5, nthread = 2, nrounds = 1, subsample = 0.5,
                objective = "multi:softprob", num_class = num_class,
                base_score = seq(from=0.01, to=0.99, length.out=105))
 # predict for softmax returns num_class probability numbers per case:
 pred <- predict(bst, dtrain)
 str(pred)
 pred
 
 ptrain <- predict(bst, dtrain, outputmargin=TRUE)
 setinfo(dtrain, "base_margin", ptrain)
 
 bst <- xgb.train(max_depth=2, eta=0.1, num_class = num_class,
                  nthread = 2, objective='multi:softprob',
                  data = dtrain, nrounds = 100)
 retrain <- predict(bst, dtrain)
retrain

################
library(RColorBrewer)
library(ggrepel)
library(tidyverse)

my_pal <- colorRampPalette(brewer.pal(9, "Set1"))

## create new matrix ##
new_mat<-matrix(nrow=40, ncol = 4)
colnames(new_mat)<-c("Zones", "ssoilcmb", "Erosion_t", "area..sq.m.")

for ( i in 1:nrow(new_mat)){ 
  new_mat[i,4]<-as.numeric(sample(0:20, 1))
  new_mat[i,3]<-as.numeric(sample(0:20, 1))
  a<-sample(c("S2","S3","S4","S5","S1"),1)
  b<-sample(c("Deep","Moderate","Shallow"),1)
  new_mat[i,1]<-sample(c("High Precip","Moderate Precip","Low Precip"),1)
  
  new_mat[i,2]<-paste0(a,"_",b)
}

m_dt<-as.data.frame(new_mat)
m_dt$Erosion_t<-as.numeric(m_dt$Erosion_t)
m_dt$area..sq.m.<-as.numeric(m_dt$area..sq.m.)

# calculate parea
m_dt<- m_dt %>%
  group_by(Zones)%>%
  mutate(per_er=signif((`Erosion_t`/sum(`Erosion_t`))*100,3),
         per_area=signif((`area..sq.m.`/sum(`area..sq.m.`))*100,3))

## Rearranging data:

a<-data.frame(m_dt$Zones,m_dt$ssoilcmb, m_dt$per_er)
b<-data.frame(m_dt$Zones,m_dt$ssoilcmb, m_dt$per_area)
c<-data.frame(Zones=m_dt$Zones,ssoilcmb=m_dt$ssoilcmb,
              Parameter=c(rep("Erosion",40),rep("Area",40)),
              Values=c(m_dt$per_er,m_dt$per_area))

## New Plot ##
c$Zones <- factor(c$Zones,levels(c$Zones)[c (2,3,1)])

g1 <- ggplot(c, aes(x="", y=Values, fill=ssoilcmb)) + 
  geom_bar(stat="identity", width=1,
           position = position_fill())+
  coord_polar(theta = "y", start=0) +
  facet_wrap(Zones~Parameter, nrow = 3,
             labeller = labeller(.multi_line = FALSE)) +
  geom_text_repel(aes(label = paste0(Values, "%")),
                  position = position_fill()) +
  scale_fill_manual(values=my_pal(15)) +
  labs(title = "Erosions")+
  theme_minimal() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5,
                                                    color = "#666666"))

library(grid)
library(gridExtra)

my_caption <- grobTree(linesGrob(unit(c(0, 1), "npc"), unit(1, "npc")),
                    textGrob("Caption", x=0, hjust=0))

#Plot All Together
allplot <- grid.arrange(g1,my_caption,heights=c(1,0.05))
grid.draw(allplot)

############
#install.packages("tidyverse")
library(tidyverse)
#install.packages("janitor")
library(janitor)
#install.packages("tabulizer")
library(tabulizer)
url <- "https://doccs.ny.gov/system/files/documents/2020/06/doccs-covid-19-confirmed-by-facility-6.30.2020.pdf"
tab1 <- tabulizer::extract_tables(url, method = "lattice") %>% 
  as.data.frame() %>%
  dplyr::slice(-1,-2) %>% 
  janitor::row_to_names(row_number = 1)

############
library(tidyverse)
test <- read_csv2(file = "test.txt", trim_ws = TRUE)


ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class)) +
  scale_colour_discrete() +
  xlab("test label x") +
  ylab("test label y") +
  ggtitle("test ggplot") +
  theme(legend.position="left",
        legend.background=element_rect(fill="gray98",
                                       size=.5, linetype="blank", 
                                       colour="gray0"), text=element_text(size=16)) + theme_bw()

# install package
#install.packages("pheatmap")
# load package
library(pheatmap)
# install DESeq if necessary
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("DESeq")
# load package
library(DESeq)
library(tidyverse)
library(viridis)
# load data and subset
example_file <- system.file ("extra/TagSeqExample.tab", package="DESeq")
data <- read.delim(example_file, header=T, row.names="gene")
data_subset <- as.matrix(data[rowSums(data)>50000,])

# create heatmap using pheatmap
pheatmap(data_subset, color = viridis(12))
pheatmap(t(data_subset))

##reinstall key packages after update
# install.packages("mlr3")
# install.packages("Matrix")
# install.packages("data.table")
# install.packages("readr")
# install.packages("stringr")
# install.packages("mlr", repos="https://cran.ms.unimelb.edu.au/", source=TRUE, dependencies=TRUE)
# install.packages("caret", repos="https://cran.ms.unimelb.edu.au/", source=TRUE, dependencies=TRUE)
# install.packages("tidyverse")
# install.packages("parallelMap")
# install.packages("DiagrammeR")
# install.packages("corrplot")
# install.packages("e1071")
# install.packages("Hmisc")
# install.packages("ROCR")
# install.packages("corrr")
# install.packages("xgboost")
# install.packages("knitr")
# install.packages("viridis")
# install.packages("reshape2")
# install.packages("RColorBrewer")
# install.packages("janitor")
# library(devtools)
# install_github("AppliedDataSciencePartners/xgboostExplainer")

############

library(tidyverse)
iris %>% select_at(3:5)
iris %>% select(3:5)
############

library(data.table)
id<- c(rep(1,5), rep(2,5),rep(3,5))
time <-c (1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
death <-c(0,1,0,1,1,0,0,1,1,0,0,1,1,0,1)
table<-data.table(id, time, death)
table[, death_ideal := ifelse(cumsum(death) == 0, NA, 1), by = id]


library(tidyverse)
#install.packages("ggthemes")
library(ggthemes)

Year <- list("2002", "2001", "2000", "1999", "1998", "1997")
Value <- list(0.0109, -0.0273, 0.0113, 0.0148, 0.00841, 0.0361)
df <- do.call(rbind, Map(data.frame, Year=Year, Value=Value))

ggplot(df, aes(x = Year, y = Value)) +
  geom_col(aes(fill = Value)) +
  scale_fill_gradient2_tableau(limits = c(-0.04, 0.04))


install.packages("remotes")
remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
library(tidyverse)

ggplot(data = subset(penguins, !is.na(sex)),
       aes(x = island, fill = sex)) +
  geom_bar(width = 0.5) +
  geom_text(stat='count', aes(label=..count..),
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 65, vjust= 0.6)) +
  labs(title = "All Accessible Penguin Sex Count",
       x = "island",
       y = "Number of Individuals",
       fill = "Sex")

#install.packages("beeswarm")
library(beeswarm)
df <- data.frame(x = c(LETTERS), y = "1", 
                 z = c(rnorm(26, 11, 4)))
beeswarm(z ~ y, data = df,
         pwcol = c("black", rep("grey15", 25)),
         pwpch = c(23, rep(1, 25)),
         pwbg = c("red", rep("transparent", 25)),
         corral = "wrap", method = "center", 
         xlab = "", ylab = "variable",
         las=1
)

with(df, symbols(x = y, y = z, circles = c(0.01, rep(NA, 25)), add = TRUE))

library(data.table)
library(ggplot2)
library(lubridate)
dat <- as.data.table(x=1:10)
dat$V1 <- as.Date(dat$V1,origin = "2000-01-01")
dat$y_a = c(2,3,4,2,4,2,5,5,4,3)
dat$y_b = c(4,5,5,6,4,3,4,5,6,5)

sections <- as.Date(c(3,5,8),origin = "2000-01-01")

ggplot(data = dat, aes(x = V1, y = y_a)) +
  geom_line(colour = "black") +
  geom_smooth(alpha = 0, colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2000-01-04"), linetype = "Major"),
             colour = "red") +
  geom_vline(aes(xintercept = as.Date("2000-01-06"), linetype = "Major"),
             colour = "red") +
  geom_vline(aes(xintercept = as.Date("2000-01-09"), linetype = "Major"),
             colour = "red") +
  geom_vline(aes(xintercept = as.Date(7, origin = "2000-01-01"),
             linetype = "Important"),
             colour = "darkgreen") +
  xlab("Time") +
  ylab("Height") +
  scale_linetype_manual(name = "colour", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("red", "darkgreen"))))


#install.packages(c("psych","vegan"), dependencies=TRUE)
library(psych)
library(vegan)
temp <- tempfile()
download.file("https://github.com/NESCent/popgenInfo/blob/master/data/wolf_geno_samp_10000.zip?raw=true",
              temp)
gen <- read.csv(unzip(temp, "wolf_geno_samp_10000.csv"), row.names=1)
dim(gen)
sum(is.na(gen))
gen.imp <- apply(gen,
                 2,
                 function(x) replace(x,
                                     is.na(x),
                                     as.numeric(names(which.max(table(x))))))
sum(is.na(gen.imp)) # No NAs
env <- read.csv(url("https://raw.githubusercontent.com/NESCent/popgenInfo/master/data/wolf_env.csv"))
str(env)
env$individual <- as.character(env$individual)
env$land_cover <- as.factor(env$land_cover)
# Confirm that genotypes and environmental data are in the same order
identical(rownames(gen.imp), env[,1]) 
pairs.panels(env[,5:16], scale=T)
pred <- subset(env, select=-c(precip_coldest_quarter, max_temp_warmest_month, min_temp_coldest_month))
table(pred$land_cover)
pred <- subset(pred, select=-c(land_cover))
pred <- pred[,5:12]
colnames(pred) <- c("AMT","MDR","sdT","AP","cvP","NDVI","Elev","Tree")
pairs.panels(pred, scale=T)
wolf.rda <- rda(gen.imp ~ ., data=pred, scale=T)
wolf.rda
RsquareAdj(wolf.rda)
summary(eigenvals(wolf.rda, model = "constrained"))
screeplot(wolf.rda)
#signif.full <- anova.cca(wolf.rda, parallel=getOption("mc.cores"))
#signif.full
#signif.axis <- anova.cca(wolf.rda, by="axis", parallel=getOption("mc.cores"))
#signif.axis
vif.cca(wolf.rda)
plot(wolf.rda, scaling=3)
plot(wolf.rda, choices = c(1, 3), scaling=3)
levels(env$ecotype) <- c("Western Forest","Boreal Forest","Arctic","High Arctic","British Columbia","Atlantic Forest")
eco <- env$ecotype
bg <- c("#ff7f00","#1f78b4","#ffff33","#a6cee3","#33a02c","#e31a1c")
# axes 1 & 2
plot(wolf.rda, type="n", scaling=3)
abline(h = 0, v = 0, col = "white", lwd = 2)
box()
points(wolf.rda, display="species", pch=20, cex=0.7, col="gray32", scaling=3)           # the SNPs
points(wolf.rda, display="sites", pch=21, cex=1.3, col="gray32", scaling=3, bg=bg[eco]) # the wolves
text(wolf.rda, scaling=3, display="bp", col="#0868ac", cex=1)                           # the predictors
legend("bottomright", legend=levels(eco), bty="n", col="gray32", pch=21, cex=1, pt.bg=bg)

# axes 1 & 3
plot(wolf.rda, type="n", scaling=3, choices=c(1,3))
abline(h = 0, v = 0, col = "white", lwd = 2)
box()
points(wolf.rda, display="species", pch=20, cex=0.7, col="gray32", scaling=3, choices=c(1,3))
points(wolf.rda, display="sites", pch=21, cex=1.3, col="gray32", scaling=3, bg=bg[eco], choices=c(1,3))
text(wolf.rda, scaling=3, display="bp", col="#0868ac", cex=1, choices=c(1,3))
legend("topleft", legend=levels(eco), bty="n", col="gray32", pch=21, cex=1, pt.bg=bg)

#####
cars

#install.packages("forestplot")
library(forestplot)
# Cochrane data from the 'rmeta'-package
cochrane_from_rmeta <- 
  structure(list(
    mean  = c(NA, NA, 0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, NA, 0.531), 
    lower = c(NA, NA, 0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, NA, 0.386),
    upper = c(NA, NA, 0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, NA, 0.731)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -11L), 
    class = "data.frame")

tabletext<-cbind(
  c("", "Study", "Auckland", "Block", 
    "Doran", "Gamsu", "Morrison", "Papageorgiou", 
    "Tauesch", NA, "Summary"),
  c("",
    "Comparison",
    "Placebo 1",
    "Placebo 2",
    "Placebo 3",
    "Placebo 4",
    "Placebo 5",
    "Treatment 1",
    "Treatment 2",
    NA,
    ""),
  c("",
    "Relative Risk \n 95% CI",
    "0.88 (0.84-0.92)",
    "0.87 (0.81-0.94)",
    "0.88 (0.84-0.92)",
    "0.87 (0.81-0.94)",
    "0.88 (0.84-0.92)",
    "0.88 (0.84-0.92)",
    "0.87 (0.81-0.94)",
    NA,
    "0.87 (0.81-0.94)"),
  c("", "OR", "0.58", "0.16", 
    "0.25", "0.70", "0.35", "0.14", 
    "1.02", NA, "0.53"),
  c("", "F", "1.1", "1.3", 
    "0.2", "5", "3.1", "0", 
    "0.1", NA, "4.1"))

forestplot(tabletext, 
           cochrane_from_rmeta,
           graph.pos = 3, 
           new_page = TRUE,
           is.summary=c(rep(FALSE,11)),
           clip=c(0.1,2.5), 
           xlog=TRUE,
           col=fpColors(box="royalblue",
                        line="darkblue",
                        summary="royalblue"))


##########################3

# Some data
df <- data.frame(
  x = 1:10,
  y = 1:10,
  colour = factor(sample(1:3, 10, replace = TRUE)),
  size = factor(sample(1:3, 10, replace = TRUE)))

library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

### Step 1
# Draw a plot with the colour legend
(p1 <- ggplot(data = df, aes(x=x, y=y)) +
    geom_point(aes(colour = colour)) +
    theme_bw() +
    theme(legend.position = "top", legend.background = element_rect(fill = "lightsteelblue")))

# Extract the colour legend - leg1
leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")

### Step 2
# Draw a plot with the size legend
(p2 <- ggplot(data = df, aes(x=x, y=y)) +
    geom_point(aes(shape = size)) +
    theme_bw() +
    theme(legend.background = element_rect(fill = "lightseagreen")))

# Extract the size legend - leg2
leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p2)), "guide-box")

# Step 3
# Draw a plot with no legends - plot
(plot <- ggplot(data = df, aes(x=x, y=y)) +
    geom_point(aes(shape = size, colour = colour)) +
    theme_bw() +
    theme(legend.position = "none"))

### Step 4
# Arrange the three components (plot, leg1, leg2)
# The two legends are positioned outside the plot: 
# one at the top and the other to the side.
plotNew <- arrangeGrob(leg1, plot, 
                       heights = unit.c(leg1$height, unit(1, "npc") - leg1$height), ncol = 1)

plotNew <- arrangeGrob(plotNew, leg2,
                       widths = unit.c(unit(1, "npc") - leg2$width, leg2$width), nrow = 1)

grid.newpage()
grid.draw(plotNew)

# OR, arrange one legend at the top and the other inside the plot.
plotNew <- plot + 
  annotation_custom(grob = leg2, xmin = 7, xmax = 10, ymin = 0, ymax = 4)

plotNew <- arrangeGrob(leg1, plotNew,
                       heights = unit.c(leg1$height, unit(1, "npc") -  leg1$height), ncol = 1)

grid.newpage()
grid.draw(plotNew)

library(ggplot2)
library(dplyr)
library(patchwork)
data <- structure(
  list(
    Fac_Map = structure(
      c(1L, 1L, 1L, 1L, 2L, 2L,
        2L, 2L, 3L, 3L, 3L, 3L),
      .Label = c("Fac1", "Fac2", "Fac3"),
      class = "factor"
    ),
    S_Residency = structure(
      c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L,
        1L, 1L, 2L, 2L),
      .Label = c("Intl", "Local"),
      class = "factor"
    ),
    Period = structure(
      c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
        2L, 1L, 2L),
      .Label = c("2019 P2", "2020 P2"),
      class = "factor"
    ),
    Result = c(92.9, 91.1, 85.8, 87.9, 94.1, 91.7, 87.5, 88.6,
               90, 90.1, 87.4, 88.9)
  ),
  class = "data.frame",
  row.names = c(NA,-12L)
)
List <- split(data, data$Fac_Map)
#Create list
List <- split(data,data$Fac_Map)
#Now function to plot
myplotfun <- function(x)
{
  G <- ggplot(x,aes(x=Period,y=Result))+
    geom_point() +
    facet_grid(. ~ S_Residency,
               scales = "free", space = "free") +
    ylim(range(x$Result)) +
    xlab("")+ylab("")+ggtitle(unique(x$Fac_Map))
  return(G)
}
#Apply for plots
List2 <- lapply(List,myplotfun)
#Wrap final plot
wrap_plots(List2[-2], nrow = 1)


library(tidyverse)
First_set <- read.csv(text =
"Set Quarter True False
First Q1 30 20
First Q2 25 25",
sep = " ", header = TRUE)

Second_set <- read.csv(text =
"Set Quarter True False
Second Q1 35 25
Second Q2 35 35",
sep = " ", header = TRUE)

combined <- dplyr::full_join(First_set, Second_set)

ggplot(data = combined) +
  geom_bar()

#install.packages("gganimate")
#install.packages("gifski")
#install.packages("transformr")

library(tidyverse)
library(gganimate)
library(gifski)
library(transformr)
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(size = Sepal.Length)) +
  transition_states(Sepal.Length,
                    transition_length = 2,
                    state_length = 1) +
  geom_smooth(aes(colour = Species), method = "lm", se = FALSE) +
  transition_states(Species,
                    transition_length = 3,
                    state_length = 1)


# get matches between two vectors
# if doesn't match, print NA
v1<-c("T","A","G","R","A","G")
v2<-c("R","A","G","T","A","G")
combined_list <- c()
for (f in seq_along(v1)){
  if(v1[f] == v2[f])
    combined_list[f] <- v1[f]
  else
    combined_list[f] <- NA
}
combined_list

hello=function(p,n=nchar(p))`if`(n-lengths(gregexpr("h", p)),"err", strrep("Hello World",n))
hello("hhhhh")

#install.packages("ISLR")
library(ISLR)
library(xgboost)

options(max.print = 10000)

auto = ISLR::Auto
auto$name<-NULL
auto$origin<-NULL

dtrain <- xgb.DMatrix(data=as.matrix(auto[,-1]),label=as.matrix(auto[,1]))

param <- list(booster = 'gbtree'
              , objective = 'reg:squarederror'
              , learning_rate = 0.1)

xgb <- xgb.train(params = param
                 , data = dtrain
                 , nrounds = 4)

xgb_leaf <- data.frame(predict(xgb, dtrain, predleaf = T))
xgb_leaf

xgb_dt_tree <- data.frame(xgb.model.dt.tree(feature_names = NULL, model = xgb))
xgb_dt_tree

library(ggplot2)
library(gridExtra)
library(cowplot)
x <- LETTERS[1:25]
y <- paste0("var", seq(1,10))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(250, 0, 10)
data$Z <- ifelse(data$Z > 2, 1, 0)
train_test <- ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile(colour = "grey50", show.legend = FALSE) +
  scale_fill_viridis_c() +
  theme(text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  coord_equal()

a <- LETTERS[1:6]
b <- paste0("var", seq(1,6))
data2 <- expand.grid(A=a, B=b)
data2$C <- c("0")
val <- ggplot(data2, aes(A, B, fill = C)) + 
  geom_tile(colour = "grey50", show.legend = FALSE) +
  scale_fill_manual(values = "#35b779ff") +
  theme(text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  coord_equal() +
  ggtitle(label = "Validation")

plot_grid(train_test, val, scale = c(1, .3)) +
  draw_text("Train", x = 0.12, y = 0.75, colour = "#FDE725FF", size = 28) +
  draw_text("and", x = 0.27, y = 0.75, size = 28) +
  draw_text("Test", x = 0.4, y = 0.75, colour = "#440154FF", size = 28) +
  draw_text("Validation", x = 0.75, y = 0.75, colour = "#35b779ff", size = 28)

sessionInfo()

#install.packages("ggforce")
#install.packages("ggExtra")
library(tidyverse)
library(ggbeeswarm)
#library(ggExtra)

cv_df <- tribble(~Group, ~`Predictive Accuracy`,
                 "Alz_AIBL", 0.5584795,
                 "Alz_AIBL", 0.5906433,
                 "Alz_AIBL", 0.5385965,
                 "Alz_AIBL", 0.5584795,
                 "Alz_AIBL", 0.5473684,
                 "Alz_AIBL", 0.5912281,
                 "Alz_AIBL", 0.5040936,
                 "Alz_AIBL", 0.5368421)
ggplot(data = cv_df, aes(x = Group, y = `Predictive Accuracy`)) +
  geom_boxplot(outlier.shape = NA, width = 0.3) +
  geom_quasirandom(groupOnX = TRUE, width = 0.15, color = "#F8766D",
                   size = 3) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_y_continuous(limits = c(0.5,1),
                     labels = scales::percent) +
  theme_minimal(base_size = 20) +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank())
#ggMarginal(p, type = "histogram")

#install.packages("readxl")
library(tidyverse)
library(readxl)
read_excel("AllclusPathwayEDIT.xlsx") %>% 
  pivot_longer(!Cluster, names_to = "gene_counts", values_to = "count") %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  ggplot(aes(x = Cluster, y = count, fill = gene_counts)) +
  geom_bar(position="stack", stat = "identity") +
  theme(legend.position = "right",
        legend.key.size = unit(0.4,"line"),
        legend.text = element_text(size = 7),
        legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 1))
ggsave(filename = "example.png", dpi = 500, height = 20, width = 35, units = "cm")

library(tidyverse)
library(gplots)
library(ggpubr)

dt <- as.table(as.matrix(mtcars[1:10,]))
balloonplot(t(dt), xlab ="", ylab="", label = FALSE, show.margins = FALSE, cum.margins=F)

mtcars[1:10,] %>%
ggballoonplot(size.range = c(-0.5,10),
              rotate.x.text = FALSE,
              fill = "skyblue",
              color = "skyblue",
              ggtheme = theme_minimal(base_size = 14)) +
  ggtitle(label = "Balloon Plot for x by y",
          subtitle = "Area is proportional to Frequency") +
  scale_x_discrete(position='top') +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 14),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        axis.ticks = element_blank()) +
  geom_tile(color = "black", fill = "transparent")

ggsave(filename = "example_balloon_plot.png", device = "png", dpi = 600)

library(tidyverse)
chess_df <- tribble(~ID, ~rated, ~turns, ~victory_status, ~winner, ~increment_code, ~cream_rating, ~charcoal_rating, ~opening_name,
"1", "FALSE", 13, "outoftime", "cream", 15+2, 1500, 1191, "Slav Defense: Exchange Variation",
"2", "TRUE", 16, "resign", "charcoal", 5+10, 1322, 1261, "Nimzowitsch Defense: Kennedy Variation",
"3", "TRUE", 61, "mate", "cream", 5+10, 1496, 1500, "King's Pawn Game: Leonardis Variation",
"4", "TRUE", 61, "mate", "cream", 20+0, 1439, 1454, "Queen's Pawn Game: Zukertort Variation",
"5", "TRUE", 95, "mate", "cream", 30+3, 1523, 1469, "Philidor Defense",
"6", "FALSE", 5, "draw", "draw", 10+0, 1250, 1002, "Sicilian Defense: Mongoose Variation")

chess_games_rated<-chess_df$cream_rating 
chess_games_victory<-chess_df$victory_status
boxplot(chess_games_rated~chess_games_victory, subset = c(chess_df$victory_status != "draw"))

chess_df %>%
  filter(victory_status != "draw") %$%
  boxplot(cream_rating ~ victory_status)

ggplot(subset(chess_df, victory_status != "draw"), aes(x = victory_status, y = cream_rating)) +
  geom_boxplot()

chess_df %>% 
  filter(victory_status != "draw") %>% 
  ggplot(data = ., aes(x = victory_status, y = cream_rating)) +
  geom_boxplot()


library(tidyverse)
library(ggpubr)
list.function <-  function() { 
  sample1 <- data.frame(gene_biotype= c("protein_coding", "lncRNA", "intronic"), counts = c(1, 1, 1))
  sample2 <- data.frame(gene_biotype= c("protein_coding", "lncRNA", "intronic"), counts = c(2, 2, 2))
  sample3 <- data.frame(gene_biotype= c("protein_coding", "lncRNA", "intronic"), counts = c(3, 3, 3))
  sample4 <- data.frame(gene_biotype= c("protein_coding", "lncRNA", "intronic"), counts = c(4, 4, 4))
  sapply(paste('sample', seq(1,4,1), sep=''), get, environment(), simplify = FALSE) 
}

my.list3 <- list.function()

for (f in seq_along(my.list3)) {
  a <- ggplot(data = my.list3[[f]], aes(y = counts,
                               x = gene_biotype,
                               fill = gene_biotype)) +
    geom_bar(stat = "identity") +
    xlab("Groups") + 
    ylab("Counts") +
    theme_classic() +
    ggtitle(names(my.list3[f]))
  b <- ggplot(data = my.list3[[f]], aes(y = counts,
                                        x = gene_biotype,
                                        fill = gene_biotype)) +
    geom_bar(stat = "identity") +
    xlab("Groups") + 
    ylab("Counts") +
    theme_classic() +
    ggtitle(names(my.list3[f]))
  ggsave(filename = paste(names(my.list3[f]), ".png", sep = ""),
         plot = ggarrange(a, b, ncol = 1, nrow = 2, labels = c("A", "B")),
         device = "png", width = 10, height = 5, units = "in")
}

library(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
dtrain <- xgb.DMatrix(train$data, label=train$label)
test <- agaricus.test
dtest <- xgb.DMatrix(test$data, label=test$label)

cv <- xgb.cv(data = train$data, label = train$label, nfold = 5, max_depth = 2,
             eta = 1, nthread = 4, nrounds = 10, objective = "binary:logistic",
             prediction = TRUE, early_stopping_rounds = 1)
print(cv$best_iteration)
head(cv$pred)

trained_model <- xgb.train(data = dtrain, max_depth = 2,
              eta = 1, nthread = 4, nrounds = 3,
              watchlist = list(train = dtrain, eval = dtrain),
              objective = "binary:logistic")
head(predict(trained_model, dtrain))

trained_model <- xgb.train(data = dtrain, max_depth = 2,
                           eta = 1, nthread = 4, nrounds = 10,
                           watchlist = list(train = dtrain, eval = dtrain),
                           objective = "binary:logistic")
head(predict(trained_model, dtrain))

#####
GlucoseConc5min <- c(100, 1000, 500, 500, 800, 300, 300)
GlucoseConc10min <- c(100, 300, 450, 100, 300, 1000, 300)

plant.df <- data.frame(values=c(GlucoseConc5min, GlucoseConc10min),
                       group= c(rep("Glucose concentration for 5 minutes",7),
                                rep("Glucose concentration for 10 minutes",7)))

boxplot(plant.df$values ~ plant.df$group, main= "Reaction Times effect on Glucose Concentration",
        xlab= "Time given to react",
        ylab= "Glucose concentraion (mg/dL)",
        col= c("lightseagreen","mediumpurple4"))

library(magrittr)
plant.df %$%
  boxplot(values ~ group, main= "Reaction Times effect on Glucose Concentration",
          xlab= "Time given to react",
          ylab= "Glucose concentraion (mg/dL)",
          col= c("lightseagreen","mediumpurple4"))

#####

#install.packages("lm.beta")
#install.packages("ggplot2")
library(lm.beta)
library(tidyverse)
library(viridis)
set.seed(18)
dat = data.frame(
  State = c("MA", "MA", "MA", 
            "RI", "RI", "RI", 
            "WA", "WA", "WA",
            "CA", "CA", "CA",
            "NY", "NY", "NY",
            "CO", "CO", "CO"),
  
  Exam  = c("ACT", "SAT", "GRE",
            "ACT", "SAT", "GRE",
            "ACT", "SAT", "GRE",
            "ACT", "SAT", "GRE",
            "ACT", "SAT", "GRE",
            "ACT", "SAT", "GRE"),
  
  regression_coeffs = runif(18)) # Regression Coefficients

p_values = runif(18, min = 0.000001, max = .99) # P-Values
label <- round(x = dat$regression_coeffs, digits = 3)
dat <- cbind(dat, p_values)
# Plot using ggplot and geom_tile
p <- ggplot(data = dat %>% 
              mutate(p_values = ifelse(p_values <= 0.05,
                                       p_values,
                                       "NA")),
            aes(x = State, y = Exam)) +
  geom_tile(aes(fill = regression_coeffs),
            color = "black") +
  scale_fill_continuous(type = "viridis",
                        na.value = "grey75") +
  geom_tile(aes(alpha = p_values),
            color = "black",
            show.legend = FALSE) +
  scale_alpha_discrete(na.value = "grey75") +
  geom_text(aes(x = State, y = Exam),
            label = label,
            size = 4,
            color = ifelse(p_values <= 0.05, "white", "black"))
p

###########

library(tidyverse)
library(zoo)
test_df <- tribble(~ID, ~x,
"1.", -0.8166667,
"2.", -0.2148148,
"3.", -0.6351852,
"4.", -0.4166667,
"5.", -0.3200000,
"6.", -1.5581818,
"7.", -0.8563636,
"8.", -1.2709091,
"9.", -0.2600000,
"10.", -1.8090909,
"11.", 3.2200000,
"12.", 3.3581818,
"13.", 0.8545455,
"14.", 2.8454545,
"15.", -0.8818182,
"16.", -0.4192308,
"17.", -1.9529412,
"18.", -1.5680000,
"19.", -0.7510204,
"20.", -1.2142857,
"21.", -1.7816327,
"22.",  0.2795918,
"23.", -0.8877551,
"24.", -0.5530612,
"25.", -0.2877551) %>% 
  mutate(rolling_average = zoo::rollmean(test_df$x, k = 4, fill = NA))
test_df

###############
options(scipen = 100000)
library(palmerpenguins); library(tidyverse); library(janitor)

penguins_raw %>%
  clean_names() %>%
  mutate(clutch_int = factor(str_replace(interaction(clutch_completion,
                                                     sex),
                                         '\\.', ' / '),
                             ordered=TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = clutch_int,
               y = body_mass_g,
               fill = sex),
           position = "stack") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_grid(scales = "free",
             rows = vars(species), drop = FALSE,
             cols = vars(island), margins = TRUE)

################

library(tidyverse)
library(ggrepel)

plot_data <- function(ValueX) {
  ValueX <- ensym(ValueX)
  ggplot(df, aes(y = !!ValueX,
                   x = Name)) +
    geom_text_repel(aes(label =  ifelse(!!ValueX < 0,
                        Name, NA))) +
    geom_point() +
    theme_bw(base_family = "Helvetica", base_size = 14) +
    ggtitle(ValueX) +
    theme(axis.ticks.x = element_blank(),
          legend.position = "none")
}

df <- readr::read_table("  Name      Value1     Value2     Value3
1   A1 -0.05970872 -1.1651404  1.3516952
2   A2  0.44143488 -0.7270722 -1.9870423
3   A3  0.34616897 -0.3891095  0.9123736
4   A4  0.49289331  1.3957877 -0.2689896
5   A5 -1.39354557  0.9429327  1.0719274") %>% 
  select(-c(X1))

## Collate unaltered colnames into a vector
vector_of_colnames <- colnames(df)[-1]

## Plot
lapply(vector_of_colnames, plot_data)

pdf(file=paste0("All_plots.pdf"))
lapply(vector_of_colnames, plot_data)
dev.off()

cutoff = 0
for(i in colnames(data)[-1]){
  png(filename = paste(i,".png"))
  print(ggplot(data,
               aes_string(x = "Name", y = as.name(i))) +
          geom_point() +
          geom_text_repel(aes(label = ifelse((as.name(i) < 0),
                                             Name, NA))))
  dev.off()
}


#######################
# Load libraries
library(tidyverse)

# Create dataframes
df <- data.frame("Gene" = c("GENE1", "GENE2", "GENE3"),
                 "CELL1" = c(0.7, 0.7, 0.5), 
                 "CELL2" = c(1, 0.4, 0.3), 
                 "CELL3" = c(0.7, 0.73, 0.61))
df2 <- data.frame("Gene" = c("GENE1", "GENE2", "GENE3"),
                  "CELL1" = c(183, 2, 19), 
                  "CELL2" = c(24, 1.8, 11.1), 
                  "CELL3" = c(18.9, 3.3, 22.9))
# Pivot df1 to 'long' format
data <- pivot_longer(data = df, cols = c(CELL1, CELL2, CELL3))
# Pivot df2 to 'long' format and scale values
data2 <- pivot_longer(data = df2, cols = c(CELL1, CELL2, CELL3))
# Plot "data" using "data2" to set the tile sizes
ggplot(data = data, aes(x = name, y = Gene)) +
  geom_tile(data = data2, aes(fill = value, height = value / 150, width = value / 150)) +
  theme(panel.background = element_blank())


df <- data.frame("CELL1" = c(0.7, 0.7, 0.5), 
                 "CELL2" = c(1, 0.4, 0.3), 
                 "CELL3" = c(0.7, 0.73, 0.61))
rownames(df) <- c("GENE1", "GENE2", "GENE3")
df2 <- data.frame("CELL1" = c(183, 2, 19), 
                  "CELL2" = c(24, 1.8, 11.1), 
                  "CELL3" = c(18.9, 3.3, 22.9))
rownames(df) <- c("GENE1", "GENE2", "GENE3")

heatmap.2(x = as.matrix(df))

##################
library(ggplot2)
library(ggridges)

data_url = 'https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv'
gapminder = read.csv(data_url)
ggplot(gapminder, aes(y=as.factor(year),
                      x=lifeExp)) +
  geom_density_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +  
  scale_x_continuous(expand = c(0, 0))+
  theme_bw()

########

test <- NULL
for(i in 1:1000){
  p <- rgamma(1,239,10)
  yrep <- rpois (10,p)
  test <- c(test,yrep)}
hist (test, xlab="T (yrep)", yaxt="n", cex=1,col = "yellow")
lines(rep(22,2), col="red", c(0,1600))
print(mean(test<=22))

########
require("survival")
require("survminer")

fit<- survfit(Surv(time, status) ~ sex, data = lung)

ggplot1 <- ggsurvplot(fit, data = lung)$plot
df1 <- data.frame(time=fit$time, nRisk=fit$n.risk, nRiskRel=fit$n.risk/max(fit$n.risk))  
ggplot1 + geom_point(aes(x=time, y=nRiskRel), data = df1, alpha=0.5, size=3)
#ggplot1 + coord_flip()
########
library(tidyverse)
library(ggbeeswarm)
level <-c(1,2,3,5,2,4,3,1,3)
pay1 <- c(10,21,32,12,41,21,36,14,17)
pay2 <- c(26,36,5,6,52,12,18,17,19)
data <- data.frame(level, pay1, pay2)
data %>% pivot_longer(-level) %>%
  ggplot(aes(x = name, y = value, fill = name))+
  geom_point(position = position_quasirandom(width = 0.2, groupOnX = TRUE),
             size = 4, shape = 21) +
  geom_boxplot(width = 0.25, outlier.shape = NA, alpha = 0.5)

#########
library("tidyverse")

groups_df <- tibble(
  fi = c("a", "b", "c"),
  id = c(10, 12, 14)
)
companies <- c("b", "c")

group_id_lookup <- function(new_comp){
  group_id_idx <- which(groups_df$fi == new_comp)
  group_id <- pull(groups_df[group_id_idx, 2])
  print(group_id)
  # Do more things
}


group_id_lookup("b", groups_df) # OK
group_id_lookup("c", groups_df) # OK

result <- lapply(companies, group_id_lookup)

ifelse(runif(1, 0, 1) <= 0.3, 10, 20)

#########

library(tidyverse)
install.packages('titanic')
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived, labels = c("Didn't Survive", "Survived")),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% group_by(Sex) %>% summarise(., sum(as.numeric(Survived)))

ggplot(titanic, aes(y = Survived)) +
  geom_bar()

#########
library(tidyverse)
library(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')

bst <- xgboost(agaricus.train$data, agaricus.train$label, nrounds = 50,
               eta = 0.1, max_depth = 3, subsample = .5,
               objective = "binary:logistic",
               nthread = 2, verbose = 0)

xgb.plot.shap(agaricus.test$data, model = bst, features = "odor=none")

contr <- predict(bst, agaricus.test$data, predcontrib = TRUE, approxcontrib = FALSE)
pred <- predict(bst, agaricus.test$data)

## Use "plot = FALSE" to return the data to "mat", instead of the rendered plot
mat <- xgb.plot.shap(agaricus.test$data[5:6,], contr[5:6,], model = bst,
                     top_n = 12, n_col = 3, plot = FALSE)

## Format the data
SHAP <- as.matrix(mat$shap_contrib[1,]) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  set_names(c("Variable", "SHAP"))

Score <- as.matrix(mat$data[1,]) %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  set_names(c("Variable", "Score"))

Pred <- ifelse(pred[1] <= 0.5, 0, 1)

SHAP_Score <- left_join(SHAP, Score, by = "Variable")

SHAP_Score_Pred <- cbind(SHAP_Score, Pred)

id <- rownames(agaricus.test[1,])

ggplot(SHAP_Score_Pred, aes(y = SHAP, x = Score)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey75") +
  geom_point(pch = 3, cex = 3, col = "red") +
  ggtitle(label = paste("ID =", id, "; Prediction for this observation =", Pred, sep = " ")) +
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(breaks = c(0,1)) +
  facet_wrap(facets = vars(Variable))

##########

recursive.factorial <- function(x) {
  if (x == 0 || x > 10)
    return (1)
  else
    return (print(x * recursive.factorial(x-1)))
}
recursive.factorial(4)
recursive.factorial(13)

##########
library(tidyverse)
data.frame("Interest_Rate" = runif(n = 40, min = 0, max = 0.25),
           "Interest_Rate_y" = rep(1.1, 40),
           "Mean_Value_y" = rep(0.21, 40)) %>%
  ggplot() +
  geom_point(aes(y = Interest_Rate_y, x = Interest_Rate),
             size = 10, col = "deepskyblue", alpha = 0.25) +
  geom_point(aes(y = Mean_Value_y, x = mean(Interest_Rate)),
             size = 8, pch = 17, col = "red") +
  geom_hline(yintercept = 0.75) +
  coord_cartesian(ylim = c(0,1.5), clip = "off") +
  scale_x_continuous(name = "Interest Rate", labels = scales::percent) +
  theme_classic(base_size = 16) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

############
library(tidyverse)
library(vroom)
library(data.table)
files <- fs::dir_ls(glob = "ctrl_*txt")
columns_to_keep <- c("Row", "Column", "Plane", "Timepoint",
                     "Field", "Object No", "X", "Y", "Bounding Box",
                     "Position X [µm]", "Position Y [µm]", "Cell Type",
                     "stimulation", "Compound", "Concentration", "units",
                     "combination", "treatmentsum", "Pid_treatmentsum")

data <- map(files, ~vroom::vroom(.x))
concat <- rbindlist(data, fill = TRUE)
what_I_want <- subset(concat, select = columns_to_keep)
###########
library(tidyverse)
df <- structure(list(t = 1:20, x = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                     0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), count_visit_triage1 = c(42L, 
                                                                                                              55L, 61L, 52L, 58L, 38L, 47L, 46L, 66L, 44L, 24L, 17L, 40L, 25L, 
                                                                                                              18L, 23L, 34L, 35L, 22L, 23L), count_visit_triage2 = c(175L, 
                                                                                                                                                                     241L, 196L, 213L, 189L, 163L, 181L, 166L, 229L, 224L, 153L, 139L, 
                                                                                                                                                                     125L, 145L, 134L, 115L, 152L, 153L, 136L, 154L), count_visit_triage3 = c(120L, 
                                                                                                                                                                                                                                              114L, 106L, 88L, 108L, 103L, 103L, 93L, 80L, 81L, 88L, 94L, 94L, 
                                                                                                                                                                                                                                              77L, 91L, 100L, 93L, 70L, 79L, 77L), count_visit_triage4 = c(3L, 
                                                                                                                                                                                                                                                                                                           0L, 0L, 1L, 2L, 2L, 0L, 4L, 4L, 2L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 
                                                                                                                                                                                                                                                                                                           0L, 1L, 2L)), row.names = c(NA, -20L), class = c("tbl_df", "tbl", 
                                                                                                                                                                                                                                                                                                                                                            "data.frame"))
df1 <- df %>%
  filter(x == 0) %>% 
  select(count_visit_triage1, t) %>%
  rename(y = count_visit_triage1) %>% 
  mutate(grp = "y1")
df2 <- df %>%
  filter(x == 1) %>% 
  select(count_visit_triage1, t) %>%
  rename(y = count_visit_triage1) %>% 
  mutate(grp = "y2")
df3 <- df %>%
  filter(x == 0) %>% 
  select(count_visit_triage2, t) %>%
  rename(y = count_visit_triage2) %>% 
  mutate(grp = "y3")
df4 <- df %>%
  filter(x == 1) %>% 
  select(count_visit_triage2, t) %>%
  rename(y = count_visit_triage2) %>% 
  mutate(grp = "y4")
df5 <- df %>%
  filter(x == 0) %>% 
  select(count_visit_triage3, t) %>%
  rename(y = count_visit_triage3) %>% 
  mutate(grp = "y5")
df6 <- df %>%
  filter(x == 1) %>% 
  select(count_visit_triage3, t) %>%
  rename(y = count_visit_triage3) %>% 
  mutate(grp = "y6")
df7 <- df %>%
  filter(x == 0) %>% 
  select(count_visit_triage4, t) %>%
  rename(y = count_visit_triage4) %>% 
  mutate(grp = "y7")
df8 <- df %>%
  filter(x == 1) %>% 
  select(count_visit_triage4, t) %>%
  rename(y = count_visit_triage4) %>% 
  mutate(grp = "y8")
df_all <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

ggplot(df_all, aes(x = t, y = y)) +
  geom_smooth(aes(color = grp), se = FALSE) + 
  geom_vline(xintercept = 10, linetype = "dashed") +
  theme_bw() +
  scale_color_brewer(palette = "Paired", name = "legend")

# The code above turns into:
df %>%
  pivot_longer(starts_with("count_")) %>%
  ggplot(aes(t, value, col = name, group = paste(x, name))) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 10, linetype = "dashed") + 
  theme_bw()

#########
#install.packages("ggnewscale")
library(ggplot2)
library(ggnewscale)
library(cowplot)

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Heatmap 
p1 <- ggplot(data, aes(X, Y, color = Z)) + 
  geom_tile() +
  scale_color_gradient(low = "white", high = "red") +
  new_scale_color() +
  geom_point(aes(color = Z)) +
  theme(legend.position = "none")

p2 <- ggplot(data, aes(X, Y, color = Z)) + 
  geom_tile() +
  scale_color_gradient(low = "white", high = "red") +
  guides(color = guide_colorbar(frame.colour = "black", frame.linewidth = 1.5))

p3 <- ggplot(data, aes(X, Y, color = Z)) + 
  geom_point(aes(color = Z)) +
  guides(color = guide_colorbar(frame.colour = "black", frame.linewidth = 1.5))

p2_legend <- get_legend(p2)
p3_legend <- get_legend(p3)
legends <- plot_grid(p2_legend, p3_legend, ncol = 1, nrow = 2)

plot_grid(p1, legends, ncol = 2, align = "h", rel_widths = c(0.9, 0.1)) 

##################################
institutions <- runif(10, 0, 100)
success <- sample(x = c(0,1), size = 10, replace = TRUE)
df <- data.frame(institutions, success)

library(ggplot2)
ggplot(df) +
  geom_boxplot(aes(y = institutions, x = success, group = success))

##################################
library(tidyverse)
A <- data.frame(A = c("Amsterdam",
                      "Copenhagen",
                      "LA",
                      "Lisbon",
                      "London", 
                      "Madrid",
                      "New York",
                      "Paris",
                      "Rome",
                      "Stockholm"))

B <- data.frame(B = c("Amsterdam",
                      "Buenos Aires",
                      "Copenhagen",
                      "LA",
                      "London", 
                      "Sydney",
                      "Tokyo"))
merge(A, transform(B, A = B), all.x = TRUE)


library(tidyverse)

obs <- c("Gene1", "Gene2", "Gene3", "Gene4","Gene5", "Gene6")
func1 <- c("A", "B", "C", "D", "C", "A")
func2 <- c("A1", "B1", "C1", "D1", "C2", "A2")
Cond1 <- c(0.007623561, 0.004639893, 0.000994121, 0.017494429, 0.000366445, 0.006663334)
Cond2 <- c(0.011299941, 0.009994388, 0.001012428, 0.013695669, 0.000299771, 0.010287904)
Cond3 <- c(0.005055458, 0.016826251, 0.001311254, 0.016115009, 0.000242897, 0.004583889)
df <- data.frame(obs, func1, func2, Cond1, Cond2, Cond3)

col<-rep("black", length(unique(df$func1)))
names(col) <- unique(df$func1)
col[which(names(col)=="A")] <- 'red'
col[which(names(col)=="D")] <- 'blue'

library(tidyverse)
g <- ggtern(data=df, aes(x=Cond1,y=Cond2,z=Cond3)) +
  theme_bw() +
  geom_point(data = subset(df, !(func1 %in% c("A", "D"))),
             aes(fill=func1), shape=21, size = 20, colour="black") +
  geom_point(data = subset(df, func1 %in% c("A", "D")),
             aes(fill=func1), shape=21, size = 20, colour="black") +
  scale_fill_manual(values=col) +
  labs(x="Cond1",y="Cond2",z="Cond3") +
  scale_T_continuous(breaks=unique(df$x))+ 
  scale_L_continuous(breaks=unique(df$y))+ 
  scale_R_continuous(breaks=unique(df$z))

print(g)

###########
library(tidyverse)
df <- data.frame (rating1  = c(5,8,7,8,9,6,9,7,8,5,8,5),
                  rating2  = c(2,7,8,4,9,3,6,1,7,3,9,1),
                  rating3  = c(0,6,1,2,7,2,9,1,6,2,3,1),
                  race = c("asian", "asian", "asian","black","asian","black","white","black","white","black","white","black"),
                  gender = c("male","female","female","male","female","male","female","male","female","male","female","male")
)

for (rac in unique(df$race)){
tmp_df <- df %>% 
    filter(race == rac)
print(rac)
print(t.test(tmp_df$rating1,
         rep(mean(df$rating1),
             length(tmp_df$rating1))))
}

for (gend in unique(df$gender)){
  tmp_df <- df %>% 
    filter(gender == gend)
  print(gend)
  print(t.test(tmp_df$rating1,
               rep(mean(df$rating1),
                   length(tmp_df$rating1))))
}

anova_one_way <- aov(rating1 + rating2 + rating3 ~ race + gender, data = df)
summary(anova_one_way)
TukeyHSD(anova_one_way)

##############
#install.packages("rentrez")
library(rentrez)
entrez_dbs()
entrez_db_searchable("protein")
res <- entrez_search(db = "protein",
              term = "Signal recognition particle subunit SRP72",
              )
##########
data("diamonds")
basePlot <- diamonds[ names(diamonds)[!names(diamonds) %in% c("color", "clarity")] ]

#################################################################################

# Compute the analysis of variance#
library(tidyverse)
library(magrittr)
data("diamonds")
basePlot <- diamonds[ names(diamonds)[!names(diamonds) %in% c("color", "clarity")] ]
basePlot$status_desc <- as.factor(basePlot$cut)

basePlot %>% select(-c(cut, status_desc)) %>% 
  map(aov(data = basePlot, cut ~ .x) %>% summary()[[1]][4:5])

for (var in names(basePlot)){
  test <- aov(data = basePlot, formula = carat ~ basePlot[[var]])
  print(var)
  summary_res <- summary(test)[[1]][4:5]
  print(summary)
}

apply(X = basePlot, MARGIN = 2,
      FUN = function(x) basePlot %$%
        aov(data = ., formula = carat ~ x) %>%
        summary())

summary_res[[1]][4:5]
options(scipen = 100000)

diamonds %>% 
  select(-c(color, clarity)) %$%
  interaction.plot(x.factor = carat, trace.factor = cut, response = price,
                   fun = mean, type = "p", legend = TRUE,
                   pch = c(0,1,2,5,6), col = c("#00204DFF", "#414D6BFF",
                                               "#7C7B78FF", "#BCAF6FFF",
                                               "#FFEA46FF"))
table <-
  diamonds %>% 
  select(-c(color, clarity)) %>%
  summarise(summary(aov(data = ., formula = carat ~ depth * price))[[1]][1:5])
  

#######################
library(ggplot)
income <- runif(n = 10, min = 0, max = 30000)
cohort <- factor(sample(x = 0:1, size = 10, replace = TRUE),
                    labels = c("control", "treatment"))
NSW <- data.frame(income, cohort)

chart1 = ggplot(data = NSW, aes(x = cohort, y = income)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_y_continuous(labels=scales::dollar_format(),
                     expand = c(0, 0), name = "Mean Income")
chart1

#######################

library(tidyverse)
library(plotrix)
library(magrittr)

set.seed(123)
precipitation <- data.frame(Ref = runif(730,1,5), A1 = runif(730,0,8), G2 = runif(730,2,6), G3 = runif(730,1,7)) 
Max_Temp <-  data.frame(Ref = runif(730,-5,30), A1 = runif(730,-8,32), G2 = runif(730,-2,28), G3 = runif(730,-10,25))
Min_Temp <-  data.frame(Ref = runif(730,-20,5), A1 = runif(730,-25,10), G2 = runif(730,-25,6), G3 = runif(730,-15,10))

par(mfrow=c(1,1))

precipitation %>% 
  pivot_longer(names_to = "Models", values_to = "values", -Ref) %>% 
  filter(Models == "A1") %$%
  taylor.diagram(Ref, values, col = "blue", pch = 1)

precipitation %>% 
  pivot_longer(names_to = "Models", values_to = "values", -Ref) %>% 
  filter(Models == "G2") %$%
  taylor.diagram(Ref, values, col = "red", pch = 2, add = TRUE)

precipitation %>% 
  pivot_longer(names_to = "Models", values_to = "values", -Ref) %>% 
  filter(Models == "G3") %$%
  taylor.diagram(Ref, values, col = "green", pch = 3, add = TRUE)

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep=",")
fit <- aov(data = wine, formula = V2 ~ V1)
summary(fit)

####################
library(ggplot2)
hashes <- data.frame(
  row.names = c("AAACCTGAGTGTTTGC-1",
                "AAACCTGCACTCAGGC-1","AAACCTGGTAAGGGAA-1",
                "AAACCTGGTTCAGCGC-1","AAACCTGTCAAGGTAA-1",
                "AAACCTGTCCATTCTA-1","AAACGGGAGATATGGT-1","AAACGGGAGGATCGCA-1",
                "AAACGGGAGGCCCTCA-1","AAACGGGCAATGAAAC-1",
                "AAACGGGCAATGGATA-1","AAACGGGCACATCTTT-1",
                "AAACGGGCATGACATC-1","AAACGGGGTAAGGATT-1","AAACGGGGTATATGGA-1",
                "AAAGATGAGGCTCAGA-1","AAAGATGCAAGCCTAT-1",
                "AAAGATGCATTCGACA-1","AAAGATGGTCGCTTTC-1",
                "AAAGATGGTCGGCACT-1","AAAGATGTCAAACAAG-1","AAAGATGTCTTGTATC-1",
                "AAAGCAAAGAGGTTAT-1","AAAGCAAAGCCACCTG-1",
                "AAAGCAACAAGGACTG-1","AAAGCAACAATAGAGT-1","AAAGCAACAATCGGTT-1",
                "AAAGCAACACAGTCGC-1","AAAGCAACAGGTCGTC-1",
                "AAAGCAACATATGGTC-1","AAAGCAACATGTCTCC-1",
                "AAAGCAATCCGAACGC-1","AAAGCAATCGGTCCGA-1","AAAGCAATCTCATTCA-1",
                "AAAGTAGAGCCCGAAA-1","AAAGTAGAGGAGTACC-1",
                "AAAGTAGAGTTATCGC-1","AAAGTAGCATAAAGGT-1",
                "AAAGTAGGTCGACTGC-1","AAAGTAGTCAAACCAC-1","AAAGTAGTCACTTCAT-1",
                "AAATGCCAGATGCCTT-1","AAATGCCAGCGATATA-1",
                "AAATGCCAGGATGCGT-1","AAATGCCCAAGTCTAC-1",
                "AAATGCCCATTGCGGC-1","AAATGCCGTTTAAGCC-1","AAATGCCTCCATTCTA-1",
                "AAATGCCTCTCGGACG-1","AACACGTAGAGTGACC-1",
                "AACACGTAGCTCCCAG-1","AACACGTCATCGGAAG-1","AACACGTCATCGTCGG-1",
                "AACACGTGTTATGTGC-1","AACACGTTCAACGGGA-1",
                "AACACGTTCGATAGAA-1","AACCATGAGCCAGTTT-1",
                "AACCATGAGGCCGAAT-1","AACCATGCACCCATTC-1","AACCATGCACGTAAGG-1",
                "AACCATGCAGAAGCAC-1","AACCATGTCCAGGGCT-1",
                "AACCATGTCTGGTATG-1","AACCATGTCTGTACGA-1",
                "AACCGCGAGATTACCC-1","AACCGCGAGCACCGTC-1","AACCGCGAGGTGCTTT-1",
                "AACCGCGAGTCCTCCT-1","AACCGCGAGTGTCCCG-1",
                "AACCGCGCAAAGGAAG-1","AACCGCGCATGTTCCC-1",
                "AACCGCGGTTCCAACA-1","AACCGCGTCAAGGTAA-1","AACGTTGAGCTGGAAC-1",
                "AACGTTGCATCCGCGA-1","AACGTTGGTAAGGATT-1",
                "AACGTTGGTGGGTCAA-1","AACGTTGTCAGCTCTC-1","AACGTTGTCCAGAAGG-1",
                "AACGTTGTCCGTAGGC-1","AACGTTGTCGTTACGA-1",
                "AACTCAGAGTAGGCCA-1","AACTCAGCAGGAATCG-1",
                "AACTCAGCATCTGGTA-1","AACTCAGGTTCGGGCT-1","AACTCAGTCCAATGGT-1",
                "AACTCAGTCCTCTAGC-1","AACTCAGTCTCTGTCG-1",
                "AACTCAGTCTTACCTA-1","AACTCCCAGGGCATGT-1",
                "AACTCCCCAAGGGTCA-1","AACTCCCCAGCTCCGA-1","AACTCCCCATTTCAGG-1",
                "AACTCCCGTACTCTCC-1","AACTCCCGTAGTAGTA-1",
                "AACTCCCTCATCGGAT-1","AACTCCCTCCACGTTC-1",
                "AACTCCCTCCAGGGCT-1","AACTCCCTCGTTGCCT-1","AACTCTTAGAACTGTA-1",
                "AACTCTTAGAGTGAGA-1","AACTCTTAGGCGCTCT-1",
                "AACTCTTCAAGGTGTG-1","AACTCTTTCATCTGTT-1","AACTCTTTCTTTCCTC-1",
                "AACTGGTAGAGGGCTT-1","AACTGGTAGCTACCTA-1",
                "AACTGGTCAATCCAAC-1","AACTGGTCAGCGTAAG-1",
                "AACTGGTCATCACCCT-1","AACTGGTCATCAGTCA-1","AACTGGTCATTTCACT-1",
                "AACTGGTGTCAACATC-1","AACTGGTGTGTCGCTG-1",
                "AACTGGTTCACGCATA-1","AACTGGTTCAGTTGAC-1",
                "AACTGGTTCGCCATAA-1","AACTGGTTCGTTTATC-1","AACTTTCAGCTCCTTC-1",
                "AACTTTCCATCGGTTA-1","AACTTTCGTGTAATGA-1",
                "AACTTTCGTGTTAAGA-1","AACTTTCTCAACCAAC-1",
                "AACTTTCTCAGCTTAG-1","AACTTTCTCCGCGTTT-1","AAGACCTAGACACGAC-1",
                "AAGACCTAGATGCGAC-1","AAGACCTAGTCCCACG-1",
                "AAGACCTAGTCCGTAT-1","AAGACCTGTCCCGACA-1","AAGACCTGTGTAAGTA-1",
                "AAGACCTTCCAAAGTC-1","AAGACCTTCTAACGGT-1",
                "AAGACCTTCTGCGTAA-1","AAGCCGCAGAACTGTA-1",
                "AAGCCGCAGATCTGAA-1","AAGCCGCAGTGTACGG-1","AAGCCGCGTATTACCG-1",
                "AAGCCGCGTTTAGGAA-1","AAGCCGCTCAGAGACG-1",
                "AAGGAGCAGAATCTCC-1","AAGGAGCAGTTGTAGA-1",
                "AAGGAGCCACGCCAGT-1","AAGGAGCCACGGATAG-1","AAGGAGCGTCTAAAGA-1",
                "AAGGAGCTCAAGATCC-1","AAGGAGCTCACTCCTG-1",
                "AAGGAGCTCCAACCAA-1","AAGGAGCTCGGATGGA-1",
                "AAGGAGCTCTTACCGC-1","AAGGCAGAGACAGGCT-1","AAGGCAGAGGGTTCCC-1",
                "AAGGCAGAGGTCATCT-1","AAGGCAGAGTCAAGCG-1",
                "AAGGCAGAGTTTCCTT-1","AAGGCAGCACAAGACG-1","AAGGCAGGTCCTCTTG-1",
                "AAGGCAGGTGGACGAT-1","AAGGCAGTCAGAGACG-1",
                "AAGGCAGTCCGTAGTA-1","AAGGTTCCAATAAGCA-1",
                "AAGGTTCCACTTCGAA-1","AAGGTTCCAGCATACT-1","AAGGTTCCAGGGATTG-1",
                "AAGGTTCCAGTATAAG-1","AAGGTTCGTGCAGTAG-1",
                "AAGGTTCGTTAGAACA-1","AAGGTTCTCCGCGCAA-1",
                "AAGTCTGAGGCATGGT-1","AAGTCTGCATCACAAC-1","AAGTCTGCATGCTAGT-1",
                "AAGTCTGGTCATTAGC-1","AAGTCTGGTCCAGTTA-1",
                "AAGTCTGGTTCCACTC-1","AATCCAGAGCGCCTCA-1",
                "AATCCAGAGGGTATCG-1","AATCCAGAGTGGAGTC-1","AATCCAGCAAGGGTCA-1",
                "AATCCAGGTAAGTAGT-1","AATCCAGGTATAGTAG-1"),
  Hashtag1 = c(1501,21,1366,21,
               17,2331,15,16,1250,21,21,5,18,23,1338,29,
               23,1890,11,11,18,13,24,11,2936,4759,13,20,
               1024,19,21,15,22,10,1829,880,14,17,2863,
               20,26,2966,17,19,42,40,18,20,12,2248,
               3272,14,18,17,18,31,15,1145,17,16,20,10,14,
               27,12,30,20,23,1031,10,2814,10,2023,15,
               11,15,17,1598,21,20,11,2827,14,21,2022,21,
               1650,16,14,18,33,34,16,2286,14,23,28,15,
               16,1417,25,20,5471,9,1584,12,3470,15,9,
               307,18,26,5460,11,14,1913,1822,1141,18,18,
               16,4031,16,30,18,10,1707,15,19,30,14,31,
               1417,15,20,13,15,18,22,13,16,15,14,22,7,
               27,25,2227,16,1588,11,3915,12,17,23,10,
               2677,11,12,16,12,25,12,19,33,18,11,23,19,
               20,5793,28,19,26,12,2041,18,17,1339,24),
  Hashtag2 = c(22,16,11,14,13,
               16,11,1300,30,937,8,1165,1496,12,15,11,14,
               10,14,10,1378,15,2130,3,11,16,11,18,24,
               22,1591,732,2888,16,16,11,1244,18,14,13,
               2287,13,20,12,28,25,12,575,458,21,130,11,
               8,570,822,23,1389,9,13,23,10,11,19,20,9,
               1148,12,7,14,6,264,5,7,902,668,9,738,11,
               13,531,16,151,22,10,8,20,15,3740,23,9,
               28,528,12,14,6,4852,5816,10,15,12,3633,
               1240,10,10,10,18,31,10,188,801,858,20,20,
               389,874,21,15,9,1097,2370,19,13,481,20,650,
               1757,7,11,16,24,10,15,22,3544,16,936,
               801,10,958,15,21,15,13,790,8,3812,14,11,
               748,19,10,26,12,4205,11,450,13,15,6,3483,
               4591,11,11,382,567,12,15,16,11,11,11,2025,
               12,1004,140,16,15,22,20,1021)
)
library(tidyverse)
hashes %>% 
  filter(Hashtag1 <= 15 | Hashtag2 <= 15) %>% 
  ggplot(aes(x=Hashtag1, y = Hashtag2)) +
  geom_point()

library(tidyverse)
library(magrittr)

sample_data = data.frame(id1 = rnorm(100),
                id2 = rnorm(100),
                id3 = rnorm(100),
                age = rnorm(100),
                sex = sample(x = c("male", "female"),
                             size = 100,
                             replace = TRUE),
                cost = rnorm(100))

sample_data %$%
  lm(formula = as.matrix(sample_data[1:3]) ~ age + sex + cost) %>% 
  summary()

#############################
set.seed(1);
tickets = NULL;
simNum = 5000;
for(i in 1:simNum){
  tickets = rbind(tickets, c(sum(box = sample(1:13, 3, replace = TRUE)), 
                             sum(box = sample(1:13, 3, replace = TRUE)), sum(box =  sample(1:13, 3, replace = TRUE))));
}
tickets = data.frame(tickets);
names(tickets) =c("sum1", "sum2", "sum3");


#create data frame of all the sums 
allSums = data.frame(c(tickets$sum1, tickets$sum2, tickets$sum3))
num_of_cols_in_proptable <- names(prop.table(table(allSums[1])))
colors_for_proptable <- ifelse(num_of_cols_in_proptable %in% c(7, 11, 21), "red", "white")

barplot(prop.table(table(allSums[1])), col = colors_for_proptable)
legend("right", legend = c("win", "lose"), fill = c("red", "white"))

##############################
#install.packages("passport")
library(passport)
test <- c("Australiar" ,"Austria", "Britain", "USA", "Canada")
parse_country(x = test, how = "regex", to = "en")

#############################

library(tidyverse)
library(titanic)
data("Titanic")
TrainRF <- na.omit(titanic_train)
TrainRF$Survived <- as.factor(TrainRF$Survived)
set.seed(1013)
random_model <- randomForest::randomForest(TrainRF$Survived~.,TrainRF)
p1 <- predict(random_model,TrainRF)
#############################

library(tidyverse)
library(ggrepel)
data("mtcars")
mtcars %>%
  sample_n(size = 10) %>% 
  select(disp) %>% 
  ggplot(aes(y = disp,
             x = "Type of car",
             label = rownames(.),
             color = rownames(.))) +
  geom_point(size = 4, alpha = 0.5) +
  geom_text_repel(nudge_x = -.5) +
  ylab("Displacement") +
  ggtitle("Cars (from 'mtcars')") +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

library(titanic)
library(ggbeeswarm)
data("Titanic")
titanic_train %>% 
  na.omit() %>% 
  sample_n(200) %>% 
  select(Age) %>% 
  ggplot(aes(y = Age,
             x = "Age of passangers")) +
  geom_boxplot() +
  geom_quasirandom(groupOnX = TRUE,
                   size = 2, alpha = 0.5) +
  ggtitle("Passengers (from 'titanic')") +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

data("cars")
cars %>% 
  na.omit() %>% 
  mutate(Average_speed = factor(speed)) %>% 
  count(Average_speed, name = "frequency") %>%
  sample_n(10) %>% 
  ggplot(aes(x = Average_speed,
             y = frequency)) +
  geom_col() +
  ggtitle("Average Speed (from 'cars')") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
  

data("Titanic")
titanic_train %>% 
  na.omit() %>% 
  sample_n(300) %>% 
  select(Fare, Survived) %>% 
  mutate(Survived = factor(Survived,
                           labels = c("Died",
                                      "Lived"))) %>% 
  ggplot(aes(y = Fare,
             x = Survived,
             group = Survived)) +
  geom_boxplot() +
  geom_quasirandom(groupOnX = TRUE,
                   size = 2, alpha = 0.5) +
  ggtitle("Passengers (from 'titanic')") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        legend.position = "none")


data("Titanic")
titanic_train %>% 
  na.omit() %>% 
  sample_n(200) %>%
  select(Parch) %>% 
  filter(Parch < 5) %>% 
  ggplot(aes(y = Parch)) +
  geom_bar() +
  ggtitle("Passengers (from 'titanic')") +
  ylab("Number of Parents/Children Aboard") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

titanic_train %>%
  na.omit() %>% 
  mutate(Survived = factor(Survived,
                           labels = c("Died",
                                      "Lived"))) %>%  
  mutate(Interaction = interaction(Survived, Sex)) %>% 
  select(Interaction, Age) %>% 
  ggplot(aes(y = Age,
             x = Interaction)) +
  geom_boxplot() +
  geom_quasirandom(aes(color = Interaction),
                   size = 2, alpha = 0.5) +
  coord_flip() +
  ggtitle("Passengers (from 'titanic')") +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

library(janitor)
library(ggpubr)
library(palmerpenguins)
part_1 <- penguins %>% 
  na.omit() %>% 
  select(species, flipper_length_mm) %>% 
  mutate(freq_table = cut(flipper_length_mm, breaks = 10)) %>% 
  ggplot(aes(y = species, x = freq_table, fill = flipper_length_mm)) +
  geom_tile() +
  ggtitle("'Palmer's Penguins' dataset") +
  scale_fill_continuous(name = "Flipper length (mm)") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

part_2 <- penguins %>% 
  na.omit() %>% 
  select(species, flipper_length_mm) %>% 
  mutate(freq_table = cut(flipper_length_mm, breaks = 10)) %>%
  tabyl(freq_table, species) %>% 
  ggtexttable()

ggarrange(part_1, part_2, ncol = 1)

####################################

remotes::install_github("mtoto/ufc.stats")
library(ufc.stats)
ufc_stats %>%
  group_by(fighter) %>%
  filter(winner == "W") %>% 
  count(winner, sort = TRUE)

############################
library(tidyverse)
library(palmerpenguins)
#install.packages("gt")
library(gt)
penguins %>% 
  group_by(species) %>%
  na.omit() %>% 
  mutate(Minimum = as.numeric(min(flipper_length_mm)),
         Maximum = max(bill_length_mm),
         Mean = mean(bill_depth_mm)) %>% 
  select(species, Minimum, Maximum, Mean) %>% 
  unique() %>% 
  gt(groupname_col = TRUE)

################################
library(tidyverse)
dataframe <- tribble(~"Rownames", ~"English_E", ~"Currier_C", ~"Primrose_P1", ~"Primrose_P2", ~"Bluetail_B", ~"Resource_Availability",
                     "Clay_lbs", 10, 15, 10, 10, 20, 130,
                     "Enamel_lbs", 1, 2, 2, 1, 1, 13,
                     "Dry_Room_hrs", 3, 1, 6, 6, 3, 13,
                     "Kiln_hrs", 2, 4, 2, 5, 3, 45,
                     "Contribution_to_Earning", 51, 102, 66, 66, 89, 23) %>% 
  column_to_rownames(var = "Rownames") %>% 
  mutate(across(-English_E, as.character))
str(dataframe)

################################
library(tidyverse)
dataset <- tribble(~"category", ~"rate",
                   "a", 0.5,
                   "a", 0.3,
                   "b", 0.2,
                   "b", 0.5,
                   "c", 0.3,
                   "c", 0.2,
                   "d", 0.5,
                   "d", 0.3
                   )
dataset %>% 
  mutate(group = ifelse(category %in% c("a", "b"), "A", "B")) %>% 
  filter(group == "B")

####################
library(tidyverse)
iris %>% 
  group_by(Species) %>%
  summarise_all(mean)
  
iris %>% 
  group_by(Species) %>%
  dplyr::summarise(across(.cols = everything(), .fns = list(mean(.[Species == "setosa"]) <= mean(.[Species == "virginica"]))))

ttest_fun <- function(x){
  x <- enquo(x)
  group_1 <- iris %>% 
    filter(Species == "setosa") %>% 
    select(!!x)
  group_2 <- iris %>% 
    filter(Species == "virginica") %>% 
    select(!!x)
  test <- t.test(group_1, group_2)
  p_val <- test$p.value
  print(p_val)
}

ttest_fun(Sepal.Length)

iris %>% 
  summarise(across(-Species, ttest_fun))

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
library(tidyverse)

data.frame(x = rnorm(n = 50, mean = 0.5, sd = 0.1)) %>%
  mutate(y = -2 * log((1 - x))) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 2.25) +
  theme_bw()

library(tidyverse)
data.frame(x = rnorm(n = 50, mean = 0.5, sd = 0.2)) %>%
  mutate(y = (-2 * log(1 - x))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_smooth(col = "black", se = FALSE) +
  geom_point(aes(x = median(x), y = median(y)),
             col = "blue", size = 4) +
  ggtitle(label = "test", subtitle = "test 2") +
  expand_limits(x = c(0,1))

#########################

library(tidyverse)
library(ggsci)
library(ggforce)

testdata <- data.frame(values = sort(rnorm(1000)))
testdata$type <- "known"
testdata[501:1000,2] <- "novel"

# Desired code
ggplot(testdata) +
  stat_bin(aes(x = values, fill = type),
           binwidth = 1, color="white") +
  scale_fill_npg() + theme_light() +
  facet_zoom(zoom.data = ifelse(type == "novel", NA, FALSE), 
             xlim = c(0, 4), ylim = c(0, 300),
             horizontal = TRUE)

###############
a <- c('id1','id2','id3','id4','id5')
b <- c(5,10,7,2,3)
d <- c(5.2,150,123,5,7)
e <- c(5.4,0,10,3,5)

df1 <- data.frame(a,b,d,e)
library(tidyverse)
df1 %>% 
  mutate(new_col = ifelse((b >= (0.95 * d) & b <= (1.05 * d) & d >= (0.95 * e) & d <= (1.05 * e)),
                          "TRUE", "FALSE"))

df1 <- as_tibble(data.frame(item = c('1','2','3','4'), 
                            selected_col1 = c('sec_3','sec_3','sec_4','sec_3'), 
                            selected_col2 = c('sec_4','sec_5',NA,'sec_6'),
                            selected_col3 = c('sec_5','sec_6',NA,NA),
                            selected_col4 = c(NA,NA,NA,NA),
                            sec_1 = c('lorem ipsum1', 'lorem ipsum2','lorem ipsum3','lorem ipsum4'),
                            sec_2 = c('lorem ipsum5', 'lorem ipsum6','lorem ipsum7','lorem ipsum8'),
                            sec_3 = c('lorem ipsum9', 'lorem ipsum10','lorem ipsum11','lorem ipsum12'),
                            sec_4 = c('lorem ipsum13', 'lorem ipsum14','lorem ipsum15','lorem ipsum16'),
                            sec_5 = c('lorem ipsum17','lorem ipsum18','lorem ipsum19','lorem ipsum20'),
                            sec_6 = c('lorem ipsum21', 'lorem ipsum22','lorem ipsum23','lorem ipsum24'),
                            sec_7 = c('lorem ipsum25',' lorem ipsum26','lorem ipsum27','lorem ipsum28')))
df2 <- as_tibble(data.frame(item = c('1','2','3','4'), 
                            selected_col1 = c('sec_3','sec_3','sec_4','sec_3'), 
                            selected_col2 = c('sec_4','sec_5',NA,'sec_6'),
                            selected_col3 = c('sec_5','sec_6',NA,NA),
                            selected_col4 = c(NA,NA,NA,NA),
                            sec_1 = c('lorem ipsum1', 'lorem ipsum2','lorem ipsum3','lorem ipsum4'),
                            sec_2 = c('lorem ipsum5', 'lorem ipsum6','lorem ipsum7','lorem ipsum8'),
                            sec_3 = c('lorem ipsum9', 'lorem ipsum10','lorem ipsum11','lorem ipsum12'),
                            sec_4 = c('lorem ipsum13', 'lorem ipsum14','lorem ipsum15','lorem ipsum16'),
                            sec_5 = c('lorem ipsum17','lorem ipsum18','lorem ipsum19','lorem ipsum20'),
                            sec_6 = c('lorem ipsum21', 'lorem ipsum22','lorem ipsum23','lorem ipsum24'),
                            sec_7 = c('lorem ipsum25',' lorem ipsum26','lorem ipsum27','lorem ipsum28'),
                            selection = c('lorem ipsum9 lorem ipsum13 lorem ipsum17','lorem ipsum10 lorem ipsum18 lorem ipsum22','lorem ipsum15','lorem ipsum12 lorem ipsum24')))


res <- list()
for (i in unique(df3$item)){
  df1 %>% 
    pivot_longer(cols = -c(item)) %>%
    filter(item == i) %>% 
    mutate(selection = ifelse(name %in% value, value, "NA")) %>% 
    filter(selection != "NA") -> res[[i]]
}

combined = do.call(rbind, res)
#############

library(tidyverse)
data.frame(X = rep(1:5, each = 25),
           Y = rep(factor(rev(LETTERS[-26]),
                          levels = rev(LETTERS[-26])), 5),
           Z = rnorm(125, 5, 1)) %>%
  mutate(X = ifelse(X %in% c(1,2,5), X, NA)) %>%
  na.omit() %>% 
  ggplot(aes(x = X, y = Y, fill = Z)) +
  geom_raster() +
  facet_wrap(~X, ncol=3, scales="free_x") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
######################

#install.packages("rvest")
library(rvest)
library(tidyverse)

links <- c("https://www.oncotarget.com/article/27825/text/",
           "https://www.oncotarget.com/article/27823/text/",
           "https://www.oncotarget.com/article/27824/text/",
           "https://www.oncotarget.com/article/27816/text/",
           "https://www.oncotarget.com/article/27820/text/",
           "https://www.oncotarget.com/article/24065/text/",
           "https://www.oncotarget.com/article/27815/text/",
           "https://www.oncotarget.com/article/27818/text/",
           "https://www.oncotarget.com/article/27817/text/",
           "https://www.oncotarget.com/article/27814/text/",
           "https://www.oncotarget.com/article/27801/text/",
           "https://www.oncotarget.com/article/27808/text/",
           "https://www.oncotarget.com/article/27811/text/",
           "https://www.oncotarget.com/article/27804/text/",
           "https://www.oncotarget.com/article/27800/text/",
           "https://www.oncotarget.com/article/27803/text/",
           "https://www.oncotarget.com/article/27794/text/",
           "https://www.oncotarget.com/article/27797/text/",
           "https://www.oncotarget.com/article/27786/text/",
           "https://www.oncotarget.com/article/27806/text/",
           "https://www.oncotarget.com/article/27796/text/",
           "https://www.oncotarget.com/article/27746/text/",
           "https://www.oncotarget.com/article/27655/text/",
           "https://www.oncotarget.com/article/27767/text/",
           "https://www.oncotarget.com/article/27782/text/",
           "https://www.oncotarget.com/article/27769/text/",
           "https://www.oncotarget.com/article/27784/text/",
           "https://www.oncotarget.com/article/27772/text/",
           "https://www.oncotarget.com/article/27775/text/",
           "https://www.oncotarget.com/article/27787/text/",
           "https://www.oncotarget.com/article/27790/text/",
           "https://www.oncotarget.com/article/27777/text/",
           "https://www.oncotarget.com/article/27774/text/",
           "https://www.oncotarget.com/article/27737/text/",
           "https://www.oncotarget.com/article/27783/text/",
           "https://www.oncotarget.com/article/27766/text/",
           "https://www.oncotarget.com/article/27743/text/",
           "https://www.oncotarget.com/article/27778/text/",
           "https://www.oncotarget.com/article/27776/text/",
           "https://www.oncotarget.com/article/27742/text/",
           "https://www.oncotarget.com/article/27763/text/",
           "https://www.oncotarget.com/article/27726/text/",
           "https://www.oncotarget.com/article/27759/text/",
           "https://www.oncotarget.com/article/27757/text/",
           "https://www.oncotarget.com/article/27750/text/",
           "https://www.oncotarget.com/article/27725/text/",
           "https://www.oncotarget.com/article/27702/text/",
           "https://www.oncotarget.com/article/27760/text/",
           "https://www.oncotarget.com/article/27747/text/",
           "https://www.oncotarget.com/article/27613/text/")

get_word_count <- function(link){
  scrape_link <- read_html(link)
  title <- scrape_link %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    print()
  word_count <- scrape_link %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    str_subset("doi.org|PubMed|Oncotarget|Metrics|\t\t\t",
               negate = TRUE) %>% 
    str_count('\\w+') %>% 
    sum()
  return(word_count)
}

list_of_results <- lapply(links, get_word_count)
colours_for_plot <- c("black", "red")

Wordcount <- unlist(list_of_results)
tibble(Wordcount) %>% 
  ggplot(aes(y = Wordcount,
             x = "Research Papers")) +
  geom_boxplot() +
  geom_jitter(aes(colour = "black")) +
  geom_point(aes(y = 3520, colour = "red")) +
  theme_bw(base_size = 16) +
  ggtitle("Oncotarget",
          subtitle = "50 most recently published papers") +
  theme(axis.title.x = element_blank(),
        plot.title.position = "plot") +
  scale_color_brewer(palette = "Set1", direction = -1,
                     labels = c("Published", "This paper"))

shapiro.test(Wordcount)


first_col <- "SpA"
secondFixedcol <- "SpecB"
other_cols <- c("C", "D", "E", "F")
interaction(first_col, secondFixedcol, other_cols)

######################
library(devtools)
install_github('andreacirilloac/updateR')
library(updateR)
updateR()

######################
library(rvest)
library(tidyverse)
library(gt)
barometer<-read_html("https://en.wikipedia.org/wiki/Global_Corruption_Barometer") %>% 
  html_nodes("table") %>%
  html_table()

barometer[1] %>% 
  as.data.frame() %>%
  rename(`Country/Territory` = Country.Territory,
         `% of people who paid bribes` = `X..of.people.who.paid.bribes.3.`) %>% 
  gt()
#####################
library(rvest)
url <- read_html("http://finviz.com/quote.ashx?t=AAPL")
scraped <- html_nodes(url,"table.snapshot-table2") %>%
  html_table(fill=TRUE) %>% data.frame()
#####################
contr.treatment(n = 5, contrasts = 4)[-1, -1]
lm(y ~ cond, data=all_data, contrasts=contr)
#####################
library(tidyverse)
data("midwest")
bar_chart <- function(midwest) {
  data_summary <- midwest %>%
    dplyr::group_by(state) %>%
    filter(state %in% c("IL", "MI", "IN", "OH", "WI")) %>% 
    summarize(poptotal = mean(poptotal))
  return(data_summary)
}
bar_chart(midwest)
#####################
library(tidyverse)
library(ggpubr)
data1 <- data.frame(
  V1=replicate(18, {sample(1:18, 1)}),
  V2=replicate(18, {sample(1:3, 1)}),
  V3=replicate(18, {sample(1:3, 1)})
)

graph1 <- ggplot(data1, aes(x=V1, y=V2, fill=V3))+geom_tile(size=0.5)+coord_equal()

data2 <- data.frame(
  V1=replicate(18, {sample(1:3, 1)}),
  V2=replicate(18, {sample(1:3, 1)})
)
graph2 <- ggplot(data2, aes(x=c("A"), y=V1,fill=V2))+geom_tile(size=0.5)+coord_equal()

ggarrange(graph1, graph2, labels = c("A", "B"), ncol = 2, nrow = 1, widths = c(0.85, 0.15))
ggsave(filename = "example.png", width = 12, height = 3, units = "in")
##################
library(rvest)
library(stringr)
library(reshape2)
library(tidyverse)

soundcloud <- read_html("https://soundcloud.com/charts/top?genre=all-music&country=CA")

artist_name <- soundcloud %>%
  html_nodes('a') %>%
  html_text()

##################
library(reshape2)
library(tidyverse)
#install.packages("microbenchmark")
library(microbenchmark)
m = matrix(1:6,2,3)
m_df <- as.data.frame(m)
microbenchmark::microbenchmark(list = c("matrix(m, dimnames=list(t(outer(colnames(m), rownames(m), FUN=paste)), NULL))",
                                        "melt(m_df)",
                                        "pivot_longer(m_df, cols = everything())"))
##################
library(tidyverse)
df <- mtcars[1:5, 1:6] %>% 
  format(round(2), signif(2), nsmall = 2)
df[5,] <- as.integer(df[5,])
df
###################
install.packages("nlme", type = "source")
###################
library(xts)
library(scales)
library(tidyverse)
library(lubridate)

set.seed(123)
day = seq(as.Date("2000/1/1"), as.Date("2020/1/1"), by = "day")
amount <- rnorm(7306, 100, 10)
data <- data.frame(day, amount)
y.mon <- stats::aggregate(amount ~ format(as.Date(day),
                                          format="%Y/%m"),
                          data = data,
                          FUN = sum)
names(y.mon) <- c("Date", "Amount")
y.mon$Date <- lubridate::ym(y.mon$Date)
str(y.mon)

ggplot(y.mon[-241,], aes(x = Date, y = Amount)) +
  geom_line(aes(group = 1)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y/%m",
               expand = c(0,0)) +
  scale_y_continuous(label = label_dollar(),
                     limits = c(2000,3300)) +
  theme(axis.text.x = element_text(angle = 55,
                                   hjust = 1.05))
####################
library(gt)
library(tidyverse)
library(glue)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 %>%
  dplyr::filter(date >= start_date & date <= end_date) %>%
  dplyr::select(-adj_close) %>%
  gt() %>%
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) %>%
  fmt_date(
    columns = vars(date),
    date_style = 3
  ) %>%
  fmt_currency(
    columns = vars(open, high, low, close),
    currency = "USD"
  ) %>%
  fmt_number(
    columns = vars(volume),
    suffixing = TRUE
  ) -> gt_fig

pdf(file = "test.pdf")
gt_fig
dev.off()
##########################
library(naniar)
library(tidyverse)
library(titanic)

data("Titanic")

titanic_train %>% 
  na.omit() %>% 
  sample_n(200) %>%
  select(Parch) %>% 
  filter(Parch < 5) %>% 
  ggplot(aes(y = Parch)) +
  geom_bar() +
  ggtitle("Passengers (from 'titanic')") +
  ylab("Number of Parents/Children Aboard") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

#install.packages("naniar")
vis_miss(titanic_train, warn_large_data=TRUE) +
  theme(axis.text.x = element_text(size = 6, angle = 60))

vis_miss(titanic_train, warn_large_data=TRUE) +
  theme(axis.text.x = element_blank())
################
install.packages("data.table", type = "source")
install.packages("nlme", type = "source")
###############
## reprex
library(tidyverse)
library(ggplot2)
library(scales)
dat <- data.frame(name = c(rep("bill", 3), rep("maria", 3), rep("claudio", 3)),
                  lap = rep(0:2, 3),
                  pos = c(1, 1, 2, 
                          2, 3, 1,
                          3, 2, 3))
dat$pos <- factor(dat$pos, levels = c(3, 2, 1),
                  labels = c("Third", "Second", "First"),
                  ordered = TRUE)
dat %>% 
  ggplot(aes(x = lap, y = pos,
             color = name, group = name)) +
  geom_point() +
  geom_path() +
  geom_text(data = subset(dat, lap == 2),
            aes(label = name, colour = name, x = Inf, y = pos),
            hjust = -.1) +
  scale_x_continuous(breaks = pretty_breaks(3)) +
  theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(1,3,1,1), "lines")) 
############
library(datasets)
library(ggplot2)
#install.packages("gghighlight")
library(gghighlight)

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

iris$cluster <- as.factor(irisCluster$cluster)
  ggplot(iris, aes(Petal.Length, Petal.Width, color = factor(cluster))) +
    geom_point() +
  gghighlight(cluster == 3, keep_scales = TRUE)

########################
library(tidyverse)
options(scipen = 10000000)
var1 <- tribble(~'empfte',~'empft_rate',~'wage_st',~'wage_min',~'pmeal',~'hrsopen',~'bonus',
                  3, 3, 0.7, 22, 2.0, 8, 5,
                  4, 1, 0.7, 22, 2.5, 9, 5.1,
                  2, 1, 0.6, 22, 2.1, 2, 5.2,
                  3, 6, 0.8, 22, 2.9, 5, 5.3,
                  3, 6, 0.8, 22, 2.9, 5, 5.4,
                  3, 5, 0.8, 22, 2.9, 5, 5.8)

t.test_func <- function(name){
  test <- t.test(x = var1$empfte, y = name)
  return(test$p.value)
}

list_of_results <- lapply(var1, t.test_func)
table <- t(data.frame("empfte vs" = list_of_results))
colnames(table) <- c("P-Value")
table

library(xtable)
xtable(t(data.frame("empfte vs" = list_of_results)))
#################
counts_dds <- counts(dds)
topgenes <- c("ENSDARG00000000002", "ENSDARG00000000489", "ENSDARG00000000503",
              "ENSDARG00000000540", "ENSDARG00000000529", "ENSDARG00000000542")

heatmap.data <- counts_dds[rownames(counts_dds) %in% topgenes,]
##############
library(tidyverse)
library(palmerpenguins)
#install.packages("cmocean")
library(cmocean)
library(cowplot)
library(ggbeeswarm)

plot_1 <- penguins %>% 
  na.omit() %>%
  ggplot(aes(x = species, y = flipper_length_mm, color = body_mass_g)) +
  geom_quasirandom()  +
  scale_color_cmocean(name = "deep")

plot_2 <- penguins %>% 
  na.omit() %>%
  ggplot(aes(x = species,
             y = flipper_length_mm,
             color = body_mass_g)) +
  geom_quasirandom()  +
  scale_color_cmocean(name = "deep",
                      limits = c(2500, 4000),
                      oob = scales::squish,
                      labels = c("2500", "3000", "3500", "4000+"))
cowplot::plot_grid(plot_1, plot_2, labels = "AUTO")

####################
library(ggplot2)
library(dplyr)
#install.packages("ggrepel")
library(ggrepel)
#Code
mpg %>%
  mutate(Color=ifelse(class == '2seater','2seater','Other')) %>%
  ggplot(aes(displ, hwy, colour = Color)) + 
  geom_point() +
  geom_text_repel(aes(label = ifelse(Color == '2seater', '2seater', "")),
                   force_pull = 0, show.legend = FALSE)

##################
library(data.table)
DT <- data.table( 
  Pol_No = c('a','b','b','c','c','c','c','c','c','c')  
  , Veh_No = c(1,1,2,1,1,1,2,3,3,3)
  , Value = c(1,1,2,3,4,5,6,3,4,5)
)
DT
######################
# Load libraries
library(tidyverse)

# Create dataframe
data <- tibble(income = c(1000,2000,5000),
               percentage = c(10,30,60))

# Change 'income' to a factor and specify levels/labels
data$income <- factor(data$income, 
                      levels = c("5000",
                                 "2000",
                                 "1000"),
                      labels = c("2000-5000",
                                 "1000-2000",
                                 "0-1000"))

# Plot the data
# This type of plot requires a 'value' for the x axis
# This can be a string ("X axis title") or blank ("")
ggplot(data, aes(fill = income, x = "X axis title", y = percentage)) +
  geom_bar(position="fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent)
########################
library(tidyverse)
library(ggsignif)
mydata <- data.frame(Rep=rep(paste0('rep',1:5), 4),
                     Population=rep(paste0(LETTERS[1:4],'cells'), each=5),
                     Value=c(runif(15,1,5), runif(5, 30,40)))


P <- ggplot(mydata, aes(x = Rep, y = Value)) +
  geom_point(color = "red", shape = 16, size = 5) +
  geom_line(aes(group = 1)) +
  facet_wrap(. ~Population, ncol = 2) +
  theme_light() +
  ylim(c(0,60)) +
  geom_signif(comparisons=list(c("rep1", "rep2"),
                               c("rep1", "rep3"),
                               c("rep3", "rep4")),
              annotations = c("foo 1v2", "foo 1v3", "foo 3v4"),
              textsize=4,
              size=1,
              step_increase = 0.1)

# build the plot (get a list of data frames out of it)
P2 <- ggplot2::ggplot_build(P)
# in list 3 we have access to each annotation
head(P2$data[[3]], 20)
# plot
grDevices::pdf.options(reset = TRUE, onefile = FALSE)
grDevices::pdf(file="test.pdf", height=10, width=10)
print(#or ggsave()
  graphics::plot(ggplot2::ggplot_gtable(P2))
)
grDevices::dev.off()
############
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
BiocManager::install(version = "3.12")
BiocManager::install(c("Homo.sapiens", "EnsDb.Hsapiens.v75", "TxDb.Hsapiens.UCSC.hg19.knownGene"))
############
x <- c(1,2,3,4,1,7,1)
y <- c("A","b","a","F","A",".A.","B")
data <- cbind(x,y)

xnew <- c(1,1,3,7,1,2,4)
ynew <- c("A","A","a",".A.","B","b","F")
datanew <- cbind(xnew,ynew)


library(dplyr)
library(tidyverse)

data %>% 
  as.data.frame() %>% 
  group_by(x, y) %>% 
  summarise(records = n()) %>% 
  arrange(y, x)

data %>%
  as.data.frame() %>% 
  mutate(z = toupper(gsub("[[:punct:]]", "", y))) %>%
  arrange(z, x) %>%
  select(-z)
##############
install.packages("vroom")
library(vroom)
data <- vroom("https://www.bfs.admin.ch/bfsstatic/dam/assets/15324797/master")
############
# For loop method
t = seq(0.01, 5, by = 0.001)
result <- data.frame()
for(i in seq_along(t)){
    result[i, 1] <- t[i]
    result[i, 2] <- round(2^t[i], 3)
    result[i, 3] <- round(exp(-3*t[i]), 3)
}
summary(result)

# Vectorized / faster method
dat <- data.frame(t = seq(0.01, 5, by = 0.001))
dat <- data.frame(t = seq(0.01, 5, by = 0.001),
                  a = round(2^dat$t, 3),
                  b = round(exp(-3 * dat$t), 3))
summary(dat)
#############
library(tidyverse)
library(cowplot)
df <- structure(list(Variables = c("AREA_MN_LAND", "Semi_habitats_PLAND", 
                               "SHDI_CROP"), Estimate = c(-0.463930572435947, 0.0937050717425011, 
                                                          0.925024309144037), Std.Error = c(0.164850815862808, 0.0345256062907729, 
                                                                                            0.556521436298426), lowerCI = c(-0.787032234349095, 0.0260361268681767, 
                                                                                                                            -0.16573766262538), upperCI = c(-0.1408289105228, 0.161374016616825, 
                                                                                                                                                            2.01578628091345), importance = structure(c(1, 1, 0.589768028982561
                                                                                                                                                            ), n.models = c(AREA_MN_LAND = 2, Semi_habitats_PLAND = 2, SHDI_CROP = 1
                                                                                                                                                            ), class = c("sw", "numeric"))), row.names = c(NA, -3L), class = "data.frame")

p1 <- df %>%
  ggplot(aes(x = Variables, y = Estimate, color = importance)) + 
  geom_hline( yintercept = 0, color = 'red', linetype = 'dashed', lwd = 0.5) +
  geom_errorbar(aes(ymin = Estimate - lowerCI, ymax = Estimate + upperCI),
                width = 0, lwd = 1.5) + 
  geom_text(aes(label = Variables), nudge_y = 0.5, nudge_x = 0.1) +
  coord_flip() +
  geom_point(size = 4)  +
  scale_color_gradient(low="blue", high="red")  +
  scale_x_discrete(limits=c("AREA_MN_LAND","Semi_habitats_PLAND","SHDI_CROP"),
                   name = "") +
  theme_minimal(base_size = 16) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

p2 <- df %>% 
  ggplot(aes(x = "", y = importance, fill = Estimate)) +
  geom_bar(position = "fill", stat = "identity", color = "black") +
  geom_text(aes(y = c(.2, .6, .9), label = Variables),
            color = "white", angle = 90) +
  scale_y_continuous(labels = scales::percent, name = "Relative Importance") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.text.x = element_blank(), panel.grid.major.x = element_blank())


cowplot::plot_grid(p2, p1, nrow = 1, rel_widths = c(0.4, 1))
###############
library(tidyverse)
color_names <- c("black", "grey", "white", "purple")

my_colors <- tribble(
  ~black, ~grey, ~white,  ~purple,
  NA,     "grey", NA,     "purple",
  NA,     NA,     "white", NA,
  "black",NA,     NA,      NA,
  NA,     "grey",  NA,     NA
)

my_colors %>% 
  mutate(across(.cols = everything(),
                .fns = ~ifelse(is.na(.x), 0, 1)))
############
library(tidyverse)
TP_FP <- tribble(~Value, ~Group,        
                -0.00540, "False positive",
                0.331, "True positive",
                -1.11, "False positive",
                1.4365, "False positive",
                -0.586, "True positive",
                1.26, "True positive",
                0.5463, "False positive",
                3.245, "False positive",
                -0.950, "False positive",
                10.4354, "True positive")

ggplot() +
  geom_histogram(data = TP_FP %>% filter(Group == "False positive"),
                 aes(y = -(..density..), x = Value, fill = Group),
                 col = "black", binwidth = 0.1) +
  geom_histogram(data = TP_FP %>% filter(Group == "True positive"),
                 aes(y = ..density.., x = Value, fill = Group),
                 col = "black", binwidth = 0.1) +
  labs(x = "R Ratio", y = "Number of proteins") +
  theme_bw() + theme(legend.position = "right")
###############
# library(rpart)
# car.test.frame$Reliability = as.factor(car.test.frame$Reliability)
# z.auto <- rpart(Reliability ~ ., car.test.frame)
# plot(z.auto, margin = 0.25)
# text(z.auto, pretty = TRUE, cex = 0.8,
#      splits = TRUE, use.n = TRUE, all = FALSE)
# 
# list.rules.rpart <- function(model)
# {
#   if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
#   #
#   # Get some information.
#   #
#   frm     <- model$frame
#   names   <- row.names(frm)
#   ylevels <- attr(model, "ylevels")
#   ds.size <- model$frame[1,]$n
#   #
#   # Print each leaf node as a rule.
#   #
#   for (i in 1:nrow(frm))
#   {
#     if (frm[i,1] == "<leaf>")
#     {
#       # The following [,5] is hardwired - needs work!
#       cat("\n")
#       cat(sprintf(" Rule number: %s ", names[i]))
#       cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
#                   ylevels[frm[i,]$yval], frm[i,]$n,
#                   round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
#       pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
#       cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
#     }
#   }
# }
# 
# list.rules.rpart(z.auto)
##############
spells <- structure(list(id = c("10003A", "1001", "10012A", "10013A", "10016A", 
                      "10019A", "1001A", "10023A", "1002A", "10037A", "1004", "10042A", 
                      "10045A", "1005", "10051A", "10054A", "1006", "10064A", "10065A", 
                      "10075A", "10076A", "10082A", "10087A", "10094A", "10095A", "10097A", 
                      "10098A", "100A", "10103A", "10104A", "10106A", "10121A", "10124A", 
                      "10126A", "10132A", "1013A", "10144A", "10146A", "1014A", "1015", 
                      "10153A", "10156A", "10159A", "10161A", "1017", "10171A", "10175A", 
                      "10178A", "1018", "10186A"), whz1 = c(0, 1, 0, 0, 0, 0, 0, 1, 
                                                            0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 
                                                            1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0
                      ), whz2 = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 
                                  0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0), whz3 = c(0, 0, 0, 0, 0, 
                                                                                0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 
                                                                                0, 0, 0), whz4 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 
                                                                                                   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                   0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), whz5 = c(0, 0, 
                                                                                                                                                          0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 
                                                                                                                                                          0, 0, 0, 0, 0, 0), PAM_complete = c("ETW", "ETW", "NWZ", "NWZ", 
                                                                                                                                                                                              "LOW", "NWZ", "NWZ", "INT", "NWZ", "ETW", "NWZ", "PEW", "ETW", 
                                                                                                                                                                                              "INT", "NWZ", "INT", "ETW", "NWZ", "ETW", "ETW", "NWZ", "ETW", 
                                                                                                                                                                                              "ETW", "NWZ", "NWZ", "NWZ", "NWZ", "NWZ", "NWZ", "PEW", "NWZ", 
                                                                                                                                                                                              "ETW", "NWZ", "INT", "NWZ", "INT", "NWZ", "INT", "NWZ", "LOW", 
                                                                                                                                                                                              "PEW", "NWZ", "NWZ", "INT", "ETW", "NWZ", "ETW", "NWZ", "ETW", 
                                                                                                                                                                                              "NWZ")), row.names = c(NA, -50L), class = c("tbl_df", "tbl", 
                                                                                                                                                                                                                                          "data.frame"))
library(tidyverse)
library(pheatmap)
library(cowplot)

spells2 <- as.data.frame(spells) %>%
  arrange(PAM_complete)

#Df for wheeze columns
whz <- spells2 %>%
  dplyr::select(2:6)

#Create separate df for cluster  
c5 <- spells2$PAM_complete %>% 
  as.data.frame()
colnames(c5) <- "names"

#Wheeze and cluster need the same row names (id)
rownames(whz) <- spells2$id
rownames(c5) <- spells2$id

c5$names <- as.factor(c5$names)

combined <- cbind(c5, whz)

# To get the 'default' pheatmap colour scheme
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 75, c = 100)[1:n]
}
scales::show_col(gg_color_hue(5))

# Specify colours for each group
ann_colors = list(
  names = c(ETW = "#FF9289", INT = "#FF8AFF",
            LOW = "#00DB98", NWZ = "#00CBFF",
            PEW = "#BEC100"))


# Generate the plots
col = c("grey95", "darkblue")
p <- pheatmap(whz, legend_breaks = 0:1,
         legend_labels = c("No wheeze", "Wheeze"),
         fontsize = 10, show_rownames = FALSE,
         cluster_rows = FALSE, color = col,
         cluster_cols = FALSE, annotation_row = c5)

col_1 <- c("grey95", "#FF9289")
p1 <- pheatmap(combined %>% filter(names == "ETW") %>% select(-c(names)),
         show_rownames = FALSE, show_colnames = FALSE,
         cluster_rows = FALSE, cluster_cols = FALSE,
         legend = FALSE, annotation_legend = FALSE,
         color = col_1, annotation_names_row = FALSE,
         annotation_colors = ann_colors, 
         annotation_row = combined %>% filter(names == "ETW") %>% select(names))

col_2 <- c("grey95", "#FF8AFF")
p2 <- pheatmap(combined %>% filter(names == "INT") %>% select(-c(names)),
         show_rownames = FALSE, show_colnames = FALSE,
         cluster_rows = FALSE, cluster_cols = FALSE,
         legend = FALSE, annotation_legend = FALSE,
         color = col_2, annotation_names_row = FALSE,
         annotation_colors = ann_colors, cellheight = 7,
         annotation_row = combined %>% filter(names == "INT") %>% select(names))

col_3 <- c("grey95", "#00DB98")
p3 <- pheatmap(combined %>% filter(names == "LOW") %>% select(-c(names)),
               show_rownames = FALSE, show_colnames = FALSE,
               cluster_rows = FALSE, cluster_cols = FALSE,
               legend = FALSE, annotation_legend = FALSE,
               color = col_3, annotation_names_row = FALSE,
               annotation_colors = ann_colors,
               annotation_row = combined %>% filter(names == "LOW") %>% select(names))

# Because all whz values = 0 for NWZ,
# you need to change one value to '1'
# in order for pheatmap to generate a plot
combined[23,2] <- 1

col_4 <- c("grey95", "grey95")
p4 <- pheatmap(combined %>% filter(names == "NWZ") %>% select(-c(names)),
               show_rownames = FALSE, show_colnames = FALSE,
               cluster_rows = FALSE, cluster_cols = FALSE,
               legend = FALSE, annotation_legend = FALSE,
               color = col_4, annotation_names_row = FALSE,
               annotation_colors = ann_colors,
               annotation_row = combined %>% filter(names == "NWZ") %>% select(names))

col_5 <- c("grey95", "#BEC100")
p5 <- pheatmap(combined %>% filter(names == "PEW") %>% select(-c(names)),
               show_rownames = FALSE,
               cluster_rows = FALSE, cluster_cols = FALSE,
               legend = FALSE, annotation_legend = FALSE,
               color = col_5,
               annotation_colors = ann_colors,
               annotation_row = combined %>% filter(names == "PEW") %>% select(names))

heatmaps <- cowplot::plot_grid(p1[[4]], p2[[4]], p3[[4]],
                   p4[[4]], p5[[4]], ncol = 1,
                   rel_heights = c(1.3, 0.7, 0.3, 2.4, 0.8))
cowplot::plot_grid(heatmaps, p$gtable, ncol = 2, rel_widths = c(0.7, 1), labels = "AUTO")

my_levels <- rownames(combined)
my_colours <- c("#FF9289", "#FF8AFF", "#00DB98", "#00CBFF", "#BEC100")

combined %>%
  rownames_to_column(var = "IDs") %>% 
  pivot_longer(cols = -c(IDs, names),
               names_to = "Trial",
               values_to = "Wheeze") %>%
  rename(Group = names) %>% 
  mutate(IDs = factor(IDs, levels = my_levels)) %>% 
  ggplot() +
  geom_tile(aes(y = rev(IDs),
                x = Trial,
                fill = Group,
                alpha = Wheeze),
            color = "black") +
  scale_alpha_continuous(breaks = c(0, 1),
                         labels = c("No", "Yes")) +
  scale_fill_manual(values = my_colours) +
  theme_minimal() +
  theme(panel.grid = element_blank())

combined %>%
  rownames_to_column(var = "IDs") %>% 
  pivot_longer(cols = -c(IDs, names),
               names_to = "Trial",
               values_to = "Wheeze") %>%
  rename(Group = names) %>% 
  mutate(IDs = factor(IDs, levels = my_levels)) %>% 
  ggplot() +
  geom_tile(aes(y = rev(IDs),
                x = Trial,
                fill = Group,
                alpha = Wheeze),
            color = "black") +
  geom_tile(aes(x = -0.1, y = rev(IDs), fill = Group),
            show.legend = FALSE) +
  coord_cartesian(c(0.8, 5)) +
  scale_fill_manual(values = my_colours) +
  scale_alpha_continuous(breaks = c(0, 1),
                         labels = c("No", "Yes")) +
  theme(plot.margin=unit(c(1,0,0,0), units="lines"))

#############
library(tidyverse)
# Define what "1" and "2" relate to (Male / Female)
gender_code <- c("1" = "Male", "2" = "Female")
# Create some 'fake' data to plot
tibble("Gender" = sample(1:2, 50, replace = TRUE),
       "Value" = rnorm(50, 10, 1)) %>% 
  # Change "Gender" from an integer to a factor
  mutate("Gender" = as.factor(Gender)) %>% 
  # Plot the data by splitting Males and Females into
  # two separate boxes using facet_grid
  ggplot(aes(x = Value)) +
  geom_histogram(aes(fill = Gender)) +
  facet_grid(cols = vars(Gender),
             labeller = labeller(Gender = gender_code))
############
library(tidyverse)
library(ggsignif)

d <- tibble(
  Rodent = c(rep("Mouse",36),
             rep("Hamster",32),
             rep("Guinea pig",29),
             rep("Gerbil",37)),
  `Weight (gm)` = rnorm(134,25,20),
  `Long whiskers` = c(rep("+",36),rep("-",32),rep("+",29),rep("-",37)),
  `Long tail` = c(rep("+",36),rep("-",32),rep("+",29),rep("-",37)),
)

p1 <- d %>% 
  ggplot(aes(x = Rodent,y = `Weight (gm)`)) +
  geom_boxplot() +
  theme_minimal(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20)) +
  geom_signif(comparisons = list(c("Gerbil", "Guinea pig")))

d1 <- d %>% 
  select(-`Weight (gm)`) %>%
  count(Rodent, `Long whiskers`, `Long tail`) %>% 
  distinct() %>% 
  mutate(n = as.character(n)) %>% 
  pivot_longer(-Rodent)

p2 <- d1 %>% 
  ggplot(aes(Rodent, fct_rev(name), label = value)) +
  geom_text(size = 8) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14))

library(cowplot)
plot_grid(p1, p2, ncol = 1, align = "v",
          rel_heights = c(1, 0.5))
############
library(caret)
set.seed(300)
trainIndex <- createDataPartition(iris$Species, p = .75, 
                                  list = FALSE, 
                                  times = 1)
irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]

str(irisTrain)
str(irisTest)
prop.table(table(irisTrain$Species))
prop.table(table(irisTest$Species))
#############
#install.packages("bmem")
library(bmem)

powermodel1 <-'

x2 ~ start(.6)*x1 + x*x1 
x3 ~ start(.6)*x2 + x*x2

m2 ~ start(.15)*x1 + a*x1 + start(.3)*m1 + m*m1 
m3 ~ start(.15)*x2 + a*x2 + start(.3)*m2 + m*m2

y2 ~ start(.5)*m1 + b*m2 + start(.3)*y1 + y*y1
y3 ~ start(.5)*m2 + b*m2 + start(.3)*y2 + y*y2 + start(0.05)*x1 + c*x1

x1 ~~ start(.15)*m1

x1 ~~ start(.15)*y1


y1 ~~ start(.5)*m1

'
indirect <- 'ab:=a*b'

N<-200

system.time(bootstrap<-bmem::power.boot(powermodel1, indirect, N, nrep = 10, nboot = 10, parallel = 'multicore'))
system.time(bootstrap<-bmem::power.boot(powermodel1, indirect, N, nrep = 30, nboot = 30, parallel = 'multicore'))
system.time(bootstrap<-bmem::power.boot(powermodel1, indirect, N, nrep = 60, nboot = 60, parallel = 'multicore'))
system.time(bootstrap<-bmem::power.boot(powermodel1, indirect, N, nrep = 100, nboot = 100, parallel = 'multicore'))

library(tidyverse)
benchmark <- tibble(bootstraps = c(10, 30, 60, 100),
                    times = c(4.021, 30.122, 121.103, 311.236)) 

ggplot(benchmark, aes(x = bootstraps, y = times)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 5)

fit <- lm(data = benchmark, times~poly(bootstraps,
                                       2, raw=TRUE))
summary(fit)
newtimes <- data.frame(bootstraps = seq(100, 1000, length = 4))
predict(fit, newdata = newtimes)

print(27975.5655/60/60)
####################
library(Cairo)
library(tidyverse)
x <- 1:10; y = x*x
plot = qplot(x, y, geom=c("point", "line"))

#ggsave
ggsave(plot, filename = "test.png", 
       width = 9, height = 15, units = "cm", dpi = 300, type = "cairo")

#Cairo
Cairo(90, 150, file="test2.png", type="png", bg="white", res = 300)
plot
dev.off()
##################
library(tidyverse)
#install.packages("ggpmisc")
library(ggpmisc)

df <- structure(list(mouse_ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
                                  10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 22L, 23L, 24L, 25L, 
                                  26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 
                                  39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
                                  52L, 53L, 54L, 55L), treatment = structure(c(1L, 1L, 1L, 1L, 
                                                                               1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
                                                                               3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 
                                                                               5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L
                                  ), .Label = c("not challenged", "vehicle control", "high", 
                                                "medium", "low", "reference"
                                  ), class = "factor"), value.x = c(0.003725, 0.0208, 0.004475, 
                                                                    0, 0.00895, 1.00625, 1.0125, 1.014, 1.1025, 0.925, 0.897, 0.99, 
                                                                    1.1495, 1.0125, 1.08, 0.88425, 1.001, 0.864, 0.89175, 0.9425, 
                                                                    0.943, 1.07325, 0.73575, 0.606, 0.682, 0.79925, 0.87, 0.60225, 
                                                                    0.756, 0.891, 0.6555, 0.572, 0.253, 0.255, 0.396, 0.4495, 0.299, 
                                                                    0.39, 0.3, 0.5365, 0.378, 0.475, 0.73575, 0.4895, 0.468, 0.90625, 
                                                                    0.3905, 0.4995, 0.60375, 0.744, 0.75, 0.5535), value.y = c(0, 
                                                                                                                               0, 0, 0, 0, 5.775, 4.6875, 4.992, 7.245, 6.0125, 3.795, 4.99125, 
                                                                                                                               7.26275, 4.35375, 4.3875, 3.6025, 4.389, 3.852, 3.444, 4.205, 
                                                                                                                               5.207, 4.77, 3.052, 2.65125, 2.024, 3.6835, 2.9, 1.5695, 2.7, 
                                                                                                                               2.619, 2.964, 1.936, 0.539, 0.408, 1.056, 1.085, 0.897, 0.795, 
                                                                                                                               0.5, 1.0915, 0.5355, 0.575, 2.8885, 2.0915, 1.755, 3.40625, 1.42, 
                                                                                                                               1.6095, 2.835, 2.3715, 2.7, 1.927)), row.names = c(NA, -52L), 
                class = c("tbl_df", "tbl", "data.frame"))

ggplot(data = df, aes(x = value.x, y = value.y)) +
  geom_point(aes(color = treatment)) +
  geom_smooth(method = lm, se = TRUE, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
               parse = TRUE)
##############
set.seed(300)
weights <- rnorm(50, 3000, 1000)
hist(weights)

h <- hist(weights)
weight <- c(min(h$breaks), h$breaks)
density <- c(0, h$density, 0)
xfit <- seq(min(weights), max(weights), length = 40)
yfit <- dnorm(xfit, mean = mean(weights), sd = sd(weights))
plot(weight, density, type="s", ylim=c(0, max(density, yfit)),
     main = "Histogram of weight with fitted curve")
lines(xfit, yfit, col="red")

##############
#install.packages("remotes")
library(remotes)
remotes::install_github('cole-trapnell-lab/leidenbase')

##############
library(MASS)
set.seed(300)
par(xpd=TRUE)
par(mar=c(4, 4, 4, 6))
a = rnorm(12, 10, 10)
b = rnorm(12, 10, 5)
c = rnorm(12, 5, 10)
group <- sample(c("#FF9289", "#FF8AFF", "#00DB98", "#00CBFF"),
                12, replace=TRUE)

d = cbind(a, b, c)
rownames(d) <- group

parcoord(d[, c(3, 1, 2)], col = group)
title(main = "Plot", xlab = "Variable", ylab = "Values")
axis(side = 2, at = seq(0, 1, 0.1),
     tick = TRUE, las = 1)
legend(3.05, 1, legend = c("A", "B", "C", "D"), lty = 1,
       col = c("#FF9289", "#FF8AFF", "#00DB98", "#00CBFF"))

##############
library(lubridate)
date1<-ymd("2021/01/01")
date2<-ymd("2021/01/31")
dates <- seq(date1, date2, 1)
sample(dates, 10)

##############
library(MASS)
set.seed(300)
par(xpd=TRUE)
par(mar=c(4, 4, 4, 6))
a = rnorm(12, 10, 10)
b = c(rep("Var_1", 3),
      rep("Var_2", 3),
      rep("Var_3", 3),
      rep("Var_4", 3))
c = rnorm(12, 5, 10)
group <- sample(c("#FF9289", "#FF8AFF", "#00DB98", "#00CBFF"),
                12, replace=TRUE)

d = data.frame("A" = a,
          "B" = factor(b,
                       labels = c(0.2, 0.4, 0.6, 0.8),
                       levels = c("Var_1", "Var_2",
                                  "Var_3", "Var_4"),
                       ordered = TRUE),
          "C" = c,
          "Group" = group)

parcoord(d[, c(1, 2, 3)], col = group)
title(main = "Plot", xlab = "Variable", ylab = "Values")
axis(side = 2, at = seq(0, 1, 0.1),
     tick = TRUE, las = 1)
legend(3.05, 1, legend = c("A", "B", "C", "D"), lty = 1,
       col = c("#FF9289", "#FF8AFF", "#00DB98", "#00CBFF"))


library(MASS)
set.seed(123)
par(xpd=TRUE)
par(mar=c(4, 4, 4, 6))
a = rnorm(12, 10, 10)
b = c(rep("Var_1", 3),
      rep("Var_2", 3),
      rep("Var_3", 3),
      rep("Var_4", 3))
c = rnorm(12, 5, 10)
group <- c(rep("#FF9289", 3),
           rep("#FF8AFF", 3),
           rep("#00DB98", 3),
           rep("#00CBFF", 3))

d = data.frame("A" = a,
               "Factor" = b,
               "C" = c,
               "Group" = group)

d$Factor <- sapply(d$Factor, switch,
                   "Var_1" = 0.8,
                   "Var_2" = 0.6,
                   "Var_3" = 0.4,
                   "Var_4" = 0.2)

parcoord(d[, c(1, 2, 3)], col = group)
title(main = "Plot", xlab = "Variable", ylab = "Values")
axis(side = 2, at = seq(0, 1, 0.1),
     tick = TRUE, las = 1)
legend(3.05, 1, legend = c("A", "B", "C", "D"), lty = 1,
       col = c("#FF9289", "#FF8AFF", "#00DB98", "#00CBFF"))
mtext(text = "Var 1", side = 1, adj = 0.6, padj = -30)
mtext(text = "Var 3", side = 1, adj = 0.6, padj = -12)
mtext(text = "Var 2", side = 1, adj = 0.6, padj = -21)
mtext(text = "Var 4", side = 1, adj = 0.6, padj = -3)
#############
#install.packages("keras")
library(keras)
library(magrittr)

mnist <- dataset_mnist()
X <- mnist$train$x

Xreduced <- X / 255

pcaX <- princomp(Xreduced)
Xfinal <- as.matrix(Xreduced) %*% pcaX$loadings[,1:45]

library(factoextra)
res.pca <- prcomp(Xreduced, scale = FALSE)
fviz_eig(res.pca)
#############
#install.packages("vroom")
library(vroom)
library(data.table)

file <- vroom(file = "test.csv", delim = ",",
      col_names = TRUE, na = character())
setDT(file)
str(file)
#install.packages("data.table", type = "source")
library(data.table)
file <- fread(file = "test.csv", quote = '"')
View(file)
#############
f = c((1:99)/100)
true.ci = 0
lowerpi <- c(0.000000000, -0.036814587, -0.026344980, -0.007382581,  0.016293067,  0.043202881, rep(0, 93))
upperpi <- c(0.0000000, 0.1168146, 0.1863450, 0.2473826, 0.3037069, 0.3567971, rep(0, 93))

true.ci = 0
list_of_ci_vals <- list()

for (val in f){
  for (i in 1:26){
    if (val >=lowerpi[i] & val<=upperpi[i]){
      true.ci = true.ci + (1*dbinom(i-1,25,prob=val))
      list_of_ci_vals[i] <- true.ci
    }
  }
}

list_of_ci_vals
###############
label =  after_stat(scales::percent(cumsum((count / sum(count))), accuracy = 0.01))
library(scales)
count = seq(0, 10, 1)
label = scales::percent(cumsum((count / sum(count))), accuracy = 0.01)
label
################
library(tidyverse)
dataframe<- structure(list(Location = c("D", "B", "D", "C", "E", "E", "A", 
                                        "E", "A", "E", "E", "E", "A", "A", "B", "C", "D", "A", "C", "E"
), Status = c("with rain", "with rain", "without rain", "without rain", 
              "without rain", "without rain", "with rain", "with rain", "without rain", 
              "with rain", "with rain", "with rain", "without rain", "with rain", 
              "with rain", "without rain", "with rain", "without rain", "with rain", 
              "with rain"), error = c("R-squared", "RMSE", "MAPE", "MAPE", 
                                      "MAPE", "R-squared", "MAPE", "RMSE", "R-squared", "R-squared", 
                                      "RMSE", "AAE", "R-squared", "R-squared", "R-squared", "RMSE", 
                                      "RMSE", "RMSE", "R-squared", "AAE"), cluster = c("zero", "hundred", 
                                                                                       "twohudred", "twohudred", "zero", "fifty", "hundred", "twohudred", 
                                                                                       "hundred", "hundred", "moretwohudred", "moretwohudred", "moretwohudred", 
                                                                                       "fifty", "zero", "zero", "twohudred", "twohudred", "moretwohudred", 
                                                                                       "fifty"), value = c(0.383082674189092, NA, NA, NA, 2.03705186848321, 
                                                                                                           0.748813283077537, 22.6165326921659, 11.4742198771168, NA, NA, 
                                                                                                           NA, NA, NA, 0.220767743134449, 0.943069692331292, 6.44758774662155, 
                                                                                                           NA, NA, NA, 4.40798820584615)), row.names = c(NA, -20L), class = "data.frame")
ggplot(dataframe, aes(x = Location, y = value)) +
  geom_col() +
  facet_wrap( ~ Status + error + factor(cluster), nrow = 2, ncol = 10, switch = "x")
##############
par(mar=c(6,6,6,6)+0.1)
library(raster)
data(volcano)
volcanoR <- raster(volcano)
plot(volcanoR, axis.args = list(cex.axis = 2, lwd = 2))
################
for (i in 1:12){
  print(toupper(month.abb[i + 1]))
}
################
  library(tidyverse)
  
  df1 <- data.frame(date=as.Date(seq(ISOdate(2019,1,1), by="1 day", length.out=365)),
                    value=runif(365))
  
  df2 <- data.frame(date=as.Date(seq(ISOdate(2019,1,1), by="1 day", length.out=365)),
                    value=runif(365)+3)
  
  df1$Lines <- factor("Line 1")
  df2$Lines <- factor("Line 2")
  df3 <- rbind(df1, df2)
  
  ggplot(df3) +
    geom_line(df3, mapping = aes(x = date, y = value, alpha = Lines)) +
    geom_vline(aes(xintercept = as.Date("2019-06-15"), colour = "Milestone 1"), linetype = "dashed") +
    geom_vline(aes(xintercept = as.Date("2019-07-20"), colour = "Milestone 2"), linetype = "dashed") +
    geom_vline(aes(xintercept = as.Date("2019-09-15"), colour = "Milestone 3"), linetype = "dashed") +
    scale_color_manual(name="Milestones",
                       breaks=c("Milestone 1","Milestone 2","Milestone 3"),
                       values = c("Milestone 1" = "red",
                                  "Milestone 2" = "blue",
                                  "Milestone 3" = "green")) +
    scale_alpha_manual(name = "test",
                       breaks = c("Line 1", "Line 2"),
                       values = c(0.95, 0.5),
                       label = c("Line 22", "Line 33"))
#################
library(ggplot2)
library(cowplot)
  
#data
coef<-c(7.780000e-01, -2.350000e-01, -2.820000e-01, -3.090000e-01, 7.560000e-01, -0.0210000, -2.000000e-01, -0.1790000000, -2.690000e-01, -0.10300000)
LCI<-c(7.240000e-01, -3.670000e-01, -3.940000e-01, -4.310000e-01, 6.950000e-01, -0.1720000, -3.510000e-01, -0.3050000000, -4.290000e-01, -0.25600000)
UCI<-c( 8.280000e-01, -9.700000e-02, -1.450000e-01, -1.890000e-01, 8.020000e-01,  0.1240000, -6.480000e-02, -0.0369000000, -1.280000e-01,  0.05850000)
varno<-1:10
dat_1<-data.frame(cbind(coef,LCI,UCI,varno))
  
dat_2<- data.frame(cbind(sample(1:5, 10000, replace=T), rep(seq(1:10),1000)),c(rep(1,5000),rep(2,5000)))
colnames(dat_2)<-c("score","group","time")

#plots
  
a<-ggplot(data=dat_1, aes(x=factor(varno), y=-coef, ymin=-LCI, ymax=-UCI))+ 
    geom_errorbar(width=0,size = 2,color="steelblue")+
    geom_point(size=5, color="steelblue")+
    geom_hline(yintercept=0, color="black", linetype="dashed", alpha=.5)+  #add x=0 line
    scale_y_continuous(name = "r")+
    ggtitle("A)")+
    theme_minimal()+ 
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
b<-ggplot(dat_2, aes(y=score, x=factor(group), fill=factor(time)))+ 
    geom_violin(position=position_dodge(0.5), trim=T)+
    scale_x_discrete()+
    labs(fill = "time",y="ratings")+
    ggtitle("B)")+
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          legend.position = "bottom")
  
  
plot_grid(a, b, align = "v", ncol = 1 )
############
library(tidyverse)
library(scales)
library(ggbeeswarm)
library(cowplot)

us <- read_csv("us.csv")

#uncounting data so that each new death is one observation
us_covid_deaths <- us %>%
  mutate(new_deaths = c(0, diff(deaths))) %>%
  filter(new_deaths > 0) %>%
  mutate(type = "death") %>%
  select(-c("cases", "deaths")) %>%
  uncount(new_deaths)

beeswarm_plot <- ggplot(us_covid_deaths, aes(x = date, y = type, stroke = 0)) +
  geom_quasirandom(shape = ".", groupOnX= FALSE) +
  scale_x_date(limits = as.Date(c("2020-02-20", "2021-01-26"))) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = "none",
        panel.grid = element_blank())
beeswarm_plot

jitter_plot <- ggplot(us_covid_deaths,
                          aes(x = date, y = type, stroke = 0, group = 1)) +
  geom_jitter(shape = ".") + 
  scale_x_date(limits = as.Date(c("2020-02-20", "2021-01-26"))) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = "none",
        panel.grid = element_blank())
jitter_plot

freq_plot <- ggplot(us_covid_deaths,
                   aes(x = date)) +
  geom_freqpoly() + 
  scale_x_date(limits = as.Date(c("2020-02-20", "2021-01-26"))) +
  theme(axis.title = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), legend.position = "none",
        panel.grid = element_blank())
freq_plot
  
plot_grid(beeswarm_plot, jitter_plot, freq_plot, ncol = 1, labels = "AUTO")

jitter_plot2 <- ggplot(us_covid_deaths,
                       aes(x = date, y = type, group = 1)) +
  geom_jitter(shape = ".", size = 20) + 
  scale_x_date(limits = as.Date(c("2020-02-20", "2021-02-26"))) +
  theme(axis.title = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), legend.position = "none",
        panel.grid = element_blank())
jitter_plot2
###########
# Load library
library(tidyverse)

# Create some 'test' (fake) data
sample_df <- data_frame(id_number = (1:100),
                        political_ort = c(rep("liberal", 70),
                                          rep("conservative", 30)),
                        ratings = sample(1:7, size = 100, replace = TRUE))

# Take the fake data
undersampled_df2 <- sample_df %>% 
  # Group the two categories (liberal / conservative) to treat them separately
  group_by(political_ort) %>% 
  # And randomly sample 30 rows from each category (liberal / conservative)
  sample_n(size = 30, replace = FALSE) %>% 
  # (because there are only 30 conservatives in total they are all included)
  ungroup()
###################
install.packages("tableone")
library(tableone)
library(survival)
data(pbc)
CreateTableOne(data = pbc)
##################
library(tidyverse)
library(kableExtra)

sr <- c('Positive (+)','Positive (+)','Negative (-)','Positive (+)')
lr <- c('Positive (+)','Neutral','Negative (-)','Positive (+)')
d <- c('GDP Response to Positive','','CPI Response to Positive','')
s <- c('AS Shock','AD Shock','AS Shock','AD Shock')

data.frame(d,s,sr,lr) %>% 
  kable(col.names = c('','','Short Run Effect','Long Run Effect'),'html') %>% 
  kable_styling() %>% 
  row_spec(0, col = 'white', background = '#7c3042') %>% 
  footnote(general="<b>These signs represent prior expectations for the short and long run dynamics
           of variable responses to respective shocks, based on a conventional AS-AD framework<b>", escape = FALSE) %>% 
  column_spec(1:4,bold = T) %>% 
  save_kable(file = "table.png", density = 700)
sessionInfo()
#################
library(tidyverse)

list_of_files <- fs::dir_ls(glob = "*.txt")

df <- list_of_files %>%
  map_df(read_csv, .id = "ID")
  
df %>%
  separate(`some text`, c("name", "value"), "carrier ") %>% 
  filter(grepl("\\b\\.*[0-9]+\\.*[0-9]*", value)) %>% 
  mutate(value = str_extract(value, pattern = "\\b\\.*[0-9]+\\.*[0-9]*")) %>% 
  mutate(value = parse_number(value)) %>% 
  mutate(name = str_extract(name, pattern = "probability|BRCA1|BRCA2")) %>% 
  mutate(name = factor(name), ID = factor(ID))
  
cleaned_dataframe <- df %>%
  separate(`some text`, c("name", "value"), "carrier ") %>% 
  filter(grepl("\\b[0-9]+\\.?[0-9]", value)) %>% 
  mutate(value = str_replace_all(value, pattern = "is ", replacement = "")) %>% 
  mutate(value = parse_number(value)) %>% 
  mutate(name = str_extract(name, pattern = "probability|BRCA1|BRCA2"))
###########
remotes::install_github("rstudio-education/gradethis", force = TRUE)
library(gradethis)
library(learnr)

sessionInfo()
############
library(tidyverse)
library(zoo)
test <- c("Jan2000", "Feb2020")
as.yearmon(test)
#>[1] "Jan 2000" "Feb 2020"

dates <- seq(as.yearmon(test)[1],
             as.yearmon(test)[2],
             length.out = 100)
values <- rbinom(n = 100, size = 10, prob = 0.5)
data <- tibble(dates, numbers)

ggplot(data, aes(x = dates, y = values)) +
  geom_line()
###############
BiocManager::install("EGSEA")
BiocManager::install("KEGGdzPathwaysGEO")
library(EGSEA)
###############
library(tidyverse)
#install.packages("lme4")
library(lme4)
set.seed(123)
data <- data.frame(id = rep(1:3, each = 30), 
                   intervention = rep(c("a","b"), each= 2, times=45),
                   area = rep(1:3, times = 30), 
                   "dv1" = rep(c(rnorm(15, mean = 20, sd = 7),
                                 rnorm(15, mean = 40, sd = 7)), times = 3),
                   "dv2" = rep(c(rnorm(15, mean = 20, sd = 7),
                                 rnorm(15, mean = 40, sd = 7)), times = 3),
                   outcome = rep(c(rbinom(15, 0, prob = .95),
                                   rbinom(15, 1, prob = .95)), times = 3))

data$id <- as.factor(data$id)
data$intervention <- as.factor(data$intervention)
data$area <- as.factor(data$area)
data$outcome <- as.factor(data$outcome)

model_1 <- glmer(
  outcome ~ dv1 + (1 | id/area), 
  data = data, 
  family = binomial(link = "logit")
)

library(ggplot2)
ggplot(data, aes(x = dv1, y = as.numeric(outcome) - 1, color = factor(area))) +
  stat_smooth(method="glm", color="black", se=FALSE,
              method.args = list(family=binomial)) + 
  geom_point() +
  facet_wrap(~id)
#############
library(tidyverse)
full %>%
  ggplot(aes(x = order, y = mean, fill = type, width = 0.5)) + 
  scale_fill_manual(values = c("003900", "003901")) + 
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .2, position = position_dodge(.9)) +
  geom_text(aes(label = round(mean, digits =1)),
            position = position_dodge(width=1.0),
            vjust = -2.0, size = 3.5) +
  theme(legend.position = "right") + 
  labs(title = "Behavioral intentions and expected convincingness for single-reason messages") +
  coord_cartesian(ylim = c(1, 6.5), expand = FALSE) +
  scale_y_continuous(breaks = seq(1,6,1)) + 
  theme(axis.text = element_text(size = 7)) + 
  theme(legend.title = element_blank()) +
  xlab("Single-Reason Message") + 
  ylab("Rating of intentions or expected convincngess") + 
  scale_x_continuous(breaks = c(1.5, 3.5, 5.5, 7.5),
                     labels = c("Animals", "Environment",
                                "Health", "Money"))

full %>%
  ggplot(aes(x = order, y = mean -1, fill = type, width = 0.5)) + 
  scale_fill_manual(values = c("003900", "003901")) + 
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - se - 1, ymax = mean + se - 1), width = .2, position = position_dodge(.9)) +
  geom_text(aes(label = round(mean, digits =1)), position = position_dodge(width=1.0), vjust = -2.0, size = 3.5) +
  theme(legend.position = "right") + 
  labs(title = "Behavioral intentions and expected convincingness for single-reason messages") +
  theme(axis.text = element_text(size = 7)) + 
  theme(legend.title = element_blank()) +
  xlab("Single-Reason Message") + 
  ylab("Rating of intentions or expected convincngess") + 
  scale_x_continuous(breaks = c(1.5, 3.5, 5.5, 7.5),
                     labels = c("Animals", "Environment", "Health", "Money")) +
  coord_cartesian(ylim=c(0, 6)) +
  scale_y_continuous(breaks = seq(0,6,1),
                     labels = seq(1,7,1))
###############
library(reprex)
reprex::reprex({
  library(tidyverse)
  lm_fit <- lm(mpg ~ cyl+disp, data = mtcars)
  summarise_lm_like_object <- function(lm_fit){
    out_summ <- broom::tidy(lm_fit) %>%
      mutate(sig = ifelse(p.value <= 0.001, "***",
                          ifelse(p.value <= 0.01, "**",
                                 ifelse(p.value <= 0.05, "*", ".")))) %>% 
      rename("Estimate" = estimate,
             "Std. Error" = std.error,
             "t value" = statistic,
             "Pr(>|t|)" = p.value,
             "Significance" = sig)
    
    print.data.frame(out_summ, row.names = FALSE)
    cat("---\n")
    cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  }
  
  summarise_lm_like_object(lm_fit)
})
#############
library(caret)
vector <- runif(10, min=0, max=101)
vector
sampling_index <- createDataPartition(y = vector, times = 1,
                                      p = 0.2, list = FALSE)
vector[sampling_index]
############
library(tidyverse)
palette_light_custom <- c("#FF61c3", "#FF6D5C", "#FFA200", "#FFFA5C", "#82FF73", "#53c1e8", "#C478FF", "#C478FF", "#C478FF")
palette_midtone_custom <- c("#f84eb7", "#f3413a", "#f48c00", "#fffa5c", "#56dc5b", "#219ed4", "#b86ef4", "#b86ef4", "#b86ef4")
palette_dark_custom <- c("#e0008a", "#eb0008", "#d97d00", "#fffa5c", "#15a736", "#0085cf", "#8745c6", "#8745c6", "#8745c6")
palette_outline_custom <- c("#ad006b", "#b50006", "#c26f00", "#f7f000", "#006e19", "#005ba1", "#6729a7", "#6729a7", "#6729a7")

scales::show_col(palette_light_custom)
scales::show_col(palette_midtone_custom)
scales::show_col(palette_dark_custom)
scales::show_col(palette_outline_custom)

pal_light <- function() { scales::manual_pal(palette_light_custom) }
pal_midtone <- function() { scales::manual_pal(palette_midtone_custom) }
pal_dark <- function() { scales::manual_pal(palette_dark_custom) }
pal_outline <- function() { scales::manual_pal(palette_outline_custom) }

scale_colour_custom <- function(shade, ...) { 
  if (shade == "light") {
    discrete_scale("colour", "custom", pal_light(), ...) 
  } else if (shade == "midtone") {
    discrete_scale("colour", "custom", pal_midtone(), ...) 
  } else if (shade == "dark") {
    discrete_scale("colour", "custom", pal_dark(), ...) 
  } else if (shade == "outline") {
    discrete_scale("colour", "custom", pal_outline(), ...) 
  } else {
    stop("Incorrect or missing shade parameter in scale_color_custom()")
  }
}

scale_fill_custom <- function(shade, ...) { 
  if (shade == "light") {
    discrete_scale("fill", "custom", pal_light(), ...) 
  } else if (shade == "midtone") {
    discrete_scale("fill", "custom", pal_midtone(), ...) 
  } else if (shade == "dark") {
    discrete_scale("fill", "custom", pal_dark(), ...) 
  } else if (shade == "outline") {
    discrete_scale("fill", "custom", pal_outline(), ...) 
  } else {
    stop("Incorrect or missing shade parameter in scale_color_custom()")
  }
}

ggplot(data=iris,
       aes(x = Sepal.Length,
           color = Species, 
           fill = Species)) + 
  geom_density() + 
  scale_colour_custom("outline") +
  scale_fill_custom("dark") +
  theme_minimal()
##################
library(tidyverse)
ID <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Happiness <- c(2, 3, 10, 7, 6, 8, 3, 9, 5, 1)
Smoke <- c("yes", "yes", "no", "yes", "no", "no", "no", "no", "yes", "no")
Exercise <- c("no", "yes", "no", "yes", "yes", "yes", "yes", "no", "no", "yes")
df <- tibble("ID" = ID, "Happiness" = Happiness,
                 "Smoke" = Smoke, "Exercise" = Exercise)
df %>% 
  mutate(Smoke = ifelse(Smoke == "yes",
                        "Smoker",
                        "Non-Smoker"),
         Exercise = ifelse(Exercise == "yes",
                           "Exercises",
                           "Doesn't Exercise"),
         Interaction = factor(str_replace(interaction(Smoke, Exercise),
                                          '\\.', '\n'),
                              ordered=TRUE)) %>% 
  ggplot(aes(x= Interaction, y = Happiness)) + 
  geom_boxplot(aes(fill = Smoke)) +
  geom_point(aes(shape = Exercise), size = 4) +
  labs(title = "Happiness by Smoking/Exercise",
       y = "Happiness") +
  theme_classic(base_size = 16) +
  theme(axis.title.x = element_blank())
#############
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("airway")
#BiocManager::install("DESeq2")
library(tidyverse)
library(airway)
data("airway")
se <- airway
library(DESeq2)

dds <- DESeqDataSet(se, design = ~ cell + dex)
keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep,]
dds <- DESeq(dds)
res <- results(dds, name="dex_untrt_vs_trt")
summary(res)
res$lfcSE <- NULL
res$stat <- NULL
res$pvalue <- NULL
res2 <- as.data.frame(res) %>% 
  mutate(padj = ifelse(padj <= 0.05, TRUE, FALSE))
str(res2)
res

res3 <- tibble::rownames_to_column(res2, "ID")
rownames(res3) <- res3[,1]
res3[,1] <- NULL
res3

plotMA(res2, ylim=c(-2,2))

##############
#install.packages("pheatmap")
library(pheatmap)
# use the log transform on the data set
select <- order(rowMeans(counts(dds,normalized=TRUE)),
                decreasing=TRUE)[1:20]
df <- as.data.frame(colData(dds)[,"dex"])
rownames(df) <- colnames(assay(dds)[select,])
colnames(df) <- "Dex" 
df$Dex <- sort(df$Dex)
pheatmap(assay(dds)[select,], cluster_rows=FALSE, show_rownames=FALSE,
         cluster_cols=FALSE, annotation_col=df)
#############
#install.packages("rstan", dependencies = TRUE)
#library(rstan)
#example(stan_model, package = "rstan", run.dontrun = TRUE)
#############
library(tidyverse)
library(lubridate)
df <- tribble(~"Date", ~"App", ~"Customers",
              "Jan-01", "A", "Cust1",
              "Feb-01", "B", "Cust2",
              "Mar-01", "A", "Cust1",
              "Apr-01", "B", "Cust2",
              "May-01", "C", "Cust1")
df %>% 
  mutate(Date = dmy(paste("01-", df$Date , sep ="")),
         interaction = interaction(factor(App), Customers)) %>%
  ggplot(aes(x = Date, y = Customers, color = interaction)) +
  geom_count() +
  geom_line()
###############
library(rvest)
library(tidyverse)
library(zoo)

url <- read_html("https://academic.oup.com/neuro-oncology/article/20/7/873/4843984")
scraped <- html_nodes(url,"table") %>%
  html_table(fill=TRUE) %>%
  data.frame()
colnames(scraped) <- c("ID", "Author", "Journal", "PMID", "Year", "Data", "Type", "No. Patients")
Glioma_table <- scraped[2:8] %>%
  mutate(Author = str_remove(Author, "[[:digit:]]+"))
write_tsv(x = Glioma_table, file = "Glioma_table.txt")
##############
install.packages("aplpack")
library(aplpack)
x11()
##############
n <- 3
mat <- t(replicate(n = n, expr = paste(letters[1:n], 1:n, sep = "")))
mat[upper.tri(mat, diag = TRUE)]

paste0(rep(c('a', 'b', 'c'), 1:3), 1:6)
##############
library(tidyverse)
library(palmerpenguins)

penguins %>% 
  na.omit() %>%
  mutate(species = factor(ifelse(species == "Adelie", 1, 2))) %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(colour = species), notch = TRUE, varwidth = TRUE) +
  geom_jitter(width = 0.2, aes(colour = species)) + 
  theme(legend.position = "none") + 
  labs(title="User Engagement Score", x="Condition", y="Score (max 140)") +
  scale_x_discrete(label = c("Stealth", "Non-stealth"))
#############
# Load libraries
library(tidyverse)

# Create colnames
column_names <- c("Deli", "Bakery", "Deli.1", "Bakery.1", "Deli.2", "Produce", 
                  "Bakery.2", "Deli.3", "Bakery.3", "Deli.4", "Bakery.4", "Deli.5", "Bakery.5")
# Make some fake data
df <- as.data.frame(replicate(n = 13, expr = sample(c(1:8, NA), 13, replace = TRUE))) %>% 
  rename_with(~ column_names)
#############
library(ggplot2)

df <- data.frame(year = c(2011,2012,2013,2014,2015,2016,2017,2018), 
                 value = c(337,423,551,661,846,1387,2222,3580))

ggplot(df, aes(year, value)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = value, y = (value - 50)*0.9), color = "red") +
  theme(axis.text.y = element_text(colour = "red"))
###############
library(tidyverse)
library(palmerpenguins)
library(ggbeeswarm)

penguins %>% 
  na.omit() %>%
  select(species, island, bill_length_mm) %>%
  rename(Species = species, Island = island) %>% 
  ggplot(aes(x = Island, y = bill_length_mm, fill = Species)) +
  geom_boxplot(width=.4, outlier.shape = NA,
               position = position_dodge2(preserve = "single")) +
  geom_quasirandom(aes(colour = Species), groupOnX = TRUE,
                   width=.2, alpha = 0.5, dodge.width = 0.4) +
  theme_bw(base_size = 16) +
  ylab("Bill Length (mm)")
#################
library(tidyverse)
library(lubridate)
tibble::tribble(
  ~Date,  ~ndvi,
  "2020-05-18", 0.7655,
  "2020-06-14",  0.723,
  "2020-07-12", 0.6178,
  "2020-08-21",  0.437,
  "2020-09-07", 0.4763,
  "2020-09-10", 0.4928,
  "2020-09-12", 0.4831,
  "2020-09-22", 0.4774,
  "2020-10-02", 0.5794,
  "2020-10-07",  0.606
) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  ggplot(aes(Date, ndvi)) +
  geom_line() +
  scale_x_datetime(name = "Month of the Year",
                   sec.axis = dup_axis(name = "Week of the Year",
                                       labels = scales::time_format("%W")))
###############
paste(quote(c(B,b,B,b,b,b,B,b)),'uffalo',sep='')[-1]
##############
squareMat<-matrix(c(0,10,20,30),2,2)

Reduce_method <- function(exponent) {
  Reduce('%*%', init = diag(nrow = nrow(squareMat)), replicate(exponent, squareMat, simplify=F))
}
Reduce_2 <- function(exponent) {
  Reduce(function(x,notused) {squareMat %*% x}, init = diag(nrow = nrow(squareMat)), 1:exponent)
}

loop_method <- function(exponent){
  out<-diag(nrow = nrow(squareMat))
  for(i in seq_len(exponent)){out<-squareMat%*%out}
  out
}

library(expm)
expm_method <- function(exponent){
  squareMat %^% exponent
}

microbenchmark::microbenchmark(Reduce_method(5), Reduce_2(5), loop_method(5), expm_method(5))
#################
library(ggpubr)
data <- read.table("ratio_m2.m1.txt",header = TRUE)

ggboxplot(data = data, x = "Group", y = "Ratio",
          width = 0.5, size = 0.8, 
          xlab = "", ylab = "Ratio") +
  stat_compare_means(comparisons = list(c("High", "Low"),
                                        c("High", "No"),
                                        c("Low", "No")))
#################
df <- as.data.frame(replicate(n = 13, expr = sample(c(1:8, NA), 13, replace = TRUE)))
df$names <- LETTERS[1:13]
df
colnames(df) <- df$names
df[14] <- NULL
df
#################
library(tidyverse)
#install.packages("gapminder")
library(gapminder)
gapminder %>%
  mutate(is_long = ifelse(lifeExp > 80, 1, 0)) %>%
  mutate(fit = (0.00003 * gdpPercap)^2 - .253) %>% 
  ggplot(aes(x = gdpPercap)) +
  geom_jitter(aes(y = is_long), width = 0,
              height = 0.01, alpha = 0.5) +
  geom_line(aes(y = fit), colour = "blue", lty = 2) +
  annotate("text", x = 50000, y = 0.5,
           label = paste("y = -0.253 + .00003*x", "\U00B2", sep = "")) +
  coord_cartesian(ylim = c(0,1))
#################
library(tidyverse)
set.seed(123)
df <- data.frame(group = as.factor(rep(1:3, each = 50)),
                 week = rep(1:50, 3),
                 rate = c(round(700 - rnorm(50, 100, 10) - 1:50 * 2, 0),
                          round(1000 - rnorm(50, 200, 10) - 1:50 * 2, 0),
                          round(1000 - rnorm(50, 200, 10) - 1:50 * 2, 0)))
df %>%
  ggplot(aes(x = week,
             y = rate,
             group = group,
             lty = group)) + 
  geom_line() +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess")
################
library(tidyverse)
x <- c("_1_1", "_1_2", "_1_3", "_2_1", "_2_2",
       "_2_3", "_3_1", "_3_2", "_3_3", "_4_3")
paste0("asf",x)
test_1 <- t(as_tibble(rnorm(10, 5.5, .35)))
colnames(test_1) <- paste0("asf",x)

paste0("hjk",x)
test_2 <- t(as_tibble(rnorm(10, 5.5, .35)))
colnames(test_2) <- paste0("asf",x)

test_renamed <- as.data.frame(test) %>% 
  rename_with(~ str_replace(., "asf", "Var1 ")) %>% 
  rename_with(~ sub(" _", " Type", .x)) %>% 
  rename_with(~ sub("_", " Time", .x))
####################
library(tidyverse)
ggplot(test_1) +
  geom_col() +
  ggtitle(subtitle = "a subtitle")

plot(mtcars[3:4])
title("My Plot")
mysubtitle = "Subtitle here"
mtext(side=3, line=0.25, at=0, adj=-0.5, mysubtitle)

plot(mtcars[3:4])
title(main = "My Plot")
mysubtitle = "Subtitle here"
anothersubtitle = "Another subtitle here"
mtext(side = 3, line = 0.25, at = 1, adj = -2, mysubtitle)
mtext(side = 2, line = 2, anothersubtitle)
##################
#install.packages("rstatix")
library(rstatix)
library(tidyverse)

ID = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3) 
Form = c("A", "A", "A", "B", "B", "B", "A", "A", "A", "B", "B", "B", "A", "A", "A", "B", "B", "B")
Pen = c("Red", "Blue", "Green", "Red", "Blue", "Green", "Red", "Blue", "Green","Red", "Blue", "Green","Red", "Blue", "Green","Red", "Blue", "Green")
Time = c(20, 4, 6, 2, 76, 3, 86, 35, 74, 94, 14, 35, 63, 12, 15, 73, 87, 33)
original.df <- data.frame(ID, Form, Pen, Time)

df <- original.df %>%
  dplyr::select(ID, Form, Pen, Time)
df <- df %>%
  dplyr::group_by(ID, Form, Pen) %>%
  dplyr::summarise(Time = mean(Time))
df <- df %>%
  convert_as_factor(ID, Form, Pen)
df$Time <- as.numeric(df$Time)
str(df)
df <- ungroup(df)
str(df)

aov <- rstatix::anova_test(data = df, dv = Time, wid = ID, within = c(Form, Pen))
#############
# Load libraries
library(tidyverse)

# Get data
ID <- seq(1:50)
Happiness <- sample(1:100, 50, replace = TRUE)
Smoke <- sample(c("yes", "no"), 50, replace = TRUE)
Exercise <- sample(c("yes", "no"), 50, replace = TRUE)
df <- tibble("ID" = ID, "Happiness" = Happiness,
             "Smoke" = Smoke, "Exercise" = Exercise)

# Source Ben Marwick's code for Violin Plots
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Make Raincloud plot
# Raincloud theme
raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_blank(),
  axis.text = element_text(size = 14),
  axis.text.y = element_text(vjust = 0.3),
  legend.title=element_text(size=14),
  legend.text=element_text(size=14),
  legend.position = "right",
  plot.title = element_text(lineheight=.8,
                            face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black',
                             size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black',
                             size=0.5, linetype='solid'))

# Plot the thing
df %>% 
  mutate(Smoke = ifelse(Smoke == "yes",
                        "Smoker",
                        "Non-Smoker"),
         Exercise = ifelse(Exercise == "yes",
                           "Exercises",
                           "Doesn't Exercise"),
         Interaction = factor(str_replace(interaction(Smoke, Exercise),
                                          '\\.', '\n'),
                              ordered=TRUE)) %>% 
  ggplot(aes(x = Interaction, y = Happiness, fill = Smoke)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),
                   alpha = .8) +
  geom_point(aes(shape = Exercise),
             position = position_jitter(width = .05),
             size = 2, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip(xlim=c(1.25,4.25)) +
  labs(title = "Happiness by Smoking/Exercise",
       y = "Happiness") +
  scale_fill_discrete(guide = guide_legend(override.aes = list(shape = c(".", ".")))) +
  scale_shape_discrete(guide = guide_legend(override.aes = list(size = 3))) +
  theme_classic(base_size = 16) +
  theme(axis.title.x = element_blank()) +
  raincloud_theme
################
library(tidyverse)

yieldcurve_sa <- structure(list(Date = structure(c(18629, 18656, 18629, 18656, 
                                                   18629, 18656, 18629, 18656, 18629, 18656, 18629, 18656, 18629, 
                                                   18656), class = "Date"), Key = structure(c(1L, 1L, 2L, 2L, 3L, 
                                                                                              3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L), .Label = c("3M", "2Y", "5Y", 
                                                                                                                                              "10Y", "20Y", "25Y", "30Y"), class = "factor"), Value = c(2.89, 
                                                                                                                                                                                                        4.92, 4.53, 4.655, 6.665, 6.685, 8.735, 8.735, 10.845, 10.825, 
                                                                                                                                                                                                        10.905, 10.885, 10.81, 10.8)), row.names = c(NA, -14L), class = "data.frame")

ggplot(yieldcurve_sa, aes(x = Key, y = Value, col = factor(Date), group=factor(Date))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c('#7c3042','#c7af76')) +
  labs(y="", x = 'Time to Maturity', caption = '*Source - SARB') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'left',
        legend.margin=margin(0,0,0,0),
        legend.key.height = unit(2.6, "cm")) +
  guides(color = guide_legend(label.theme = element_text(angle = -90),
                              override.aes = list(size = 3))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))
############
MyVar<-0.23
Fbelow <- expression('F'[below]*'=')
plot(mtcars[2:3], main = Fbelow)
mtext(text = MyVar, side = 3, adj = 0.625, padj = -1.75, cex = 1.5)
############
library(tidyverse)
library(zoo)

SKILL_1_OCC_FINAL <- tibble::tribble(
   ~date, ~ratio,
   "1990-1",    "0.5",
   "1990-2",    "0.6",
   "1990-3",    "0.5",
   "1991-1",    "0.7",
  "1991-2",    "0.65",
  "1991-3",    "0.65"
  )
SKILL_1_OCC_FINAL$date <- as.Date(zoo::as.yearmon(SKILL_1_OCC_FINAL$date))


occratiocomb <- ggplot(SKILL_1_OCC_FINAL, aes(x = date, y=ratio, group = 1)) +
  geom_line() + 
  geom_smooth(method = "loess", fill = "red", colour = "red") +
  theme(axis.text.x=element_text(angle = 60, hjust = 1)) +
  scale_x_date(date_minor_breaks = "1 month",
               date_breaks = "1 year",
               date_labels = "%Y-%m")
print(occratiocomb)
sessionInfo()
##############
library(ggpubr)
library(tidyverse)
library(ggsignif)

anno_df <- compare_means(mpg ~ am, group.by = "vs",
                         data = mtcars, method = "t.test") %>% 
  mutate(max_mpg = mtcars %>%
           group_by(am, vs) %>%
           summarize(max_mpg = max(mpg)*1.2) %>%
           filter(vs == 1) %>% 
           ungroup()) %>% 
  mutate(p.adj.sig = ifelse(p.adj < 0.001, "***",
                            ifelse(p.adj < 0.01, "**",
                                   ifelse(p.adj < 0.05,
                                          "*", "ns"))))

ggboxplot(mtcars, x = "am", y = "mpg", facet.by = "vs") +
  ggsignif::geom_signif(data=anno_df,
                        aes(xmin=group1, xmax=group2,
                            annotations=p.adj.sig,
                            y_position=max_mpg[["max_mpg"]]),
                        manual=TRUE)
############
library(tidyverse)
test_dataframe <- tibble::tribble(
    ~unique.Id, ~Zone, ~Part,
  "P-1",            "KRT",       "AS,RT",
  "P-1",            "AFR",       "AS,RT",
  "P-2",            "KRT",       "AS,RT",
  "P-2",            "KRT",       "AS,RT",
  "P-2",            "KRT",       "AS,RT",
  "P-3",            "RGT",        NA,
  "P-4",             NA,         "RGT",
  "P-4",             NA,         "RGT",
  "P-5",             NA,          NA,
  "P-6",             NA,          NA
  )
test_dataframe %>%
  filter(Zone == "RGT" | is.na(Zone) & is.na(Part)) 
###################
library(tidyverse)
library(ggpmisc)
#install.packages("cowplot")
library(cowplot)
df2<-structure(list(days = c(0L, 0L, 4L, 4L, 10L, 10L, 17L, 17L, 24L, 
                             24L, 66L, 66L, 81L, 81L, 94L, 94L, 116L, 116L), soil_type = c("mineral", 
                                                                                           "organic", "mineral", "organic", "mineral", "organic", "mineral", 
                                                                                           "organic", "mineral", "organic", "mineral", "organic", "mineral", 
                                                                                           "organic", "mineral", "organic", "mineral", "organic"), slope = c(-0.8, 
                                                                                                                                                             -0.18, -1.48, -0.29, -1.6, -0.19, -0.89, 0.01, -0.68, 0, -0.01, 
                                                                                                                                                             0, 0, 0, -0.06, 0, -0.06, 0)), row.names = c(NA, -18L), class = c("tbl_df", 
                                                                                                                                                                                                                               "tbl", "data.frame"))


df1<-structure(list(carbon = c(1.4, 0.8, 1.6, 0.1, 0.4, 0.4, 0.4, 
                               1.3, 0.4, 1.1, 0.2, 1, 0.4, 0.4, 0.5, 0.8, 0.1, 0.5, 0.4, 0.6, 
                               1.1, 0.6, 0.2, 0.2, 0.4, 0.1, 0.3, 0.5, 1.4, 0.3, 0.3, 1.1, 0.3, 
                               0.7, 0.4, 0.4, 1.1, 0.1, 0.6, 1.3, 0.1, 1.6, 0.4, 0.5, 0.5, 1.2, 
                               0.5, 0.5, 1.4, 0.8, 1.6, 0.1, 0.4, 0.4, 0.4, 1.1, 0.2, 1, 0.4, 
                               0.4, 0.5, 0.8, 0.1, 0.5, 0.4, 0.6, 1.1, 0.6, 0.2, 0.2, 0.4, 0.1, 
                               0.3, 0.5, 1.4, 0.3, 0.3, 1.1, 0.3, 0.7, 0.4, 0.4, 1.1, 0.1, 0.6, 
                               1.3, 0.1, 1.6, 0.4, 1.2, 0.5, 0.5), days = c(0L, 0L, 0L, 0L, 
                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 94L, 94L, 94L, 
                                                                            94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 
                                                                            94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 
                                                                            94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 94L, 
                                                                            94L, 94L), water = c(2, 1.6, 1.9, 1.2, 1.3, 1.5, 1.7, 1.9, 1.3, 
                                                                                                 1.8, 1.3, 1.8, 1.8, 1.9, 1.7, 1.4, 1, 1.3, 1, 1.9, 1.7, 1.2, 
                                                                                                 1, 1, 1.7, 0.9, 1, 1.7, 1.9, 1.4, 1.3, 1.5, 1.6, 1.3, 1.5, 1.6, 
                                                                                                 1.9, 1.4, 1.7, 2, 1.2, 2, 1.8, 2, 1.8, 1.7, 1.5, 2, 2, 1.6, 1.9, 
                                                                                                 1.2, 1.3, 1.5, 1.3, 1.8, 1.3, 1.8, 1.8, 1.9, 1.7, 1.4, 1, 1.3, 
                                                                                                 1, 1.9, 1.7, 1.2, 1, 1, 1.7, 0.9, 1, 1.7, 1.9, 1.4, 1.3, 1.5, 
                                                                                                 1.6, 1.3, 1.5, 1.6, 1.9, 1.4, 1.7, 2, 1.2, 2, 1.8, 1.7, 1.5, 
                                                                                                 2), soil_type = c("organic", "mineral", "organic", "mineral", 
                                                                                                                   "mineral", "mineral", "mineral", "organic", "mineral", "organic", 
                                                                                                                   "mineral", "mineral", "mineral", "mineral", "mineral", "mineral", 
                                                                                                                   "mineral", "mineral", "mineral", "mineral", "organic", "mineral", 
                                                                                                                   "mineral", "mineral", "mineral", "mineral", "mineral", "mineral", 
                                                                                                                   "organic", "mineral", "mineral", "organic", "mineral", "mineral", 
                                                                                                                   "mineral", "mineral", "organic", "mineral", "mineral", "organic", 
                                                                                                                   "mineral", "organic", "mineral", "mineral", "mineral", "organic", 
                                                                                                                   "mineral", "mineral", "organic", "mineral", "organic", "mineral", 
                                                                                                                   "mineral", "mineral", "mineral", "organic", "mineral", "mineral", 
                                                                                                                   "mineral", "mineral", "mineral", "mineral", "mineral", "mineral", 
                                                                                                                   "mineral", "mineral", "organic", "mineral", "mineral", "mineral", 
                                                                                                                   "mineral", "mineral", "mineral", "mineral", "organic", "mineral", 
                                                                                                                   "mineral", "organic", "mineral", "mineral", "mineral", "mineral", 
                                                                                                                   "organic", "mineral", "mineral", "organic", "mineral", "organic", 
                                                                                                                   "mineral", "organic", "mineral", "mineral")), row.names = c(NA, 
                                                                                                                                                                               -92L), class = "data.frame")

##
p1 <- ggplot(df1, aes(carbon, 
                      water, 
                      fill=soil_type)) +
  geom_smooth(method = "lm",
              formula = formula, 
              color="black") +
  geom_point(aes(shape=soil_type, 
                 color=soil_type,  
                 size=soil_type)) +
  scale_fill_manual(values=c("green3", "brown")) + 
  scale_color_manual(values=c("black", "black")) + 
  scale_shape_manual(values=c(21, 24))+
  scale_size_manual(values=c(2.7, 2.0))+
  labs(shape="soil_type", 
       color="soil_type") +
  theme_bw() +
  facet_wrap(~days, 
             ncol = 2)+
  stat_poly_eq(
    aes(label = paste(stat(adj.rr.label),
                      stat(p.value.label), 
                      sep = "*\", \"*")),
    formula = formula, 
    rr.digits = 2, 
    p.digits = 1, 
    parse = TRUE,size=3.5)


p1_no_legend <- ggplot(df1, aes(carbon, 
                      water, 
                      fill=soil_type)) +
  geom_smooth(method = "lm",
              formula = formula, 
              color="black") +
  geom_point(aes(shape=soil_type, 
                 color=soil_type,  
                 size=soil_type)) +
  scale_fill_manual(values=c("green3", "brown")) + 
  scale_color_manual(values=c("black", "black")) + 
  scale_shape_manual(values=c(21, 24))+
  scale_size_manual(values=c(2.7, 2.0))+
  labs(shape="soil_type", 
       color="soil_type") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~days, 
             ncol = 2)+
  stat_poly_eq(
    aes(label = paste(stat(adj.rr.label),
                      stat(p.value.label), 
                      sep = "*\", \"*")),
    formula = formula, 
    rr.digits = 2, 
    p.digits = 1, 
    parse = TRUE,size=3.5)


p2 <- ggplot(df2, aes(x=days, 
                      y=slope, 
                      fill=soil_type))+
  geom_point(aes(shape=soil_type,  
                 color=soil_type,  
                 size=soil_type)) +
  scale_fill_manual(values=c("green3", "brown")) + 
  scale_shape_manual(values=c(21, 24))+
  scale_color_manual(values=c("black", "black")) + 
  scale_size_manual(values=c(2.7, 2.0))+
  labs(shape="soil_type", 
       color="soil_type") +
  theme_bw() +
  theme(legend.position = "none")

legend <- cowplot::get_legend(p1)

cowplot::plot_grid(p1_no_legend, p2, legend, nrow = 1)

cowplot::plot_grid(p1_no_legend, p2, legend, nrow = 1,
                   rel_widths = c(1, 0.5, 0.3))
###############
library(tidyverse)
library(palmerpenguins)
library(ggpubr)

p1 <- penguins %>% 
  na.omit() %>%
  ggplot(aes(x = flipper_length_mm,
             y = bill_length_mm / max(bill_length_mm),
             colour = island)) +
  geom_line() +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

p2 <- penguins %>% 
  na.omit() %>%
  ggplot(aes(x = bill_depth_mm,
             y = bill_length_mm / max(bill_length_mm),
             colour = island)) +
  geom_line() +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggarrange(p1, p2)

ggarrange(p1, p2, common.legend = TRUE)
#############
library(tidyverse)
library(ggrepel)
library(palmerpenguins)

penguins %>% 
  na.omit() %>%
  # arrange the df by your y variable
  arrange(desc(bill_length_mm)) %>% 
  # create a new variable (called "label_one_point")
  # and give the single highest y variable point a label
  mutate(label_one_point = ifelse(bill_length_mm == first(bill_length_mm),
                                  "PENGUINS!! \n(or whatever label you want)", NA)) %>% 
  # plot the thing
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point() +
  geom_text_repel(aes(label = label_one_point),
                  # you can move the label closer or further away
                  point.padding = unit(20, "cm"),
                  # and you probably don't want a 'segment', so set this high
                  # a segment is a line attaching a geom_text to it's point
                  min.segment.length = unit(20, "cm"),
                  force_pull = 0.1, size = 5,
                  show.legend = FALSE) +
  theme_light()
#############
library(tidyverse)
df = data.frame(col1 = c(0,0,0,0,0,0),
                col2 = c(0,0,10,10,0,0),
                col3 = c(11,11,0,0,0,0),
                col4 = c(0,0,0,0,12,12),
                col5 = c(0,0,0,0,0,0))
df.new <- df %>%
  summarise(col.new = rowSums(across(everything())))
df.new
#################
library(ggplot2)
#install.packages("sysfonts")
library(sysfonts)
#install.packages("showtext")
library(showtext)
font_add("Work Sans", regular = "~/Downloads/WorkSans-Regular.ttf")
showtext_auto()
ggplot(mtcars, aes(wt, mpg)) +
  geom_point()+
  theme(text = element_text(family = "Work Sans", size = 100),
        axis.text = element_blank())
##################
library(ggpubr)
library(ggsci)

# Create data frame
GROUP <- c()
TEST <- c()
VALUE <- c()
for (i in 0:100) {
  gp <- c('Group1','Group2','Group1 and Group2')
  ts <- c('Test1','Test2') 
  GROUP <- append(GROUP, sample(gp, 1))
  TEST <- append(TEST, sample(ts, 1))
  VALUE <-  append(VALUE, sample(1:200, 1))
}
df <- data.frame(GROUP, TEST, VALUE)

# Seed
set.seed(123)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(df, varname = "VALUE", groupnames = c("TEST", "GROUP"))

# Plot
p1 <- ggplot(df, aes(x = factor(GROUP, levels = c('Group1','Group2','Group1 and Group2')),
               y = VALUE, color = TEST)) +
  geom_jitter(aes(fill = TEST), shape = 21, stroke = 0,
              position = position_jitterdodge(jitter.width = 0.2)) +
  geom_linerange(data = df2, aes(ymin=VALUE-sd, ymax=VALUE+sd),
                position=position_dodge(width = .75)) +
  geom_point(data = df2, aes(y = VALUE), size = 3,
             position = position_dodge(width = 0.75)) +
  scale_color_jco() +
  scale_fill_npg() +
  labs(x = '', y = 'Values\n') +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "top")

p2 <- ggerrorplot(df, x = "GROUP", y = "VALUE",
                  desc_stat = "mean_sd",
                  add = c("jitter"),
                  color = "TEST", 
                  palette = "jco",
                  add.params = list(size = 0.2),
                  order = c('Group1','Group2','Group1 and Group2')
) +
  labs(x = '', y = 'Values\n') +
  theme(legend.title = element_blank())

cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2, labels = "AUTO")
#############
sex <- c("125F", "X28345M", "2M3")
output <- ifelse(grepl(sex, pattern = "F", ignore.case = TRUE), "F", "M")
fsex <- grepl(sex, pattern = "F", ignore.case = TRUE)
#############
library(tidyverse)
so<-data.frame(expand.grid(x=c(1:5),sys=c("a","b","c","d")))
so$y<-c(1,2,1,3,2,2,1,3,2,3,4,3,2,3,4,5,4,3,4,5)
#make the second dataframe:
so2<-data.frame(sys=c("a","b","c","d"),yint=c(1.4,2.3,3.5,4.6),low=c(1.2,2.1,3.4,4.1),
                upp=c(1.6,2.7,3.6,4.7))  

ggplot(so,aes(x=x,y=y,colour=sys)) + 
  geom_point(position=position_jitter()) +
  geom_hline(data=so2,aes(yintercept=yint, colour=sys)) +
  geom_rect(data = so2, aes(ymin = low, ymax = upp,
                            xmin = 0.5, xmax = 5.5, fill=sys),
            alpha = 0.2, inherit.aes = FALSE)

ggplot(so,aes(x=x,y=y,colour=sys)) + 
  geom_point(position=position_jitter()) +
  geom_hline(data=so2,aes(yintercept=yint, colour=sys)) +
  geom_ribbon(data = so2, aes(ymin = low, ymax = upp, x = x, colour=sys), alpha = 0.1, inherit.aes = FALSE)
################
library(tidyverse)
df <- tibble(Names = sample(letters, 100, replace = TRUE),
                 Values = runif(100, 0, 1000))

central_values <- tibble(measurement = c("mean", "median"),
                         value = c(mean(df$Values), median(df$Values)))

ggplot(df, aes(x=Values)) + 
  geom_histogram(color="darkblue", fill="lightblue", binwidth=17)+
  xlab("Number of days recorded")+ # for the x axis label
  ylab("Number of individuals")+
  geom_vline(data = central_values,
             aes(xintercept = value,
                 color = measurement), size=1.2)+
  scale_color_manual(values = c("red", "darkblue"),
                     name = NULL) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "grey50"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position = "top") +
  guides(color = guide_legend(title=NULL, keywidth = 2, keyheight = 3,
                              label.theme = element_text(size = 16)))
#############
install.packages("~/Downloads/tibble_3.1.0.tgz", repos=NULL)
library(tibble)
#############
library(tidyverse)
# install DESeq if necessary
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("DESeq")
# load packages
library(DESeq)
#install.packages("pheatmap")
library(viridis)

# load dataset
example_file <- system.file ("extra/TagSeqExample.tab", package="DESeq")
data <- read.delim(example_file, header=T, row.names="gene")

# subset the dataset to get a reasonable number of rows to plot (top 50 rows)
data_subset <- data %>% top_n(50)

# add a column "group"
group <- tibble("colours" = sample(size = 50, c("#31688EFF", "#35B779FF"), replace = TRUE),
                "labels" = ifelse(colours == "#31688EFF", "group_1", "group_2"))

# create heatmap
heatmap(as.matrix(data_subset), RowSideColors = group$colours,
        labRow = group$labels)

data <- structure(list(Group = c("O Oral", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "E Oral", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "Combo Oral", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "Control Oral", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
), gene.1 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
              1L, 1L, 1L, 1L, 1L, 1L), trait.1 = c(0L, 0L, 0L, 1L, 0L, 0L, 
                                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                   0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 
                                                   0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 
                                                   0L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                   0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 
                                                   1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 
                                                   1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                   0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 
                                                   1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 
                                                   1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 0L), trait.2 = c(0L, 
                                                                                                                0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 
                                                                                                                0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 
                                                                                                                1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                1L), trait.3 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                 0L, 0L, 0L, 0L, 0L, 0L, 0L), trait.4 = c(0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 
                                                                                                                                                                          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                          0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                          1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L), trait.5 = c(0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                                                                                           1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 
                                                                                                                                                                                                                                           1L), trait.6 = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                                                                                                            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 
                                                                                                                                                                                                                                                            1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                                                                                                                                                                            0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 
                                                                                                                                                                                                                                                            0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                                                                                                            1L, 1L, 1L, 1L, 1L, 0L, 0L)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                              -162L))
# Fill in the blank cells in "Group"
while(length(ind <- which(data$Group == "")) > 0){
  data$Group[ind] <- data$Group[ind -1]
}

# Specify colours for each group
rc <- ifelse(data$Group == "O Oral", "#440154FF",
             ifelse(data$Group == "E Oral", "#31688EFF",
                    ifelse(data$Group == "Combo Oral",
                           "#35B779FF", "#FDE725FF")))
# Plot the heatmap
heatmap(as.matrix(data[-1]), labRow = data$Group,
        RowSideColors = rc, Rowv = NA, revC = TRUE,
        margins = c(6, 16), col = c("black", "green"))
# Add a legend for the colours
par(lend = 1)
legend("topright", lwd = 10,
       legend = c("O Oral", "E Oral", "Combo Oral", "Control Oral"),
       col = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"))

library(gplots)
heatmap.2(as.matrix(data[-1]), 
        trace = "none", density.info = "none",
        dendrogram = "column", Rowv = FALSE,
        sepcolor="white", colsep = c(1:7), rowsep = c(1:162),
        sepwidth = c(0.01, 0.01), RowSideColors = rc,
        keysize = 1, margins = c(6, 20))

####################
library(tidyverse)

Source <- c(rep("Water", 12), rep("Oil", 12))
Range <- rep((c(rep("First", 4), rep("Second", 8))),2)
Xaxis <- c(0,1,2,5,0,1,2,5,10,20,30,40,
           0,1,2,5,0,1,2,5,10,20,30,40)
Yaxis <- c(0,1,2,5,0,1,2,5,10,20,30,40,
           0,1,2,5,0,1,2,5,10,20,30,40)

DF <- data.frame(Source, Range, Xaxis, Yaxis)

ggplot(data = DF, aes(x = Xaxis, y = Yaxis)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(Range~Source,
             scales = "free")
##################
# Create an empty (NULL) vector to store the results of each run
results <- list()

# Create the function
draw <- function(){
  # Start with draw_num = 0
  draw_num <- 0
  # Draw a letter
  firstdraw <- sample(LETTERS, 1)
  # Add 1 to draw_num
  draw_num <- draw_num + 1
  # Draw another letter
  nextdraw <- sample(LETTERS, 1)
  # Add another 1 to draw_num
  draw_num <- draw_num + 1
  # Print the letters (you can omit this if you don't need it)
  print(firstdraw)
  print(nextdraw)
  # Then use a while loop to print the letters and add 1 to draw_num
  # until the next letter comes before the previous
    while(nextdraw > firstdraw) {
      firstdraw <- nextdraw
      nextdraw <- sample(LETTERS, size = 1)
      draw_num <- draw_num + 1
      print(nextdraw)
    }
  # Then print draw_num
  print(draw_num)
  # And append draw_num to the results vector
  results[] <- c(results, draw_num)
}

# Run the function a bunch of times
draw()

# Then you can see the average number of letters drawn across all runs
mean(results)
################
library(tidyverse)
df <- data.frame(
  id=c(1,1,1,2,2,2,3,3,3),
  year=as.character(c(2014, 2015, 2016, 2015, 2015, 2016, NA, NA, 2016)),
  code=c(1,2,2, 1,2,3, 3,4,5),
  dv1=1:9,
  dv2=2:10
)
cols <- c("dv1", "dv2")
df %>% na.omit() %>% group_by(year) %>% summarise(across(all_of(cols), c(mean = mean, sd = sd)))
###############
# Load libraries
library(tidyverse)

# Make example data
ID <- seq(1:50)
Happiness <- sample(1:100, 50, replace = TRUE)
Smoke <- sample(c("yes", "no"), 50, replace = TRUE)
Exercise <- sample(c("yes", "no"), 50, replace = TRUE)
df <- tibble("ID" = ID, "Happiness" = Happiness,
             "Smoke" = Smoke, "Exercise" = Exercise)

# Plot the data
df %>% 
  mutate(Smoke = ifelse(Smoke == "yes",
                        "Smoker",
                        "Non-Smoker"),
         Exercise = ifelse(Exercise == "yes",
                           "Exercises",
                           "Doesn't Exercise"),
         Interaction = factor(str_replace(interaction(Smoke, Exercise),
                                          '\\.', '\n'),
                              ordered=TRUE)) %>% 
  ggplot(aes(x = Interaction, y = Happiness, fill = Smoke)) + 
  geom_point(aes(shape = Exercise),
             position = position_jitter(width = .1),
             size = 2, alpha = 0.8) +
  geom_boxplot(width = .2, outlier.shape = NA, alpha = 0.5) +
  labs(title = "Happiness by Smoking/Exercise",
       y = "Happiness") +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank())
#################
library(tidyverse)
Score <- rbinom(50, 50, 0.25)
Height <- sample(150:200, 50, replace = TRUE)
Age <- sample(18:90, 50, replace = TRUE)
df <- tibble("Score" = Score, "Height (cm)" = Height,
             "Age" = Age)
df %>% 
  mutate(Age_Category = case_when(Age %in% c(18:30) ~ "18 to 30",
                                  Age %in% c(31:40) ~ "31 to 40",
                                  Age %in% c(41:50) ~ "41 to 50",
                                  Age > 50 ~ "over 50")) %>% 
  ggplot(aes(y = Score, x = `Height (cm)`,
             shape = Age_Category, color = Age_Category)) +
  geom_point()
#################
library(RColorBrewer)
scales::show_col(brewer.pal(6, name = "Purples"))

palette_blues <- colorRampPalette(colors = c("white", "#004b88"))(12)
scales::show_col(palette_blues[4:12])
##################
library(tidyverse)
set.seed(3)
df <- data.frame(lambda = c(rep(0, 6), rep(1, 6), rep(1.5, 6)),
                 approach = rep(c(rep("A", 3), rep("B", 3)), 3),
                 value = rnorm(18, 0, 1))

ggplot(data = df, aes(x = factor(lambda), y = value)) +
  geom_boxplot(aes(fill = approach))
################
library(tidyverse)
options(scipen = 100000)
A=c(-682937,-476874)
B=c(27469,28648)
C=c(-19389,-13986)
Z= cbind(A,B,C)
a = data.frame(Year=c(2017,2018),Z)
a %>% pivot_longer(cols = -(Year)) %>% 
  ggplot() +
  geom_rect(aes(ymin = ifelse(value < 0, value, 0),
                ymax = ifelse(value > 0, value, 0),
                xmin = ifelse(name == "A", 0.6,
                              ifelse(name == "B", 1.6, 2.6)),
                xmax = ifelse(name == "A", 1.4,
                              ifelse(name == "B", 2.4, 3.4)))) +
  facet_wrap(~Year) +
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c("A", "B", "C"))
################
library(tidyverse)
library(vroom)
library(fs)

files <- fs::dir_ls(glob = "subject_*.csv")
data <- purrr::map(files, ~vroom::vroom(.x))
list2env(data, envir = .GlobalEnv)

# You can also combine all the dataframes if they have the same columns, e.g.
library(data.table)
concat <- data.table::rbindlist(data, fill = TRUE)
###############
# Downloaded obo from http://purl.obolibrary.org/obo/ncit.obo
#install.packages("ontologyIndex")
for (i in 1:10){
  options(timeout = 5*60)
  library(ontologyIndex)
  temp <- tempfile()
  download.file("http://purl.obolibrary.org/obo/ncit.obo", mode = "wget", temp)
  data <- get_ontology(temp)
  unlink(temp)
  rm(data)
  rm(temp)
  Sys.sleep(sample(1:10, 1))
}
##############
library(psych)
n = 1000
kindness <- rnorm(n)
beauty <- rnorm(n)
score <- kindness + beauty
c50 <- quantile(score, .50)
marriage <- ifelse(score>=c50, 0, 1)
df <- data.frame(marriage, beauty, kindness)

pairs.panels(df, digits = 3, pch = 21,
             bg=c("red","purple")[factor(df$marriage)])

pairs.panels(df, digits = 3, pch = 21,
             bg=viridis::viridis(2)[factor(df$marriage)])
################
library(tidyverse)
df <- data.frame("ID" = c("Householders under 25", "Householders 25-44",
                          "Householders 45-64", "Householders 65+"),
                 "Median Household Income" = c(21835, 38317, 40424, 23763),
                 "Margin of Error" = c(3695, 1609, 2327, 1592),
                 check.names = FALSE)
df
df %>% 
  mutate("Median_minus_MOE" = `Median Household Income` - `Margin of Error`,
         "Median_plus_MOE" = `Median Household Income` + `Margin of Error`,
         "ID" = factor(ID, levels = c("Householders under 25",
                                      "Householders 25-44",
                                      "Householders 45-64",
                                      "Householders 65+"),
                       ordered = TRUE)) %>% 
  ggplot(aes(y = `Median Household Income`, x = ID, colour = ID)) +
  geom_pointrange(aes(ymin = Median_minus_MOE,
                      ymax = Median_plus_MOE),
                  size = 1) +
  ylab("Income ($)") +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(label = scales::dollar_format(),
                     breaks = seq(15000, 45000, 5000)) +
  coord_flip(ylim = c(15000, 45000)) +
  theme_classic(base_size = 16, base_family = "Helvetica") +
  ggtitle("Median Household Income ± Margin of Error in Newark, NJ") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
###########
library(ggplot2)
#install.packages("sysfonts")
library(sysfonts)
#install.packages("showtext")
library(showtext)
font_add(family = "charter", regular = "/System/Library/Fonts/Supplemental/charter.ttc")
showtext_auto()
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  ggtitle("Charter ttc Example") +
  theme(text = element_text(family = "charter", size = 20),
        axis.text = element_blank())
############
library(tidyverse)
data <- tibble::tribble(~v1, ~v2, ~v3, ~v4,
                1, "CC", "DD", "EE",
                2, "CC", "PP", "RR",
                3, "EE", "QQ", "LL",
                4, "OO", "RR", "EE",
                5, "UU", "EE", "DD")
data %>% 
  filter(v2 %in% c("DD", "EE") & v3 %in% c("DD", "EE") |
         v3 %in% c("DD", "EE") & v4 %in% c("DD", "EE") |
         v2 %in% c("DD", "EE") & v4 %in% c("DD", "EE"))
#############
library(tidyverse)
data <- tibble::tribble(~Education, ~Women, ~Men,
                        "Less than 9th grade", 21284, 34479,
                        "9th to 12 grade (no diploma)",	22472,	36837,
                        "High school graduate",	29119,	46833,
                        "Some college, no degree",	35499,	52852,
                        "Associate degree",	38149,	55795,
                        "Bachelor's degree",	54411,	84486,
                        "Master's degree",	69022,	99363,
                        "Doctorate",	90906,	151019
                        )
data$Education <- fct_reorder(data$Education, data$Women, .desc = TRUE)

data %>%
  ggplot(aes(x = Education)) +
  geom_linerange(aes(ymin = Women, ymax = Men, x = Education),
                 size = 1.5, alpha = 0.25) +
  geom_point(aes(y = Women), colour = "#CB5416") +
  geom_point(aes(y = Men), colour = "#267266") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  ylab("Income") +
  theme_bw(base_size = 16) +
  theme(axis.title.y = element_blank())
#############
library(tidyverse)

df <- tibble(ID = 1:200,
                 brand = sample(c("Brand A", "Brand B", "Brand C",
                                  "Brand M", "Brand N", "Brand O",
                                  "Brand X", "Brand Y", "Brand Z"),
                                size = 200, replace = TRUE))
df_int <- df %>% 
  transmute(brand_int = case_when(brand %in% c("Brand A", "Brand B", "Brand C") ~ 1,
                                 brand %in% c("Brand M", "Brand N", "Brand O") ~ 2,
                                 brand %in% c("Brand X", "Brand Y", "Brand Z") ~ 3))


df_int <- df_grouped %>% 
  mutate(brand_int = ifelse(brand_group == "A", 1, ifelse(brand_group == "M", 2, 3)))
#####################
library(dplyr)
library(purrr)
library(gt)
library(ggplot2)


dat = tibble(
  RowLabel = letters[1:5],
  Numeric = seq(100,500,100)
) %>% 
  mutate(
    plotData = RowLabel %>% map(function(pos){
      tibble(y=runif(10)*100) %>% 
        arrange(desc(y)) %>% 
        mutate(x=row_number())
    }),
    plot_obj = plotData %>% map(function(df){
      df %>%
        ggplot(aes(x=x,y=y))+
        geom_col()
    }),
    plot_grob = plot_obj %>% map(cowplot::as_grob)
  )

tab %>% 
  gt::gtsave(filename = "test.pdf", vwidth = 180, vheight = 250)
sessionInfo()

##############

library(tidyverse)
df <- structure(list(Name = c("Apple", "Pear", "Strawberries", "Blackberry"
), Characteristic_1 = c("Yellow", "British", "French", "Blue"
), Characteristic_2 = c("Italian", "Yellow", "Red", "Austrian"
)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"
))

# R has 657 built in colour names. You can see them using the `colours()` function.
# Chances are your colours are contained in this list.
list_of_colours <- str_to_title(colours())
list_of_colours <- c(list_of_colours, "octarine")
df2 <- df %>% 
  mutate(Colour = ifelse(!is.na(str_extract(Characteristic_1, paste(list_of_colours, collapse = "|"))),
                       Characteristic_1, Characteristic_2),
         Origin = ifelse(is.na(str_extract(Characteristic_1, paste(list_of_colours, collapse = "|"))),
                         Characteristic_1, Characteristic_2)) %>% 
  select(Name, Colour, Origin)
df2

##################
library(tidyverse)
library(gt)
ex <- structure(list(Var1 = c(0.0015, 0.22058, 0.335, 0.3747612, 0.4199048
), Var2 = c(0.0002778, 0.1424279, 0.1233159, 0.1684, 0.1816663
)), class = "data.frame", row.names = c(NA, -5L))

ex %>%
  gt() %>%
  data_color(vars(`Var1`, `Var2`),
             scales::col_numeric(palette = c("red", "white"),
                                 domain = c(0, 0.5)))

#################
library(ggplot2)
library(reshape)
mtcars <- melt(mtcars, id="am")
mtcars$am <- as.character(mtcars$am)
p <- ggplot(mtcars, aes(x = variable, y = value)) + 
  geom_boxplot(aes(color = am), width = 0.4, size = 0.4, position = position_dodge(0.6), 
               outlier.shape=NA, notch=T) +
  scale_color_manual(values = c("red", "blue"),
                     name="No name", labels=c("A", "B")) + 
  guides(color = guide_legend(reverse=TRUE)) + 
  coord_flip()
p
################
library(tidyverse)
library(ggforce)

df <- structure(list(name = structure(c("a", "b", "c", "d"), format.stata = "%9s"), 
                     val = c(2, 4, 1.5, 3)), row.names = c(NA, -4L), class = "data.frame")

df %>% 
  ggplot(aes(val, name))+
  geom_point() +
  annotate("text", y = 1.5, x = 1, label = "Group 1", angle = 90, size = 8) +
  annotate("text", y = 3.5, x = 1, label = "Group 2", angle = 90, size = 8) +
  annotate("text", y = 1.5, x = 1.2, label = "}", size = 24) +
  annotate("text", y = 3.5, x = 1.2, label = "}", size = 24)

df %>% 
  mutate(group = ifelse(name %in% c("a", "b"), "group a + b", "group c + d")) %>% 
  ggplot(aes(val, name))+
  geom_point() +
  geom_mark_ellipse(aes(fill = group,
                        color = group))
################
library(tidyverse)
library(rvest)
#install.packages("RSelenium")
library(RSelenium)

driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$navigate("https://karriere.nrw/stellenausschreibung/dba41541-8ed9-4449-8f79-da3cda0cc07c")
webElem <- remote_driver$findElement("id", "popup_close")
webElem$clickElement()
out <- remote_driver$findElement(using = "class", value="css-1nedt8z")
scraped <- out$getElementText()
scraped

##############
library(tidyverse)
year<-seq(2000, 2010,2)
erate1<-runif(6,0,1)/6
erate2<-runif(6,0,1)/6
erate3<-runif(6,0,1)/6
erate4<-runif(6,0,1)/6
barplotdf<-data.frame(erate1,erate2,erate3,erate4,year)

formatted_barplotdf <- barplotdf %>% 
  pivot_longer(cols = -c(year)) %>%
  mutate(sex = ifelse(year %in% c(2000, 2002, 2004), "Female", "Male"),
         day = case_when(name == "erate1" ~ "Fri",
                         name == "erate2" ~ "Sat",
                         name == "erate3" ~ "Sun",
                         name == "erate4" ~ "Thur"))

labels <- formatted_barplotdf %>% 
  group_by(sex, day) %>% 
  summarise(perc = round(sum(value), 4))

ggplot(formatted_barplotdf, aes(x = day, fill = day, y = value)) +
  geom_col() +
  facet_wrap("sex") +
  scale_y_continuous(labels = scales::percent_format(),
                     name = "Percent") +
  scale_fill_discrete(label = c(1, 2, 3, 4)) +
  geom_text(data = labels, aes(y = perc,
                               label = paste(perc * 100, "%", sep = ""),
                               vjust = -0.5))

##############
library(tidyverse)

df1 <- tibble::tribble(
  ~NAME, ~PRODUCT, ~AGENT,        ~DATE_PURCHASE,
  "Karen",   "M_14",  "X_1", "8-25-20021 18:21:28",
  "Jean",   "M_78",  "X_3", "8-26-20021 18:11:06",
  "Jean",   "M_71",  "X_4", "8-26-20021 18:21:01",
  "Jean",   "M_64",  "X_4", "8-27-20021 20:21:59",
  "Keith",   "M_57",  "X_4", "8-27-20021 20:21:02",
  "Alba",   "M_50",  "X_1", "8-28-20021 20:21:03",
  "Alba",   "M_43",  "X_3", "8-29-20021 20:21:04",
  "Alex",   "M_36",  "X_2", "8-25-20021 20:21:05"
)

df2 <- tibble::tribble(
  ~NAME,        ~TYPE,         ~DATE_OF_CALL,        ~DATE_PURCHASE,
  "Karen",   "COMPLAIN", "8-26-20021 18:21:28", "8-25-20021 18:21:28",
  "Jean", "CX_SERVICE", "8-27-20021 18:11:06", "8-26-20021 18:11:06",
  "Jean",   "COMPLAIN", "8-28-20021 18:21:01", "8-26-20021 18:21:01",
  "Jean", "CX_SERVICE", "8-29-20021 20:21:59", "8-27-20021 20:21:59",
  "Keith", "CX_SERVICE", "8-29-20021 20:21:02", "8-27-20021 20:21:02",
  "Alba",   "COMPLAIN", "8-30-20021 20:21:03", "8-28-20021 20:21:03",
  "Alex", "CX_SERVICE", "8-25-20021 21:21:05", "8-29-20021 20:21:04"
)

joined_df <- dplyr::full_join(df1, df2, by = "NAME")

joined_df %>% 
  group_by(NAME) %>% 
  select(-c(DATE_PURCHASE.y)) %>%
  top_n(n = 1, wt = DATE_PURCHASE.x) %>% 
  top_n(n = 1, wt = DATE_OF_CALL) %>% 
  rename("LAST_PRODUCT_PURCHASED" = "PRODUCT",
         "DATE_PURCHASE" = "DATE_PURCHASE.x")
################
library(tidyverse)
#install.packages("webuse")
library(webuse)
#install.packages("labelled")
library(labelled)

auto <- webuse("auto")
auto %>% 
  mutate(labels = labelled::to_factor(auto$foreign, levels = "labels")) %>% 
  select(labels)

auto$labels
###############
library(tidyverse)
library(vroom)
text <- "Dynamic Columns\tMeasure Names\tDynamic Demographics Title\tFirst COVID-19 Dose Administered\tMeasure Values\tVax Complete\r\nAmerican Indian or Alaska Native\tCompleted\tRace\t2856\t0.042746728\t1532\r\nAmerican Indian or Alaska Native\tInitiated\tRace\t2856\t0.079689723\t1532\r\nAsian\tCompleted\tRace\t36975\t0.130168327\t17353\r\nAsian\tInitiated\tRace\t36975\t0.277356877\t17353\r\nBlack or African-American\tCompleted\tRace\t110031\t0.091539309\t66419\r\nBlack or African-American\tInitiated\tRace\t110031\t0.151645789\t66419\r\nMulti-racial\tCompleted\tRace\t91322\t0.507244993\t74389\r\nMulti-racial\tInitiated\tRace\t91322\t0.622708025\t74389\r\nNative Hawaiian or Other Pacific Islander\tCompleted\tRace\t3506\t0.234959927\t2316\r\nNative Hawaiian or Other Pacific Islander\tInitiated\tRace\t3506\t0.355686314\t2316\r\nWhite\tCompleted\tRace\t1267155\t0.161650926\t822187\r\nWhite\tInitiated\tRace\t1267155\t0.249136485\t822187\r\n"
df <- vroom::vroom(text)
###############
library(tidyverse)
library(factoextra)

data("iris")

# Select a single point for each category (i.e. setosa = the 25th value)
# label the selected value, then label the rest of the points with nothing ("")
iris$label <- c(rep("", 24), "setosa", rep("", 25),
                    rep("", 23), "versicolor", rep("", 26),
                    rep("", 24), "virginica", rep("", 25))

# Remove species column (5) and label column and scale the data
iris.scaled <- scale(iris[, -c(5,6)])

# K-means clustering
km.res <- kmeans(iris.scaled, 3, nstart = 10)

# Visualize clusters
fviz_cluster(km.res, iris[, -c(5,6)], alpha = 0.2, shape = 19, geom = c("point")) +
  geom_text(aes(label = iris$label))

###############
library(tidyverse)

tickers <- c("msft","xic.to","fb","aapl","goog","nflx","aal","bmo","xef.to","xec.to","vsp.to","mcd")
Weights <- rep(1/12*100,12)

# Combine tickers and weights
tickers_and_weights <- paste(tickers, paste(round(Weights, 1), "%", sep = ""))
# Convert to a factor
`Tickers and Weights` <- factor(tickers_and_weights, levels = tickers_and_weights)

ypos <- cumsum(Weights) - 0.5*Weights
ypos <- 100 - ypos

ggplot() + theme_bw() +
  geom_bar(aes(x = "", y = Weights, fill = `Tickers and Weights`),
           stat = "identity", color = "white") + 
  coord_polar("y", start = 0) +
  ggtitle("Portfolio Weights") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  #scale_fill_brewer(palette = "PiYG", name = "Tickers") +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(hjust = 0.5, size = 15),
        legend.key.size = unit(0.8,"cm"))

#################
library(tidyverse)
library(ggsignif)
library(ggbeeswarm)

df = rbind(
  data.frame(type="A", subtype="c", value=rnorm(mean=1, 100)),
  data.frame(type="B", subtype="c", value=rnorm(mean=1.5, 100)),
  data.frame(type="A", subtype="d", value=rnorm(mean=2, 100)),
  data.frame(type="B", subtype="d", value=rnorm(mean=2.5, 100))
)

df$Category <- factor(interaction(df$type, df$subtype),
                      levels = c("A.c", "A.d", "B.c", "B.d"),
                      labels = c("Type A\nSubtype c", "Type A\nSubtype d",
                                 "Type B\nSubtype c", "Type B\nSubtype d"))

ggplot(df, aes(x=Category, y=value)) +
  geom_boxplot(aes(colour = subtype),
               outlier.shape = NA) +
  geom_signif(comparisons = list(c("Type A\nSubtype c", "Type B\nSubtype c"),
                                 c("Type A\nSubtype c", "Type A\nSubtype d"),
                                 c("Type B\nSubtype c", "Type B\nSubtype d"),
                                 c("Type A\nSubtype d", "Type B\nSubtype d")),
              test = "wilcox.test", step_increase = 0.075,
              map_signif_level = TRUE, tip_length = 0) +
  geom_quasirandom(aes(fill = type), shape = 21, 
                groupOnX = TRUE, size = 2, alpha = 0.5) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 0.3)

###################
library(tidyverse)
#install.packages("vroom")
library(vroom)

options(scipen = 100000)

data <- vroom(file = "~/Desktop/FARS2019NationalCSV.zip")

data %>%
  mutate(MONTH = factor(sample(month.abb, nrow(data), replace = TRUE),
                        levels = month.abb)) %>%
  select(STATE_FORMAT, A_CRAINJ, MONTH) %>%
  filter(STATE_FORMAT != "No") %>% 
  ggplot(aes(y = A_CRAINJ, x = MONTH)) +
    geom_col() +
  facet_wrap(~STATE_FORMAT, ncol = 1, strip.position = "right") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_y_continuous(breaks = c(0, 300))
ggsave(filename = "example_1.png", width = 15, height = 45, units = "cm")

#################
library(tidyverse)
Country <- data_frame(T = runif(10))
State <- data_frame(T = runif(10))

plot(Country$T, State$T)

new_df <- data_frame(Country$T, State$T)
ggplot(new_df, aes(x = Country$T, y = State$T)) +
  geom_point() +
  labs(title = "Temperatury Country vs. State", subtitle = "2010-2020", 
       x = "Country T", y = "State T")+ 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
##################
library(tidyverse)

Code=c("A","B","C","E","D")
Desciption=c("Color is not Good","Odour is not good","Astetic Issue","Odour is not good","Lighting issue")
Company=c("Asian Paints","Burger","Asian Paints","Burger","Burger")
`Room number`=c("Room_1","Room_1","Room_2","Room_3","Room_2")
Rating=c("2","3","5","4","3")
DF1=data.frame(Code,Desciption,Company,`Room number`,Rating)

Code=c("A","B")
Desciption=c("Color is not Good","Odour is not good")
Company=c("Asian Paints","Burger")
`Room number`=c("Room_1","All Rooms")

DF2=data.frame(Code,Desciption,Company,`Room number`)

dplyr::anti_join(x = DF1, y = DF2, by = "Desciption")
##################
library(tidyverse)
t <- structure(list(Col1 = c("Area 1", "Area 2", "Area 3", "Area 4", 
                        "Area 5", "Area 6"), COl2 = c("BILLY JOE", "SALLY SUE", "LAURA Lane", 
                                                      "JOS<c9> SOME NAME", "NORMA NAME", "ESTER BETH")), class = c("spec_tbl_df", 
                                                                                                                   "tbl_df", "tbl", "data.frame"), row.names = c(NA, -6L), spec = structure(list(
                                                                                                                     cols = list(Col1 = structure(list(), class = c("collector_character", 
                                                                                                                                                                    "collector")), COl2 = structure(list(), class = c("collector_character", 
                                                                                                                                                                                                                      "collector"))), default = structure(list(), class = c("collector_guess", 
                                                                                                                                                                                                                                                                            "collector")), skip = 1L), class = "col_spec"))

data.frame(lapply(t, function(x) {
  gsub(pattern = "<c9>",
       replacement = "\U00C9", x)
  }))

t %>% 
  summarise(across(everything(),
                   ~ str_extract(string = .x,
                                 pattern = "<.*>")))

t2 <- t %>% 
  mutate(across(everything(),
                ~ gsub(pattern = "<c9>",
                       replacement = "\U00C9", .x)))
#################
library(tidyverse)
data = data.frame(
  id = 1:5,
  x = c("A", "A", "B", "C", "D")
)

lookup = data.frame(
  old = c("A", "D"),
  new = c("a", "d")
)

set.seed(1)
data <- data[sample(1:5, 1E7, replace = T),] 

dplyr_coalesce <- function(){
  library(dplyr)
  lookupV <- setNames(lookup$new, lookup$old)
  data %>% 
    mutate(x = coalesce(lookupV[ x ], x))
}

dplyr_coalesce_transmute <- function(){
  library(dplyr)
  data %>%
    left_join(lookup, by = c("x" = "old")) %>%
    transmute(x = coalesce(new, x))
}

data_table <- function(){
  library(data.table)
  lookupV <- setNames(lookup$new, lookup$old)
  data <- copy(data)
  setDT(data)
  data[ x %in% names(lookupV), x := lookupV[ x ] ]
}

purrr_modify_if <- function(){
  library(dplyr)
  library(purrr)
  lookupV <- setNames(lookup$new, lookup$old)
  data %>% 
    mutate(x = modify_if(x, x %in% lookup$old, ~ lookupV[.x]))
}

stringr_str_replace_all_update <- function(){
  library(dplyr)
  library(stringr)
  lookupV <- setNames(lookup$new, do.call(sprintf, list("^\\Q%s\\E$", lookup$old)))
  
  data %>% 
    mutate(x = str_replace_all(x, lookupV))
}

base_named_vector <- function(){
  lookupV <- c(with(lookup, setNames(new, old)), set_names(setdiff(unique(data$x), lookup$old)))
  lookupV[data$x]
}

base_ifelse <- function(){
  lookupV <- setNames(lookup$new, lookup$old)
  with(data, ifelse(x %in% lookup$old, lookup$new, x))
}

plyr_mapvalues <- function(){
  library(plyr)
  data %>% 
    dplyr::mutate(x = plyr::mapvalues(x, lookup$old, lookup$new, warn_missing = F))
}

base_match <- function(){
  data$x[data$x %in% lookup$old] <- lookup$new[match(data$x, lookup$old, nomatch = 0)]
}

base_local_safe_lookup <- function(){
  lv <- structure(lookup$new, names = lookup$old)
  
  safe_lookup <- function(val) {
    new_val <- lv[val]
    unname(ifelse(is.na(new_val), val, new_val))
  }
  
  safe_lookup(data$x)
}

dplyr_recode <- function(){
  library(dplyr)
  lookupV <- setNames(lookup$new, lookup$old)
  
  data %>%
    mutate(x = recode(x, !!!lookupV))
}

library(microbenchmark)
bench <- microbenchmark(dplyr_coalesce(),
                        #data_table(),
                        purrr_modify_if(),
                        dplyr_coalesce_transmute(),
                        stringr_str_replace_all_update(),
                        base_named_vector(),
                        base_ifelse(),
                        plyr_mapvalues(),
                        base_match(),
                        base_local_safe_lookup(),
                        dplyr_recode(),
                        times = 10L)

bench$expr <- forcats::fct_rev(forcats::fct_reorder(bench$expr, bench$time, mean))
ggplot2::autoplot(bench)
#######################
library(tidyverse)
df = tribble(~Date, ~Name,  ~Team,  ~Status,    ~Hours_Type,    ~Hours, ~Standard,  ~Deficit,   ~Overtime,  ~Leave,
             "April 16 2021",    "Jeff", "Coastal",  "FT",   "Billable", 40, 40, 0,  0,  0,
             "April 23 2021",    "Jeff", "Coastal",  "FT",   "Billable", 40, 40, 0,  0,  0,
             "April 16 2021",    "Jeff", "Coastal",  "FT",   "Leave",    0,  0,  0,  0, 0,
             "April 23 2021",    "Jeff", "Coastal",  "FT",   "Leave",    0,  0,  0,  0, 0,
             "April 16 2021",    "Megan",    "Coastal",  "FT",   "Billable", 40, 40, 0,  0,  0,
             "April 23 2021",    "Megan",    "Coastal",  "FT",   "Billable", 40, 40, 0,  0,  0,
             "April 16 2021",    "Megan",    "Coastal",  "FT",   "Leave",    0, 0,   0,  0, 0,
             "April 23 2021",    "Megan",    "Coastal",  "FT",   "Leave",    0, 0,   0,  0, 0,
             "April 16 2021",    "Minden",   "Coastal",  "FT",   "Billable", 16, 16, 24, 0,  0,
             "April 23 2021",    "Minden",   "Coastal",  "FT",   "Billable", 28, 28, 12, 0,  0,
             "April 16 2021",    "Minden",   "Coastal",  "FT",   "Leave",    24, 0,  0,  0, 24,
             "April 23 2021",    "Minden",   "Coastal",  "FT",   "Leave",    0,  0,  0,  0, 0)

df %>% 
  pivot_longer(cols = c(Standard, Deficit, Overtime, Leave)) %>% 
  arrange(Date) %>%
  group_by(Date, Name) %>% 
  summarise(name, value) %>% 
  filter(value != 0) %>% 
  pivot_wider(id_cols = c(Date, Name)) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(Deficit_Corrected = ifelse(Leave > 0 | Deficit > 0,
                                    Deficit - Leave, Deficit),
         Standard_Corrected = Standard + Leave) %>%
  ggplot(aes(x = Name, y = Standard_Corrected)) +
  geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
  geom_text(aes(label = Deficit_Corrected), nudge_y = -5) +
  geom_text(aes(label = paste("Leave = ", Leave, "hrs / ", "Deficit = ", Deficit, "hrs", sep = "")), nudge_y = 1.5) +
  facet_wrap(~ Date, ncol = 1)

df %>% 
  pivot_longer(cols = c(Hours, Standard, Deficit, Overtime, Leave)) %>% 
  arrange(Date, Name) %>% 
  select(-c(Hours_Type)) %>% 
  group_by(Date, Name, name) %>% 
  summarise(value = max(value)) %>% 
  pivot_wider(id_cols = c(Date, Name),
              names_from = name,
              values_from = value) %>% 
  mutate(Deficit = Deficit - Leave,
         Hours = Standard + Leave) %>%
  pivot_longer(-c(Date, Name)) %>% 
  filter(name != "Hours") %>%
  rename(hours = value) %>% 
  ggplot(aes(x = Name, y = hours, fill = name)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(hours != 0,
                               paste(name,
                                     hours,
                                     sep = " = "), "")),
            position = position_stack(vjust = 0.5)) +
  facet_wrap(~ Date, ncol = 1) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank())
##################
library(tidyverse)
library(ggridges)

leafnumber <- c(runif(1, 6, n = 100),
                runif(3, 9, n = 100),
                runif(5, 12, n = 100),
                runif(10, 15, n = 100),
                runif(15, 18, n = 100))

PU <- factor(c(rep("1", 100), rep("2", 100),
        rep("3", 100), rep("4", 100), rep("5", 100)),
        levels = c("1", "2", "3", "4", "5"))

weeknumber <- c(8,8,8,8,8,9,9,9,9,9)

df <- data.frame(leafnumber, PU, weeknumber)

ggplot(df, aes(x = leafnumber, y = PU, height = stat(density))) + 
  geom_density_ridges(stat = "binline", bins = 40,
                      scale = 0.95, draw_baseline = FALSE) +
  coord_flip()
##################
library(tidyverse)
library(caret)
mydf <- structure(list(pred_class = c("dog", "dog", "fish", "cat", "cat", 
                                      "dog", "fish", "cat", "dog", "fish"), true_class = c("cat", "cat", 
                                                                                           "dog", "cat", "cat", "dog", "dog", "cat", "dog", "fish")), row.names = c(NA, 
                                                                                                                                                                    10L), class = "data.frame")

conf_matrix <- confusionMatrix(factor(mydf$pred_class),
                               reference = factor(mydf$true_class),
                               mode = "everything", )
conf_matrix
#####################
library(tidyverse)

df <- structure(list(ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 
                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), elevation = c(150L, 
                                                                                         140L, 130L, 120L, 110L, 120L, 130L, 140L, 150L, 90L, 80L, 70L, 
                                                                                         66L, 60L, 50L, 66L, 70L, 72L, 68L, 65L, 60L, 68L, 70L), code = c(NA, 
                                                                                                                                                          NA, "W", NA, NA, NA, "W", NA, NA, NA, NA, NA, "W", NA, NA, "W", 
                                                                                                                                                          NA, NA, "W", NA, NA, "W", NA)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                              -23L))

df %>% 
  mutate(ind = ifelse(cumsum(!is.na(df$code)) %% 2 == 0, NA, "W")) %>% 
  mutate(code = ifelse(ind == "W" | code == "W", "W", NA)) %>% 
  select(-c(ind))

df2 <- df %>%
  mutate(code = coalesce(code, c(NA, "W")[cumsum(!is.na(df$code)) %% 2 + 1]))
#####################
library(tidyverse)
library(ggthemes)

d<-rnorm(1000, 0, 1); hist(d, bty="n")
d <- tibble(values = rnorm(1000, 0, 1))

ggplot(d, aes(x = values)) +
  geom_histogram() +
  geom_rangeframe() +
  theme_tufte()
#####################s
# Create the dataframe
df <- data.frame(MonthYr = c(202101, 202102, 202103, 202104),
           Count1 = c(123456, 123425, 123452, 123455),
           Count2 = c(123456, 123429, 123453, 123454),
           Count3 = c(123455, 123428, 123455, 123455),
           Count4 = c(123455, 123428, 123455, 123455),
           Count5 = c(123455, 123428, 123455, 123455))

# Make MonthYr a factor
df$MonthYr = factor(x = df$MonthYr,
       levels = c("202101", "202102", "202103", "202104"),
       labels = c("Jan - 2021", "Feb - 2021", "Mar - 2021", "Apr - 2021"))

# Reshape the dataframe to the "long" format
df2 <- reshape(df, varying = 2:6, v.names = c("Value"),
        direction = "long")

df2$Count <- factor(df2$time,
                   levels = c(1, 2, 3, 4, 5),
                   labels = c("Count 1", "Count 2", "Count 3", "Count 4", "Count 5"))
df2

library(ggplot2)
library(gridExtra)

plot1 <- ggplot(df2, aes(x = MonthYr, y = Value, fill = Count)) +
  geom_bar(width = 0.5, position = position_dodge(0.7), stat = "identity") +
  coord_cartesian(ylim = c(123405, 123460)) +
  theme_dark(base_size = 16) +
  theme(axis.title = element_blank(),
        legend.position = "bottom")

table_theme <- ttheme_default(base_size = 14, padding = unit(c(8, 8), "mm"))
table1 <- tableGrob(df, rows = NULL, theme = table_theme)
grid.arrange(plot1, table1, nrow = 2, heights = c(1, 0.75))



# Plot the distribution of Counts for each MonthYr
plot(x = df2$MonthYr, y = df2$Value, xlab = "Month - Year",
     ylab = "Values", main = "Distribution of Counts for each Timepoint")

"Jan 2021" <- df2$Value[df2$MonthYr == "Jan - 2021"]
"Feb 2021" <- df2$Value[df2$MonthYr == "Feb - 2021"]
"Mar 2021" <- df2$Value[df2$MonthYr == "Mar - 2021"]
"Apr 2021" <- df2$Value[df2$MonthYr == "Apr - 2021"]
dev.off()
par(mfrow = c(2, 2))
hist(`Jan 2021`, las = 2, xlab = "")
hist(`Feb 2021`, las = 2, xlab = "")
hist(`Mar 2021`, las = 2, xlab = "")
hist(`Apr 2021`, las = 2, xlab = "")

hist(df2$Value, col = df2$MonthYr, breaks = 30, freq = TRUE)
legend("topleft", c("Jan", "Feb", "Mar", "Apr"), fill = 1:4)

breaks <- seq(min(df2$Value), max(df2$Value), 0.5)
yaxis <- seq(1, 4, length.out = 20)
plot(x = df2$Value, y = yaxis, type = "n", ylab = "Count", xlab = "Value")
hist(`Jan 2021`, add = TRUE, breaks = breaks, col = 1, border = 1)
hist(`Feb 2021`, add = TRUE, breaks = breaks, col = 2, border = 2)
hist(`Mar 2021`, add = TRUE, breaks = breaks + 0.33, col = 3, border = 3)
hist(`Apr 2021`, add = TRUE, breaks = breaks + 0.66, col = 4, border = 4)
legend("topleft", c("Jan", "Feb", "Mar", "Apr"), fill = 1:4)

#############

library(vroom)
dataset <- vroom("https://raw.githubusercontent.com/rnorouzian/v/main/fa.csv")

#############
library(tidyverse)
#remotes::install_github("moodymudskipper/boomer")
library(boomer)
library(magrittr)

df <- tibble(
  a = c(1, 2, 3),
  b = c("m", "n", "o"),
  c = c("p", "q", "r")
)

df %>% 
  names(.)[1] %>% 
  boom()

#####################
library(tidyverse)
library(ggsignif)
temp <- data.frame(sample = seq(1:83), 
                   condition = c("KO", "KO", "KO", "KO", "KO", "KO", "KO", "KO", "KO", "KO", 
                                 "KO", "KO", "KO", "KO", "KO", "KO", "KO", "WT", "WT", "WT", "WT", 
                                 "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", 
                                 "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", 
                                 "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", 
                                 "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", 
                                 "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", "WT", 
                                 "WT", "WT", "WT", "WT", "WT", "WT", "WT"),
                   set = c("di", "tet", "mono", "di", "tet", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono", "di", "tet", "mono", "di", "tet", "mono", "di", 
                           "tet", "mono"),
                   score = c(0, 0, 0, -5.95940584704586, 10.0932562947815, -3.16676604569923, 
                             4.46133814098881, -1.9888611720281, -7.08194974795108, 3.17097628218171, 
                             -1.83986980857496, -0.843268716519414, 1.40178526106758, -0.340339302553342, 
                             -3.76913603338144, 4.28943971347741, -3.20197704274428, -3.54168755452774, 
                             15.414510676737, -5.85333426479177, -1.87949902971026, 14.1424002410594, 
                             -2.14726619139082, -5.01378228438499, 11.1131227496058, 0.097013584879446, 
                             -6.55527134311774, 12.2234175105232, -2.55259067519978, -6.68392512983342, 
                             15.8358484731832, -2.27825891764331, -5.40451835097939, 11.2240240941934, 
                             -1.53075128507785, -4.80008896082703, 15.3667539728667, -4.81370852797055, 
                             -2.69976280917806, 21.9926791189896, 0.61798090190648, -8.68663007652496, 
                             13.8852585926079, -0.80329005076484, -16.8459570277756, 13.3500356549569, 
                             -5.59531483186873, -3.33602772657725, 11.338954967882, -2.12614700145763, 
                             -3.37418493461362, 11.5903340330526, -2.87224785160433, -14.7792521265679, 
                             7.7542705233175, -2.06876649679246, -1.31032740187699, 17.6666627835987, 
                             -0.824420163207606, -4.82659116096503, -0.0626028094479509, -1.90431942338018, 
                             -20.3180652520285, 6.83632028067972, -1.58570945276274, -2.77261532150261, 
                             2.98168865160908, -1.89922364414076, -5.80584721371712, 7.13599592922731, 
                             -1.23964885854847, -5.1236580504364, 14.5532078112838, -3.82017971494402, 
                             -4.39741499313133, 11.7342969461195, -2.5852194454582, -3.18458897513284, 
                             12.7762556862722, 1.1245622869403, -2.98430760077172, 13.981837061262, 
                             -1.19532384849617))

anno_df = compare_means(score ~ condition, group.by = "set", data = temp) %>%
  mutate(y_pos = c(3, 24, 2.5))

ggplot(temp, aes(x=condition, y=score, fill=set)) +
  stat_summary(fun=mean, geom="bar", colour="black", alpha=1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.25) +
  geom_point(color="black", shape=21, show.legend=TRUE, size=3, stroke=1) +
  facet_wrap(~set, scales="free") +
  geom_signif(data = anno_df, aes(xmin=group1,
                                  xmax=group2,
                                  annotations=p.signif,
                                  y_position = y_pos),
              manual = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))
###############
library(vroom)
df <- vroom(file = "~/Downloads/advanced-r-statistical-programming-and-data-models-master/ICPSR_04690/DS0001/04690-0001-Data.tsv")
###############
library(factoextra)
library(tidyverse)
pca_model <- prcomp(USArrests, scale=TRUE)

Zcored_data <- data.frame(scale(USArrests))

loads <- pca_model$rotation

(PC1 =    loads[1,1]*Zcored_data[[1]]+
    loads[2,1]*Zcored_data[[2]]+
    loads[3,1]*Zcored_data[[3]]+
    loads[4,1]*Zcored_data[[4]])

(PC2 =    loads[1,2]*Zcored_data[[1]]+
    loads[2,2]*Zcored_data[[2]]+
    loads[3,2]*Zcored_data[[3]]+
    loads[4,2]*Zcored_data[[4]])
pca_model$x
#####################
library(tidyverse)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/10_OneNumSevCatSubgroupsSevObs.csv", header=T, sep=",") %>%
  mutate(tip = round(tip/total_bill*100, 1))

# Grouped
data %>%
  ggplot(aes(fill=sex, y=tip, x=day)) + 
  geom_violin(trim = FALSE, position="dodge", alpha=0.5) +
  xlab("") +
  ylab("Tip (%)") +
  ylim(0,40) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 0.75,
               position = position_dodge(width = 0.9))
#######################
library(tidyverse)
abline(v = c(1990,1991,1992,1993,1994, 1995,1996, 1997,1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,2008,2009,2010, 2011,2012,2013,2014,2015,2016,2017),
col = c("red","red","red","red","red","red","red", "red","red","red","red","red","red","red", "red","red","red","red","red","red","red", "red","red","red","red","red","red","red"),
lty = c(2,2,2,2,2,2,2, 2,2,2,2,2,2,2, 2,2,2,2,2,2,2, 2,2,2,2,2,2,2),
lwd = c(1,1,1,1,1,1,1, 1,1,1,1,1,1,1, 1,1,1,1,1,1,1, 1,1,1,1,1,1,1),
h = c(200,400,600,800,1000))

abline(v = 1990:2017, col = "red", lty = 2, h = seq(200, 1000, 200))
#######################
library(tidyverse)

Data %>% 
  mutate(category = case_when(diagnosis %in% c("circulatory", "digestive", "EMI", "mental",
                                            "musculoskeletal", "neoplasm", "nervous",
                                            "poorly_defined", "skin") ~ "Fewer",
                              diagnosis %in% c("congenital", "external_cause",
                                                   "genitourinary","injury",
                                                   "pregnancy_childbirth",
                                                   "respiratory") ~ "NoSigDiff",
                              diagnosis == "infection" ~ "More",
                              diagnosis == "blood" ~ "Blood")) %>% 
ggplot(aes(x = diagnosis1, y = number_diagnoses, fill = category))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text.y=element_text(angle=90,hjust=0.5,vjust=0.5))
#####################
library(tidyverse)
df <- tibble(year = "2015",
             month = rep(1:3, each = 8),
             category = rep(c("Cat A", "Cat B",
                              "Cat C", "Cat D"), 6),
             number = c(180, 98, 457, 303, sample(1:100, 20,
                                                  replace = TRUE)),
             type = rep(c("5 year", "10 year"), each = 4, 3))
df %>% 
  pivot_wider(values_from = number, names_from = type)
###################
library(ggplot2)
test <- cbind.data.frame(value = rnorm(200), group = factor(rep(1:4, 50)),
                         medcol = rep(c("light", "dark", "light", "dark"), 50))

hcl <- farver::decode_colour(c("black", "white", "darkred", "lightblue"), "rgb", "hcl")
label_col <- ifelse(hcl[, "l"] > 50, "grey25", "grey90")

ggplot(data = test, aes(x = group, y = value,
                             fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("1" = "black",
                               "2" = "white",
                               "3" = "darkred",
                               "4" = "lightblue")) +
  stat_summary(aes(color = group), geom = "crossbar", width = 0.7,
               fatten=0.1, show.legend = FALSE, 
               fun.data = function(x){c(y=median(x),
                                        ymin=median(x),
                                        ymax=median(x))}) +
  scale_colour_manual(values = label_col)


test2 <- cbind.data.frame(value = rnorm(400), group = factor(rep(1:8, 50)),
                         medcol = rep(c("light", "dark", "light", "dark"), 100))

hcl <- farver::decode_colour(RColorBrewer::brewer.pal(n = 8, name = "Blues"), "rgb", "hcl")
label_col <- ifelse(hcl[, "l"] > 60, "grey25", "grey80")

ggplot(data = test2, aes(x = group, y = value,
                        fill = group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  stat_summary(aes(color = group), geom = "crossbar", width = 0.7,
               fatten=0.1, show.legend = FALSE, 
               fun.data = function(x){c(y=median(x),
                                        ymin=median(x),
                                        ymax=median(x))}) +
  scale_colour_manual(values = label_col)
  #############################
library(tidyverse)
df <- tibble::tribble(
  ~MeanCost, ~Std, ~MedianCost, ~LowerIQR, ~UpperIQR, ~StatusGroup, ~AgeGroup,
       700L, 500L,        650L,      510L,      780L,       "Dead",   "Young",
       800L, 600L,        810L,      666L,     1000L,      "Alive",   "Young",
       500L, 200L,        657L,      450L,      890L,   "Comatose",   "Young",
       300L, 400L,        560L,      467L,      670L,       "Dead",     "Old",
       570L, 600L,        500L,      450L,      600L,      "Alive",     "Old",
       555L, 500L,        677L,      475L,      780L,   "Comatose",     "Old",
       333L, 455L,        300L,      200L,      400L,       "Dead",  "Middle",
       678L, 256L,        600L,      445L,      787L,      "Alive",  "Middle",
      1500L, 877L,        980L,      870L,     1200L,   "Comatose",  "Middle"
  )

df %>% 
  mutate(AgeGroup = factor(AgeGroup, levels = c("Young", "Middle", "Old"))) %>% 
  ggplot(aes(x = AgeGroup, fill = StatusGroup)) +
  geom_boxplot(aes(
    lower = LowerIQR, 
    upper = UpperIQR, 
    middle = MedianCost, 
    ymin = MedianCost - Std, 
    ymax = MedianCost + Std),
    stat = "identity", width = 0.5) +
  geom_point(aes(y = MeanCost),
             position = position_dodge(width = 0.5),
             shape = 4)
######################
spineplot(table(mtcars$cyl, mtcars$gear),
          xlab = "Number of Cylinders",
          ylab = "Number of Gears",
          main = "Spineplot: mtcars dataset")
spineplot(factor(mtcars$cyl) ~ mtcars$hp,
          xlab = "Horsepower",
          ylab = "Number of Gears",
          main = "Spineplot: mtcars dataset")
#####################
library(tidyverse)
df <- tibble::tribble(
  ~patient_id, ~treatment_1, ~treatment_2, ~date_dummy,
           3L, "2012-01-04", "2012-03-27",          0L,
           3L, "2021-07-11", "2012-10-20",          0L,
           3L, "2013-04-04", "2013-06-22",          0L,
          12L, "2012-12-09", "2013-11-09",          0L,
          18L, "2012-02-25", "2012-03-26",          0L,
          25L, "2012-10-06", "2013-12-29",          1L,
          25L, "2013-04-06", "2013-07-07",          0L
  )
df2 <- df %>% 
  mutate(date_dummy = sample(0:1, nrow(.), replace = TRUE))
df2
library(lubridate)
df %>%
  mutate(date_dummy = ifelse(ymd("2012-12-11")
                             %within%
                               interval(treatment_1,
                                        treatment_2),
                             1, 0))
###################
library(tidyverse)
df <- tribble(~"mode of transportation",       ~"distance<1km",    ~"distance2km-5km",    ~"distance>5km", 
"Car,truck,van",                 4,                  45,               53, 
"Bus",                           3,                  13,               24, 
"Subway,elevated,rail",          0,                  34,               67)

df %>% 
  pivot_longer(cols = -c(`mode of transportation`),
                         values_to = "Frequency",
                         names_to = "Distance") %>%
  mutate(Distance = factor(Distance,
                           levels = c("distance<1km",
                                      "distance2km-5km",
                                      "distance>5km"))) %>% 
  ggplot(aes(fill = Distance, y = Frequency, x = `mode of transportation`)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()
#####################
cpu_log <- c()
for (i in seq(1, 1000, 1)) {
  print(i)
  cpu_log[[i]] <- system("ps -A -o %cpu | awk '{ cpu += $1} END {print cpu}' ",
                          intern = TRUE)
}

plot(y = cpu_log, x = 1:1000)
###################
library(tidyverse)
set.seed(321)

simulate_max <- function(input_coins, N_number_of_tosses, alpha = 0.05){
  maxList <- NULL
    CoinIteration <- sample(input_coins)  
    testresults <- map_df(
      CoinIteration,
      function(x) broom::tidy(binom.test(x = x, n = N_number_of_tosses,
                             p = alpha)
      )
    )
    thisRandMax <- max(testresults$estimate)
    maxList <<- c(maxList, thisRandMax)
  return(testresults)
}
    
  negative_likelihood <- function(p){
      dbinom(50, 100, p * (1/N_number_of_tosses))*-1
    }
    MLE <- nlm(negative_likelihood, 0.5, stepmax = 0.5)
    MLE$maximum <- MLE$minimum * -1
    thisRandMax <- max(testresults$statistic)
    maxList <- c(maxList, thisRandMax)

coins <- rbinom(50, 100, 0.5)

max <- simulate_max(input_coins = coins,
                    N_number_of_tosses = 100, alpha = 0.05)



test2 <- negative_likelihood(coins[3])*-1

test <- binom.test(x = 50, n = 100, p = coins[3]*1/100)
test$p.value
####################
library(tidyverse)
df <- tibble::tribble(
       ~NumID,    ~col1,     ~col2, ~col3, ~col4,                 ~col5,
  "tt0035790", "Action", "History", "War", 2017L,       "Walter Huston",
  "tt0035790", "Action", "History", "War", 2017L,     "Harry Davenport",
  "tt0035790", "Action", "History", "War", 2017L,        "Dana Andrews",
  "tt0066853",  "Drama",    "2016",    NA,    NA, "Ivan de Albuquerque",
  "tt0066853",  "Drama",    "2016",    NA,    NA,      "Rubens Correia"
  )

df %>% 
  group_by(NumID) %>% 
  mutate(count = row_number()) %>% 
  pivot_wider(names_from = count, values_from = col5)
#####################
par(xpd = TRUE)
par(mar=c(2,4,3,6))
mtcars_cyl_percentage <- as.matrix(table(mtcars$cyl)*100/sum(table(mtcars$cyl)))
barplot(mtcars_cyl_percentage, border="white", col = 4:2, main = "Stacked Barplot: mtcars dataset",
        ylab = "Proportion (%)")
legend("topleft", c("8 cylinders", "6 cylinders", "4 cylinders"),
       inset = c(1, 0), bty = "n", fill = 2:4)

#####################
library(tidyverse)
devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(ggrepel)
options(scipen = 1000000)

salaries <- nba_insider_salaries(assume_player_opt_out = T,
                                       assume_team_doesnt_exercise = T,
                                       return_message = T)
salaries %>% 
  group_by(nameTeam) %>% 
  filter(str_detect(slugSeason, "2020")) %>% 
  select(nameTeam, value) %>%
  summarise(total = sum(value)) %>% 
  mutate(perc = total / sum(total)) %>%
  mutate(ymax = cumsum(perc),
         ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelPosition = (ymax + ymin) / 2) %>%
  rename(`Team Name` = nameTeam,
         `Total Salaries` = total) %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(aes(fill = `Total Salaries`)) +
  geom_label_repel(x = 4, aes(y = labelPosition,
                             label = `Team Name`),
                  min.segment.length = 0, size = 2, force = 3) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle("NBA Total Salaries", subtitle = "2020 Season") +
  scale_fill_continuous(labels = scales::dollar) +
  theme_void()
################
library(tidyverse)

rank <- tribble(~"species", ~"ab", ~"rank",
"PEGR2", 462.10,  792,
"COUM", 269.70, 1126,
"KRGR", 207.04, 1314,
"KRER", 177.11, 1439,
"PEBR", 176.00, 1446,
"CAMI12", 140.71, 1623)

ggplot(rank, aes(x = reorder(rank, -ab), y = ab)) +
  geom_bar(stat = "identity") +
  labs(x = "Abundance Rank", y = "Abundance") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c(792, 1623),
                   labels = c(792, 1623))
#################
library(tidyverse)
library(lubridate)

df <- tribble(~"ID", ~"UniSA_Customer_No", ~"Sale_Date",
              1, "235762609", "2014-01-01",
              2, "235762609", "2014-03-01",
              3, "235893712", "2014-01-01",
              4, "89371212", "2014-01-04",
              5, "89371212", "2014-01-04",
              6, "89371212", "2015-01-04")
df %>% 
  mutate(Sale_Date = ymd(Sale_Date)) %>% 
  filter(Sale_Date %within% interval("2014-01-01", "2015-01-01")) %>% 
  group_by(UniSA_Customer_No) %>% 
  summarise(Total_in_year_2014 = n())
################
library(ggplot2)
df <- data.frame(var = "", val = 0)

ggplot(df) + 
  geom_point(aes(val, var), color = "red", alpha = 1, size = 10) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0,1)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "black"))

df <- data.frame(var = c(0, 1, 2, 3), val = c(0, 1, 2, 3))

ggplot(df) + 
  geom_point(aes(val, var), color = "red", alpha = 1, size = 6) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 3)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 3)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_line(color = "black"))
##################
library(tidyverse)
df <- read_table2("Entry     A.    B.    C.    D.    Value
O60701    1     1     1     0     2.7181970
Q8WZ42    1     1     1     1     3.6679832
P60981    1     1     0     0     2.2974231
Q15047    1     0     0     0     0.5535473
Q9UER7    1     0     0     0     4.1030394")

df %>% 
  pivot_longer(cols = -c(Entry, Value)) %>% 
  ggplot(aes(x = Value, y = Entry, fill = name, color = name, shape = factor(value))) +
  geom_jitter(width = 0, height = 0.25, size = 4, alpha = 0.5) +
  scale_shape_manual(values = c(21, 22)) +
  hrbrthemes::theme_ipsum_ps(base_size = 16)
################
library(tidyverse)
library(rvest)

page <- read_html("https://thehustle.co/daily/page/33/") %>% 
  html_elements("div.col-md-12.daily-wrap.clearfix") %>% 
  html_element("a") %>% 
  html_attr("href")
################
library(tidyverse)
df1 <- data.frame(country = c("a", "b", "c"), year1 = c(1, 2, 3), year2 = c(1, 2, 3))
df2 <- data.frame(country = c("a", "b", "d"), year1 = c(1, 2, 3), year2 = c(1, 2, 3))
df3 <- rbind(df1, df2)

df3 %>%
  group_by(country) %>% 
  summarise(across(.cols = everything(), .fns = sum))
###############
library(tidyverse)
#install.packages("vroom")
library(vroom)
#install.packages("ggrepel")
library(ggrepel)

df <- vroom(file = "example.txt")

df %>% 
  pivot_longer(cols = -c(Name), names_to = "category") %>% 
  ggplot(aes(x = Name, y = category, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(Name == "BCI0503", category, "")),
            nudge_x = -0.5) +
  geom_text_repel(aes(label = ifelse(category == "TEM", Name, "")),
                  nudge_y = 2) +
  scale_fill_gradient(low = "beige", high = "red", breaks = c(0, 1)) +
  coord_polar(theta="x") +
  theme_void()

#install.packages("UpSetR")
library(UpSetR)

df_int <- df %>% 
  mutate(across(c(2:9), as.integer)) %>% 
  as.data.frame()

upset(df_int, order.by = "freq", sets.bar.color = "#56B4E9", nsets = 8)
######################
library(tidyverse)
df <- tribble(~"Speak_Dur", ~"CNC_count", ~"TNT_count",
0.5, 1, 0,
0.8, 0, 1,
4.3, NA, 1,
5.5, 1, 0)

df2 <- df %>% 
  mutate(CNC_dur = ifelse(CNC_count == 1, Speak_Dur, 0),
         TNT_Dur = ifelse(TNT_count == 1, Speak_Dur, 0))
write.table(x = df2, file = "example.txt", sep = "|", row.names = FALSE)
######################
library(tidyverse)

df1 <- tibble::tribble(
   ~`03m`, ~`06m`,  ~`09m`, ~`12m`, ~`18m`,  ~`24m`, ~`36m`,
        1,   0.25,  -0.002,    0.1, -0.057,  -0.063,    NaN,
     0.25,      1,   0.384,  0.265,  0.209,   0.021,  0.209,
  -0.0021,  0.374,       1,  0.454,  0.426, -0.0132,  0.361,
      0.1,  0.265,   0.454,      1,  0.286,   0.118,  0.414,
  -0.0572,  0.209,   0.426,  0.286,      1,   0.248,  0.409,
  -0.0632, 0.0213, -0.0132,  0.118,  0.248,       1, -0.163,
      NaN,  0.209,   0.361,  0.414,  0.409,  -0.163,      1
  ) %>%
  as.data.frame() %>% 
  magrittr::set_rownames(c("03m", "06m", "09m", "12m", "18m", "24m", "36m")) %>% 
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "Correlation")

df2 <- tibble::tribble(
           ~V1,         ~V2,          ~V3,          ~V4,         ~V5,        ~V6,         ~V7,
           NaN, 0.146821255, 0.5032691552, 0.2744200984, 0.696125633, 0.68544985, 0.809989049,
     0.1468213,         NaN, 0.0046248855, 0.0255566484, 0.084404798,  0.4282244, 0.081061255,
     0.5032692, 0.004624886,          NaN, 0.0004159696, 0.002517379, 0.53866956, 0.006688649,
     0.2744201, 0.025556648, 0.0004159696,          NaN, 0.040601011, 0.21443715, 0.002414332,
     0.6961256, 0.084404798, 0.0025173792, 0.0406010109,         NaN, 0.07311703, 0.008312505,
     0.6854499, 0.428224404, 0.5386695582, 0.2144371492, 0.073117029,        NaN, 0.840898661,
      0.809989, 0.081061255, 0.0066886493, 0.0024143322, 0.008312505, 0.84089866,         NaN
  ) %>%
  as.data.frame() %>% 
  magrittr::set_colnames(c("03m", "06m", "09m", "12m", "18m", "24m", "36m")) %>% 
  magrittr::set_rownames(c("03m", "06m", "09m", "12m", "18m", "24m", "36m")) %>% 
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "Significance")

df3 <- df2 %>% 
  mutate(Asterisks = ifelse(Significance <= 0.0005, "***",
                            ifelse(Significance <= 0.005, "**",
                            ifelse(Significance <= 0.05, "*", NA))))

ggplot() + 
  geom_raster(data = df1, aes(x = samples, y = f_id, fill = Correlation)) +
  geom_text(data = df2, aes(x = samples, y = f_id, label = round(Significance, 3))) +
  geom_text(data = df3, aes(x = samples, y = f_id, label = Asterisks),
            nudge_y = 0.25, nudge_x = 0.25, size = 6) +
  scale_fill_viridis_c(na.value = "black")
######################
library(tidyverse)
library(palmerpenguins)

df1 <- penguins %>% 
  mutate(first_column = sample(1:100, nrow(penguins), replace = TRUE)) %>% 
  select(first_column, body_mass_g, flipper_length_mm, bill_length_mm)

df1 %>% 
  transmute(across(.cols = -c(first_column), .fns = ~ .x * first_column))
######################
w <- ggplot(bothyearsdf,
            aes(x = as.Date(Data.date_local),
                y = Data.first_max_value)) +
  geom_point() + 
  geom_smooth(color = 'red') +
  labs(x = "Days", y = "PM2.5 Max Value",
  title = "Wake County, NC Max PM2.5 Value for 2019 and 2020")

w
####################
library(tidyverse)

d1 <- data.frame(Y = "Y1", 
                 N=1:3, 
                 C= c(1, 3, 2),
                 D= c(3, 1, 4),
                 E= c(0, 1, 1),
                 Q= c(2, 0, 3)) 

d2 <- data.frame(Y = "Y2", 
                 N=1:3, 
                 E= c(0, 1, 1),
                 H= c(3, 1, 4),
                 S= c(2, 2, 0)) 


d3 <- data.frame(Y = "Y3", 
                 N=1:4, 
                 C= c(4, 2, 1, 3),
                 E= c(3, 1, 4, 2),
                 H= c(1, 3, 2, 1), 
                 U= c(3, 3, 1, 1))
d4 <- full_join(d1, d2)
join_all <- full_join(d3, d4)
join_all %>% 
  replace(is.na(.), 0)
########################
library(tidyverse)
#install.packages("googledrive")
library(googledrive)

my_id <- "0B5vbZG4PAmN6UEFpemtFQzI3bWs"
y <- drive_reveal(as_id(my_id))
y$drive_resource[[1]]$size

x <- drive_ls(as_id("1G0ekfu2J0sWQFHSOZj8T92oetG7eBHNV"))
x$drive_resource[[1]]$size

size <- NULL
for(i in seq_along(x$drive_resource)){
  print(x$drive_resource[[i]]$size)
  size[[i]] <- x$drive_resource[[i]]$size
}

paste(sum(as.numeric(size))/1000000, "Mb", sep = " ")

drive_deauth()

#####################
library(tidyverse)
library(lubridate)
library(ggpmisc)

df <- tibble::tribble(
     ~Date, ~Speed,
  "1/2019",  4500L,
  "2/2019",  3400L,
  "3/2019",  5300L,
  "4/2019",  2000L
  )
df$Date <- lubridate::my(df$Date)

ggplot(df, aes(x = Date, y = Speed)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE)
#################
# Load packages
library(tidyverse)
library(stringdist)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(sjmisc)
library(vroom)
#install.packages("keras", type = "source")
library(keras)
library(reticulate)
library(purrr)


dataset <- vroom("texts.csv", delim = ",")
# Data cleaning: filter by scenario, eliminate stop words 
topic_model <- dataset %>% filter(startsWith(scenario_id_fr, "2_")) # start with scenario 2 as a test
unnested <- topic_model %>% unnest_tokens(word, text) # separating by words
unnested <- unnested %>% anti_join(stop_words) # getting rid of stop words ("the", "and", etc.)
topic_model <- unnested %>% group_by(scenario_id_fr) %>%
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()  # rejoining words back into strings

text_data <- topic_model$text

# maybe should do lemmatization here? (figuring out the most basic form or lemma of each word) 

# prepare for NLP with keras 
tokenizer <- text_tokenizer(num_words = 20000) # create token for 20,000 most common words in data
tokenizer %>% fit_text_tokenizer(text_data)

# define a generator function for model training 
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    
    list(x, y)
  }
  
}

# define variables 
embedding_size <- 256  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.
input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

# create the embedding matrix
embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

# use dot product to calculate text similarity 
dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

# create and run the model 
model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

summary(model)

#reticulate::conda_create("r-reticulate")

model %>%
  keras::fit(
    skipgrams_generator(text_data, tokenizer, skip_window, num_sampled), 
    steps_per_epoch = 1, epochs = 10
  )


embedding_matrix <- get_weights(model)[[1]]

words <- data_frame(
  word = names(tokenizer$word_index), 
  id = as.integer(unlist(tokenizer$word_index))
)
words <- words %>%
  filter(id <= tokenizer$num_words) %>%
  arrange(id)

embedded_matrix <- embedding_matrix[1:239,1:128]
row.names(embedded_matrix) <- c(words$word)

embedded_matrix["call", , drop = FALSE]

#install.packages("text2vec")
library(text2vec)
find_similar_words <- function(word, embedded_matrix, n = 5) {
  similarities <- embedded_matrix[word, , drop = FALSE] %>%
    sim2(embedded_matrix, y = ., method = "cosine")
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}
find_similar_words("call", embedded_matrix)
##############
library(tidyverse)
library(corrplot)
library(psych)
M <- matrix(runif(25, min = -1, max = 1), nrow = 5)
rownames(M) <- 1:5
colnames(M) <- 1:5
df <- corr.test(M)

df$r %>%
  as.data.frame() %>% 
  rownames_to_column("id") %>%
  pivot_longer(-c(id), names_to = "samples", values_to = "Correlation") %>% 
  ggplot() + 
  geom_raster(aes(x = samples, y = id, fill = Correlation)) +
  scale_fill_distiller(palette = "RdBu",
                       breaks = c(-0.8, -0.2, 0.2, 0.8),
                       labels = c("strong -ve corr",
                                  "weak -ve corr",
                                  "weak +ve corr",
                                  "strong +ve corr"),
                       limits = c(-1, 0.99))
################
library(tidyverse)
library(lubridate)
library(janitor)

df <- tibble::tribble(
                                      ~ProdID,                 ~Date,    ~class, ~price, ~set,
                                   "PD-10011", "2021-05-01 10:12:16", "Regular",  1000L, "ZR",
                                   "PD-10712", "2021-05-02 18:12:06", "Premium",  1000L, "ZR",
                                   "PD-10213", "2021-05-02 16:02:59", "Premium",  1000L, "ZR",
                                   "PD-10814", "2021-05-03 17:12:06", "Premium",  1000L, "RS",
                                   "PD-10315", "2021-05-04 19:10:11",   "Other",  1000L, "RR",
                                   "PD-10616", "2021-05-04 13:18:14", "Expired",  1000L, "ZR",
                                   "PD-10617", "2021-05-04 15:14:19", "Regular",  1000L, "ZR"
                                   )
df %>%
  mutate(Date = ymd_hms(Date)) %>% 
  filter(Date %within% interval("2021-05-01", "2021-05-31")) %>%
  mutate(class = case_when(class == "Premium" & set == "RS" ~ "RS (Premium)",
                           class != "Premium" & class != "Regular" ~ "miscellaneous",
                           class == "Premium" ~ "Premium",
                           class == "Regular" ~ "Regular")) %>% 
  group_by(class, price) %>% 
  summarise(count_of_prod_ID = n()) %>% 
  ungroup() %>% 
  mutate(`Sum of Price` = count_of_prod_ID * price,
         `Count (%)` = round((count_of_prod_ID / 7)*100, 2)) %>%
  janitor::adorn_totals() %>% 
  select(-c(price)) %>% 
  arrange(desc(count_of_prod_ID))
###################
library(tidyverse)

wrap_letters <- function(x) {
  sapply(strsplit(x, ''), paste0, collapse = '\n')
}
wrap_letters("test")

df <- ToothGrowth %>% 
  mutate(dose = factor(dose),
         supp_label = case_when(
           supp == "OJ" ~ "Orange Juice", 
           T ~ "Ascorbic Acid"))
bp <- ggplot(df, aes(x=dose, y=len, group=dose)) + 
  geom_boxplot(aes(fill=dose)) +
  facet_grid(supp_label ~ dose, labeller = labeller(supp_label = wrap_letters)) +
  theme(
    strip.text.y = element_text(
      angle = 0
      )
  )
bp
##################
library(tidyverse)
library(palmerpenguins)

penguins %>% 
  na.omit() %>% 
  ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot() +
  facet_wrap(~year + island) +
  scale_y_continuous(breaks = scales::pretty_breaks(4))
####################
library(lubridate)
df <- data.frame(seq(ymd_h("2017-01-01-00"), ymd_h("2020-01-31-24"), by = "hours"))
df$close <- rnorm(nrow(df), 3000, 150)
df$up <- ifelse(sign(rnorm(27025))==-1,0,1)

colnames(df) <- c("date", "close", "up")
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
df$hour <- hour(df$date)
df$day <- day(df$date)
df$month <- month(df$date)
df$year <- year(df$date)

library(tidyverse)
df %>% 
  mutate(test = ifelse(up == 1 & is.na(lag(up, 1)), 1, 
                       ifelse(up == 1 & lag(up, 1) == 0, 1, 
                              ifelse(up == 1 & lag(up, 1) == 1 & lag(up, 2) == 0, 2,
                                     ifelse(up == 1 & lag(up, 1) == 1 & lag(up, 2) == 1, 3, 0)))))
###############
library(factoextra)
library(tidyverse)

data(iris)
res.pca <- prcomp(iris[, -5],  scale = TRUE)

DF <- as.data.frame(res.pca$x)

new_DF <- tibble(`Percentage of Variance Explained` = c(sd(DF$PC1)^2 / 4,
                                              sd(DF$PC2)^2 / 4,
                                              sd(DF$PC3)^2 / 4,
                                              sd(DF$PC4)^2 / 4),
                 Dimensions = 1:4)
ggplot(new_DF, aes(x = Dimensions, y = `Percentage of Variance Explained`)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format())

#################
begin = Sys.time()
x = rnorm(100000000, mean = 0, sd = 1)
end = Sys.time()
end - begin
#################
library(tidyverse)

ggplot(mtcars, aes(factor(cyl), disp)) +
  geom_col() +
  facet_grid( ~ factor(gear), switch = "x") +
  theme(
    strip.placement = "outside",
    strip.background.x = element_rect(fill = "white", color = "red", linetype = "FFFFFFFD")
)
###############
library(tidyverse)
library(lubridate)
library(vroom)

B <- vroom("test.txt")
B$Month <- gsub(pattern = "\\.", replacement = " ", x = B$Month) %>% 
  dmy(., quiet = FALSE, tz = NULL, truncated = 0)

site_code <- c(
  Berke = "(a)",
  Hudson = "(b)",
  Shirud = "(c)"
)

ggplot(B, aes(x = date, y = density, group = date)) +
  geom_boxplot(aes(fill = site)) +
  facet_wrap(~ site, labeller = labeller(site = site_code)) +
  theme_classic() +
  stat_summary(aes(shape = site), fun = median, geom = "point", size = 2.5) +
  labs(x = "\nMonth", y = bquote('Density'~(mg))) +
  theme(aspect.ratio=0.75,
        axis.title.x = element_text(color = 'black',
                                    face = 'bold',
                                    size = 16,
                                    hjust = 0.5),
        axis.title.y = element_text(color = 'black',
                                    face = 'bold',
                                    size=18, 
                                    hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(color = "black",
                                   face = 'bold',
                                   size=12),
        strip.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        strip.text = element_text(size = 16, hjust = 0,
                                  face = "bold"))
#################
library(tidyverse)
library(knitr)
df <- structure(list(n = 120559L, min = -143.53, q1 = c(`25%` = 0), 
                     median = 417.524, mean = 512.838195862607, q3 = c(`75%` = 896.6585), 
                     max = 23424.314), class = "data.frame", row.names = c(NA, 
                                                                           -1L))
knitr::kable(df, format = "simple")

#################
library(tidyverse)
test <- read.table(text = "PID Category    column1 column2 column3
123    1             54    2.4  24.5
324    1             52    NA   NA
356    1             NA    NA   NA
378    2             56    3.2  NA
395    2             NA    3.5  NA
362    2             NA    8.9  NA
789    3             65   12.6  23.8
759    3             66    NA   26.8
762    3             67    NA   27.2
741    3             69   8.5   23.3",
                   header = TRUE)
test %>% 
  mutate(across(everything(), ~ifelse(!is.na(.x), .x, mean(.x, na.rm = TRUE))))
##################
library(tidyverse)

df1 <- tibble::tribble(
  ~Year, ~Commodity, ~Commodity.Code, ~value1_k, ~value2_k,
  2010,        "A",             721,       100,       100,
  2010,        "B",             792,       200,       200
) %>% 
  group_by(Year, Commodity)

df2 <- tibble::tribble(
  ~Year, ~Commodity, ~Commodity.Code, ~value1_k, ~value2_k,
  2011,        "C",            7242,       111,       123,
  2011,        "D",            7421,       222,       234
) %>% 
  group_by(Year, Commodity)

str(df1)
str(df2)

mylist <- list(df1, df2)

mylist2 <- lapply(1:2, function(i){
  mylist[[i]] %>% ungroup() %>% mutate(div=.[[4]] /.[[5]])
})

mylist2

mylist2 <- lapply(1:2, function(i){
  mylist[[i]] %>% mutate(div=.[[4]] /.[[5]])
})

###################
library(tidyverse)
library(lubridate)
library(vroom)

xhamster <- vroom("xhamster.csv")
xhamster$upload_date<-as.Date(xhamster$upload_date,format="%d/%m/%Y")
xhamster$Year <- year(ymd(xhamster$upload_date))

xhamster %>% 
  filter(Year %in% 2007:2013) %>% 
  filter(grepl("Amateur", channels, fixed = TRUE)) %>%
  group_by(Year) %>% 
  summarise(Year = Year, id = n_distinct(id),
            nb_views = sum(nb_views, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(Frequency = id / sum(id),
         nb_views_proportion = nb_views / sum(nb_views)) %>% 
  ggplot(aes(x = Year)) +
  geom_col(aes(y = Frequency)) +
  geom_point(aes(y = nb_views_proportion)) +
  geom_line(aes(y = nb_views_proportion)) +
  annotate(geom = "text", x = 2008, y = 0.15, label = "nb_views") +
  scale_x_continuous(breaks = c(2007:2013),
                   labels = c(2007:2013)) +
  ylab(label = "Count") +
  xlab(label = "Amateur") +
  labs(title = "Usage of 'Amateur' as a tag from 2007 to 2013",
       caption = "Data obtained from https://sexualitics.github.io/ under a CC BY-NC-SA 3.0 license") +
  theme_minimal(base_size = 14)

#################
library(tidyverse)

test_df <- tibble::tribble(
                  ~Cars, ~Points, ~Score, ~Weigh,
            "Mazda RX4",     3.9,   2.62,  16.46,
        "Mazda RX4 Wag",     3.9,  2.875,  17.02,
           "Datsun 710",    3.85,   2.32,  18.61,
       "Hornet 4 Drive",    3.08,  3.215,  19.44,
    "Hornet Sportabout",    3.15,   3.44,  17.02,
              "Valiant",    2.76,   3.46,  20.22,
           "Duster 360",    3.21,   3.57,  15.84,
            "Merc 240D",    3.69,   3.19,     20,
             "Merc 230",    3.92,   3.15,   22.9,
             "Merc 280",    3.92,   3.44,   18.3,
            "Merc 280C",    3.92,   3.44,   18.9,
           "Merc 450SE",    3.07,   4.07,   17.4,
           "Merc 450SL",    3.07,   3.73,   17.6,
          "Merc 450SLC",    3.07,   3.78,     18,
   "Cadillac Fleetwood",    2.93,   5.25,  17.98,
  "Lincoln Continental",       3,  5.424,  17.82,
    "Chrysler Imperial",    3.23,  5.345,  17.42,
             "Fiat 128",    4.08,    2.2,  19.47,
          "Honda Civic",    4.93,  1.615,  18.52,
       "Toyota Corolla",    4.22,  1.835,   19.9,
        "Toyota Corona",     3.7,  2.465,  20.01,
     "Dodge Challenger",    2.76,   3.52,  16.87,
          "AMC Javelin",    3.15,  3.435,   17.3,
           "Camaro Z28",    3.73,   3.84,  15.41,
     "Pontiac Firebird",    3.08,  3.845,  17.05,
            "Fiat X1-9",    4.08,  1.935,   18.9,
        "Porsche 914-2",    4.43,   2.14,   16.7,
         "Lotus Europa",    3.77,  1.513,   16.9,
       "Ford Pantera L",    4.22,   3.17,   14.5,
         "Ferrari Dino",    3.62,   2.77,   15.5,
        "Maserati Bora",    3.54,   3.57,   14.6,
           "Volvo 142E",    4.11,   2.78,   18.6
  )
write.csv(test_df, "test_data.txt", quote = FALSE, row.names = FALSE)
################
library(tidyverse)
library(vroom)

# Import some example data from github
df <- vroom("https://raw.githubusercontent.com/quantixed/Maps-N-Flags/master/exampleData.csv")

# Take the dataframe
df %>% 
  # Select the column for "Country"
  select(Country = Nationality) %>%
  # Create a new variable called "cost" made up of random numbers
  mutate(cost = as.integer(rnorm(nrow(.), mean = 100, sd = 20))) %>%
  # Sort and group the data by Country
  arrange(Country) %>% 
  group_by(Country) %>% 
  # Summarise each group (each country) and show the mean
  summarise(cost, mean_cost_AUD = mean(cost, na.rm = TRUE))

###################
library(tidyverse)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

data <- MyData
data.pca <- prcomp(data[-1], scale. = TRUE)
data.class <- factor(x = c("recyclable", "recyclable", "recyclable", "recyclable",
                "recyclable", "not recyclable", "not applicable",
                "not applicable", "not applicable", "not recyclable",
                "recyclable", "recyclable", "recyclable", "recyclable",
                "not recyclable"),
                levels = c("recyclable", "not recyclable", "not applicable"))

ggbiplot(data.pca, obs.scale = 1, var.scale = 1,
         groups = data.class, ellipse = TRUE,
         circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
               legend.position = 'top')
###############
library(tidyverse)
set.seed(1)
df <- data_frame("x" = sample(x = 0:700, size = 1000000, replace = TRUE),
                 "y" = sample(x = 0:400, size = 1000000, replace = TRUE),
                 "z" = sample(x = 0:200, size = 1000000, replace = TRUE))

ifelse_func <- function(df){
  df$z <- ifelse((df$x > 300 & df$x < 600) & (df$y > 0 & df$y < 100) & (df$z > 160), 115, df$z)
}

transform_func <- function(df){
  transform(df, z = replace(z, 300 < x & x < 600 & 0 < y & y < 100 & z > 160, 115))
}

rowsums_func <- function(df){
  df$z[!rowSums(!(df >list(300, 0, 160) & df < list(600, 100, Inf)))] <- 115
}

library(data.table)
dt_func <- function(df){
  setDT(df)
  df[x > 300 & x < 600 & y > 0 & y < 100 & z > 160, z := 115]
  }

mbm <- microbenchmark::microbenchmark(ifelse_func(df), transform_func(df),
                                      rowsums_func(df), dt_func(df))
autoplot(mbm)

system.time(ifelse_func(df))
system.time(transform_func(df))
system.time(rowsums_func(df))
system.time(dt_func(df))

###############
library(tidyverse)
library(vroom)

df <- vroom("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")
mod1 = lm(area ~ temp, data = df)
confint(mod1, "temp", level=0.90)

mod1$coefficients
df %>% 
  mutate(fitted = mod1$coefficients[2] * temp + mod1$coefficients[1]) %>% 
  ggplot(aes(x = temp)) +
  geom_point(aes(y = area)) +
  geom_smooth(aes(y = area), formula = y ~ x, method = "lm", level = 0.90) +
  geom_line(aes(y = fitted)) +
  coord_cartesian(ylim = c(-0.01, 200))
################
library(tidyverse)
sdss16 <- data.frame(
  z = c(0.845435,2.035491,
        1.574227,1.770552,2.024146,2.309,0.25,2.102771,
        1.991313,2.497),
  umag = c(18.964,22.0825,
           22.5173,22.3475,18.7664,22.4403,21.6832,22.3606,
           19.6818,21.942)
)
sdss16 %>% 
  mutate(z_category = case_when(z < quantile(z, 0.25) ~ "Low",
                                  z >= quantile(z, 0.25) &
                                  z <= quantile(z, 0.75) ~ "Medium",
                                  z > quantile(z, 0.75) ~ "High"))
###############
library(tidyverse)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))

ggplot(df) +
  geom_col_pattern(aes(x = level, 
                       y = outcome, 
                       pattern_fill = level), 
                   pattern = "crosshatch",
                   fill    = "white",
                   colour  = "black") +
  theme_bw(base_size = 18) +
  theme(legend.position = 'none')
################
install.packages("topicmodels", type = "source")
install.packages("ldatuning", type = "source")
install.packages("Rmpfr")

library(topicmodels)
library(ldatuning)
data("AssociatedPress")

owl <- ldatuning::FindTopicsNumber(AssociatedPress, topics = c(1:10),
                                   metrics = c("Griffiths2004", "CaoJuan2009",
                                               "Arun2010", "Deveaud2014"),
                                   method = "Gibbs", control = list(seed = 1234),
                                   mc.cores = parallel::detectCores() - 1,
                                   verbose = T)
###############
library(tidyverse)
data <- structure(1:5, .Label = c("AVE_prQD_AFR_p10", "PER_prVD_DSR_p9", "PA_prSX_AR_p8", 
                                  "prAV_AES_p7", "prGR_AXXR_p6", "prQW_AWAR_p5"), class = "factor")
str_split(data, "_")
###############
library(tidyverse)
df <- read.table(text = "Variable Subject Value
  V1       A      1
  V1       B      0
  V1       C      1
  V2       A      0
  V2       B      1
  V2       C      0
  V3       A      1
  V3       B      1
  V3       C      1", header = TRUE)
###############
library(tidyverse)
data()
data(mtcars)
df <- mtcars
head(df)

df2 <- df %>% 
  mutate(ratio = disp / hp)
head(df2)

df$ratio2 <- df$disp / df$hp
###############
library(tidyverse)
iris2 <- iris %>%
  group_by(Species) %>%
  mutate(sepal_length_group = case_when(Sepal.Length <= quantile(Sepal.Length, 0.25) ~ "p25",
                                        Sepal.Length > quantile(Sepal.Length, 0.25) & Sepal.Length <= quantile(Sepal.Length, 0.5) ~ "p50",
                                        Sepal.Length > quantile(Sepal.Length, 0.5) & Sepal.Length <= quantile(Sepal.Length, 0.75) ~ "p75",
                                        Sepal.Length > quantile(Sepal.Length, 0.75) ~ "p100")) %>% 
  ungroup()
str(iris2)
iris3 <- as_tibble(iris) %>% 
  filter(Species == "setosa") %>% 
  mutate(sepal_length_group = case_when(Sepal.Length <= quantile(Sepal.Length, 0.25) ~ "p25",
                                      Sepal.Length > quantile(Sepal.Length, 0.25) & Sepal.Length <= quantile(Sepal.Length, 0.5) ~ "p50",
                                      Sepal.Length > quantile(Sepal.Length, 0.5) & Sepal.Length <= quantile(Sepal.Length, 0.75) ~ "p75",
                                      Sepal.Length > quantile(Sepal.Length, 0.75) ~ "p100"))
identical(iris2, iris3)

###############
df <- titanic::titanic_train
df2 <- df %>%
  select(PassengerId, Name, Sex, Age) %>% 
  na.omit() %>% 
  mutate(Age = as.integer(Age))
write.csv(df2, file = "titanic.csv")
###############
library(tidyverse)
library(janitor)

test_df <- tibble("test_µ" = c(1,2,3), 
                  "test_%23ñ" = c("fesfñUaâ", "fesââçç", "ËÔ†"), 
                  "test_normal" = c("wordsñ", "£Ÿüõ", "ÁÑ¿F"))

test_df %>% 
  clean_names() %>% 
  mutate(across(everything(), ~ make_clean_names(.)))
###############
library(tidyverse)
library(lubridate)

set.seed(101)

data <- tibble(date1 = sample(
  seq(ymd('2021-03-20'), ymd('2021-05-20'), by = 'day'), 
  10000, replace = TRUE),
  date2 = sample(seq(ymd('2021-03-20'), ymd('2021-05-20'), by = 'day'), 
                 10000, replace = TRUE),
  date3 = sample(seq(ymd('2021-03-20'), ymd('2021-05-20'), by = 'day'), 
                 10000, replace = TRUE),
  date4 = sample(seq(ymd('2021-03-20'), ymd('2021-05-20'), by = 'day'), 
                 10000, replace = TRUE),
  date5 = sample(seq(ymd('2021-03-20'), ymd('2021-05-20'), by = 'day'), 
                 10000, replace = TRUE))

rowwise_func <- function(data){
  data %>%
    rowwise() %>%
    mutate(earliest_date = min(c(date1, date2, date3, date4, date5),
                               na.rm = TRUE)) %>% 
    ungroup()
}

pmap_func <- function(data){
  data %>% 
    mutate(try_again = pmap(list(date1, date2, date3, date4, date5), 
                          min, na.rm = TRUE))
  }

det_func1 <- function(data){
  data %>%
  mutate(min_date = pmap_dbl(select(., matches("^date")), min) %>% as.Date(origin = "1970-01-01"))
}

det_faster <- function(data){
  data[["min_date"]] <- data %>% 
    mutate(across(where(is.Date), as.integer)) %>% 
    as.matrix() %>% 
    apply(1, function(x) x[which.min(x)]) %>%
    as.Date(origin = "1970-01-01")
}

transform_func <- function(data){
  as_tibble(transform(data, earliest_date = pmin(date1, date2, date3, date4, date5, na.rm = TRUE)))
}

dt_func <- function(data){
  setDT(data)
  data[, earliest_date := pmin(date1, date2, date3, date4, date5, na.rm = TRUE)]
}

times <- microbenchmark::microbenchmark(rowwise_func(data), pmap_func(data), det_func1(data), det_faster(data), transform_func(data), dt_func(data))
autoplot(times)

data2 <- transform_func(data)
data3 <- rowwise_func(data)
identical(data2, data3)

boomer::boom(transform(data, earliest_date = pmin(date1, date2, date3, date4, date5, na.rm = TRUE)))
boomer::boom(ddata %>%
               rowwise() %>%
               mutate(earliest_date = min(c(date1, date2, date3, date4, date5),
                                          na.rm = TRUE)) %>% 
               ungroup())
##################
library(tidyverse)
data(us_rent_income)
df <- us_rent_income
df2 <- df %>%
  bind_rows(., df, df, df) %>% 
  arrange(NAME) %>% 
  filter(variable == "rent") %>% 
  mutate(duplicates = ifelse(
    lag(NAME, n = 1, default = "FALSE") != NAME, "FALSE", "TRUE")
    ) %>% 
  select(NAME, variable, estimate, duplicates)

df3 <- df2 %>% 
  group_by(NAME) %>% 
  mutate(duplicate_ordinality = cumsum(duplicates == "TRUE")) %>% 
  ungroup()

##############
library(palmerpenguins)
example_df <- penguins
penguins_clean <- na.omit(penguins)
str(penguins_clean)
boxplot(penguins$body_mass_g ~ penguins$species + penguins$sex)

#############
library(ggplot2)
library(forcats)

genes <- factor(x = c(
    'ABC',
    'CDE',
    'EFG',
    'HIJ',
    'JKL',
    'LMN',
    'ABC',
    'CDE',
    'EFG',
    'HIJ',
    'JKL',
    'LMN',
    'ABC',
    'CDE',
    'EFG',
    'HIJ',
    'JKL',
    'LMN',
    'ABC',
    'CDE',
    'EFG',
    'HIJ',
    'JKL',
    'LMN',
    'ABC',
    'CDE',
    'EFG',
    'HIJ',
    'JKL',
    'LMN',
    'ABC',
    'CDE',
    'EFG',
    'HIJ',
    'JKL',
    'LMN'
  ), levels = c(
    'LMN',
    'JKL',
    'HIJ',
    'EFG',
    'CDE',
    'ABC'
  ),
  ordered = TRUE
)
concentration <-
  fct_inorder(
    f = c(
      'PR1.2',
      'PR1.2',
      'PR1.2',
      'PR1.2',
      'PR1.2',
      'PR1.2',
      'PR2.4',
      'PR2.4',
      'PR2.4',
      'PR2.4',
      'PR2.4',
      'PR2.4',
      'PR3.6',
      'PR3.6',
      'PR3.6',
      'PR3.6',
      'PR3.6',
      'PR3.6',
      'PR1.2T',
      'PR1.2T',
      'PR1.2T',
      'PR1.2T',
      'PR1.2T',
      'PR1.2T',
      'PR2.4T',
      'PR2.4T',
      'PR2.4T',
      'PR2.4T',
      'PR2.4T',
      'PR2.4T',
      'PR3.6T',
      'PR3.6T',
      'PR3.6T',
      'PR3.6T',
      'PR3.6T',
      'PR3.6T'
    ),
    ordered = TRUE
  )
foldchange <-
  c(
    577.19,
    2.642,
    -697.90,
    50.23,
    12.582,
    -30.542,
    -15.376,
    30.674,
    -1.973,
    -5.324,
    -132.761,
    146.678,
    500.19,
    2.233,-656.90,
    49.23,
    13.582,
    -80.542,
    577.19,
    2.642,
    -697.90,
    50.23,
    12.582,
    -30.542,
    577.19,
    2.642,
    -697.90,
    50.23,
    12.582,
    -30.542,
    577.19,
    2.642,
    -697.90,
    50.23,
    12.582,
    -30.542
  )
pval <- c(4, 2, 2, 2, 3, 3, 2, 3, 1, 1, 4, 4, 4, 2, 2, 2, 3, 3)
data <- data.frame(genes, concentration, foldchange, pval)


# now make dot plot using ggplot2
p <- ggplot(data, aes(x = concentration, y = genes, size = pval)) +
  geom_point(alpha = 0.9) +
  geom_point(aes(colour = cut(foldchange,
                              breaks = c(
    500,
    250,
    100,
    50,
    25,
    10,
    5,
    2,
    -2,
    -5,
    -10,
    -25,
    -50,
    -100,
    -250,
    -500
  )))) +
  labs(y = "Gene") +
  scale_color_manual(
    name = "fold change",
    values = c(
      "(500, Inf]" = "firebrick4",
      "(250,500]" = "firebrick",
      "(100,250]" = "red3",
      "(500,100]" = "red2",
      "(25,50]" = "red",
      "(10,25]" = "firebrick2",
      "(5,10]" = "firebrick1",
      "(2,5]" = "rosybrown1",
      "(-2,2]" = "gray98",
      "(-5,-2]" = "lightskyblue",
      "(-10,-5]" = "deepskyblue",
      "(-25,-10]" = "dodgerblue2",
      "(-50,-25]" = "dodgerblue4",
      "(-100,-50]" = "blue",
      "(-250,-100]" = "blue3",
      "(-500,-250]" = "darkblue",
      "(-Inf,-500]" = "navy"),
    labels = c("500", "250", "100", "50", "25", "10", "5", "2", "-2", "-5", "-10", "-25", "-50", "-100", "-250", "-500"),
    breaks = c(500, 250, 100, 50, 25, 10, 5, 2, -2, -5, -10, -25, -50, -100, -250, -500))
p + scale_size(breaks = c(1,2,3,4), labels=c("pval >0.05", "0.01<pval<0.05", "0.001<pval<0.01", "pval<0.001"))
####################
library(data.table)
library(tidyverse)
library(zoo)

tiddlywinks <- tibble("Jimmy" = sample(0:1, 1000, replace = TRUE),
                      "John" = ifelse(Jimmy == 1, 0, 1),
                      "DateTime" = seq.Date(from = as.Date("2020-01-01") - 1000,
                                            to = as.Date("2020-01-01"),
                                            length.out = 1000),
                      "Date" = DateTime + 1)
setDT(tiddlywinks)
tiddlywinks[, Jimmy_sum_past_30_days := rollsumr(Jimmy, 30, fill = NA)]
tiddlywinks[, John_sum_past_30_days := rollsumr(John, 30, fill = NA)]
tiddlywinks[, Jimmy_average_past_30_days := rollmeanr(Jimmy, 30, fill = NA)*100]
tiddlywinks[, John_average_past_30_days := rollmeanr(John, 30, fill = NA)*100]

tiddlywinks2 <- sample_n(tiddlywinks, 500) %>% 
  arrange(DateTime)
tiddlywinks2[, Jimmy_sum_past_30_days := rollsumr(Jimmy, 30, fill = NA)]
tiddlywinks2[, John_sum_past_30_days := rollsumr(John, 30, fill = NA)]
tiddlywinks2[, Jimmy_average_past_30_days := rollmeanr(Jimmy, 30, fill = NA)*100]
tiddlywinks2[, John_average_past_30_days := rollmeanr(John, 30, fill = NA)*100]
tiddlywinks2[1:31]
##################
library(tidyverse)
library(readxl)

sal <- read_excel("salinity.xlsx", sheet="Sheet1")
sal2 <- sal %>%
  mutate(year = factor(year),
         date = as.Date(day, origin = "1995-01-01") -1) %>%
  mutate(Data = rowMeans(select(., `Larv-PLarv`, Adults, Juvenile))) %>%
  pivot_longer(-c("year","day","date")) %>% 
  filter(year %in% 1995:2005)

ggplot(sal2, aes(x = date, y = value, colour = year)) + 
  geom_line() +
  facet_wrap(~factor(name, levels = c("Larv-PLarv", "Juvenile", "Adults", "Data")), ncol = 1) +
  ylab("Salinity") +
  scale_x_date(breaks = seq(as.Date("1995-01-15"),
                            as.Date("1995-12-15"),
                            by = "1 month"),
               date_labels = "%b") +
  theme_bw()
##################
library(tidyverse)
df <- read.table(text = "               t1             t2      Rg Rg_SE
1              WT            Fat  0.6818 0.0962
2              WT          FatPC  0.1853 0.1354
3              WT            DTD -0.1684 0.1232
4             Fat        DefCode      NA 0.1608", header = TRUE)

df2 <- df %>% 
  mutate(Rg = ifelse((Rg_SE*2) > abs(Rg), NA, Rg))

df3 <- df %>% 
  mutate(Rg = if_else((Rg_SE * 2) > abs(Rg), NA_real_, Rg))

identical(df2, df3)
#####################
library(tidyverse)
library(palmerpenguins)

penguins %>%
  na.omit() %>%
  filter(island == "Dream") %>%
  ggplot(aes(x = species, y = 1, fill = sex)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  facet_grid(~ island, scales = "free_x", space = "free_x") +
  theme(legend.position = "none", panel.spacing.x = unit(0, "npc")) +
  guides(colour = FALSE) +
  theme_void()

penguins %>%
  na.omit() %>%
  filter(island == "Dream") %>%
  ggplot(aes(x = species, y = 1, fill = sex)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  facet_grid(~ island, scales = "free_x", space = "free_x") +
  theme_void() +
  theme(legend.position="none", panel.spacing.x = unit(0, "npc"))
  
#################
library(tidyverse)
options(timeout = 6000)
url <- "http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/KettunenJ_27005778_GCST003664/"
## query the url to get all the file names ending in '.gz'
zips <- XML::getHTMLLinks(
  url, 
  xpQuery = "//a/@href['.gz'=substring(., string-length(.) - 2)]"
)
zips
## create a new directory 'myzips' to hold the downloads
dir.create("C:/Work/GWAS/download")
## save the current directory path for later
wd <- getwd()
## change working directory for the download
setwd("C:/Work/GWAS/download")
## create all the new files
file.create(zips)
## download them all
lapply(paste0(url, zips), function(x) download.file(x, basename(x)))
## reset working directory to original
setwd(wd)
#####################
library(tidyverse)

df1 <- mtcars %>%
  as_tibble() %>% 
  add_count(disp, name = "duplicates?")

df2 <- mtcars %>% 
  group_by(carb) %>% 
  add_count(disp, name = "duplicates?") %>% 
  ungroup()

identical(df1, df2)
###################
library(tidyverse)
library(palmerpenguins)

penguins$sex <- factor(penguins$sex)
ggplot(penguins, aes(x = bill_length_mm)) +
  geom_histogram(binwidth = 1, color="white", fill="steelblue", position = "dodge")

ggplot(penguins, aes(x = bill_length_mm, fill = sex)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  scale_fill_manual(values = c("skyblue", "dodgerblue"))
##################
library(tidyverse)
dat1 <- read.table(text = "var1 txn_date 
5 2020-10-25
1 2020-10-25
3 2020-10-26
4 2020-10-27
1 2020-10-27 
3 2020-10-31  
3 2020-11-01 
8 2020-11-02 ", header = TRUE)

dat1$txn_date <- as.Date(dat1$txn_date)
dat1 %>% 
  mutate(days = txn_date - txn_date[1] + 1)
##################
library(tidyverse)
dat1 <- read.table(text = "Trial_Type CT_tib_all CT_lum_all CT_tho_all CT_gps_all CT_vest_all
1 Pre             0.244      0.209      0.309      0.315       0.310
2 Post            0.254      0.211      0.302      0.313       0.316",
                   header = TRUE) %>% 
  as_tibble()

dat1 %>%
  pivot_longer(cols = -c(Trial_Type)) %>%
  pivot_wider(names_from = Trial_Type, values_from = value)
#################
x = seq(from = -1.2, to = 1.2, by = 0.1)
y <- x^4 + x^3 - x^2 - x + 1
expression_to_plot <- "x^4 + x^3 - x^2 - x + 1"
plot(x, y, type = "l")               
mtext(expression_to_plot, 3)
#################
library(tidyverse)
library(ggbeeswarm)

data(diamonds)
dat1 <- diamonds
dat1 %>% 
  ggplot(aes(x = cut, y = carat^2)) +
  geom_violin()

dat1 %>% 
  ggplot(aes(x = cut, y = carat^2)) +
  geom_quasirandom(groupOnX = TRUE)

dat1 %>% 
  ggplot(aes(x = cut, y = carat^2) ) +
  geom_hex(bins = 10)
#################
library(tidyverse)

df <- data.frame(x = c(1,2,3,4,1,2,3,4,1,2,3,4),
                 y = c(1,1,1,2,2,2,3,3,3,4,4,4))

df_arrow <- data.frame(x = c(0, 0),
                       y = c(0, 0),
                       xend = c(0, 8),
                       yend = c(8, 0)) 

top_line <- data.frame(x = c(0,1,2,3,4,5,6,7),
                       y = c(1,2,3,4,5,6,7,8))

bottom_line <- data.frame(x = c(1,2,3,4,5,6,7,8),
                          y = c(0,1,2,3,4,5,6,7))

df %>% 
  dplyr::mutate(z = ifelse(x > y + 1, "a",
                           ifelse(x < y - 1, "b", "c"))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(shape = z, color = z), size = 5) +
  geom_line(data = top_line, aes(x = x, y = y)) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  geom_line(data = bottom_line, aes(x = x, y = y)) +
  scale_x_continuous(breaks = 1:7, expand = expansion(add = c(0, 1)))+
  scale_y_continuous(breaks = 1:7, expand = expansion(add = c(0, 1)))+
  coord_fixed(xlim = c(0, 7), ylim = c(0, 7), clip = "off")+
  geom_segment(data = df_arrow, aes(x = x, xend = xend, y = y, yend = yend), size = 0.75, colour = "black",
               arrow = arrow(angle = 20, length = unit(3, "mm"), ends = "last", type = "closed"), linejoin = "mitre") +
  annotate("text", x = c(7.8, 0.3), y = c(0.3, 7.8), label = c("italic(x)", "italic(y)"), parse = TRUE, size = 6)+
  labs(x = NULL,
       y = NULL)+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(1, "mm"),
        text = element_text(size = 18),
        legend.position = "none") +
  scale_colour_manual(values = c('red', 'blue', 'black')) +
  scale_shape_manual(breaks = c("a", "b", "c"),
                     values = c("a" = 95, "b" = 3, "c" = 19))
##################
library(survminer)
library(survival)

fit<- survfit(Surv(time, status) ~ sex, data = lung)
ggsurvplot(fit, data = lung,
           legend.labs = c("< 50 years", "> 50 years"))
##################
# Load packages
library(tidyverse)
#install.packages("splitTools")
library(splitTools)

# set seed
set.seed(123)

# create data
people <- data.frame(Name = paste("Name", 1:51),
                     Var1 = sample(c("A", "B"), 51, prob = c(0.3, 0.7), replace = TRUE),
                     Var2 = sample(1:2, 51, replace = TRUE))
table(people$Var1, people$Var2)

# proportion of "people" in each split
prop <- 1/9
inds <- partition(people$Var1, p = c(a = prop, b = prop, c = prop,
                                     d = prop, e = prop, f = prop,
                                     g = prop, h = prop, i = prop))
# split the patients (load dfs into a list)
dfs <- list()
for (i in 1:9){
  dfs[[i]] <- people[inds[[i]],]
}
# name the dfs
names(dfs) <- c("df_01", "df_02", "df_03", "df_04", "df_05",
                "df_06", "df_07", "df_08", "df_09")

# check requirements (at least 1 "A" in Var1)
for (i in seq_along(dfs)){
  if(!nrow(filter(dfs[[i]], Var1 == "A")) >= 1){
    print("error")
  }
}

# If no error, load dataframes into global environment
list2env(dfs, envir=.GlobalEnv)
####################
library(data.table)
#install.packages("mltools")
library(mltools)
set.seed(123)

foo_data <- data.table(
  x = rep("A", 6),
  y = sample(1:3, 6, replace = T),
  var1 = sample(c("One", "Two"),  6, replace = T),
  var2 = sample(c(3, 4),  6, replace = T),
  stringsAsFactors = TRUE
)

one_hot(foo_data)
##############
library(tidyverse)
from <- c("NYC","PAR", "SYD", "MAD")
to <- c("PAR", "SYD", "MEL", "BCN")
date <- c("05/07","05/07", "05/07", "06/08")
step <- c(1, 2, 3, 1)
df <- data.frame(from, to, date, step)
df %>% 
  mutate(trip = case_when(step == 1 & lead(step, 2L) == 3 ~ 
                            paste(from, to, lead(to, 1L),
                                  lead(to, 2L), sep = "-"),
                          step == 2 & lag(step, 1L) == 1 & lead(step, 1L) == 3 ~ 
                            paste(lag(from, 1L), lag(to, 1L), to, lead(to, 1L), sep = "-"),
                          step == 2 & lead(step, 1L) != 3 ~
                            paste(lag(from, 1L), from, to, sep = "-"),
                          step == 3 & lag(step, 1L) == 2 & lag(step, 2L) == 1 ~ 
                            paste(lag(from, 2L), lag(to, 2L), lag(to, 1L), to, sep = "-"),
                          step == 1 & lead(step, 1L, default = 1) != 2 ~
                            paste(from, to, sep = "-")))


relation <- function(dat){
  .relation <- function(x){
    k = unique(sort(c(dat[dat[, 1] %in% x, 2], x, dat[dat[, 2] %in% x, 1])))
    if(setequal(x,k)) toString(k) else .relation(k)}
  sapply(dat[,1],.relation)
}
df$trip <- relation(df)
df
###################
library(tidyverse)
df <- data.frame(responses=c(rep("A",5),
                             rep("B",15),
                             rep("C", 25)))
ggplot(df, aes(responses))+
  geom_bar(aes(y = ..prop.. * 100, group = 1)) +
  geom_text(aes(y = ..prop.. * 100, label = scales::percent(..prop..), group = 1),
            stat = "count", vjust = 0, nudge_y = 1)
###################
plot.new(); text(0.5,0.5, bquote("1,513"), cex=5)
################
library(tidyverse)
v <- ggplot(faithfuld, aes(waiting, eruptions, z = density)) +
  geom_contour()
################
library(tidyverse)
data <- tibble::tribble(
   ~Condition,             ~ENSG, ~average.raw.counts,
  "Untreated", "ENSG00000260456",        1.190091e-05,
    "Treated", "ENSG00000183570",        1.195156e-05,
  "Untreated", "ENSG00000260451",        1.290091e-05,
  "Treated", "ENSG00000183572",        1.295156e-05,
  "Untreated", "ENSG00000260454",        1.390091e-05,
  "Treated", "ENSG00000183575",        1.395156e-05,
  "Untreated", "ENSG00000260457",        1.110091e-05,
  "Treated", "ENSG00000183578",        1.115156e-05
  )
data %>% 
  mutate(bin = cut_width(average.raw.counts, width = 0.000002, boundary = 0)) %>%
  ggplot(aes(x = bin, y = average.raw.counts)) +
  geom_boxplot(aes(fill = Condition)) +
  scale_fill_manual(values = c("#f60013", "#21fffe")) +
  theme_minimal()
##############
# Load libraries
library(tidyverse)

# Create fake data
dx <- list()
for (i in 1:25){
  dx[[i]] <- c(paste("I",
                     round(rnorm(n = 50, mean = 21, sd = 5), 1),
                     sep = ""))
}

name_list <- paste("dx", 1:25, sep = "")
Data1 <- as.data.frame(dx, col.names = name_list)

# Create a variable called "AMI" to count the occurrences of values:
# "I21.0","I21.1","I21.2","I21.3","I21.4","I21.9"

Data1 %>% 
 mutate(AMI = ifelse(rowSums(
   sapply(select(., starts_with("dx")),
          function(x) grepl(pattern = paste(c("I21.0","I21.1","I21.2","I21.3","I21.4","I21.9"),
                                            collapse = "|"), x)
   )) > 0, 1, 0)
 )
################
library(tidyverse)
dat1 <- read.table(text = "Team  Date          Stat
ATL   2014-10-29    2.77833333
ATL   2014-11-01    2.22500000
ATL   2014-11-05    2.68166667
ATL   2014-11-07    -1.56833333
ATL   2014-11-08    2.09333333
BOS   2014-10-29    4.70000000
BOS   2014-11-01    4.12166667
BOS   2014-11-03    1.46833333
BOS   2014-11-05    -0.15500000
BOS   2014-11-07    0.40500000",
                   header = TRUE)
dat1 %>% 
  group_by(Team) %>% 
  summarise(mean = mean(Stat))

###############
library(tidyverse)
library(olsrr)
library(reshape2)
library(ggpmisc)
data(surgical)

## Regression 
pre1 <-setdiff(names(surgical), c("y", "pindex"))
mod_dres<-NULL
for (j in pre1) {
  model  <- lm(y  ~  pindex + get(j), data = surgical)
  bmodel <- broom::tidy(model)
  bmodel$term[3]<-j
  bmodel<-bmodel[3,]
  mod_dres<-rbind(mod_dres,bmodel)
}
mod_dres

## Matching the significant variables with the orginal data and reshaping  
pre1.plot = melt(surgical[,-c(2)], id.vars='y') %>% 
  dplyr::filter(variable %in% mod_dres$term) 

## plot the predictors varaibles 
ggplot(pre1.plot) +
  geom_jitter(aes(value, y, colour=variable),
              colour="darkorange", size = 3) + 
  geom_smooth(aes(value, y, colour=variable),
              formula = y ~ x, method="lm", se=FALSE,
              colour="darkorange") +
  stat_poly_eq(formula = y ~ x, 
               aes(x = value, y = y, label = paste(..rr.label..,
                                                   ..p.value.label..,
                                                   sep = "~~~~")), 
               parse = TRUE) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16,face="bold", colour = "black"),
        axis.text = element_text(size = 16,face="bold", colour = "black"),
        axis.line = element_line(colour='black'),
        axis.ticks = element_line(colour='black'),
        plot.title = element_text(hjust = 0.5,size=18,face="bold"),
        legend.position = "bottom",
        legend.title = element_text(color = "Black", size = 9, face = "bold"),
        legend.text=element_text(color = "Black", size = 9, face = "bold"),
        strip.text.x = element_text(size = 16,face="bold", colour = "black")) +
  labs(x = "value",title = " ") +
  facet_wrap(~variable, scales="free_x",nrow = 2, ncol = 4) + 
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_blank(),
        legend.title = element_blank())
####################
library(tidyverse)
dat1 <- data.frame(
  stringsAsFactors = FALSE,
                          Condition = c(1L,1L,1L,1L,1L,1L,1L,1L,1L,
                                        1L,2L,2L,2L,2L,2L,2L,2L,2L,2L,
                                        2L),
                         TargetWord = c("Target1","Target1","Target1",
                                        "Target1","Target1","Target2","Target2",
                                        "Target2","Target2","Target2",
                                        "Target3","Target3","Target3","Target3",
                                        "Target3","Target4","Target4","Target4",
                                        "Target4","Target4"),
                       WordProduced = c("table","word","chair","pole",
                                        "skate","car","house","shoes","girl",
                                        "life","computer","ball","court",
                                        "plane","sky","tree","five","help",
                                        "shave","love"),
                        WPcondition = c("A","B","A","C","D","B","A",
                                        "A","A","C","D","B","F","C","D",
                                        "A","C","D","A","B")
                 )
dat2 <- dat1 %>% 
  group_by(Condition, TargetWord) %>% 
  summarise(MeanWPcondition = mean(WPcondition == "A"))
dat2

###################
library(tidyverse)

dat1 <- tibble(Date = seq.Date(from = as.Date("2015-03-20"), 
                               to = as.Date("2015-03-30"),
                               length.out = 30),
               direction = rep(c("N", "null", "U"), 10),
               Detections = sample(x = c(1:5),
                                   size = 30,
                                   replace = TRUE))
dat2 <- dat1 %>% 
  mutate(Detections = replace(Detections,direction == "null", 0))

ggplot() +
  geom_line(data = dat2 %>% filter(direction != "null"),
            aes(x = Date, y = Detections,
                col = direction), size = 1) +
  geom_line(data = dat2 %>% filter(direction == "null"),
            aes(x = Date, y = Detections),
            show.legend = FALSE, col = "#009E73", size = 1) +
  theme_bw(base_size = 16, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("RM 2015")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Direction",
                     values = c("#D55E00","#0072B2"),
                     labels = c("Downstream","Upstream")) +
  scale_y_continuous(breaks = seq(0,10,2),
                     expand = c(0.01,0),
                     limits = c(0,10))
##################
library(tidyverse)

set.seed(1)

dat1 <- tibble(Date = seq.Date(from = as.Date("2015-03-20"), 
                               to = as.Date("2015-03-31"),
                               length.out = 30),
               direction = rep(c("N", "null", "U"), 10),
               Detections = sample(x = c(1:5),
                                   size = 30,
                                   replace = TRUE))
dat2 <- dat1 %>% 
  mutate(Detections = Detections - 1) %>% 
  mutate(Detections = replace(Detections, direction == "null", 0))

ggplot() +
  geom_line(data = dat2 %>% filter(direction != "null"),
            aes(x = Date, y = Detections,
                col = direction), size = 1) +
  geom_line(data = dat2 %>% filter(direction == "null"),
            aes(x = Date, y = Detections),
            show.legend = FALSE, col = "#009E73", size = 1) +
  theme_bw(base_size = 16, base_family = "serif") +
  ggtitle("RM 2015") +
  scale_color_manual(name = "Direction",
                     values = c("#D55E00","#0072B2"),
                     labels = c("Downstream","Upstream")) +
  scale_y_continuous(breaks = seq(0, 10, 2),
                     expand = c(0.01, 0),
                     limits = c(0, 10)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%m-%Y",
               limits = c(as.Date("2015-03-20"), as.Date("2015-03-31"))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.95),
        axis.title.x = element_blank())
################
library(tidyverse)
library(scales)

bounds <- c(0, -1000)

test_data <- tibble(Dates = seq.Date(as.Date("2021-05-16"),
                   as.Date("2021-06-08"),
                   by = "1 day"),
                   prof_loss = as.integer(runif(n = 24, min = -500, max = 500)))

test_data$pos_neg <- ifelse(test_data$prof_loss > 0, "Positive", "Negative")

ggplot(data = test_data, aes(x = Dates, y = prof_loss, group=1)) +
  geom_point() +
  geom_area(aes(fill = pos_neg)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5)) + 
  geom_line(size=.75) + 
  geom_hline(yintercept=0, size=.5, color = "Blue") + 
  labs(x = "Date", y = "Unrealised Profit or Loss",
       title = "Unreaslied Profit or Loss as of 7/6/2021")
###############
# Load libraries
library(tidyverse)
library(readxl)
library(httr)

# Find some data
url1 <- "https://www.bocsar.nsw.gov.au/Documents/lga/NewSouthWales.xlsx"

# Get the data and remove missing data points (NA's)
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(path = tf, 2L, skip = 5) %>% 
  na.omit()

df2 <- df %>% 
  # format the data to "long format" for plotting
  pivot_longer(cols = -c(`Premises type`)) %>%
  # Change "Premises type" and "name" to factors
  mutate(`Premises type` = factor(
    `Premises type`, levels = unique(`Premises type`))
    ) %>%
  mutate(name = factor(
    name, levels = unique(name))
    ) %>%
  # Remove the "Total" counts
  filter(`Premises type` != "Total") 

# Define colours for text (white for dark fill, black for light fill)
hcl <- farver::decode_colour(viridisLite::inferno(length(df2$value)), "rgb", "hcl")
label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

# Plot the data (log scale for fill)
ggplot(df2, aes(y = fct_rev(`Premises type`),
                x = name, fill = log(value))) +
  geom_tile() +
  geom_text(aes(label = value, color = factor(value)),
            show.legend = FALSE, size = 2.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05),
        axis.title = element_blank()) +
  scale_color_manual(values = label_col) +
  scale_fill_viridis_c(option = "inferno", na.value = "black")

# Load the raw data and format for pheatmap (expects a matrix)
dm <- read_excel(path = tf, 2L, skip = 5) %>% 
  na.omit() %>%
  column_to_rownames(var = "Premises type")

# Plot the data
pheatmap::pheatmap(as.matrix(dm), scale = "row")

library(cluster)
library(pheatmap)
df3 <- df %>% 
  select(`Premises type`, Robbery) %>% 
  column_to_rownames(var = "Premises type")

pheatmap(daisy(as.matrix(df3)),
         labels_row = rownames(df3),
         labels_col = rownames(df3))

#################
library(tidyverse)
df <- structure(list(cntry = structure(c("Austria", "Belgium", "Italy", 
                                   "Switzerland", "Spain", "Austria", "Belgium", "Italy", "Switzerland", 
                                   "Spain", "Denmark"), format.stata = "%15s"), estimate = structure(c(1, 
                                                                                                       1.5, 2, 1.20000004768372, 1.70000004768372, 4, 5.5, 6, 3, 3.09999990463257, 
                                                                                                       4.30000019073486), format.stata = "%8.0g"), cat_f = structure(c("Widening gap", 
                                                                                                                                                                       "Widening gap", "Widening gap", "Widening gap", "Widening gap", 
                                                                                                                                                                       "No change", "No change", "No change", "No change", "No change", 
                                                                                                                                                                       "No change"), format.stata = "%13s"), term = structure(c("2009", 
                                                                                                                                                                                                                                "2009", "2009", "2009", "2009", "2008", "2008", "2008", "2008", 
                                                                                                                                                                                                                                "2008", "2008"), format.stata = "%15s")), row.names = c(NA, -11L
                                                                                                                                                                                                                                ), class = "data.frame")
df %>%
  ggplot(aes(x= estimate, y=term)) +
  geom_point() +
  facet_grid(cols = vars(cat_f), rows = vars(cntry))

############
library(tidyverse)
x <- data.frame(X=c(10:1), Y=c(6,6,3,6,3,3,9,9,9,2), L=c("A","B","C","C","A","B","C","A","B","C"))


jared_mamrot <- function(x){
  x %>%
  group_by(L) %>%
  mutate(shwartz = ifelse(L == "A", X,
                          ifelse(L == "B", 1 / X,
                                 Y))) %>% 
  arrange(L, shwartz) %>% 
  select(-shwartz) 
}
jared_mamrot(x)

GKi <- function(x){
  y <- x$X
  i <- x$L == "B"
  y[i] <- y[i] * -1
  i <- x$L == "C"
  y[i] <- x$Y[i]
  x[order(x$L, y),]
}
GKi(x)

order_partly <- function(dat, ord_pat){
  result <- dat[0,]
  for (pattern_col in names(ord_pat)){
    order_col <- ord_pat[[pattern_col]][1]
    decreasing <- if (ord_pat[[pattern_col]][2] == "dec") T else F
    partial_dat <- dat[dat$L == pattern_col,]
    ord <- order(partial_dat[order_col], decreasing = decreasing)
    result <- rbind(result, partial_dat[ord, ])
  }
  result
}

Anoushiravan_R <- function(x) {
  x %>%
    group_split(L) %>%
    map_dfr(~ if(.x$L[1] == "A") {
      .x %>% arrange(.x$X)
    } else if(.x$L[1] == "B") {
      .x %>% arrange(desc(.x$X))
    } else {
      .x %>% arrange(.x$Y)
    })
}

res <- microbenchmark::microbenchmark(jared_mamrot(x), GKi(x), zerz(x),
                                      Anoushiravan_R(x))
autoplot(res)
################
library(tidyverse)
library(naniar)

data(airquality)
dat1 <- airquality
list_of_gt25_NAs <- miss_case_summary(dat1) %>% 
  filter(pct_miss >= 25)

dat2 <- dat1 %>% 
  slice(-c(list_of_gt25_NAs$case))

dat2 <- dat1[rowMeans(is.na(dat1)) < 0.25, ]
################
library(tidyverse)

data("Puromycin")
dat1 <- Puromycin %>% 
  filter(state == "treated")
dat2 <- Puromycin %>% 
  filter(state == "untreated")

mycp <- ggplot() +
  geom_violin(data = dat1, aes(x= state, y = conc, colour = "Puromycin (Treatment1)")) + 
  geom_violin(data = dat2, aes(x= state, y = conc, colour = "Puromycin (Treatment2)")) 
mycp

mycp2 <- ggplot() +
  geom_violin(data = dat1, aes(x = state, y = conc, colour = "Puromycin (Treatment1)")) +
  geom_violin(data = dat2, aes(x = state, y = conc, colour = "Puromycin (Treatment2)")) +
  scale_x_discrete(limits = c("untreated", "treated"))
mycp2
################
library(tidyverse)
library(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
dtrain <- xgb.DMatrix(train$data, label=train$label)
test <- agaricus.test
dtest <- xgb.DMatrix(test$data, label=test$label)

trained_model <- xgb.train(data = dtrain, max_depth = 2,
                           eta = 0.1, nthread = 4, nrounds = 50,
                           watchlist = list(train = dtrain, eval = dtrain),
                           objective = "binary:logistic")
head(predict(trained_model, dtest))

# https://github.com/dmlc/xgboost/blob/master/R-package/R/xgb.Booster.R
test_df <- iris[0:105,]
num_class <- 3
set.seed(11)
dtrain <- xgb.DMatrix(data = as.matrix(test_df[, -5]), label = as.numeric(test_df$Species) - 1)
bst <- xgboost(dtrain, max_depth = 6, eta = 0.1, nthread = 2, nrounds = 1, subsample = 0.5,
               objective = "multi:softprob", num_class = num_class,
               base_score = seq(from=0.01, to=0.99, length.out=105))
# predict for softmax returns num_class probability numbers per case:
pred <- predict(bst, dtrain)

################
library(tidyverse)
library(ggpubr)
library(minerva)
library(ggpmisc)

data("Puromycin")
dat <- Puromycin
cm_data <- as.numeric(unlist(dat))
res <- cor.test(cm_data, cm_data, method = "pearson")
res

cm_df <- data.frame(cm_data1 = cm_data, cm_data2 = cm_data)
ggscatter(cm_df, "cm_data1", "cm_data2",
          add = "reg.line", title = "Correlation matrix between time points of cdc15 temperature-sensitive mutant", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "time points", ylab = "time points", legend = "right", gradient_color = c("blue", "white", "red"))


data("Puromycin")
dat <- Puromycin
res <- cor.test(dat$conc, dat$rate, method = "pearson")
res
ggscatter(data = dat, x = "conc", y = "rate",
          add = "reg.line", title = "Correlation matrix between time points of cdc15 temperature-sensitive mutant", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "time points", ylab = "time points", legend = "right", gradient_color = c("blue", "white", "red"))

plot(dat$rate ~ dat$conc)
fit <- lm(dat$rate ~ log(dat$conc))
summary(fit)
x <- dat$conc
y <- predict(fit, newdata = list(x = dat$conc), interval = "confidence")
matlines(x,y)
#################
library(tidyverse)
library(ggpubr)
library(minerva)
library(ggpmisc)

data(Spellman)
dat <- Spellman
cdc15 <- select(dat, 1, 23:46)

cdc15 %>%
  pivot_longer(cols = -c(time)) %>%
  ggplot(aes(x = time, y = value)) +
  geom_point() +
  facet_wrap(~ name, ncol = 4) +
  geom_smooth(formula = y ~ x, method = "lm") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~~")), 
               parse = TRUE)

mat <- as.matrix(cdc15)[,-1]
rownames(mat) <- cdc15$time
heatmap(t(mat), Colv = NA, xlab = "Time")

pheatmap::pheatmap(t(mat), cluster_cols = FALSE)

cor.test(x = mat[,1], y = mat[,2])

ggscatter(cdc15, x = "YAR035W", y = "YAR043C", add = "reg.line",
          title = "Correlation matrix between time points of cdc15 temperature-sensitive mutant",
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "log ratio YAR035W", ylab = "log ratio YAR043C")

ggscatter(cdc15, x = "YAR035W", y = "YAR043C", fill = "time", shape = 21, add = "reg.line",
          title = "Correlation matrix between time points of cdc15 temperature-sensitive mutant",
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "log ratio YAR035W", ylab = "log ratio YAR043C")

corrplot::corrplot(corr = mat, is.corr = FALSE, method = "color")
###################
library(tidyverse)
dat1 <- read.table(text = "HUC8 YEAR RO_MM
       bcc1_45Fall_1020004 1961 112.0
       bcc1_45Fall_1020004 1962 243.7
       bcc1_45Fall_1020004 1963 233.3
       bcc1_45Fall_1020004 1964 190.3
     bcc1_M_45Fall_1020004 1961 100.9
     bcc1_M_45Fall_1020004 1962 132.3
     bcc1_M_45Fall_1020004 1963 255.1
     bcc1_M_45Fall_1020004 1964 281.9
     bnuesm_45Fall_1020004 1961  89.0
     bnuesm_45Fall_1020004 1962  89.5
     bnuesm_45Fall_1020004 1963 126.8
     bnuesm_45Fall_1020004 1964 194.3
    canesm2_45Fall_1020004 1961 186.6
    canesm2_45Fall_1020004 1962 197.4
    canesm2_45Fall_1020004 1963 229.1
    canesm2_45Fall_1020004 1964 141.8",
           header = TRUE)

dat1 %>% 
  pivot_wider(names_from = YEAR, values_from = RO_MM)

dat1 %>%
  ggplot(aes(x = YEAR, y = RO_MM, group = HUC8, color = HUC8)) +
  geom_line() +
  scale_color_manual(values = c("black", viridis::viridis(3, alpha = 0.33)))


dat1 %>%
  mutate(group = ifelse(str_detect(string = HUC8, pattern = "bcc"),
                        "group_bcc", "group_others")) %>%
  filter(group == "group_bcc") %>% 
  ggplot(aes(x = YEAR, y = RO_MM, group = HUC8, color = HUC8)) +
  geom_line() +
  ggtitle("bcc csv files only")


#################
library(tidyverse)
#install.packages("flextable")
library(flextable)

df <- data.frame(stringsAsFactors = FALSE,
                 A = c("xyz", "xyz", "abc", "abc", "abc"),
                 B = c("C1 : 12", "C2 : 13", "C1 : 12", "C2 : 34", "C3 : 43"),
                 C = c(23L, 23L, 43L, 43L, 43L))

merge_custom <- function(ft, x, columns){
  z <- rle(x)
  rows_at <- cumsum(z$lengths) - z$lengths + 1
  
  for(i in seq_along(rows_at)){
    for(j in columns)
      ft <- merge_at(x = ft, i = seq( rows_at[i], rows_at[i] + z$lengths[i] - 1), j = j)
  }
  
  ft
}

flextable(df) %>% 
  merge_custom(x = df$A, columns = 1) %>%
  theme_box()

flextable(df) %>%
  theme_box() %>% 
  merge_v(j = ~A)
##################
library(tidyverse)
#install.packages("hunspell")
#install.packages("textclean")
library(hunspell)
library(textclean)

s <- textclean::replace_names(foo_data)
trimws(gsub(sprintf('\\b(%s)\\b', 
                    paste0(unlist(hunspell::hunspell(s)), collapse = '|')), '', s))
##############
library(tidyverse)
# random data
d <- data.frame(x = rep(c("AT130", "DEA1A", "DEA2C", "SE125", "SE232"), 4),
                y = sample(1:10, 20, replace = TRUE),
                z = sample(1:10, 20, replace = TRUE),
                w = sample(1:10, 20, replace = TRUE))
colnames(d) <- c("id", "typeA", "typeB", "typeC")

for (i in colnames(d[,2:ncol(d)])) {
  type <- ensym(i)
  p <- ggplot(d, aes(y = !!type, x = id, fill = id)) +
    geom_boxplot() +
    ggtitle(type)
  print(p)
}

################
library(tidyverse)
vec1 <- rep(1:10, each = 5)
vec2 <- c(0.98, 0.99, 1, 1.01, 1.02)

vec3 <- c()
for (i in split(vec1, ceiling(seq_along(vec1)/5))){
  result <- i * sample(vec2)
  vec3 <- c(vec3, result)
}

vec3

mean(c(vec3[1:5]))
mean(c(vec3[6:10]))
##################
library(tidyverse)
case<-c(3,3,3,57,57,57,57,9,9,9)
a<-c(12,15,20,12,14,15,17,14,16,19)
c<-c(25,25,25,21,21,21,21,24,24,24)

df <- data.frame(case,a,c)
df %>%
  group_by(case) %>% 
  mutate(b = lead(a - 1, 1L, default = last(c))) %>% 
  select(case, a, b, c)

#################
library(tidyverse)
# summarySE func from
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper%20functions
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

dat1 <- read.table(text = "rownumber Consonant     Place       C1 C1_xsampa
1  Singleton  Bilabial 149.8670        tS
2   Geminate  Bilabial 161.3066        tS
3  Singleton Retroflex 115.9713         f
4   Geminate Retroflex 143.3766         f
5  Singleton    Dental 130.1839         k
6  Singleton    Dental 118.7762         k
7   Geminate    Dental 122.1802         k
8  Singleton     Velar 112.3296         s
9   Geminate     Velar 142.4654         s
10 Singleton  Bilabial 245.7727        tS
11  Geminate  Bilabial 288.2960        tS
12  Geminate Retroflex 128.9104         f
13 Singleton    Dental 103.7978         k
14  Geminate    Dental 135.6264         k
15 Singleton    Dental 208.1685         k",
                   header = TRUE)

tgc <- summarySE(dat1, measurevar = "C1", groupvars = c("Consonant", "Place"))

ggplot(tgc, aes(x=Place, y=C1,
                colour=Consonant,
                group = Consonant)) + 
  geom_errorbar(aes(ymin=C1-se,
                    ymax=C1+se),
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2))
##################
library(tidyverse)
library(lubridate)
library(scales)
library(wesanderson)
#install.packages("wesanderson")

file_url1 <- url("https://raw.githubusercontent.com/johnsnow09/covid19-df_stack-code/main/df_vaccination.csv")

df_vaccination <- read.csv(file_url1)

df_vaccination2 <- df_vaccination %>%
  mutate(Updated.On = as.Date(Updated.On))

df_vaccination2 %>% 
  filter(Updated.On > ymd("2021-02-28"),
         Updated.On < ymd("2021-06-01")) %>% 
  mutate(month_abbr = month(Updated.On, label = TRUE, abbr = TRUE)) %>% 
  group_by(month_abbr, State) %>% 
  summarise(monthly_ind_vaccinated = sum(Total.Individuals.Vaccinated_Dailycalc, 
                                         na.rm = TRUE),
            Population = first(Population), .groups = "drop") %>% 
  group_by(State) %>% 
  mutate(prc_vaccinated_per_pop = monthly_ind_vaccinated / Population,
         state_max = max(prc_vaccinated_per_pop),
         state_min = min(prc_vaccinated_per_pop)) %>% 
  na.omit() %>%
  ungroup() %>% 
  mutate(State = fct_reorder(State, state_max, max, .desc = TRUE) 
  ) %>% 
  ggplot() +
  geom_segment(aes(x = State, xend = State, y = state_min, yend = state_max),
               col = "grey") +
  geom_point(aes(x = State, y = prc_vaccinated_per_pop,
                 col = as.factor(month_abbr))) +
  geom_point(aes(x = State, y = prc_vaccinated_per_pop,
                 col = as.factor(month_abbr), size = month_abbr), show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = wes_palette("Darjeeling1", type = "discrete")) +
  scale_size_manual(values = c(2,2,3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.001),
        strip.text = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.8, 0.85)) +
  labs(title = "Vaccination rates in each Indian state",
       caption = "2021-06-11",
       color = "Proportion of Vaccinations",
       x = "", y = "")
####################
library(devtools)
#install_github("cran/DMwR")
library(DMwR)
####################
library(tidyverse)
df1 <- read.table(text = "Gene P1 P2 P3
A1 6 8 2
A2 12 6 3
A3 8 4 8", header = TRUE)

df1 %>% 
  mutate(across(.cols = -c(Gene), .fns = ~ .x / P3))

#################
library(tidyverse)
df1 <- structure(list(Pool_1.sf = c(1.04654112319058, 0.908757359307814 ), Pool_10.sf = c(1.09143254057473, 0.763847944339038), Pool_11.sf = c(1.18750942376232, 0.970606492107592), Pool_12.sf = c(1.40039457695623, 0.987404435084985 ), Pool_13.sf = c(1.40110264045617, 1.05184583712403), Pool_14.sf = c(0.975272918230445, 1.05978729086064), Pool_15.sf = c(1.18870377730237, 1.350632186878 ), Pool_16.sf = c(1.25322756941453, 1.18822754009871), Pool_17.sf = c(1.29750766166164, 0.561735111884727), Pool_18.sf = c(1.27533917021409, 1.38423737777244 ), Pool_19.sf = c(0.918846998358604, 0.95201145069032), Pool_2.sf = c(0.748425883461423, 0.618517135274528), Pool_20.sf = c(1.35027456541736, 1.11503741384757 ), Pool_21.sf = c(1.06155444204363, 0.991570644521018), Pool_22.sf = c(1.64504600710891, 1.07862501013554), Pool_23.sf = c(1.51097405304331, 0.971834605384123 ), Pool_24.sf = c(1.18420663655483, 0.881393761143161), Pool_25.sf = c(0.925122055385438, 0.93313232911786), Pool_26.sf = c(2.12016328112954, 0.829308431444176 ), Pool_27.sf = c(1.59552456085871, 0.705278334816745), Pool_28.sf = c(1.75141617967796, 0.863808031900547), Pool_29.sf = c(1.71320920062242, 0.782291400605908 ), Pool_3.sf = c(1.09209110640701, 0.776979928448013), Pool_30.sf = c(0.925564956736256, 0.905870068022084), Pool_31.sf = c(1.00114849632652, 0.713896646497438 ), Pool_32.sf = c(0.769653226223374, 1.01812180736834), Pool_33.sf = c(1.64152662148587, 0.912970524890157), Pool_34.sf = c(1.39446534544181, 0.892464822723893 ), Pool_35.sf = c(1.36553718507047, 0.709121064448927), Pool_36.sf = c(1.48178605247809, 0.766690878721894), Pool_37.sf = c(1.05050355917415, 0.862090327153509 ), Pool_38.sf = c(1.36989138311191, 1.01473830511752), Pool_39.sf = c(1.42872045770954, 1.13176474162602), Pool_4.sf = c(1.11960747784989, 0.665514805707436 ), Pool_40.sf = c(1.90897625098439, 1.28419857359682), Pool_41.sf = c(1.2570145072185, 0.987813170293439), Pool_43.sf = c(1.14927112622372, 1.33241620047574 ), Pool_44.sf = c(1.02884805988699, 1.1077339415536), Pool_45.sf = c(1, 1), Pool_46.sf = c(1.15692580371101, 1.01663753799148), Pool_47.sf = c(1.02161799920975, 0.893420612254083), Pool_48.sf = c(0.991350522776138, 0.857531005677309 ), Pool_49.sf = c(0.666054364361721, 0.95128169066564), Pool_5.sf = c(1.27677591889858, 0.65869398169343), Pool_50.sf = c(1.04592846997826, 0.820965050229932 ), Pool_51.sf = c(1.46227623256989, 1.16138433421938), Pool_52.sf = c(1.1826746106421, 1.33429257056276), Pool_53.sf = c(1.16041540250292, 0.878127525893012 ), Pool_54.sf = c(1.14285567434696, 0.870429885808645), Pool_55.sf = c(1.40863161629042, 0.485422488325543), Pool_56.sf = c(1.81157566249543, 0.519084970767436 ), Pool_58.sf = c(1.69798017279487, 1.34651488521988), Pool_59.sf = c(1.55336058362464, 0.982570924872293), Pool_6.sf = c(0.769881996423631, 0.388931536871056 ), Pool_60.sf = c(1.36877679300045, 1.02579768967408), Pool_61.sf = c(1.34960258409398, 0.983191813100761), Pool_62.sf = c(1.08159654058587, 0.76318904250517 ), Pool_63.sf = c(1.98209270942409, 1.05152970776951), Pool_64.sf = c(1.86946484050877, 1.06489241167699), Pool_65.sf = c(1.48159508541161, 0.89626404845365 ), Pool_66.sf = c(1.42400489307256, 1.30732410445944), Pool_7.sf = c(1.17869553929846, 0.620813490764102), Pool_8.sf = c(1.35783021860687, 0.77620120083204 ), Pool_9.sf = c(1.32884787662603, 0.758057408306258)), row.names = c("Glyma.01G000400", "Glyma.01G000900"), class = "data.frame")

df1 %>%
  rownames_to_column() %>%
  pivot_longer(cols = -c(rowname)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  ggplot(aes(x = name, y = value, group = rowname, color = rowname)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05))
##################
library(tidyverse)

tribble(
  ~description, ~y, ~x,
  "apples", 3.4, 1.1,
  "oranges", 5.6, 2.4,
  "mangos", 2.3, 4.8
) %>%
  {ggplot(data = ., aes(y = y, x = x)) +
      scale_x_continuous(
        breaks = .$x,
        labels = .$description
      ) +
      geom_point() +
      geom_line()
    }
###############
# Load libraries
library(tidyverse)
library(zoo)

# Create data
df <- data.frame(week = rep(seq(from = 1, to = 52, by = 1), times = 4),
                 col1 = seq(from = 5, to = 5 * 52 * 4, by = 5),
                 col2 = seq(from = 10, to = 10 * 52 * 4, by = 10),
                 brand = rep(c("brand a", "brand b",
                               "brand a", "brand b"),
                             each = 52),
                 year = rep(2018:2019, each = 104))

# Group and calculate rolling means
df2 <- df %>%
  group_by(year, brand) %>% 
  mutate(across(.cols = starts_with("col"),
         .fns = ~ rollmean(x = .x, k = 4, fill = NA, align = "left"))) %>% 
  ungroup()
################
library(tidyverse)
f_varname <- sym("cyl")
ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  facet_grid(cols = vars({{f_varname}}))
################
library(tidyverse)
df <- read.table(text = "Document n word tf-idf
                  1       10 kohl 0,22
                  2       8  x    0,5
                  3       4  b    0,2", 
                  header = TRUE)
df %>% 
  filter(str_length(word) >= 2)
###################
library(tidyverse)

stores <- data.frame(
  stringsAsFactors = FALSE,
                       state = c("california","california","nevada","nevada","arizona",
                                 "arizona"),
                       store = c("target",
                                 "walmart","target","walmart","target",
                                 "walmart"),
     num_locations = c(20L, 29L, 10L, 12L, 15L, 19L)
          )

stores %>%
  group_by(store) %>%
  summarise(avg_num_locations = mean(num_locations))
##############
library(tidyverse)
library(lubridate)
twitter %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(time = case_when(Date %within%
                     interval(ymd("2019-05-01"),
                              ymd("2020-03-31")) ~ "code 1",
                   Date %within%
                     interval(ymd("2020-04-01"),
                              ymd("2020-06-30")) ~ "code 2",
                   Date %within%
                     interval(ymd("2020-07-01"),
                              ymd("2020-12-31")) ~ "code 3",
                   Date %within%
                     interval(ymd("2021-01-01"),
                              ymd("2021-04-30")) ~ "code 4")) %>%
  mutate(time = factor(time, levels = c("code 1", "code 2",
                                        "code 3", "code 4"))) %>%
  group_by(username, agegroup, time, .drop = FALSE) %>%
  summarise("mean(compound)" = mean(compound, na.rm = TRUE))
#################
library(tidyverse)

dt <- data.frame(stringsAsFactors = FALSE, Obs = c(1L, 2L, 3L, 4L),
           Message = c("a : 3 b : 5", "c : 4 a : 2 d : 9", NA, "b : 3"))

dt %>%
  separate_rows(Message, sep = '\\s(?=[a-z])') %>%
  separate(Message, c('variable', 'value'), sep = ' : ', fill = 'right', convert = TRUE)

#################
library(tidyverse)
dat1 <- tibble::tribble(
  ~state, ~`2020-01-01`, ~`2020-01-05`, ~`2020-01-06`, ~`2020-01-10`,
    "AZ",            NA,         0.078,         -0.06,            NA,
    "AK",          0.09,            NA,            NA,           0.1,
    "MS",          0.19,          0.21,            NA,          0.38
  )

dat1 %>% 
  pivot_longer(-c(state)) %>%
  mutate(dates = as.Date(name)) %>%
  ggplot(aes(x = dates, y = value)) +
  geom_point() +
  facet_grid(rows = vars(state)) +
  scale_x_date(date_breaks = "1 day", name = "", limits = c(as.Date("2019-12-25"), as.Date("2020-01-15"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05))
###############
library(data.table)
library(tidyverse)

df <- fread("test.txt", data.table = FALSE,
                        fill = TRUE, header = FALSE) %>% 
  separate(V1, sep = ": ", into = c("names", "values")) %>% 
  na.omit() %>% 
  mutate(id = rep(1:(nrow(.)/3), each = 3)) %>% 
  pivot_wider(names_from = names, values_from = values) %>% 
  select(-c(id))
################
library(tidyverse)
library(data.table)
# Example data
df <- data.frame(
  stringsAsFactors = FALSE,
               ROW = c(2L, 7L, 8L, 9L, 10L),
                ID = c("REC1000023","REC1000032",
                       "REC1000066","REC1000078","REC1000099"),
               SEX = c("F", "M", "M", "M", "M"),
              RACE = c("1.Black","6.White","4.Asian",
                       "6.White","5.Multiracial")
)

# Create new columns
df %>% 
  mutate(Black = ifelse(RACE == "1.Black", 1, 0),
         White = ifelse(RACE == "6.White", 1, 0),
         Other = ifelse(RACE != "1.Black" & RACE != "6.White",
                        1, 0))

df %>% mutate(Black = +str_detect(RACE,'Black'),
              White = +str_detect(RACE,'White'),
              Other = +(!str_detect(RACE,'Black|White')))

df2 <- data.frame(stringsAsFactors = FALSE,
                  ROW = 1:10000,
                  ID = rep(c("REC1000023","REC1000032",
                             "REC1000066","REC1000078",
                             "REC1000099"), times = 2000),
                  SEX = sample(c("F", "M"),
                               replace = TRUE,
                               size = 10000),
                  RACE = sample(c("1.Black","6.White","4.Asian",
                           "6.White","5.Multiracial"),
                           replace = TRUE,
                           size = 10000))

library(reshape2)
dcast_func <- function(df){
  dcast(data = df, ROW + ID + SEX ~ RACE, length)
}

ronak_func <- function(df){
  df %>%
    mutate(col = sub('\\d+\\.', '', RACE), 
           col = replace(col, !col %in% c('Black', 'White'), 'Other')) %>%
    pivot_wider(names_from = col, values_from = col, 
                values_fn = length, values_fill = 0)
}

jared_func <- function(df){
  df %>% 
    mutate(Black = ifelse(RACE == "1.Black", 1, 0),
           White = ifelse(RACE == "6.White", 1, 0),
           Other = ifelse(RACE != "1.Black" & RACE != "6.White", 1, 0))
}

karthik_func <- function(df){
  df %>% mutate(Black = +str_detect(RACE,'Black'),
                White = +str_detect(RACE,'White'),
                Other = +(!str_detect(RACE,'Black|White')))
}

jpdugo17_func <- function(df){
  map_dfc(list('1.Black', '6.White'), ~ transmute(df, '{str_sub(.x, 3, -1)}' := if_else(RACE == .x, 1, 0))) %>% 
    mutate(other = if_else(Black + White == 1, 0, 1)) %>% cbind(df, .)
}

jared_func_dt <- function(df){
  setDT(df)
  df[, Black := +(df$RACE == "1.Black")][, White := +(df$RACE == "6.White")][, Other :=  1 - (df$Black | df$White)]
}

GKi1_func <- function(df) {
  df$Black <- +(df$RACE == "1.Black")
  df$White <- +(df$RACE == "6.White")
  df$Other <- 1 - (df$Black | df$White)
  df
}

GKi2_func <- function(df) {
  df$Black <- +grepl("Black", df$RACE, fixed = TRUE)
  df$White <- +grepl("White", df$RACE, fixed = TRUE)
  df$Other <- 1 - (df$Black | df$White)
  df
}

library(microbenchmark)
res <- microbenchmark(ronak_func(df),
                      jared_func(df),
                      karthik_func(df),
                      jpdugo17_func(df),
                      jared_func_dt(df),
                      GKi1_func(df),
                      GKi2_func(df))
autoplot(res)
###############
library(tidyverse)
df <- structure(list(target = c(24.4, 24.4, 24.4, 24.4, 24.4, 24.4, 
                                24.4, 24.4, 24.4, 24.4), zone1 = c(23.5, 23.3, 23.5, 23.7, 23.8, 
                                                                   23.7, 23.6, 23.6, 23.8, 23.7), zone2 = c(24.3, 24, 24.1, 24.4, 
                                                                                                            24.7, 24.6, 24.5, 24.6, 24.7, 24.7), zone3 = c(24.8, 24.8, 24.5, 
                                                                                                                                                           24.3, 24.1, 23.8, 24.2, 24.2, 24.2, 23.8), zone4 = c(24.5, 24.5, 
                                                                                                                                                                                                                24.4, 24.5, 24.4, 24.3, 24.4, 24.9, 24.5, 24.5), zone5 = c(24.3, 
                                                                                                                                                                                                                                                                           24.4, 24.5, 24.5, 24.5, 24.4, 24.6, 24.6, 24.8, 24.9), zone6 = c(23.1, 
                                                                                                                                                                                                                                                                                                                                            23, 23.1, 23.1, 23, 22.8, 22.6, 22.7, 23.1, 23.2)), row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                              -10L), class = c("tbl_df", "tbl", "data.frame"))
t_test <- function(x, y) {
  tryCatch(
    round(t.test(x=x, mu=mean(y))$p.value, 3),
    error=function(e) NA)
}

t.test(x = df$zone2, mu = 24.4)

df %>%
  mutate(across(.cols = zone1:zone6,
                         .fns = ~ t_test(x = .x, y = target))) %>%
  pivot_longer(cols = -target) %>%
  group_by(name) %>%
  summarise(mean(value), mean(result$zone2))

###################
library(tidyverse)
library(vroom)
library(ggthemes)
options(timeout = 2400)
NYSdata <- vroom("https://www.nycourts.gov/LegacyPDFS/court-research/OCA-STAT-Act.csv")
new <- c("row_num", "court_type", "region", "district", "county", "court", "arresting_agency", "arrest_type", "arraign_year", "arraign_month", "top_charge_at_arraignment", "severity", "weight", "law", "article_section", "attempt_flag", "gender", "ethnicity", "race", "arrest_age", "docket_status", "disposition_type", "disposition_detail", "dismissal_reason", "most_severe_sentence", "fines_imposed", "fees_imposed", "surcharges_imposed")
names(NYSdata) <- new
NYSdata <- select(NYSdata, -c("row_num"))
NYSdata %>%
  filter(grepl("[[:alpha:]]+", x = race)) %>%
  ggplot(aes(x = race)) +
  geom_bar() +
  xlab("Court") + 
  ylab("Number of People") + 
  labs(title = "Racial Breakdown of New York State Courts") + 
  theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(stat='count', aes(label=..count..), vjust = -.3)
#############
library(tidyverse)
library(ggridges)
library(viridis) 
library(rstatix)

data(iris)
iris$treatment <- rep(c("A","B","C"), length(iris$Species)/3)
mydf <- gather(iris,Variable,value,Sepal.Length:Petal.Width)

ggplot(mydf) +
  geom_tile(aes(x = value, y = Species, width = 1, height = 0.5), alpha = 0.1) +
  stat_density_ridges(data = mydf, aes(x = value, y = Species, fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.1, 0.9), 
                      scale = 1) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "light grey", "#0000FFA0"),
    labels = c("(0, 0.1]", "(0.1, 0.9]", "(0.9, 1]")) +
  coord_cartesian(xlim=c(-10, 10)) +
  theme_bw() +
  facet_wrap(~Variable, ncol = 1)
##############
library(tidyverse)
library(rvest)
bun_1 <- read_html("https://www.bunnings.com.au/search/products?q=paint&sort=BoostOrder&page=1/")
part_1 <- data.frame(  
  paint = bun_1 %>% html_nodes(".product-title") %>% html_text(), 
  price = bun_1 %>% html_nodes(".price-medium-size p") %>% html_text()
)
###############
# Load libraries
library(tidyverse)

# Create a 'fake' dataset with 49 variables (50 incl rownumber) and 13 obs
df_example <- data.frame(row = rep(c(1:13), 49),
                         variables = rep(x = c(letters, LETTERS)[1:49], each = 13),
                         values = runif(n = 49 * 13, 0, 100)) %>% 
  pivot_wider(id_cols = row, names_from = variables, values_from = values)
df_example

# Create a new column called "sub_id"
df_example_with_sub_id <- df_example %>%
  mutate(sub_id = paste("r", sprintf("%02d", sample(1:13)), sep = ""))
df_example_with_sub_id

# Select rownumber, sub_id, and a few columns to show that it worked
df_selected_columns <- df_example_with_sub_id %>%
  select(row, sub_id, a, b, c, d)
df_selected_columns

# Arrange obs by sub_id
arranged_by_sub_id <- df_selected_columns %>% 
  arrange(sub_id)
arranged_by_sub_id

#############
library(tidyverse)
country_num <- read.table(
  text = "Country     num
          Other       5
          Other       6
          Other       16
          USA         30
          UK          25
          China       12",
  header = TRUE
)

country_num %>%
  group_by(Country) %>%
  summarise(num = sum(num), n = n())
#############
library(tidyverse)
df <- data.frame(
  stringsAsFactors = FALSE,
  Sites = c("Site 1","Site 2","Site 3",
            "Site 4","Site 5","Site 6","Site 7","Site 8","Site 9",
            "Site 10","Site 11"),
  Values = c(184.7955548,171.1466314,
             245.5952181,188.3072784,259.9438698,210.3448318,
             173.7977541,182.5497301,198.7985429,188.0458496,215.5709303),
  Groups = c(1, 1, 3, 3, 2, 3, 1, 3, 3, 2, 2))

df %>%
  arrange(Groups, Values) %>%
  mutate(name = factor(Sites, levels = Sites),
         Groups = factor(Groups)) %>%
  ggplot(aes(x = name, y = Values, fill = Groups)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c ('royalblue1', 'slategrey', 'yellow1'))+
  ylab("Values")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###################
library(tidyverse)

df <- data.frame(y=1:10, x=1:10, col=c("a", "b"))  # Added col
h <- 7.1
ggplot(df, aes(x = x, y = y)) + 
  geom_point(aes(color = col)) +
  geom_hline(yintercept = h, col = "red") +
  geom_text(data = data.frame(x = nrow(df) - 2, y = h),
            aes(x, y), label = paste('\u2193', "max-f1 threshold", ""), vjust = -1, color = "red")
#################
# Load libraries
library(tidyverse)

# Create fake data
subjects <- data.frame(height = rnorm(100, 1.6, 0.2),
                       weight = rnorm(100, 75, 20))

# Calculate BMI and categorise subjects per wikipedia
subjects %>%
  mutate(BMI = weight / (height^2)) %>%
  mutate(`BMI category` = case_when(
    BMI < 15 ~ "Very severely underweight",
    BMI >= 15 & BMI < 16 ~ "Severely underweight",
    BMI >= 16 & BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 25 ~ "Normal",
    BMI >= 25 & BMI < 30 ~ "Overweight",
    BMI >= 30 & BMI < 35 ~ "Moderately obese",
    BMI >= 35 & BMI < 40 ~ "Severely obese",
    BMI >= 40 ~ "Very severely obese")
  )

library(palmerpenguins)
sapply(penguins, function(x) if("factor" %in% class(x)) { 
  prop.table(table(x))
  }
)
sapply(df, function(x) if("factor" %in% class(x)) {prop.table(table(x))})

df <- mtcars[, c("am", "gear")]
df$am <- factor(df$am); df$gear <- factor(df$gear)
prop.table(table(penguins))
#################
# Load libraries
library(tidyverse)
library(lubridate)

# Generate a fake dataset (minimal reproducible example)
df <- data.frame(Date = seq.Date(from = ymd("2021-01-01"),
                                 to = ymd("2021-12-31"),
                                 by = "1 day"),
                 Volume = runif(365, 0, 4e+08))

plot_labels <- c(
  "1" = "First Quarter, 2021",
  "2" = "Second Quarter, 2021",
  "3" = "Third Quarter, 2021",
  "4" = "Fourth Quarter, 2021"
)
# Plot the fake data
df %>%
  mutate(quarter = cut.Date(Date, breaks = "quarter", labels = FALSE)) %>%
  ggplot(., aes(x = Date, y = Volume)) +
  geom_col(stat = "identity",
           width = 0.9, 
           fill = "coral",
           alpha = 0.5,
           colour = "black",
           position = "dodge") +
  scale_x_date(date_breaks = "1 day", labels = scales::date_format("%m/%d")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Volume Sold by Date") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ quarter, ncol = 1, scales = "free_x",
             labeller = labeller(quarter = plot_labels))

# Save the plot
ggsave(filename = "example_1.png", width = 60, height = 10, limitsize = FALSE)
##################
#install.packages("causalweight")
library(causalweight)
data(JC)

x0=JC[,2:29]
x1=JC[,30:36]
d1=JC[,37]
d2=JC[,38]
y2=JC[,44]

output=dyntreatDML(y2=y2, d1=d1, d2=d2, x0=x0, x1=x1)
cat("dynamic ATE: ",round(c(output$effect),3),", standard error: ",
    round(c(output$se),3), ", p-value: ",round(c(output$pval),3))
output$ntrimmed
####################
# Load libraries
library(causalweight)
library(naniar)
library(xgboost)

# Load the data
mydata4 <- read.csv("for_dyn.csv", check.names = FALSE)

# Rename the potentially-problematic variable ""
colnames(mydata4)[1] <- "rownumber"

# Check for missing values (not necessary, just good practice)
naniar::any_miss(mydata4)

x0 = mydata4[,4:7]
x1 = mydata4[,10:12]

d1 = sample(x = c(0,1), size = nrow(mydata4), replace = TRUE)
d2 = sample(x = c(0,1), size = nrow(mydata4), replace = TRUE)

y2 = mydata4[,13]

output=dyntreatDML(y2=y2, d1=d1, d2=d2, x0=x0, x1=x1, fewsplits = TRUE, normalized = TRUE)
cat("dynamic ATE: ",round(c(output$effect),3),", standard error: ",
    round(c(output$se),3), ", p-value: ",round(c(output$pval),3))
output$ntrimmed
##############
library(tidyverse)
year <- c(2000,2001,2002,2003)
obs1 <- c(5,6,7,8)
obs2 <- c(1,2,3,4)
df <- data.frame(year,obs1,obs2)

df %>% pivot_longer(cols = -year, names_to = NULL, values_to = "obs")
##############
library(tidyverse)
string <- "newdatat.scat == \"RDS16\" ~ \"Asthma\","
str_extract_all(string, pattern = '".*?"')
##############
# Load libraries
library(tidyverse)

# Create fake data
set.seed(123)
survey <- data.frame(A = sample(c("response 1", "response 2",
                                  "response 3", "no response"),
                                size = 100, replace = TRUE),
                     B = sample(c("response 1", "response 2",
                                  "response 3", "no response"),
                                size = 100, replace = TRUE))
# Examine the data
head(survey)

# Calculate how many responders answered A but not B, and vice versa
sum(survey$A == "no response" & survey$B != "no response")
sum(survey$B == "no response" & survey$A != "no response")
#############
library(tidyverse)
df <- data.frame(colA=c("A","B","C"),
                 colB = c("Stringn","Stringc","Stringb"),
                 x2008 = c(2.71,3.1,6.21),
                 x2009 = c(1.72,1.68,6.18),
                 x2010 = c(1.32,2.66,4.21))
df2 <- df %>%
  pivot_longer(-c(colA, colB))

yoy <- function(x) {
  x - lag(x, 1, default = 0)
}

df2 %>%
  group_by(colA, colB) %>%
  mutate(yoy = scales::percent((value - lag(value, 1, default = NA)) / value))

##############
m <- matrix(0, 3,3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
dimnames(m)
M <- Matrix::Matrix(m)
m2 <- matrix(M, nrow = M@Dim,
             dimnames = M@Dimnames)
##############
library(tidyverse)
fruit1<-rep(c("Apples","Oranges","Apples","Grapes","Apples","Oranges"), 1000)
fruit2<-rep(c("Apples","Oranges","Apples","Grapes","Apples","Oranges"), 1000)
fruit3<-rep(c("Apples","Oranges","Apples","Grapes","Apples","Oranges"), 1000)
data<-data.frame(fruit1,fruit2,fruit3, stringsAsFactors = FALSE)

data %>%
  mutate(across(c(fruit2, fruit3), ~ recode(.x, Apples="Apple")))


fun1 <- function(data) {
  data %>%
    mutate(across(c(fruit2, fruit3), ~ recode(.x, Apples="Apple")))
}

fun2 <- function(data) {
  data %>%
    mutate(across(2:3, ~  replace(., . == 'Apples','Apple')))
}

fun3 <- function(data) {
  for (i in 2:3){
    data[i][data[i] == "Apples"] = "Apple"
  }
}

fun4 <- function(data) {
  data %>%
    mutate(across(2:3, ~ ifelse(.x == 'Apples', 'Apple', .x)))
}

fun5 <- function(data) {
  data$fruit2[data$fruit2 == "Apples"] = "Apple"
  data$fruit3[data$fruit3 == "Apples"] = "Apple"
}

fun6 <- function(data) {
  recode(data$fruit2, "Apples" = "Apple")
  recode(data$fruit3, "Apples" = "Apple")
}

res <- microbenchmark::microbenchmark(fun1, fun2, fun3, fun4, fun5, fun6)
res$expr <- forcats::fct_rev(forcats::fct_reorder(res$expr, res$time, mean))
autoplot(res)
##############
library(tidyverse)
area <- data.frame(
  land = c("68N03E220090", "68N03E244635", "68N03E244352", "68N03E223241"),
  type = c("home", "mobile", "home", "vacant"),
  object_id = c(NA, 7, NA, 34)
)

block <- rep(c("68N03E22", "68N03E24"), 100)

datalist = list()

for (value in block){
  df <- area %>% filter(is.na(object_id) & grepl(paste0("^", value),land))
  df$value <- value
  datalist[[value]] <- df # add it to your list
}

df_filtered <- dplyr::bind_rows(datalist)
df_filtered

df_filtered_2 <- area %>%
  filter(is.na(object_id) & grepl(pattern = paste0(block, collapse = "|"), x = land)) %>% 
  mutate(value = str_sub(land, 1, 8))

identical(df_filtered, df_filtered_2)

loop <- function(df) {
  datalist = list()
  
  for (value in block){
    df2 <- df %>% filter(is.na(object_id) & grepl(paste0("^", value),land))
    df2$value <- value
    datalist[[value]] <- df2 # add it to your list
  }
  df_filtered <- dplyr::bind_rows(datalist)
}

no_loop <- function(df) {
  df_filtered_2 <- df %>%
    filter(is.na(object_id) & grepl(pattern = paste0(block, collapse = "|"), x = land)) %>% 
    mutate(value = str_sub(land, 1, 8))
}

speed <- microbenchmark::microbenchmark(loop(area), no_loop(area))
autoplot(speed)

#############
# Load libraries
library(tidyverse)
library(data.table)

# Create example data
pick.nums <- function(n) {
  floor(10^(sample(3:8, n, replace = TRUE))*runif(n))
}

pick.nums(5)

df1 <- data.frame("Code 4 Dig" = c(pick.nums(600)),
                  "Name" = paste("name_", pick.nums(600), sep = ""),
                  check.names = FALSE)

df2 <- data.frame("Code 4 Dig" = c(pick.nums(28000000)),
                  "ID" = paste("ID_", c(pick.nums(28000000)), sep = ""),
                  check.names = FALSE)

# Convert example dataframes to data.tables
setDT(df1)
setDT(df2)

# Set keys for joining
setkey(df1, `Code 4 Dig`)
setkey(df2, `Code 4 Dig`)

# Join the tables
Result <- df1[df2, nomatch=0]

# Summarise to get counts
counts <- Result[, .(counts = .N), by = `Code 4 Dig`]
counts
################
library(tidyverse)
library(GGally)

# Loads some data
mtcars <- mtcars[,1:6]

# Defines function to color according to correlation
cor_func <- function(data, mapping, method, symbol, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, method=method, use='complete.obs')
  
  colFn <- colorRampPalette(c("firebrick", "white", "dodgerblue"), 
                            interpolate ='spline')
  rampcols <- colFn(100)
  match <- c(rampcols[1:10], rep("#FFFFFF", 80), rampcols[90:100])
  fill <- match[findInterval(corr, seq(-1, 1, length = 100))]
  
  ggally_text(
    label = paste(symbol, as.character(round(corr, 2))), 
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill = fill))
}

# Following the suggestion by @Jonni
pm <- ggpairs(mtcars, 
              upper = list(continuous = wrap(cor_func,
                                             method = 'spearman', symbol = "Corr:\n")),
              lower = list(continuous = function(data, mapping, ...) {
                ggally_smooth_lm(data = data, mapping = mapping)}),
              diag = list(continuous = function(data, mapping, ...) {
                ggally_densityDiag(data = data, mapping = mapping)}
              ))

pm
##############

plot_grid_modified <- function(..., plotlist = NULL, align = c("none", "h", "v", "hv"),
                      axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
                      nrow = NULL, ncol = NULL, rel_widths = 1,
                      rel_heights = 1, labels = NULL, label_size = 14,
                      label_fontfamily = NULL, label_fontface = "bold", label_colour = NULL,
                      label_x = 0, label_y = 1,
                      hjust = -0.5, vjust = 1.5, scale = 1., greedy = TRUE,
                      byrow = TRUE, cols = NULL, rows = NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  num_plots <- length(plots)
  
  if (!is.null(cols)){
    warning("Argument 'cols' is deprecated. Use 'ncol' instead.")
  }
  
  if (!is.null(rows)){
    warning("Argument 'rows' is deprecated. Use 'nrow' instead.")
  }
  
  scale <- rep_len(scale, num_plots)
  if (sum(scale <= 0) > 1){
    stop("Argument 'scale' needs to be greater than 0.")
  }
  
  # internally, this function operates with variables cols and rows instead of ncol and nrow
  if (!is.null(ncol)){
    cols <- ncol
  }
  if (!is.null(nrow)){
    rows <- nrow
  }
  
  
  # calculate grid dimensions
  if (is.null(cols) && is.null(rows)){
    # if neither rows nor cols are given, we make a square grid
    cols <- ceiling(sqrt(num_plots))
    rows <- ceiling(num_plots/cols)
  }
  # alternatively, we know at least how many rows or how many columns we need
  if (is.null(cols)) cols <- ceiling(num_plots/rows)
  if (is.null(rows)) rows <- ceiling(num_plots/cols)
  
  # if the user wants to layout the plots by column, we use the calculated rows to reorder plots
  if (!isTRUE(byrow)) plots <- plots[c(t(matrix(c(1:num_plots, rep(NA, (rows * cols) - num_plots)), nrow = rows, byrow = FALSE)))]
  
  # Align the plots (if specified)
  grobs <- align_plots(plotlist = plots, align = align, axis = axis, greedy = greedy)
  
  if ("AUTO" %in% labels) {
    count <- 0
    labels <- c()
    for (idx in seq_along(plots)) {
      if (!is.null(unlist(plots[idx]))) {
        count <- count + 1
        labels <- c(labels, LETTERS[count])
      } else {
        labels <- c(labels, "")
      }
    }
  } else if ("auto" %in% labels) {
    count <- 0
    labels <- c()
    for (idx in seq_along(plots)) {
      if (!is.null(unlist(plots[idx]))) {
        count <- count + 1
        labels <- c(labels, letters[count])
      } else {
        labels <- c(labels, "")
      }
    }
  }

  # label adjustments can be provided globally for all labels
  # or individually for each label
  hjust <- rep_len(hjust, length(labels))
  vjust <- rep_len(vjust, length(labels))
  label_x <- rep_len(label_x, length(labels))
  label_y <- rep_len(label_y, length(labels))
  
  # calculate appropriate vectors of rel. heights and widths
  rel_heights <- rep(rel_heights, length.out = rows)
  rel_widths <- rep(rel_widths, length.out = cols)
  # calculate the appropriate coordinates and deltas for each row and column
  x_deltas <- rel_widths/sum(rel_widths)
  y_deltas <- rel_heights/sum(rel_heights)
  xs <- cumsum(rel_widths)/sum(rel_widths) - x_deltas
  ys <- 1 - cumsum(rel_heights)/sum(rel_heights)
  
  # now place all the plots
  p <- ggdraw() # start with nothing
  col_count <- 0
  row_count <- 1
  for (i in 1:(rows*cols)){
    if (i > num_plots) break
    
    x_delta <- x_deltas[col_count+1]
    y_delta <- y_deltas[row_count]
    x <- xs[col_count+1]
    y <- ys[row_count]
    
    # place the plot
    p_next <- grobs[[i]]
    if (!is.null(p_next)){
      p <- p + draw_grob(p_next, x, y, x_delta, y_delta, scale[i])
    }
    # place a label if we have one
    if (i <= length(labels)){
      p <- p + draw_plot_label(labels[i], x + label_x[i]*x_delta, y + label_y[i]*y_delta, size = label_size,
                               family = label_fontfamily, fontface = label_fontface, colour = label_colour,
                               hjust = hjust[i], vjust = vjust[i])
    }
    # move on to next grid position
    col_count <- col_count + 1
    if (col_count >= cols){
      col_count <- 0
      row_count <- row_count + 1
    }
  }
  p
}

library(ggplot2)
library(cowplot)

df <- data.frame(
  x = 1:10, y1 = 1:10, y2 = (1:10)^2, y3 = (1:10)^3, y4 = (1:10)^4
)

p1 <- ggplot(df, aes(x, y1)) + geom_point()
p2 <- ggplot(df, aes(x, y2)) + geom_point()
p3 <- ggplot(df, aes(x, y3)) + geom_point()
p4 <- ggplot(df, aes(x, y4)) + geom_point()
p5 <- ggplot(mpg, aes(as.factor(year), hwy)) +
  geom_boxplot() +
  facet_wrap(~class, scales = "free_y")
# simple grid
plot_grid_modified(p1, NULL, p3, p4, labels = "AUTO")
#################
library(tidyverse)
iris$group = c ( rep ( "A", 50), rep ( "C", 25), rep ( "D", 50) ,rep ( "E", 25) )

ggplot(iris,
       aes(x =  group, 
           y =  Species ,
           colour = Species ,
           size = Sepal.Width )) +
  geom_point() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = seq(1.5, 2.5, 1)) +
  geom_vline(xintercept = seq(1.5, 3.5, 1))
#################
library(tidyverse)
stem_data <- data.frame(Grade = rep(c("A", "B", "C", "P", "NP"), 2),
                        STEMflag = factor(x = c(rep("STEM", 5), rep("NONSTEM", 5)),
                                          levels = c("STEM", "NONSTEM")),
                        percent = c(0.95, 0.93, 0.90, 0.67, 0.86,
                                    0.05, 0.07, 0.10, 0.33, 0.14))
head(stem_data)
ggplot(data = stem_data, aes(x = Grade, y = percent, fill = STEMflag,
                             label = paste(percent * 100, "%", sep = ""))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
  geom_text(position = position_stack(vjust = 0.5), size = 4)
#################
library(ggpmisc)

set.seed(123)
n <- 1000
x_vec <- runif(n)
y_vec <- 3 + 1.3 * x_vec + rnorm(n)
df <- data.frame(x = x_vec, y = y_vec)

ggplot(df, aes(x, y)) +
  geom_point() +
  ylim(0, 8) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..eq.label..,
                                          ..rr.label..,
                                          ..f.value.label..,
                                          ..p.value.label..,
                                          ..AIC.label..,
                                          ..BIC.label..,
                                          sep = "~~~~")), 
                        parse = TRUE) +
geom_smooth(formula = y ~ x, method = "lm")
###############
library(tidyverse)

data(iris)
library(mboost)

mod <- gamboost(Sepal.Width ~ bbs(Petal.Width) + bols(Species),
                data = iris)

ci <- confint(mod, level = 0.9, B = 5)
plot(ci, which = "Petal.Width")
################
library(tidyverse)
example_dataset <- structure(list(ID = 1:6,
                                  Class = c("Amphibia", "Diverse", "Reptilia", "Amphibia", "Diverse", "Reptilia"),
                                  Implementation = structure(c(1L, 1L, 1L, 2L, 2L, 2L),
                                                             .Label = c("Observed", "Potential"),
                                                             class = "factor"),
                                  cost_bil = c(16.78812696789, 0.00755587011, 3.81210675659,
                                               46.26621554247, 0.32702350757, 110.57066587724),
                                  n = c(58190L, 9L, 2482L, 40748L, 3L, 16326L)),
                             row.names = c(NA, -6L), class = "data.frame")
example_dataset$Class <- factor(example_dataset$Class)

my_pal <- colorRampPalette(c("yellow","firebrick2"))

ggplot(example_dataset, aes(x = reorder(Class, -cost_bil), y = Implementation)) +
  geom_point(aes(size = cost_bil, color = cost_bil)) +
  geom_text(aes(label = paste("n =", scales::comma(n, accuracy = 1), sep = " ")),
            nudge_y = 0.3) +
  scale_color_gradientn(colours = my_pal(6)) +
  scale_size_continuous(range = c(5, 30)) +
  guides(color = guide_legend(reverse = T),
         size = guide_legend(reverse = T)) +
  labs(size = "US$ billions", color = "US$ billions") +
  xlab("Taxonomic Class") +
  ylab("Method reliability") +
  theme_bw(base_size = 16)

############
# Setup
library(tidyverse)
#install.packages("tidymodels", type = "source")
library(tidymodels)

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

modeling_df <- parks %>% 
  select(pct_near_park_data, spend_per_resident_data, med_park_size_data) %>% 
  rename(nearness = "pct_near_park_data",
         spending = "spend_per_resident_data",
         acres = "med_park_size_data") %>% 
  mutate(nearness = (parse_number(nearness)/100)) %>% 
  mutate(spending = parse_number(spending))

# Start building models
set.seed(123)
park_split <- initial_split(modeling_df)
park_train <- training(park_split)
park_test <- testing(park_split)

tree_rec <- recipe(nearness ~., data = park_train)
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

tune_wf <- workflow() %>% 
  add_recipe(tree_rec) %>% 
  add_model(tune_spec)

set.seed(234)
park_folds <- vfold_cv(park_train)

# Make a grid of various different models
doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = park_folds,
  grid = 20,
  control = control_grid(verbose = TRUE)
)

doParallel::stopImplicitCluster()

best_rmse <- select_best(tune_res, "rmse")

# Finalize a model with the best grid
final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

final_wf <- workflow() %>% 
  add_recipe(tree_rec) %>% 
  add_model(final_rf)

final_res <- final_wf %>% 
  last_fit(park_split)

fit_to_train <- final_wf %>%
  fit(park_train)


final_res %>% 
  collect_predictions() %>% 
  ggplot(aes(nearness, .pred)) +
  geom_point() +
  geom_abline()

############
library(tidymodels)
library(palmerpenguins)
set.seed(1234)
split_penguins <-initial_split(penguins)
split_penguins
train <-training(split_penguins)
test <-testing(split_penguins)

first_recipe <- train %>%
  recipe(sex ~ .)
summary(first_recipe)

first_recipe %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_spatialsign(all_numeric_predictors())

##############
library(tidyverse)
library(palmerpenguins)
library(extrafont)
loadfonts()
font_import()
fonts()

penguins %>%
  na.omit() %>%
  group_by(species) %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, fill = species)) +
  geom_point(shape = 21) +
  scale_fill_viridis_d(begin = 0, end = 0.6, name = "Species") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Body Mass of 'Palmer' Antarctic Penguins\n",
       caption = "\nJared Mamrot 2021-06-28 | Data released under a CC0 1.0 Universal license (https://allisonhorst.github.io/palmerpenguins)") +
  xlab("Bill Length (mm)") +
  ylab("Body Mass (g)") +
  theme_classic(base_size = 18,
                base_family = "Raleway SemiBold") +
  theme(legend.position = "right",
        plot.caption = element_text(size = 7, hjust = 0),
        plot.title = element_text(size = 20))

##############
library(tidyverse)
data_points = data.frame(x=rnorm(n=16), 
                         y=rnorm(n=16), 
                         col=c(rep("red",8), rep("blue",8)),
                         shap=rep(c(rep("sq",4), rep("tr",4)), 2))
                     
p1 <- ggplot(data_points, aes(x=x, y=y, color=col, shape=shap)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, 
                                         name = NULL,
                                         breaks = NULL)) + 
  theme(legend.spacing = unit(0, "pt"), 
        legend.key.height = unit(8, "pt"), 
        legend.spacing.x = unit(1, "pt"), 
        legend.key.width  = unit(0, "pt"), 
        legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"),
        legend.justification = "top",
        legend.margin=margin(4,4,4,4),
        legend.box.spacing = margin(0.5))

ggsave(filename = "desired_look.png", plot = p1, width = 2, height = 2, units = "in")
################
library(tidyverse)
library(gapminder)

set.seed(123)

gapminder_subset <- gapminder %>% 
  pivot_longer(-c(country, continent, year)) %>% 
  filter(year == "1997" | year == "2007") %>% 
  select(-continent) %>% 
  filter(name == "gdpPercap") %>% 
  pivot_wider(names_from = year) %>% 
  select(-name) %>% 
  mutate(gdp_change = ((`2007` - `1997`) / `1997`) * 100) %>% 
  sample_n(15)


ggplot(data = gapminder_subset,
       aes(x = country, y = gdp_change - 25)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = c(-25, 0, 25, 50),
                     labels = c(0, 25, 50, 75))
############
library(tidyverse)
positiveaffect <- structure(list(Group = c("SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SH", 
                         "SH", "SH", "SH", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", 
                         "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", 
                         "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", 
                         "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", 
                         "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", 
                         "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", "HC", 
                         "HC", "HC", "HC", "HC", "HC", "HC"), Time = c(1, 1, 1, 1, 1, 
                                                                       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                       1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                                                                       2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                                                                       3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 
                                                                       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
                                                                       2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                                                                       2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                                                                       3, 3, 3), Scores = c(18, 24, 31, 11, 14, 23, 17, 32, 19, 10, 
                                                                                            35.6378909726158, 16, 22, 19, 19, 23, 20, 34, 17, 29, 12, 20, 
                                                                                            16, 30, 26, 18, 21, 20, 15, 28, 29.3521832998335, 16, 10, 15, 
                                                                                            17, 21, 14, 23, 12, 28, 17, 22, 13, 17, 13, 17, 16, 18, 19, 11, 
                                                                                            17, 16, 24, 20, 22, 19, 18, 18, 15, 28.0883375096763, 20, 10, 
                                                                                            11, 13, 10, 28.0883375096763, 16, 11, 22, 10, 16, 10, 13, 12, 
                                                                                            16, 16, 15, 17, 10, 10, 10, 10, 13, 10, 18, 24, 26, 41, 25, 32, 
                                                                                            13, 13, 28, 10, 24, 32, 36, 17, 16, 25, 38, 27, 28, 18, 14, 18, 
                                                                                            30, 27, 24, 19, 33.2978283948586, 24, 24, 14, 20, 20, 10, 22, 
                                                                                            15, 12, 24, 20, 26, 21, 20, 17, 11, 24, 10, 21, 24, 27, 22, 34.0925216652814, 
                                                                                            24, 25, 12, 15, 24, 18, 23, 27, 14, 13, 11, 21, 25, 20, 22, 15, 
                                                                                            11, 17, 10, 20, 27), ID = c("1222", "1992j", "1992i", "1592", 
                                                                                                                        "1602", "1192", "1852", "1422", "1732", "1999", "1924", "1812", 
                                                                                                                        "1752", "1762", "1782", "19992", "1892", "199912", "190", "171", 
                                                                                                                        "199924", "1912", "199943", "1982", "1802", "3812", "199945", 
                                                                                                                        "197", "1492", "1222", "1992j", "1992i", "1592", "1602", "1192", 
                                                                                                                        "1852", "1422", "1732", "1999", "1924", "1812", "1752", "1762", 
                                                                                                                        "1782", "19992", "1892", "199912", "190", "171", "199924", "1912", 
                                                                                                                        "199943", "1982", "1802", "3812", "199945", "197", "1492", "1222", 
                                                                                                                        "1992j", "1992i", "1592", "1602", "1192", "1852", "1422", "1732", 
                                                                                                                        "1999", "1924", "1812", "1752", "1762", "1782", "19992", "1892", 
                                                                                                                        "199912", "190", "171", "199924", "1912", "199943", "1982", "1802", 
                                                                                                                        "3812", "199945", "197", "3212", "3182", "3162", "3412", "3492", 
                                                                                                                        "1993", "363", "3362", "3122", "3152", "1997", "19995", "330", 
                                                                                                                        "370", "3999", "19998", "375", "374", "373", "377", "379", "380", 
                                                                                                                        "382", "3212", "3182", "3162", "3412", "3492", "1993", "363", 
                                                                                                                        "3362", "3122", "3152", "1997", "19995", "330", "370", "3999", 
                                                                                                                        "19998", "375", "374", "373", "377", "379", "380", "382", "3212", 
                                                                                                                        "3182", "3162", "3412", "3492", "1993", "363", "3362", "3122", 
                                                                                                                        "3152", "1997", "19995", "330", "370", "3999", "19998", "375", 
                                                                                                                        "374", "373", "377", "379", "380", "382")), row.names = c(NA, 
                                                                                                                                                                                  -155L), class = c("tbl_df", "tbl", "data.frame"))

positiveplot = ggplot(positiveaffect,
                      aes(x = factor(Time),
                          y = Scores,
                          fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title="Change in self-rated positive affect",
       x="\nTime",
       y = "Positive Affect Score\n") +
  theme_classic() +
  theme(legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(vjust = -2, angle = 45, face = "bold"),
        axis.text.y = element_text(vjust = -2, face = "bold"),
        axis.title = element_text(face = "bold")) +
  scale_x_discrete(labels = labelspositive)
positiveplot
#############
#install.packages("vroom")
library(vroom)
df <- data.frame(ID = c("1", "2", "3"))
list_of_csvs <- paste(df$ID, ".csv", sep = "")
vroom(file = list_of_csvs, id = "ID")
############
library(tidyverse)

iris %>% 
  group_by(Species) %>% 
  summarise(`Mean Petal Length` = mean(Petal.Length))

library(tidyverse)
test <- iris %>% 
  mutate(Petal.Length_cut = cut(Petal.Length,
                                breaks = seq(min(Petal.Length),
                                             max(Petal.Length), 0.1))) %>% 
  group_by(Species, Petal.Length_cut) %>% 
  summarise(`Mean Sepal Length` = mean(Sepal.Length)) %>% 
  ungroup() %>% 
  pivot_wider(names_prefix = "Petal.Length_", names_from = Petal.Length_cut, values_from = `Mean Sepal Length`)
##############
data=data.frame(Date=c('2005-01-01','2005-02-01','2005-03-01','2005-04-01','2005-05-01'),
                       col1 = c(1,2,3,4,5),
                       col2 = c(1,2,3,4,5))
data[,'Date']= as.POSIXct(data[,'Date'],format='%Y-%m-%d')

lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] )
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)))
  }
}

data$col_l1 <- lagpad(data$col2, 1)
##############
a <- c(1,2,3,4)
b <- c(1,3,5,7,8,9)
d <- c(1,2,5,7,9)
e <- c(1,3,4,8,0,10)
f <- c(2,3)
list_of_vecs <- list(a, b, d, e, f)
names(list_of_vecs) <- c("a", "b", "d", "e", "f")
nms <- combn( names(list_of_vecs), 2, FUN = paste0,
              collapse = "", simplify = FALSE)
combinations <- combn(list_of_vecs, 2, simplify = FALSE)
out <- lapply(combinations, function(x) intersect(x[[1]], x[[2]]))
setNames(out, nms)
remove_empty <- purrr::compact(out)

##############
library(tidyverse)
df = data.frame(Type = rep(c('A', 'B'), 250), Index = seq(500), Value = cumsum(rnorm(500)))
scales::hue_pal()(3)
seg_df <- data.frame(x = c(200, 400, 0, 100, 300),
                     xend = c(300, 500, 100, 200, 400),
                     y = c(-8, -4, 0, 4, 8), 
                     yend = c(-8, -4, 0, 4, 8), 
                     col = factor(c("#F8766D", "#F8766D", "#00BA38",
                                    "#619CFF", "#619CFF"),
                                  labels = c("Alpha", "Beta", "Gamma")))

ggplot(df, aes(Index, Value)) +
  geom_line(aes(col = Type), show.legend = TRUE) +
  geom_segment(data = seg_df, aes(x = x, xend = xend,
                                  y = y, yend = yend,
                                  fill = col),
               color = c("#F8766D", "#F8766D", "#00BA38",
                         "#619CFF", "#619CFF")) +
  scale_fill_manual("Classification",
                    values = c("Alpha", "Beta", "Gamma"),
                    guide = guide_legend(override.aes = list(
                      colour = c("#F8766D", "#00BA38", "#619CFF"))))
##############
library(tidyverse)
library(palmerpenguins)

penguins %>%
  na.omit() %>%
  ggplot(aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(shape = sex)) +
  geom_point(aes(color = island)) +
  scale_color_discrete(limits = c("Biscoe", "Torgersen", "Dream"))
############
library(data.table)

matrix1 <- read.table(text = "X1          X2
    1 ENSG00000121410
   10 ENSG00000156006
  100 ENSG00000196839", header = TRUE)

matrix2 <- read.table(text = "     Y1          Y2
    1 6.674755e-01
   10 0.000000e+00
  100 9.975216e-01", header = TRUE)

setDT(matrix1)
setDT(matrix2)

joined_matrix <- merge(matrix1, matrix2, by.x = "X1", by.y = "Y1")
#############
library(tidyverse)

df1 <- data.frame(
  stringsAsFactors = FALSE,
         row.names = c("23","24","25","26","27",
                       "28","29","30","31","32","33"),
           Subject = c("VP02_RP","VP02_RP","VP02_RP",
                       "VP02_RP","VP02_RP","VP02_RP","VP02_RP","VP02_RP",
                       "VP02_RP","VP02_RP","VP02_RP"),
             Trial = c(15L, 15L, 15L, 15L, 15L, 18L, 18L, 18L, 18L, 18L, 19L),
        Event_type = c("Picture","Sound","Picture",
                       "Nothing","Response","Picture","Sound","Picture",
                       "Nothing","Response","Response"),
              Code = c("face01_n","mpossound_test5",
                       "pospic_test5","ev_mnegpos_adj_onset","15","face01_p",
                       "mpossound_test6","negpic_test6",
                       "ev_mposnegpos_adj_onset","15","13"),
              Time = c(887969L,888260L,906623L,
                       928623L,958962L,987666L,987668L,1006031L,1028031L,
                       1076642L,1680887L)
)

df1 %>%
  filter(Event_type != lag(Event_type, 1))
###############
library(psych)
descriptives <- psych::describe(sat.act ~ gender)

for (i in seq_along(descriptives)){
  write.table(x = descriptives[[i]],
            file = "descriptive_stats.csv",
            append = TRUE,
            quote = FALSE,
            row.names = TRUE,
            sep = ",")
}


save_individually <- function(x, df) {
  write.table(df[[x]], file = paste("descriptives", x, "txt", sep = "."),
              sep = ",", append = FALSE, quote = FALSE)
}

purrr::map(names(descriptives), save_individually, descriptives)
##############
library(tidyverse)

df1 <- tibble::tribble(
  ~C1, ~C2, ~C3, ~C4, ~C5, ~C6, ~C7, ~C8, ~C9, ~C10,
  "a", "b", "c", "d", "e", "f", "g", "h", "i",  "j"
)

result_1 <- combn(names(df1), m = 2, simplify = FALSE)
result_2 <- combn(names(df1), m = 3, simplify = FALSE)

result_1[[2]]
result_2[[1]]
################
library(tidyverse)

df1 <- data.frame(ab1 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab2 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab3 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab4 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab5 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab6 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab7 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab8 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab9 = ifelse(runif(100) >= 0.5, 0, 1),
                  ab10 = ifelse(runif(100) >= 0.5, 0, 1))

heatmap(as.matrix(df1), scale = "row", col = cm.colors(256))
heatmap(as.matrix(df1), scale = "none", col = cm.colors(256))
############
df1 <- tibble::tribble(
  ~r,   ~y1,   ~y2,   ~y3,   ~y4,
  1L, 2017L,    0L,    0L,    0L,
  2L,    0L, 2018L,    0L,    0L,
  3L,    0L,    0L, 2019L,    0L,
  4L, 2017L,    0L,    0L,    0L,
  5L,    0L,    0L, 2019L,    0L,
  6L,    0L,    0L,    0L, 2020L,
  7L,    0L, 2018L,    0L,    0L
  )
df1$year <- rowSums(df1[2:5])
df1
#############
my_data <- data.frame(
  "name" = c("john", "jason", "jack", "jim", "john", "jason", "jack", "jim" ),
  "points_1" = c("150", "165", "183", "191", "151", "166", "184", "192"),
  "points_2" = c("250", "265", "283", "291", "251", "266", "284", "292")
)

my_data$var1 <- paste("<", my_data$points_1, sep = "")
my_data$var2 <- paste(">", my_data$points_1, " and ", "<", my_data$points_2, sep = "")
my_data$var3 <- paste(">", my_data$points_2, sep = "")
my_data
#############
library(tidyverse)
library(ggpubr)

sp <- c("sp1","sp1","sp1","sp2","sp2","sp2","sp3","sp3","sp3","sp4","sp4","sp4","sp5","sp5","sp5")
category <- c("a","b","c","a","b","c","a","b","c","a","b","c","a","b","c")
count <- c(1,2,1,1,4,2,3,1,3,1,4,5,2,5,1)
habitat <- c("A","A","A","B","B","B","C","C","C","D","D","D","E","E","E")
d <- data.frame(cbind(sp, category, count, habitat))

dm <- d %>%
  select(sp, category, count)%>%
  tidyr::pivot_wider(names_from = "sp", values_from = "count")%>%  #clusterで並び替え
  replace(is.na(.),0)
dm <- as.matrix(dm[, -1]) # -1 to omit categories from matrix
clust <- hclust(dist(t(dm)), method = "single")

dmc <- data.frame(x = factor(d$sp), colour = factor(d$sp))

my_fill <- scale_fill_gradient(low="grey90", high="red",  
                               breaks=c(0,5,10,15,20, 25, 30), 
                               rescale=function(x, ...) scales::rescale(x, from=c(0, 30)),
                               limits=c(0,30))

plot1 <- ggplot(d, aes(category, sp))+
  geom_tile(aes(fill = as.numeric(count)))+
  my_fill +
  scale_y_discrete(limits = colnames(dm)[clust$order]) +
  theme(legend.position = "right")

plot2 <- ggplot(dmc) +
  geom_tile(aes(x = 1, y = x, fill = colour)) +
  theme_void() +
  scale_fill_manual(values = viridis::viridis(5)) +
  theme(legend.position = "none")

ggarrange(plot2, plot1, nrow = 1, widths = c(0.25, 10), align = "hv")

##########

set.seed(1234)
ind <- sample(2, nrow(mtcars), replace = TRUE, prob = c(0.7, 0.3))
train.data <- mtcars[ind == 1, ]
test.data <- mtcars[ind == 2, ]
mlr_model<-lm(mpg ~ ., data=train.data)
print(mlr_model)
summary(mlr_model)
plot(mlr_model)

pred <-predict(mlr_model, newdata = test.data)
pred
plot(test.data$mpg, pred, xlab = "Observed", ylab = "Prediction")
############
library(tidyverse)
#install.packages("countrycode")
library(countrycode)

df1 <- tibble::tribble(
             ~country_name,
             "Afghanistan",
                 "Albania",
                 "Algeria",
                 "Andorra",
                  "Angola",
     "Antigua and Barbuda",
               "Argentina",
                 "Armenia",
               "Australia",
                 "Austria",
              "Azerbaijan",
                 "Bahamas",
                 "Bahrain",
              "Bangladesh",
                "Barbados",
                 "Belarus",
                 "Belgium",
                  "Belize",
                   "Benin",
                  "Bhutan",
                 "Bolivia",
  "Bosnia and Herzegovina",
                "Botswana",
                  "Brazil",
                  "Brunei",
                "Bulgaria",
            "Burkina Faso",
                 "Burundi",
           "Côte d'Ivoire",
              "Cabo Verde"
  )
df1$country_name <- toupper(df1$country_name)
df1$continent <- countrycode(df1$country_name, "country.name", "continent")
df1

##############
library(tidyverse)
df <- data.frame(id = c("Missing","power_1-1","power_1-2","power_1-3","power_1-4","power_1-5","power_2","power_3","power_4","power_5"),
                 mean = c(-0.0823,0.0592,-0.0556,-0.1037,-0.1303,-0.1478,-0.1857,-0.2074,-0.2231,-0.2156),
                 se = c(0.0609,0.0247,0.0216,0.0206,0.0202,0.0199,0.0194,0.0193,0.0205,0.0242), stringsAsFactors = FALSE)

colour_scale <- c("red", viridis::mako(9))

p1 <- df %>%
  rowwise() %>%
  mutate(CI95 = list(c(mean + 1.96 * se, mean - 1.96 * se)),
         CI99 = list(c(mean + 2.58 * se, mean - 2.58 * se))) %>%
  unnest(c(CI95, CI99)) %>%
  mutate(id = factor(reorder(id, -CI99))) %>%
  ggplot() +
  labs(x = NULL, y = NULL) +
  geom_line(aes(x = id, y = CI99, group = id, color = id)) +
  geom_line(aes(x = id, y = CI95, group = id, color = id), size = 3) +
  geom_point(aes(x = id, y = mean, color = id), fill = "white", shape = 23, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept=1:9+0.5, colour="lightblue", size = 0.5) +
  theme_classic() +
  scale_color_manual(values = colour_scale) +
  coord_flip()
ggsave(filename = "example_plot.png", plot = p1, width = 18, height = 7, units = "cm")
###########
library(tidyverse)
pct.df <- structure(list(group = c("a", "a", "a", "b", "b", "b"), gender = c("male", 
                                                                             "female", "male", "female", "male", "female"), var_a = c(33.3333333333333, 
                                                                                                                                      16.6666666666667, 50, 50, 50, 33.3333333333333), var_b = c(50, 
                                                                                                                                                                                                 75, 50, 75, 75, 75), var_c = c(50, 75, 75, 100, 75, 75), var_d = c(50, 
                                                                                                                                                                                                                                                                    25, 0, 25, 50, 50), var_e = c(25, 0, 50, 0, 50, 25), var_f = c(25, 
                                                                                                                                                                                                                                                                                                                                   25, 0, 50, 50, 25), var_g = c(25, 25, 0, 50, 50, 25), var_h = c(25, 
                                                                                                                                                                                                                                                                                                                                                                                                   25, 0, 50, 50, 25), avg = c(35.4166666666667, 33.3333333333333, 
                                                                                                                                                                                                                                                                                                                                                                                                                               28.125, 50, 56.25, 41.6666666666667)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          -6L))
pct.df %>% 
  pivot_longer(-c(group, gender, avg), names_to = "variable") %>% 
  group_by(variable) %>%
  summarise(n = n(),
            mean = mean(value),
            sd = sd(value),
            se = sd/sqrt(n),
            ic = se * qt((1-0.05)/2 + .5, n-1))

################
library(tidyverse)
pct.df <- structure(list(group = c("a", "a", "a", "b", "b", "b"), gender = c("male", 
                                                                             "female", "male", "female", "male", "female"), var_a = c(33.3333333333333, 
                                                                                                                                      16.6666666666667, 50, 50, 50, 33.3333333333333), var_b = c(50, 
                                                                                                                                                                                                 75, 50, 75, 75, 75), var_c = c(50, 75, 75, 100, 75, 75), var_d = c(50, 
                                                                                                                                                                                                                                                                    25, 0, 25, 50, 50), var_e = c(25, 0, 50, 0, 50, 25), var_f = c(25, 
                                                                                                                                                                                                                                                                                                                                   25, 0, 50, 50, 25), var_g = c(25, 25, 0, 50, 50, 25), var_h = c(25, 
                                                                                                                                                                                                                                                                                                                                                                                                   25, 0, 50, 50, 25), avg = c(35.4166666666667, 33.3333333333333, 
                                                                                                                                                                                                                                                                                                                                                                                                                               28.125, 50, 56.25, 41.6666666666667)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          -6L))
pct.df %>% 
  pivot_longer(-c(group, gender, avg), names_to = "variable") %>% 
  group_by(group, variable) %>%
  summarise(n = n(),
            mean = mean(value),
            sd = sd(value),
            se = sd/sqrt(n),
            ic = se * qt((1-0.05)/2 + .5, n-1)) %>%
  select(-n) %>%
  bind_rows(summarise(., across(everything(), ~if(is.numeric(.)) mean(.) else "AVG"))) %>%
  arrange(group)
###############
# Load libraries
library(tidyverse)

# Create some "fake" data with all 3 "Gaze-by's",
# shuffle the AOIs between the different "Gaze-by's"
# and only keep data for the first two minutes
df <- df4 %>% 
  bind_rows(df4 %>% mutate(Gaze_by = "B", AOI = sample(AOI)), 
            df4 %>% mutate(Gaze_by = "A", AOI = sample(AOI))) %>% 
  filter(minute %in% 0:2)

# Plot the data
df %>%
  ggplot(aes(x = start_pm,
             xend = end_pm,
             y = minute + scale(as.numeric(as.factor(AOI))) / 10,
             yend = minute + scale(as.numeric(as.factor(AOI))) / 10,
             color = AOI)) +
  geom_segment(size = 2) +
  scale_y_reverse(breaks = 0:53,
                  labels = paste0(0:53, "min"),
                  name = NULL) +
  scale_colour_manual(values = c("*" = "lemonchiffon",
                                 "A" = "darkorange",
                                 "B" = "lawngreen",
                                 "C" = "slateblue1")) +
  theme_classic(base_size = 14) +
  theme(axis.title.x.bottom = element_blank()) +
  facet_wrap(~ minute + Gaze_by, ncol = 1,
             scales = "free_y", strip.position = "right",
             labeller = labeller(minute = label_both,
                                 Gaze_by = label_both)) +
  theme(strip.text = element_text(size = 8))

df %>%
  ggplot(aes(x = start_pm,
             xend = end_pm,
             y = minute + scale(as.numeric(as.factor(Gaze_by))) / 6,
             yend = minute + scale(as.numeric(as.factor(Gaze_by))) / 6,
             color = AOI)) +
  geom_segment(size = 3) +
  scale_y_reverse(breaks = 0:53,
                  labels = paste(0:53, "min", " Gaze_by_A\n Gaze_by_B\n Gaze_by_C", sep = " "),
                  name = NULL) +
  scale_colour_manual(values = c("*" = "lemonchiffon",
                                 "A" = "darkorange",
                                 "B" = "lawngreen",
                                 "C" = "slateblue1")) +
  theme_classic(base_size = 18) +
  theme(axis.title.x.bottom = element_blank())

##############
library(tidyverse)

df <- tibble(Source = c("A","A","B","B","C","C"),
             Group = c("y","n","y","n","y","n"),
             y2.5 = c(0.592,0.471,0.182,0.285,0.024,0.031),
             y25 = c(0.633,0.53,0.217,0.325,0.059,0.081),
             y50 = c(0.673,0.547,0.24,0.347,0.08,0.106),
             y75 = c(0.699,0.58,0.267,0.370,0.103,0.130),
             y97.5 = c(0.764,0.61,0.312,0.414,0.146,0.173))

ggplot(df, aes(x = Source, fill = Group)) +
  geom_boxplot(aes(ymin = y2.5, lower = y25,
                   middle = y50, upper = y75,
                   ymax = y97.5),
               stat = "identity")

ggplot(df, aes(x = Source, color = Group)) +
  geom_boxplot(aes(ymin = y2.5, lower = y25,
                   middle = y50, upper = y75,
                   ymax = y97.5),
               stat = "identity")

ggplot(df, aes(x = interaction(Group, Source))) +
  geom_boxplot(aes(ymin = y2.5, lower = y25,
                   middle = y50, upper = y75,
                   ymax = y97.5),
               stat = "identity")

ggplot(df, aes(Source))+
  geom_boxplot(aes(ymin = y2.5, lower = y25, middle = y50, upper = y75, ymax = y97.5, 
                   group = interaction(Source, Group)),
               stat = "identity")
#############
library(tidyverse)
set.seed(123)
df <- data.frame(
  class_id = c(rep("a", 6), rep("b", 6)),
  student_id = c(rep(1, 3), rep(2, 2), rep(3, 1), rep(4, 2), rep(5, 3), rep(6, 1)),
  value = rnorm(12)
)

df %>% 
  group_by(class_id, student_id)  %>% 
  summarise(student_mean = mean(value)) %>%
  mutate(class_mean_exc_this_student = (
    sum(student_mean) - student_mean)/(n() - 1)
    )

##############
library(tidyverse)
#remotes::install_github("Financial-Times/ftplottools")
library(ftplottools)
library(extrafont)
#font_import()
#fonts()

covid %>%
  ggplot() +
  geom_line(aes(x = date, y = deaths_roll7_100k,
                group = Province_State, color = Province_State)) +
  geom_text(data = . %>% filter(date == max(date)),
            aes(color = Province_State, x = as.Date(Inf),
                y = deaths_roll7_100k),
            hjust = 0, size = 4, vjust = 0.7,
            label = c("Arizona\n", "North Carolina")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  ft_theme(base_family = "Arimo for Powerline") +
  theme(plot.margin = unit(c(1,6,1,1), "lines"),
        legend.position = "none",
        plot.background = element_rect(fill = "#FFF1E6"),
        axis.title = element_blank(),
        panel.grid.major.x = element_line(colour = "gray75"),
        plot.caption = element_text(size = 8, color = "gray50")) +
  scale_color_manual(values = c("#E85D8C", "#0D5696")) +
  scale_x_date(breaks = seq.Date(from = as.Date('2020-09-01'),
                                 to = as.Date('2021-07-01'),
                                 by = "1 month"),
               limits = as.Date(c("2020-09-01", "2021-07-01")),
               date_labels = "%b\n%Y") +
  scale_y_continuous(breaks = seq(from = 0, to = 2.4, by = 0.2)) +
  labs(title = "New deaths attributed to Covid-19 in North Carolina and Arizona",
       subtitle = "Seven-day rolling average of new deaths (per 100k)\n",
       caption = "Source: Analysis of data from John Hopkins SSE\nUpdated: 12th July 2021 | CCBY4.0")
################
library(tidyverse)
library(lubridate)

date <- c(today()+1, today()+2, today()+3, today()+4, today()+5, today()+6)
precipitation <- c(0, 0, 35, 0, 0, 35)
weatherdataframe <- data.frame(date = date, precipitation = precipitation)
date2 <- c(today()-4, today()-7)
flag2 <- c("35mm24hrs", "10mm1hr")
emaillog <- data.frame(date2, flag2)

ifelse(
  any((weatherdataframe$precipitation >= 35) & (weatherdataframe$date <= (today() + 3)))
  && 
  any((emaillog$date2 <= (today() -7)), (length(unlist(emaillog$date2)) == 0)),
  paste("flagged"),
  paste("nothing flagged")
  )
################
library(tidyverse)
df <- tibble::tribble(
     ~Item, ~Qty,    ~time_loc,
   "Apple",   2L, "15/07/2021",
   "Apple",   1L, "18/07/2021",
   "Apple",   1L, "19/07/2021",
  "Banana",   3L, "15/07/2021",
  "Banana",   4L, "18/07/2021"
  )

df %>% 
  group_by(Item) %>% 
  summarize(Qty = sum(Qty),time_loc = min(time_loc)) %>% 
  mutate(Flag = if_else(Qty > 5, "Yes","No"))
#############
library(tidyverse)

sub_funnel_data <- structure(list(funnelstep = structure(1:5, .Label = c("Sessions", 
                                                                         "Engaged Sessions", "Subscription Funnel - Arrives on Enter Email", 
                                                                         "Subscription Funnel - Arrives on Payment Details", "Direct to Paid"), 
                                                         class = "factor"), N = c(92853L, 33107L, 3469L, 3149L, 113L),
                                  Drop = c(NA, 0.356552830818606, 0.104781466155194, 0.907754396079562, 
                                           0.0358844077484916), Rate = c(1, 0.356552830818606, 0.0373601283749583, 
                                                                         0.0339138207704651, 0.00121697737283663)),
                             row.names = c(NA, 5L), class = c("tbl_df", "tbl", "data.frame"))

sub_funnel_data %>% mutate(End = lag(N),
                           xpos = 1:n() + 0.5,
                           Diff = End - N,
                           Percent = paste("\u2190 ", round(Diff / End * 100, 1), "% drop", sep = ""),
                           Rate = paste("(", round(Rate * 100, 1), "%", " of total Sessions)", sep = "")) %>% 
  ggplot(aes(x = reorder(funnelstep, desc(funnelstep)), y = N)) +
  geom_bar(stat = 'identity', fill = '#39cccc') +
  stat_summary(aes(label = paste("n = ", scales::comma(..y..), sep = "")),
               fun = "sum", geom = "text",
               col = "black", vjust = 0,
               hjust = -0.05, size = 4) +
  geom_segment(aes(x = rev(xpos), y = End, xend = rev(xpos), yend = N * 1.1)) +
  geom_text(aes(x = rev(xpos - 0.1), y = End - Diff / 1, label = Percent), hjust = -0.2) +
  geom_text(aes(x = rev(xpos - 0.7), y = End - Diff, label = Rate), color = "black", hjust = 0) +
  coord_flip()
##############
library(data.table)

df <- data.frame(X1 = rep(1:50, each = 2),
                 X2 = rep(x = 1:2, times = 50),
                 X3 = rep(x = 1:2, times = 50),
                 X4 = rep(x = 1:2, times = 50),
                 X5 = rep(x = 1:2, times = 50),
                 X6 = rep(x = 1:2, times = 50),
                 X7 = rep(x = 1:2, times = 50),
                 X8 = rep(x = 1:2, times = 50),
                 X9 = rep(x = 1:2, times = 50),
                 X10 = rep(x = 1:2, times = 50)
                 )
setDT(df)
head(df)

df2 <- df[ ,lapply(.SD, mean), by = X1, .SDcols = X2:X10]
head(df2)


#########
library(tidyverse)
library(grid)

png(filename = "example.png", width = 480, height = 480)
ggplot(mtcars) + geom_point(aes(cyl,mpg, color = cyl)) +
  scale_color_continuous(labels = c(expression(underline(" Above 65 & over")),
                                    expression(underline("45 - 64")),
                                    expression(underline("25 - 44")),
                                    expression(underline("15 - 24")),
                                    expression(underline("Under 15"))))
grid.lines(x = c(0.89, 0.98), y = 0.592,
           arrow = arrow(length = unit(1.5, "mm"), ends = "first"))
grid.lines(x = c(0.91, 0.98), y = 0.547,
           arrow = arrow(length = unit(1.5, "mm"), ends = "first"))
grid.lines(x = c(0.91, 0.98), y = 0.502,
           arrow = arrow(length = unit(1.5, "mm"), ends = "first"))
grid.lines(x = c(0.91, 0.98), y = 0.457,
           arrow = arrow(length = unit(1.5, "mm"), ends = "first"))
grid.lines(x = c(0.831, 0.98), y = 0.414,
           arrow = arrow(length = unit(1.5, "mm"), ends = "first"))
grid.gedit("GRID.line", gp = gpar(lwd = 2))
dev.off()

#############
library(tidyverse)

     