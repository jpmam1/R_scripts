library(tidyverse)
library(rstatix)

diffexpr <- structure(list(...1 = c(1, 2, 3, 4, 5, 6), Gene = c("ML087114a", 
                                                    "ML20265a", "ML463533a", "ML085213a", "ML01433a", "ML01248a"), 
               `aboral-1` = c(2.822658847, 2.822658847, 8.467976541, 47.04431412, 
                              212.6402998, 9.408862823), `aboral-2` = c(7.984581931, 13.30763655, 
                                                                        20.40504271, 90.49192855, 333.5780896, 4.435878851), `aboral-3` = c(1.712278262, 
                                                                                                                                            6.849113047, 9.41753044, 42.80695655, 415.2274785, 7.705252178
                                                                        ), `aboral-4` = c(0.983752211, 18.69129201, 12.78877874, 
                                                                                          52.13886718, 471.2173091, 13.77253095), `oral-1` = c(17974.26123, 
                                                                                                                                               1740.952875, 547.3137791, 1902.305011, 14912.23774, 443.7183737
                                                                                          ), `oral-2` = c(20852.10726, 1234.05124, 515.7229063, 2200.489964, 
                                                                                                          12982.99913, 430.1302391), `oral-3` = c(42894.61215, 2117.228589, 
                                                                                                                                                  692.7337779, 3194.136909, 22573.60827, 473.205468), `oral-4` = c(39617.46967, 
                                                                                                                                                                                                                   1759.240408, 558.5421067, 2592.036723, 26045.23093, 367.9019864
                                                                                                                                                  )), row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                  ))

diffexpr %>%
  select(-...1) %>%
  pivot_longer(-Gene) %>%
  mutate(group = str_remove(name, "-.")) %>%
  group_by(Gene) %>%
  t_test(value ~ group)


c(t(diffexpr[1,c(7:10)]))
nrow(diffexpr)
diffexpr[,c(3:6)]

sapply(1:nrow(diffexpr), \(x) t.test(as.numeric(as.character(unlist(diffexpr[x,c(3:6)]), as.numeric(as.character(unlist(diffexpr[x,c(7:10)]))))))$p.value)

#################
library(BiocManager)
# BiocManager::install(c("biomaRt", "circlize", "ComplexHeatmap", "corrplot", "DESeq2",
#                        "dplyr", "DT", "edgeR", "ggplot2", "limma", "lsmeans", "reshape2",
#                        "spatstat", "survival", "plyr"))
#install(c("genefilter", "geneplotter"))
#install.packages("DESeq_1.36.0.tar.gz", repos = NULL)
#install.packages("IMvigor210CoreBiologies_1.0.1.tar.gz", repos = NULL)
library(IMvigor210CoreBiologies)
data("fmone")
clinical_data <- fmone@phenoData@data

#################
library(tidyverse)
library(ComplexHeatmap)

mat <- df_NOKRAS %>%
  separate(col = 14, sep = ",", into = paste0("mutation_", 1:21)) %>%
  pivot_longer(starts_with("mutation_")) %>%
  select(-name) %>%
  filter(!is.na(value)) %>%
  group_by(PNum) %>%
  mutate(gene = str_extract(value, "([^ ]*)")) %>%
  select(c(PNum, gene)) %>%
  mutate(fill_val = 1) %>%
  pivot_wider(id_cols = PNum,
              names_from = gene,
              values_from = fill_val,
              values_fn = sum,
              values_fill = 0) %>%
  t()
colnames(mat) <- mat[1,]
mat <- mat[-1,]

alter_fun = list(
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "#CCCCCC", col = NA))
  },
  "0" = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "#CCCCCC", col = NA))
    },
  "1" = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "firebrick3", col = NA))
  },
  "2" = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "darkred", col = NA))
  },
  "3" = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "black", col = NA))
  }
)
column_title = "OncoPrint for KRAS-wt Patients"
heatmap_legend_param = list(title = "Number of\nAlterations")
ht_list = oncoPrint(mat, alter_fun = alter_fun,
                    column_title = column_title,
                    heatmap_legend_param = heatmap_legend_param,
                    show_column_names = TRUE,
                    remove_empty_columns = TRUE,
                    remove_empty_rows = TRUE,
                    pct_side = "right",
                    row_names_side = "left")
draw(ht_list)

#################
library(forestplot)
adjusted_rr <- c(
  NA, 1.00, 0.77, 1.46, 0.79, 0.92, NA,
  NA, 1.00, 0.69, 0.67, 0.94, NA,
  NA, 1.00, 1.21, 0.98, NA,
  NA, 1.17, 1.21, 1.09, 1.23, 1.21, 1.00, NA,
  NA, 1.00, 1.24, 1.22, 1.11, 1.28, 1.24, 0.97, 1.03, NA,
  NA, 1.20, 1.15, 1.00, NA,
  NA, 1.10, 1.14, 1.09, 1.02, 1.00, NA,
  NA, 1.00, 0.86, NA,
  NA, 1.00, 0.88, 0.87, 0.84, 0.97, NA
)

lower_ci <- c(
  NA, 1.00, 0.70, 1.38, 0.72, 0.78, NA,
  NA, 1.00, 0.61, 0.61, 0.88, NA,
  NA, 1.00, 1.15, 0.85, NA,
  NA, 1.09, 1.07, 0.96, 1.07, 1.10, 1.00, NA,
  NA, 1.00, 1.01, 1.12, 0.94, 1.12, 1.13, 0.78, 0.91, NA,
  NA, 1.13, 1.05, 1.00, NA,
  NA, 1.01, 1.06, 1.00, 0.94, 1.00, NA,
  NA, 1.00, 0.78, NA,
  NA, 1.00, 0.81, 0.80, 0.77, 0.90, NA
)

upper_ci <- c(
  NA, 1.00, 0.85, 1.55, 0.87, 1.08, NA,
  NA, 1.00, 0.78, 0.73, 0.99, NA,
  NA, 1.00, 1.29, 1.12, NA,
  NA, 1.27, 1.38, 1.25, 1.41, 1.34, 1.00, NA,
  NA, 1.00, 1.53, 1.34, 1.32, 1.46, 1.35, 1.21, 1.16, NA,
  NA, 1.27, 1.26, 1.00, NA,
  NA, 1.19, 1.24, 1.18, 1.11, 1.00, NA,
  NA, 1.00, 0.95, NA,
  NA, 1.00, 0.95, 0.94, 0.91, 1.04, NA
)

labels <- rep("label_tmp", 59)

# Forest plot code
forestplot(labeltext = labels,
           mean = adjusted_rr, 
           lower = lower_ci, 
           upper = upper_ci,
           zero = 1.0,  # Center the plot at 1.0
           xlog = FALSE,
           xlab = "Adjusted Rate Ratio",
           boxsize = 0.2,
           is.summary = is.na(adjusted_rr),
           xticks = c(0.5, 1.0, 1.5, 2.0),  # Specify tick marks
           line.margin = 0.4,  # Compress horizontally
           graph.pos = 2, # Adjust vertical spacing
           new_page = TRUE,
           txt_gp = fpTxtGp(label = gpar(cex = 0.8), 
                            ticks = gpar(cex = 0.8), 
                            xlab = gpar(cex = 1)),
           grid = TRUE)


my_ticks <- c(0.5, 1.0, 1.5, 2.0)
attr(my_ticks, "labels") <- c("0.5", "1.0", "1.5", "2.0")

# Forest plot code
forestplot(labeltext = labels,
           mean = adjusted_rr, 
           lower = lower_ci, 
           upper = upper_ci,
           zero = 1.0,  # Center the plot at 1.0
           xlog = FALSE,
           xlab = "Adjusted Rate Ratio",
           boxsize = 0.2,
           is.summary = is.na(adjusted_rr),
           xticks = my_ticks,  # Specify tick marks
           line.margin = 0.4,  # Compress horizontally
           graph.pos = 2, # Adjust vertical spacing
           new_page = TRUE,
           txt_gp = fpTxtGp(label = gpar(cex = 0.8), 
                            ticks = gpar(cex = 0.8), 
                            xlab = gpar(cex = 1)),
           grid = TRUE)

#################
library(tidyverse)
#install.packages("AER")
library(AER)
library(MASS)
library(Hmisc)


# Custom function to calculate midpoints
midpoints <- function(x, dp = 2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, dp))
}

# Loading the dataset
data("DoctorVisits")

# The quasi-poisson model
mod_qp <- glm(visits ~ illness + reduced + gender + private,
              data = DoctorVisits,
              family = "quasipoisson")


# The negative binomial model
mod_nb <- MASS::glm.nb(visits ~ illness + reduced + gender + private,
                       data = DoctorVisits)

# Plot
## Creating the data points
nb_data_points <- DoctorVisits |> 
  mutate(res_nb = (visits-predict(mod_nb, type = "response"))^2) |> 
  mutate(group = cut_interval(res_nb, n = 10),
         visits = midpoints(group)) |> 
  group_by(visits) |> 
  summarise(res_nb = mean(res_nb),
            n = n())

qp_data_points <- DoctorVisits |> 
  mutate(res_qp = (visits-predict(mod_qp, type = "response"))^2) |> 
  mutate(group = cut_interval(res_qp, n = 10),
         visits = midpoints(group)) |> 
  group_by(visits) |> 
  summarise(res_qp = mean(res_qp),
            n = n())


## Parameters from negative binomial and quasi-poisson models
theta_qp <- summary(mod_qp)$dispersion
theta_nb <- mod_nb$theta

means <- seq(0, 60, by = 0.02)

lines <- data.frame(means = means,
                    quasipoisson = means * theta_qp,
                    negbin = means + means^2/theta_nb)

## Plot points and lines
ggplot() +
  geom_point(data = nb_data_points,
             aes(x = visits, y = res_nb, size = n)) +
  geom_point(data = qp_data_points,
             aes(x = visits, y = res_qp, size = n),
             shape = 1) +
  geom_line(data = lines, aes(y = negbin, x = means)) +
  geom_line(data = lines, aes(y = quasipoisson, x = means),
            linetype = "dashed") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 60))

#################
library(data.table)
library(RcppRoll)

dt <- structure(
  list(
    A = 1:5,
    B = c(15L, 9L, 12L, 1L, 11L),
    C = c(14L, 9L, 11L, 19L, 13L)
  ),
  row.names = c(NA, -5L),
  class = c("data.table", "data.frame")
)

dt2 <- rbind(dt, matrix(0, nrow(dt), length(dt), dimnames = list(dt$A, names(dt))))

dt2[,lapply(.SD, \(x) roll_sum(x, n = nrow(dt), fill = 0, align = "left")),,.SDcols = 2:3][1:nrow(dt)]

################
library(data.table)

dt <- fread(cmd = "awk 'BEGIN{FS=\",\"} NR == 1 || $2 ~ \"2\"' ~/Desktop/test*.csv")
dt

dt2 <- fread(cmd = "grep -h \"2\" ~/Desktop/test*.csv")
dt2

#################
#install.packages("JM")
library(JM)

# download and extract https://github.com/drizopoulos/JM/files/2373350/data.zip
df1 <- read.csv("~/Desktop/data/data_sm_wide.csv")
df2 <- read.csv("~/Desktop/data/data_sm.csv")

ctrl <- lmeControl(opt='optim')
bloodfit <- lme(Age ~ tstart + tstart:Urate, random=~tstart|ID, method="REML",
                control=ctrl, data=df2)
summary(bloodfit)

coxphobject <- coxph(Surv(time=duration, event=cstatus)~Urate,
                     data=as.data.frame(df1), x=TRUE)
summary(coxphobject)

# Joint Model
jmfit <- jointModel(lmeObject = bloodfit, survObject = coxphobject,
                    timeVar = "tstart", verbose=T)
summary(jmfit)


df2$tstart2 <- df2$tstart / 364
df1$duration2 <- df1$duration / 364
ctrl <- lmeControl(opt='optim')
bloodfit <- lme(Age ~ tstart2 + tstart2:Urate, random=~tstart2|ID, method="REML",
                control=ctrl, data=df2)
summary(bloodfit)

coxphobject <- coxph(Surv(time=duration2, event=cstatus)~Urate,
                     data=na.omit(df1), x=TRUE)
summary(coxphobject)

# Joint Model
jmfit <- jointModel(lmeObject = bloodfit, survObject = coxphobject,
                    timeVar = "tstart2", verbose=T)
summary(jmfit)

#################
library(tidyverse)
#devtools::install_github("daewoooo/SVbyEye", branch = "master")
library(SVbyEye)

#################
library(tidyverse)
library(ggpubr)
library(ggstats)
library(patchwork)
var_levels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")

likert_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Neither agree nor disagree",
  "Agree",
  "Strongly agree"
)

# Set seed for reproducibility
set.seed(42)

# Create the dataframe with three Likert response columns
df <- tibble(
  var = sample(var_levels, 50, replace = TRUE),
  # Random values from A to Q
  val1 = sample(likert_levels, 50, replace = TRUE) # Random values from Likert levels
  
)

# View the first few rows of the dataframe
print(df)

dat <- df |>
  mutate(across(-var, ~ factor(.x, likert_levels))) |>
  pivot_longer(-var, names_to = "group") |>
  count(var, value, group) |>
  complete(var, value, group, fill = list(n = 0)) |>
  mutate(
    prop = n / sum(n),
    prop_lower = sum(prop[value %in% c("Strongly disagree", "Disagree")]),
    prop_higher = sum(prop[value %in% c("Strongly agree", "Agree")]),
    .by = c(var, group)
  ) |>
  arrange(group, prop_lower) |>
  mutate(y_sort = paste(var, group, sep = "."),
         y_sort = fct_inorder(y_sort))

top10 <- dat |>
  distinct(group, var, prop_lower) |>
  slice_max(prop_lower, n = 10, by = group)

dat <- dat |>
  semi_join(top10)

dat_tot <- dat |>
  distinct(group, var, y_sort, prop_lower, prop_higher) |>
  pivot_longer(-c(group, var, y_sort),
               names_to = c(".value", "name"),
               names_sep = "_") |>
  mutate(hjust_tot = ifelse(name == "lower", 1, 0),
         x_tot = ifelse(name == "lower", -1, 1))

bar_plot <- dat %>%
  select(var, n) %>%
  group_by(var) %>%
  summarise(count = sum(n)) %>%
  full_join(dat) %>%
  select(y_sort, count) %>%
  unique() %>%
  ggplot(., aes(y = y_sort, x = count)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  labs(x = "Response Count", y = "") +
  geom_text(aes(label = count), position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    # Remove x-axis text
    axis.ticks.x = element_blank()    # Remove x-axis ticks
  )

likert_plot <- dat %>%
  ggplot(aes(y = y_sort, x = prop, fill = value)) +
  geom_col(position = position_likert(reverse = FALSE)) +
  geom_text(
    aes(
      label = label_percent_abs(hide_below = .05, accuracy = 1)(prop),
      color = after_scale(hex_bw(.data$fill))
    ),
    position = position_likert(vjust = 0.5, reverse = FALSE),
    size = 3.5
  ) +
  geom_label(
    aes(
      x = x_tot,
      label = label_percent_abs(accuracy = 1)(prop),
      hjust = hjust_tot,
      fill = NULL
    ),
    data = dat_tot,
    size = 3.5,
    color = "black",
    fontface = "bold",
    label.size = 0,
    show.legend = FALSE
  ) +
  scale_y_discrete(labels = \(x) gsub("\\..*$", "", x)) +
  scale_x_continuous(labels = label_percent_abs(), expand = c(0, .15)) +
  scale_fill_brewer(palette = "BrBG") +
  facet_wrap( ~ group,
              scales = "free_y",
              ncol = 1,
              strip.position = "right") +
  theme_light() +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL)

bar_plot + likert_plot + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

bar_plot + likert_plot + plot_layout(guides = "collect", widths = c(0.2, 0.8)) &
  theme(legend.position = "bottom")

#################
library(tidyverse)

df <- read.table(text = "GeneId	Sample1_Cat1	Sample2_Cat2	Sample3_Cat2	Sample4_Cat1
GeneA	1.2	1.3	2.1	2.3
GeneB	2.3	3.1	3.4	3.6
GeneC	5.6	5.3	6.1	5.1
GeneD	8.9	7.8	6.7	7.2", header = TRUE)

sample_names <- colnames(df)
groups <- ifelse(grepl('Cat1', sample_names), 'Cat1', 'Cat2')
group_factor <- factor(groups, levels = c('Cat1', 'Cat2'))
design <- model.matrix(~group_factor, data = df)
design

model.matrix(~0+group_factor)
# https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/designmatrices.html

#################
reprex::reprex({
library(survival)
# install.packages("survminer")
library(survminer)
vet2 <-survSplit(Surv(time, status) ~., data= veteran, cut=c(90, 180), episode= "tgroup", id="id")
vfit2 <-coxph(Surv(tstart, time, status) ~ trt + prior + karno:strata(tgroup),data=vet2)
ggadjustedcurves(vfit2, data = vet2, variable = "karno",
                 ggtheme = theme_survminer(base_size = 18))
}, std_out_err = TRUE)


library(survival)
library(survminer)

vfit <- coxph(Surv(time, status) ~ ., data = veteran)
prop <- cox.zph(vfit)
prop
plot(prop[3])
abline(h = vfit$coefficients[5], lwd = 2, lty = 2, col = "firebrick3")

vet2 <-survSplit(Surv(time, status) ~.,
                 data = veteran, cut = c(30, 60),
                 episode = "tgroup", id = "id")
vfit2 <-coxph(Surv(tstart, time, status) ~ trt + prior + karno:strata(tgroup),
              data = vet2)
vfit2
prop2 <- cox.zph(vfit2)
prop2
plot(prop2[3])

plot(survfit(vfit2, newdata = vet2))

## tt() "log(time + 20)" approach
dtimes <- sort(unique(veteran$time[veteran$status==1]))
vet3 <-survSplit(Surv(time, status) ~.,
                 data = veteran, cut = dtimes,
                 episode = "tgroup", id = "id")
vet3$time_var_karno <- vet3$karno * log(vet3$time+20)
vfit4 <- coxph(Surv(tstart,time,status) ~ trt + prior + karno + time_var_karno, data = vet3)

plotdata <- data.frame( 
  tstart = c(0, dtimes[1:96]),
  time = dtimes,
  trt = rep(0, 97),
  prior = rep(c(0, 5, 10), length.out = 97),
  karno = rep(30, 97),
  tgroup = rep(c(1,2,3), length.out = 97),
  status = rep(0, 97)
)

plotdata$time_var_karno <- plotdata$karno * log(plotdata$time + 20)
plotdata2 <- plotdata
plotdata2$karno <- 60
plotdata2$time_var_karno <- plotdata2$karno * log(plotdata2$time + 20)
plotdata3 <- plotdata
plotdata3$karno <- 90
plotdata3$time_var_karno <- plotdata3$karno * log(plotdata3$time + 20)

plotdata$id <- "1"
plotdata2$id <- "2"
plotdata3$id <- "3"

col_pal <- viridisLite::viridis(n = 4)
plot(survfit(vfit4, newdata = rbind(plotdata,
                                    plotdata2,
                                    plotdata3),
             id = id, se.fit = FALSE),
     bty = "n", xlab = "Days",
     ylab = "Survival probability",
     col = col_pal[1:3])
text(160,0.15,"karno = 30", col = col_pal[1])
text(200,0.4,"karno = 60", col = col_pal[2])
text(240,0.6,"karno = 90", col = col_pal[3])

# overlay
plot(survfit(vfit2, newdata = vet2))
lines(survfit(vfit4, newdata = rbind(plotdata,
                                     plotdata2,
                                     plotdata3),
              id = id, se.fit = FALSE),
      bty = "n", xlab = "Days",
      ylab = "Survival probability",
      col = col_pal[1:3],
      lwd = 3)

###################
library(tidyverse)

df <- iris %>%
  group_by(Species) %>%
  mutate(val = Petal.Length * Petal.Width,
         sex = sample(c("Male", "Female"), n(),
                      replace = TRUE),
         Species = factor(Species))
df %>%
  ggplot(aes(x = reorder(Species, -val),
             y = val, fill = Species))+
  geom_bar(stat = 'identity') +
  facet_wrap(~sex, ncol = 1) +
  theme_classic()+
  labs(x = 'Location', y = 'Total DALYs attributable to dietary risks',
       title = '2021', fill = 'GI cancer') +
  coord_flip() +
  scale_fill_manual(values = c("setosa" = "#0078B2",
                               "versicolor" = "#13A036",
                               "virginica" = "#ED2128"))

df %>%
  ggplot(aes(x = reorder(Species, -val),
             y = val, fill = interaction(sex, Species, sep = "/"))) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.92)) +
  theme_classic()+
  labs(x = 'Location', y = 'Total DALYs attributable to dietary risks',
       title = '2021', fill = 'GI cancer') +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")

###################
library(tidyverse)
library(cowplot)

set.seed(1)
logit_results <- data.frame(
  outcome  = rep(c("Time_Sport", "Money_Sport"), each = 8),
  term     = rep(c("(Intercept)", "Var1", "Var2", "Var3"), times = 4),
  estimate = rnorm(16),
  treat    = rep(c("A", "B"), each = 4, times = 2)
)
condition_labels <- c("A" = "Treatment A", "B" = "Treatment B")

list_of_results <- logit_results %>%
  filter(term != "(Intercept)") %>%
  mutate(variable_name = case_when(
    outcome == "Time_Sport" ~ "Time for sports",
    outcome == "Money_Sport" ~ "Money for sports",
    TRUE ~ NA
    )) %>%
  group_split(outcome) %>%
  map(~{ggplot(data = .x, aes(x = estimate, y = term)) +
      geom_point(aes(color = treat), 
                 size = 2) +
      facet_wrap(~ treat, nrow = 2,
                 labeller = as_labeller(condition_labels)) + 
      labs(title = .x$variable_name)
  })

plot_grid(plotlist = list_of_results)

###################
library(tidyverse)

df <- read.table(text = "Sample	var1	var2	var3	var4	var5	var6	var7	var8
001	0.9	0.001	154	19	0.15	0.06	0.34	1.54
002	1	0.0006	166	2.6	0.02	0.005	0.017	0.99
003	1	0	151	2	0.005	0.1	0.02	2.5
004	0.8	0.015	124	1.7	0.02	0.008	0.01	0.2", header = TRUE)

df

df_long <- df %>% pivot_longer(-Sample)
df_long

ggplot(df_long, (aes(x = Sample, y = rev(name), size = value))) +
  geom_point() +
  scale_size_continuous(range = c(1,10))

ggplot(df_long, (aes(x = Sample, y = rev(name), size = value))) +
  geom_point() +
  scale_size_continuous(range = c(1,4))

# you can also specify the dimensions of the final figure when
# you save it, and make it wider / higher so the dots don't overlap
ggsave("Figure_1.png", height = 8, width = 6)

results <- df_long %>%
  group_split(name) %>%
  map(~{ggplot(data = ., aes(x = name, y = Sample,
                             size = value)) +
      geom_point(fill = "deepskyblue",
                 shape = 21) +
      scale_size_continuous(range = c(1,7)) +
      facet_wrap(~name) +
      theme_bw(base_size = 16) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "right")
    })
for (i in c(2:4, 6:8)){
results[[i]] <- results[[i]] +
  theme(axis.title.y = element_blank())
}

library(cowplot)
plot_grid(plotlist = results, nrow = 2)

###################
library(tidyverse)
#install.packages("modelsummary")
library(modelsummary)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
dat <- read.csv(url)

# rescale mm -> cm
dat$bill_length_cm <- dat$bill_length_mm / 10
dat$flipper_length_cm <- dat$flipper_length_mm / 10

mod <- lm(bill_length_cm ~ flipper_length_cm + species, data = dat)

modelplot(mod) +
  geom_pointrange(aes(x = estimate, y = term,
                      xmin = conf.low, xmax = conf.high),
                  linewidth = 2, fatten = 8)

###################
library(tidyverse)

str(mtcars)

# make cyl a factor
df <- mtcars
df$cyl <- factor(df$cyl)
modelsummary::datasummary_skim(type = "categorical", data = df)

###################
library(tidyverse)
library(ggpubr)

df <- read.table(text = "rn  P_treatment  Reproductive        Total_Phosphorus
  1 Low          Non-brooding               0.000321 
  2 Intermediate Brooding_w_embryos         0.000733 
  3 Intermediate Brooding_wo_embryos        0.000341 
  4 Low          Brooding_w_embryos         0.00101  
  5 Low          Brooding_wo_embryos        0.000410 
  6 Intermediate Non-brooding               0.000497 
  7 Intermediate Brooding_w_embryos         0.000697 
  8 Intermediate Brooding_wo_embryos        0.000415 
  9 Low          Brooding_w_embryos         0.000491 
 10 Low          Brooding_w_embryos         0.000863 
 11 Low          Brooding_wo_embryos        0.000273 
 12 Low          Brooding_wo_embryos        0.000365 
 13 Low          Non-brooding               0.000556 
 14 Low          Non-brooding               0.000317 
 15 Intermediate Brooding_w_embryos         0.000804 
 16 Intermediate Brooding_wo_embryos        0.000498 
 17 Low          Brooding_w_embryos         0.000708 
 18 Low          Brooding_wo_embryos        0.000402 
 19 Low          Brooding_wo_embryos        0.000398 
 20 Low          Brooding_w_embryos         0.000628 
 21 Low          Brooding_wo_embryos        0.000315 
 22 Low          Non-brooding               0.000619 
 23 Intermediate Brooding_w_embryos         0.000674 
 24 Intermediate Brooding_wo_embryos        0.000276 
 25 Intermediate Brooding_w_embryos         0.000899 
 26 Intermediate Brooding_wo_embryos        0.000420 
 27 Intermediate Non-brooding               0.000468 
 28 Low          Brooding_w_embryos         0.000715 
 29 Low          Brooding_w_embryos         0.000842 
 30 Low          Brooding_wo_embryos        0.000443 
 31 Low          Brooding_wo_embryos        0.000517 
 32 Low          Brooding_w_embryos         0.000843 
 33 Low          Brooding_wo_embryos        0.000589 
 34 Low          Brooding_w_embryos         0.00100  
 35 Low          Brooding_wo_embryos        0.000470 
 36 Intermediate Brooding_w_embryos         0.000573 
 37 Intermediate Brooding_w_embryos         0.000785 
 38 Intermediate Brooding_wo_embryos        0.000162 
 39 Intermediate Brooding_wo_embryos        0.000507 
 40 Low          Non-brooding               0.000249 
 41 Intermediate Brooding_w_embryos         0.00078  
 42 Intermediate Brooding_wo_embryos        0.000450 
 43 Low          Non-brooding               0.000327 
 44 Low          Non-brooding               0.000322 
 45 Low          Brooding_w_embryos         0.00091  
 46 Low          Brooding_wo_embryos        0.000480 
 47 Intermediate Brooding_w_embryos         0.000954 
 48 Intermediate Brooding_wo_embryos        0.000472 
 49 Intermediate Non-brooding               0.000461 
 50 Low          Brooding_w_embryos         0.00024  
 51 Low          Brooding_w_embryos         0.000749 
 52 Low          Brooding_w_embryos         0.000713 
 53 Low          Brooding_wo_embryos        0.000122 
 54 Low          Brooding_wo_embryos        0.000336 
 55 Low          Brooding_wo_embryos        0.000419 
 56 Low          Non-brooding               0.000319 
 57 Low          Non-brooding               0.000301 
 58 Intermediate Non-brooding               0.000341 
 59 Low          Non-brooding               0.000733 
 60 Intermediate Brooding_w_embryos         0.000688 
 61 Intermediate Brooding_w_embryos         0.000828 
 62 Intermediate Brooding_wo_embryos        0.0000546
 63 Intermediate Brooding_wo_embryos        0.000341 
 64 Intermediate Non-brooding               0.000501 
 65 Intermediate Non-brooding               0.000367 
 66 Low          Brooding_w_embryos         0.00078  
 67 Low          Brooding_wo_embryos        0.000522 
 68 Low          Non-brooding               0.000560 
 69 Intermediate Brooding_wo_embryos        0.000329 
 70 Intermediate Non-brooding               0.000340 
 71 Intermediate Non-brooding               0.000438 
 72 Intermediate Non-brooding               0.000304 
 73 Low          Non-brooding               0.000291 
 74 Low          Brooding_wo_embryos        0.000507 
 75 Low          Non-brooding               0.000462 
 76 Intermediate Non-brooding               0.000127 
 77 Intermediate Brooding_w_embryos         0.000804 
 78 Intermediate Brooding_w_embryos         0.000446 
 79 Intermediate Brooding_w_embryos         0.000747 
 80 Intermediate Brooding_wo_embryos        0.000297 
 81 Intermediate Brooding_wo_embryos        0.000308 
 82 Intermediate Brooding_wo_embryos        0.000348 
 83 Low          Non-brooding               0.000500 
 84 Low          Brooding_wo_embryos        0.000512 
 85 Intermediate Brooding_w_embryos         0.000822 
 86 Intermediate Brooding_w_embryos         0.00108  
 87 Intermediate Brooding_wo_embryos        0.000436 
 88 Intermediate Brooding_wo_embryos        0.000560 
 89 Intermediate Brooding_w_embryos         0.000871 
 90 Intermediate Brooding_wo_embryos        0.000265 
 91 Low          Brooding_w_embryos         0.000485 
 92 Low          Brooding_wo_embryos        0.000241 
 93 Low          Non-brooding               0.000608 
 94 Low          Non-brooding               0.000233 
 95 Low          Non-brooding               0.000481 
 96 Low          Brooding_w_embryos         0.00106  
 97 Low          Brooding_wo_embryos        0.000596 
 98 Intermediate Non-brooding               0.000254 
 99 Low          Non-brooding               0.000596 
100 Low          Non-brooding               0.000481 
101 Low          Brooding_w_embryos         0.000588 
102 Low          Brooding_w_embryos         0.000574 
103 Low          Brooding_w_embryos         0.00132  
104 Low          Brooding_wo_embryos        0.0000973
105 Low          Brooding_wo_embryos        0.000281 
106 Low          Brooding_wo_embryos        0.000291 
107 Low          Non-brooding               0.000265 
108 Low          Non-brooding               0.000158 
109 Intermediate Brooding_w_embryos         0.00107  
110 Intermediate Brooding_w_embryos         0.000415 
111 Intermediate Brooding_w_embryos         0.000686 
112 Intermediate Brooding_w_embryos         0.000583 
113 Intermediate Brooding_w_embryos         0.000852 
114 Intermediate Brooding_wo_embryos        0.000565 
115 Intermediate Brooding_wo_embryos        0.000137 
116 Intermediate Brooding_wo_embryos        0.000314 
117 Intermediate Brooding_wo_embryos        0.000295 
118 Intermediate Brooding_wo_embryos        0.000355 
119 Intermediate Non-brooding               0.000347 
120 Intermediate Non-brooding               0.000676 
121 Intermediate Non-brooding               0.000347 
122 Low          Brooding_w_embryos         0.000682 
123 Low          Brooding_wo_embryos        0.000298 
124 Low          Non-brooding               0.000639 
125 Low          Non-brooding               0.000368 
126 Intermediate Brooding_w_embryos         0.000974 
127 Intermediate Brooding_wo_embryos        0.000460 
128 Intermediate Non-brooding               0.000340 
129 Intermediate Brooding_w_embryos         0.0006   
130 Intermediate Brooding_w_embryos         0.000706 
131 Intermediate Brooding_wo_embryos        0.000351 
132 Intermediate Brooding_wo_embryos        0.000300 
133 Intermediate Non-brooding               0.000382 
134 Low          Brooding_wo_embryos        0.000422 
135 Low          Non-brooding               0.000405 
136 Low          Non-brooding               0.000300 
137 Low          Brooding_w_embryos         0.000839 
138 Low          Brooding_wo_embryos        0.000580 
139 Intermediate Brooding_w_embryos         0.0007   
140 Intermediate Brooding_wo_embryos        0.000241 
141 Intermediate Non-brooding               0.000413 
142 Intermediate Non-brooding               0.0000462
143 Low          Brooding_w_embryos         0.00106  
144 Low          Brooding_wo_embryos        0.000408 
145 Low          Non-brooding               0.000239 
146 Low          Non-brooding               0.000416 
147 Low          Non-brooding               0.000330 
148 Low          Non-brooding               0.000365 
149 Intermediate Non-brooding               0.000317 
150 Low          Non-brooding               0.000414 
151 Low          Non-brooding               0.000434 
152 Intermediate Non-brooding               0.0000394
153 Intermediate Brooding_w_embryos         0.000722 
154 Intermediate Brooding_wo_embryos        0.000299 
155 Low          Brooding_w_embryos         0.00104  
156 Low          Brooding_wo_embryos        0.000404 
157 Low          Non-brooding               0.000238 
158 Intermediate Brooding_w_embryos         0.000757 
159 Intermediate Brooding_w_embryos         0.000991 
160 Intermediate Brooding_wo_embryos        0.000304 
161 Intermediate Brooding_wo_embryos        0.000332 
162 Intermediate Non-brooding               0.000239 
163 Intermediate Non-brooding               0.000494 
164 Intermediate Non-brooding               0.000269 
165 Low          Brooding_w_embryos         0.000839 
166 Low          Brooding_wo_embryos        0.000392 
167 Low          Non-brooding               0.000433 
168 Low          Non-brooding               0.000506 
169 Intermediate Non-brooding               0.000449 
170 Intermediate Non-brooding               0.000180 
171 Low          Non-brooding               0.000406 
172 Intermediate Brooding_w_embryos         0.000944 
173 Intermediate Brooding_w_embryos         0.000602 
174 Intermediate Brooding_wo_embryos        0.000452 
175 Intermediate Brooding_wo_embryos        0.000331 
176 Intermediate Non-brooding               0.000260 
177 Low          Non-brooding               0.000299 
178 Intermediate Brooding_w_embryos         0.000797 
179 Intermediate Brooding_w_embryos         0.000619 
180 Intermediate Brooding_wo_embryos        0.000391 
181 Intermediate Brooding_wo_embryos        0.000358 
182 Low          Non-brooding               0.000640 
183 Intermediate Brooding_w_embryos         0.000799 
184 Intermediate Brooding_wo_embryos        0.000340 
185 Low          Brooding_w_embryos         0.000779 
186 Low          Brooding_w_embryos         0.000769 
187 Low          Brooding_wo_embryos        0.000304 
188 Low          Brooding_wo_embryos        0.000450 
189 Intermediate Brooding_w_embryos         0.000609 
190 Intermediate Brooding_wo_embryos        0.000376 
191 Intermediate Non-brooding               0.000313 
192 Low          Brooding_wo_embryos        0.000456 
193 Intermediate Brooding_w_embryos         0.000751 
194 Intermediate Brooding_w_embryos         0.000453 
195 Intermediate Brooding_wo_embryos        0.000378 
196 Intermediate Brooding_wo_embryos        0.000200 
197 Intermediate Brooding_w_embryos         0.000563 
198 Intermediate Brooding_wo_embryos        0.000342 
199 Low          Non-brooding               0.000485 
200 Intermediate Brooding_w_embryos         0.00109  
201 Intermediate Brooding_wo_embryos        0.000567 
202 Low          Brooding_w_embryos         0.00096  
203 Low          Brooding_wo_embryos        0.000598 
204 Intermediate Brooding_w_embryos         0.00063  
205 Intermediate Brooding_wo_embryos        0.000408 
206 Low          Brooding_w_embryos         0.00133  
207 Low          Brooding_wo_embryos        0.000517 
208 Intermediate Brooding_w_embryos         0.00108  
209 Intermediate Brooding_wo_embryos        0.000557", header = TRUE)

df$rn <- NULL

stat.test2 <- tibble::tribble(
  ~group1,                            ~group2,                            ~p.adj,
  "Intermediate Non-brooding",        "Low Non-brooding",                 0.6991,
  "Intermediate Non-brooding",        "Intermediate Brooding_wo_embryos", 1.0000,            
  "Intermediate Non-brooding",        "Low Brooding_wo_embryos",          1.0000,
  "Intermediate Non-brooding",        "Intermediate Brooding_w_embryos",  0.0001, 
  "Intermediate Non-brooding",        "Low Brooding_w_embryos",           0.0001,
  "Low Non-brooding",                 "Intermediate Brooding_wo_embryos", 1.0000, 
  "Low Non-brooding",                 "Low Brooding_wo_embryos",          1.0000,
  "Low Non-brooding",                 "Intermediate Brooding_w_embryos",  0.0001,
  "Low Non-brooding",                 "Low Brooding_w_embryos",           0.0001, 
  "Intermediate Brooding_wo_embryos", "Low Brooding_wo_embryos",          1.0000, 
  "Intermediate Brooding_wo_embryos", "Intermediate Brooding_w_embryos",  0.0001,
  "Intermediate Brooding_wo_embryos", "Low Brooding_w_embryos",           0.0001, 
  "Low Brooding_wo_embryos",          "Intermediate Brooding_w_embryos",  0.0001,
  "Low Brooding_wo_embryos",          "Low Brooding_w_embryos",           0.0001,
  "Intermediate Brooding_w_embryos",   "Low Brooding_w_embryos",           1.0000)

ggplot(df, aes(x=interaction(P_treatment, Reproductive, sep = " "),
               y=Total_Phosphorus)) + 
  geom_violin(aes(fill = P_treatment),
              width = 1) +
  geom_boxplot(position = position_dodge(width = 1), width = 0.1, color="black", alpha=0.5) +
  stat_pvalue_manual(stat.test2, y.position = 0.00125, step.increase = 0.1, label = "p.adj") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

###################
library(tidyverse)
library(patchwork)
#install.packages("DescTools")
library(DescTools)
#install.packages("ggside")
library(ggside)

set.seed(321)
# Define parameters
models <- c(1, 2, 3, 10, 11, 12)
metrics <- c(1, 2, 3)
n_repeats <- 144  # Number of times each model-metric combination repeats
# Expand grid to create all combinations of model and metric
dat <- expand.grid(model = models, metric = metrics)
dat <- dat[rep(seq_len(nrow(dat)), n_repeats), ]  # Repeat the rows to match desired total size
# Add a normally distributed 'value' column
dat$value <- rnorm(nrow(dat), 20, 4)

dat2 <- data.frame(matrix(ncol = 3, nrow = 24))
x2 <- c("model", "value", "metric")
colnames(dat2) <- x2
dat2$model <- rep(13, 24)
dat2$value <- rnorm(24,10,.5)
dat2$metric <- rep(c(1,2,3),8)

df <- rbind(dat, dat2)

df <- df %>% 
  mutate(model = factor(model,
                        levels = c("13", "1", "2", "3", "10", "11", "12")),
         metric = factor(metric))

desc.stats <- df %>% 
  group_by(model, metric) %>% 
  summarise(mean = mean (value),
            range.lower = range(value)[1],
            range.upper = range(value)[2],
            median = median(value),
            medianCI.lower = MedianCI(value, conf.level = 0.95, na.rm = FALSE, method = "exact", R = 10000)[2],
            medianCI.upper = MedianCI(value, conf.level = 0.95, na.rm = FALSE, method = "exact", R = 10000)[3])

desc.stats

desc.stats_filtered <- desc.stats %>% 
  filter(model != 13)

p <- ggplot(desc.stats %>% filter(model %in% c(13)),
                                  aes(x=model, y=median, group=metric)) +
  geom_point(aes(shape=metric, colour = metric),
             position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = medianCI.lower, ymax= medianCI.upper,
                    colour = metric), width=.2,
                position = position_dodge(width = 0.2)) +
  scale_colour_manual(values = c("chocolate", "grey20", "blue")) + # Apply colors for fill
  theme_classic() +
  theme(axis.title.x=element_blank(),
        plot.margin = unit(c(1,1,2,1), "lines"))
p +
  geom_xsidepoint(data = desc.stats %>% filter(!model %in% c(13)),
                  aes(shape = metric, colour = metric),
                  position = position_dodge(width = 0.2)) +
  geom_xsideline(data = desc.stats_filtered %>% filter(!model %in% c(13)),
                 aes(colour = metric)) +
  geom_xsidesegment(data = desc.stats_filtered %>% filter(!model %in% c(13)),
                    aes(y = medianCI.lower, yend= medianCI.upper,
                        colour = metric), width=.2,
                    position = position_dodge(width = 0.1),
                    arrow = arrow(length = unit(0.1, "cm"),
                                  angle=90, ends = "both"),
                    show.legend = FALSE) +
  theme(ggside.panel.scale = .5,
        ggside.axis.line.x = element_line(color = "black"))

###################
# install.packages("gplots")
library(gplots)

x <- as.matrix(gplots::catch.d)

options(repr.plot.width = 20, repr.plot.height = 45, repr.plot.res = 100)
# Generate dendrograms
row_dend <- as.dendrogram(hclust(dist(x)))
col_dend <- as.dendrogram(hclust(dist(t(x))))

# Customize dendrogram height by reordering or cutting (optional)
# Example: Cut the row dendrogram
row_dend_cut <- cutree(hclust(dist(x)), h=4.4)

clustColBar <- rainbow(length(unique(row_dend_cut)), start=0.1, end=0.9)
clustColBar <- clustColBar[as.vector(row_dend_cut)]

# Plot heatmap with custom dendrograms
heatmap.2(x, col = redblue, trace = "none", Rowv=row_dend, Colv=col_dend, scale="row",
          dendrogram="both", labRow = row_dend, cexCol = 2, margins=c(10, 10), 
          RowSideColors = clustColBar, keysize = 0.75)


###################
library(ggplot2)
library(dplyr)
p <- ggplot(data.frame(name=c("apple", "orange", "plum"),value=c(3,8,2),outlier=c(FALSE,TRUE,FALSE))) +
  geom_point(aes(x=value,y=name, colour = outlier)) +
  theme(axis.text.y = element_text(ifelse(.data[[p$layers]][[1]], 'red', 'green')))


###################
library(tidyverse)

# create a function for renaming column names to "exclude"
# if they have been seen before
drop_duplicates <- function(names, allow_ = TRUE) {
  names <- as.character(names)
  names[duplicated(names)] <- "exclude"
  names2 <- .Internal(make.names(names, allow_))
  names2
}

# create a csv file to read from
have <- tibble(ID = c(1,2,3), ID = c(4,5,6), Month = c(1,2,3),
               Month = c(4,5,6), Test = c(1,1,1), .name_repair = "none")
have

write_csv(x = have, file = "~/Desktop/test.csv")

# read in the file using the drop_duplicates() function
# and exclude columns renamed "exclude"
read_csv("~/Desktop/test.csv", name_repair = drop_duplicates) %>%
  select(!exclude)

###################
library(tidyverse)

set.seed(123)
data <- tibble(
  Treatment = rep(c("Control", "Treatment"), each = 4),
  Year = rep(2021:2024, times = 2),
  a = runif(8, 100, 150),
  b = runif(8, 100, 150),
  c = runif(8, 100, 150),
)

data <- data %>%
  mutate(Total = a + b+ c)

data_long <- data %>%
  pivot_longer(cols = -c(Year, Treatment), names_to = "Component", values_to = "Value") 

a <- data_long %>%
  filter(Component != "Total") %>%
  ggplot(aes(x = as.factor(Year), y = Value, group = Treatment)) +
  geom_line(aes(color = Treatment), size = 1) +
  geom_point(aes(color = Treatment, shape = Treatment), size = 3) +
  scale_y_continuous(limits = c(0, 200)) +
  facet_wrap(~Component, scales = "free_x", ncol = 4) +
  scale_color_manual(values = c("Control" = "blue", "Treatment" = "red"))

b <- data_long %>%
  filter(Component == "Total") %>%
  ggplot(aes(x = as.factor(Year), y = Value, group = Treatment)) +
  geom_line(aes(color = Treatment), size = 1) +
  geom_point(aes(color = Treatment, shape = Treatment), size = 3) +
  scale_y_continuous(limits = c(0, 500)) +
  facet_wrap(~Component, scales = "free_x", ncol = 4) +
  scale_color_manual(values = c("Control" = "blue", "Treatment" = "red"))

library(patchwork)
a + b + plot_layout(guides = "collect",
                    width = c(3,1),
                    axis_titles = "collect")


###################
library(tidyverse)
library(openxlsx)
library(readxl)
library(fs)

# create fake excel file for demonstration, code borrowed from this question:
#https://stackoverflow.com/questions/61601575/create-multiple-sheets-in-excel-workbook-from-r-tables-using-xlsx-package

data(mtcars)
data(ChickWeight)

# making the two datasets the same size and creating a fake id variable to merge by
ChickWeight2<-ChickWeight %>% head(32) %>% mutate(id=c(1:32))
mtcars2<-mtcars %>% mutate(id=c(1:32))

xl_lst <- list('df1_name' = ChickWeight2, 'df2_name' = mtcars2)

write.xlsx(xl_lst, file = "xl_with_2_worksheets.xlsx")
write.xlsx(xl_lst, file = "xl_with_2_worksheets_v2.xlsx")

# function to read in the file and bind the excel sheets, but this is not what I want, I want to merge the excel sheets. 
list_of_xlsx_files <- dir_ls(glob = "*.xlsx")
rm(ChickWeight, ChickWeight2, mtcars, mtcars2, xl_lst)

df_list <- lst()

for (i in seq_along(list_of_xlsx_files)) {
  path <- as.character(list_of_xlsx_files[[i]])
  df_list[[i]] <- path %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, path = path) %>%
    reduce(full_join)
  names(df_list)[i] <- path
}

list2env(df_list, envir = .GlobalEnv)
ls()

str(xl_with_2_worksheets.xlsx)
str(xl_with_2_worksheets_v2.xlsx)

###################
library(ggplot2) 
set.seed(123) 
(df <-rbind(data.frame(x=1:5, y=rnorm(5, 3), age=1),
            data.frame(x=1:5, y=rnorm(5, 5), age=2),
            data.frame(x=1:5, y=rnorm(5, 7), age=3)))

(p <- ggplot(df, aes(x, y, group=age, color=age)) +
    geom_point() + geom_line())

p +
  scale_color_fermenter(limits = c(0,3.01), breaks = 1:3) +
  theme(
    legend.text = element_text(vjust = 2.5)
  )

###################
library(tidyverse)
library(xgboost)

# read in the data from 
# https://drive.google.com/file/d/19HAcZ_czvK9AfOGpXmElRXTjTG-2EqSk/view
id <- "19HAcZ_czvK9AfOGpXmElRXTjTG-2EqSk"
column_names <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),
                         nrows = 2)
df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), skip = 2)

colnames(df) <- paste(colnames(column_names), column_names[1,], sep = "_")

df %>%
  mutate(Parameters_Date = mdy(Parameters_Date))

#################
library(tidyverse)
set.seed(42)

df1 <- data.frame(a=runif(5), b=runif(5), 
                  c=runif(5), d=rep("2019-01-01", 5))

df2 <- data.frame(a=runif(5), b=runif(5), 
                  c=runif(5), d=rep("2018-01-01", 5))

dflist <- list(df1, df2)

dflist %>%
  map(~rename_with(.x, \(x) paste(x, unique(sub("-.*", "", .x$d)), sep="_"),
                   .cols = a:c) %>% select(-d)
  )


###################
#install.packages("rcompanion")
library(rcompanion)

cldList2 <- function(data = NULL, comparison = NULL, p.value = NULL, 
                     threshold = 0.05, print.comp = FALSE, desired_order = desired_order) 
{
  if (is.null(desired_order)) {
    exit("desired_order not specified; please see ?orderedcldlist")
  }
  
  FLAG = 0
  
  if (sum(data[data[[p.value]] <= threshold,][[p.value]], na.rm = TRUE) == 0) {
    FLAG = 1
  }
  
  df = data[data[[p.value]] <= threshold,]
  combinations <- expand.grid(x = desired_order, y = desired_order)
  combinations$Comparison <- paste(combinations$y, combinations$x, sep = "-")
  combinations$score <- rownames(combinations)
  combinations$x <- NULL
  combinations$y <- NULL
  combinations
  
  df$switched <- paste(gsub(".*-", "", df[[comparison]]),
                       gsub("-.*", "", df[[comparison]]),
                       sep = "-")
  
  for (i in 1:nrow(df)) {
    for(j in seq_along(desired_order)) {
      if (desired_order[j] == gsub(".*-", "", df[[comparison]][i])) {
        df[[comparison]][i] <- df$switched[i]
        break
      } else if (desired_order[j] == gsub("-.*", "", df[[comparison]][i])) {
        break
      }
    }
  }
  df
  df <- merge(df, combinations, by = "Comparison", all.x = TRUE)
  
  ordered_df <- df[order(as.integer(df[["score"]])),]
  Comparison = ordered_df[[p.value]]
  names(Comparison) <- ordered_df[[comparison]]
  
  if (print.comp == TRUE) {
    Y = data.frame(Comparisons = Comparison, p.value = p.value, 
                   Value = Comparison, Threshold = threshold)
    cat("\n", "\n")
    print(Y)
    cat("\n", "\n")
  }
  
  MCL = multcompView::multcompLetters(Comparison)
  Group = names(MCL$Letters)
  Letter = as.character(MCL$Letters)
  if (FLAG == 0) {
    MonoLetter = as.character(MCL$monospacedLetters)
  }
  if (FLAG == 1) {
    MonoLetter = Letter
  }
  Z = data.frame(Group, Letter, MonoLetter)
  return(Z)
}

length_tukey2 <- read.table(text = "
Comparison diff         lwr          upr     p.adj
'Winter:Beggers:Mid-Summer:Beggers:Mid'  29.4223913  19.8928280  38.95195464 0.0000000
'Summer:Looe:Mid-Summer:Beggers:Mid'    -17.0816085 -26.3374620  -7.82575506 0.0000001
'Winter:Looe:Mid-Summer:Beggers:Mid'    -15.2041608 -24.1435742  -6.26474731 0.0000018
'Summer:Noss:Mid-Summer:Beggers:Mid'     -9.1104143 -18.3054013   0.08457279 0.0548154
'Winter:Noss:Mid-Summer:Beggers:Mid'     -2.2282797 -11.4180417   6.96148231 0.9997435
'Summer:Beggers:Low-Summer:Beggers:Mid'  13.0708662   3.4300630  22.71166931 0.0005807
'Winter:Beggers:Low-Summer:Beggers:Mid'  23.8704968  14.2899835  33.45101004 0.0000000
'Summer:Looe:Low-Summer:Beggers:Mid'    -20.2690529 -29.5565237 -10.98158210 0.0000000
'Winter:Looe:Low-Summer:Beggers:Mid'     -9.7459188 -18.5546802  -0.93715738 0.0157650
'Summer:Noss:Low-Summer:Beggers:Mid'    -11.9838481 -21.0283902  -2.93930611 0.0009105
'Winter:Noss:Low-Summer:Beggers:Mid'      7.9324954  -1.1718230  17.03681388 0.1602939
'Summer:Looe:Mid-Winter:Beggers:Mid'    -46.5039998 -55.7168884 -37.29111130 0.0000000
'Winter:Looe:Mid-Winter:Beggers:Mid'    -44.6265521 -53.5214723 -35.73163191 0.0000000
'Summer:Noss:Mid-Winter:Beggers:Mid'    -38.5328056 -47.6845420 -29.38106919 0.0000000
'Winter:Noss:Mid-Winter:Beggers:Mid'    -31.6506710 -40.7971576 -22.50418438 0.0000000
'Summer:Beggers:Low-Winter:Beggers:Mid' -16.3515252 -25.9510865  -6.75196384 0.0000017
'Winter:Beggers:Low-Winter:Beggers:Mid'  -5.5518946 -15.0909053   3.98711623 0.7580746
'Summer:Looe:Low-Winter:Beggers:Mid'    -49.6914442 -58.9360971 -40.44679139 0.0000000
'Winter:Looe:Low-Winter:Beggers:Mid'    -39.1683101 -47.9319149 -30.40470528 0.0000000
'Summer:Noss:Low-Winter:Beggers:Mid'    -41.4062395 -50.4068079 -32.40567100 0.0000000
'Winter:Noss:Low-Winter:Beggers:Mid'    -21.4898959 -30.5505309 -12.42926090 0.0000000
'Winter:Looe:Mid-Summer:Looe:Mid'         1.8774477  -6.7235900  10.47848551 0.9999087
'Summer:Noss:Mid-Summer:Looe:Mid'         7.9711942  -0.8951762  16.83756468 0.1275696
'Winter:Noss:Mid-Summer:Looe:Mid'        14.8533288   5.9923772  23.71428043 0.0000028
'Summer:Beggers:Low-Summer:Looe:Mid'     30.1524747  20.8245691  39.48038029 0.0000000
'Winter:Beggers:Low-Summer:Looe:Mid'     40.9521053  31.6865253  50.21768526 0.0000000
'Summer:Looe:Low-Summer:Looe:Mid'        -3.1874444 -12.1496903   5.77480151 0.9916068
'Winter:Looe:Low-Summer:Looe:Mid'         7.3356897  -1.1294751  15.80085454 0.1664206
'Summer:Noss:Low-Summer:Looe:Mid'         5.0977604  -3.6124910  13.80801170 0.7513767
'Winter:Noss:Low-Summer:Looe:Mid'        25.0141039  16.2417979  33.78640992 0.0000000
'Summer:Noss:Mid-Winter:Looe:Mid'         6.0937465  -2.4417567  14.62924969 0.4524766
'Winter:Noss:Mid-Winter:Looe:Mid'        12.9758811   4.4460069  21.50575525 0.0000430
'Summer:Beggers:Low-Winter:Looe:Mid'     28.2750269  19.2610315  37.28902233 0.0000000
'Winter:Beggers:Low-Winter:Looe:Mid'     39.0746575  30.1251736  48.02414145 0.0000000
'Summer:Looe:Low-Winter:Looe:Mid'        -5.0648921 -13.6999452   3.57016094 0.7486876
'Winter:Looe:Low-Winter:Looe:Mid'         5.4582420  -2.6597199  13.57620387 0.5513244
'Summer:Noss:Low-Winter:Looe:Mid'         3.2203126  -5.1529047  11.59352994 0.9841118
'Winter:Noss:Low-Winter:Looe:Mid'        23.1366562  14.6989051  31.57440721 0.0000000
'Winter:Noss:Mid-Summer:Noss:Mid'         6.8821346  -1.9152189  15.67948801 0.3046563
'Summer:Beggers:Low-Summer:Noss:Mid'     22.1812804  12.9137680  31.44879289 0.0000000
'Winter:Beggers:Low-Summer:Noss:Mid'     32.9809110  23.7761331  42.18568895 0.0000000
'Summer:Looe:Low-Summer:Noss:Mid'       -11.1586386 -20.0580103  -2.25926699 0.0024611
'Winter:Looe:Low-Summer:Noss:Mid'        -0.6355045  -9.0340746   7.76306555 1.0000000
'Summer:Noss:Low-Summer:Noss:Mid'        -2.8734339 -11.5189785   5.77211077 0.9952838
'Winter:Noss:Low-Summer:Noss:Mid'        17.0429097   8.3348492  25.75097011 0.0000000
'Summer:Beggers:Low-Winter:Noss:Mid'     15.2991459   6.0368175  24.56147417 0.0000044
'Winter:Beggers:Low-Winter:Noss:Mid'     26.0987764  16.8992180  35.29833487 0.0000000
'Summer:Looe:Low-Winter:Noss:Mid'       -18.0407732 -26.9347461  -9.14680029 0.0000000
'Winter:Looe:Low-Winter:Noss:Mid'        -7.5176391 -15.9104883   0.87521012 0.1311895
'Summer:Noss:Low-Winter:Noss:Mid'        -9.7555684 -18.3955558  -1.11558113 0.0120661
'Winter:Noss:Low-Winter:Noss:Mid'        10.1607751   1.4582321  18.86331813 0.0075546
'Winter:Beggers:Low-Summer:Beggers:Low'  10.7996306   1.1494889  20.44977230 0.0135847
'Summer:Looe:Low-Summer:Beggers:Low'    -33.3399191 -42.6991986 -23.98063950 0.0000000
'Winter:Looe:Low-Summer:Beggers:Low'    -22.8167849 -31.7012251 -13.93234475 0.0000000
'Summer:Noss:Low-Summer:Beggers:Low'    -25.0547143 -34.1729784 -15.93645018 0.0000000
'Winter:Noss:Low-Summer:Beggers:Low'     -5.1383707 -14.3159312   4.03918965 0.8016093
'Summer:Looe:Low-Winter:Beggers:Low'    -44.1395497 -53.4367139 -34.84238539 0.0000000
'Winter:Looe:Low-Winter:Beggers:Low'    -33.6164155 -42.4353966 -24.79743448 0.0000000
'Summer:Noss:Low-Winter:Beggers:Low'    -35.8543449 -44.9088405 -26.79984933 0.0000000
'Winter:Noss:Low-Winter:Beggers:Low'    -15.9380013 -25.0522081  -6.82379462 0.0000007
'Winter:Looe:Low-Summer:Looe:Low'        10.5231341   2.0234102  19.02285801 0.0030441
'Summer:Noss:Low-Summer:Looe:Low'         8.2852048  -0.4586370  17.02904655 0.0830012
'Winter:Noss:Low-Summer:Looe:Low'        28.2015483  19.3958886  37.00720806 0.0000000
'Summer:Noss:Low-Winter:Looe:Low'        -2.2379294 -10.4715150   5.99565630 0.9992360
'Winter:Noss:Low-Winter:Looe:Low'        17.6784142   9.3792090  25.97761940 0.0000000
'Winter:Noss:Low-Summer:Noss:Low'        19.9163435  11.3672927  28.46539437 0.0000000",
                            header = TRUE)

desired_order <- c(
  "Summer:Beggers:Mid", "Summer:Beggers:Low",
  "Winter:Beggers:Mid", "Winter:Beggers:Low",
  "Summer:Looe:Mid", "Summer:Looe:Low", 
  "Winter:Looe:Mid", "Winter:Looe:Low",
  "Summer:Noss:Mid", "Summer:Noss:Low",
  "Winter:Noss:Mid", "Winter:Noss:Low"
)

# devtools::install_github("jpmam1/orderedcldlist")
library(orderedcldlist)
orderedcldlist(data = length_tukey2,
               p.value = "p.adj",
               comparison = "Comparison",
               threshold = 0.05,
               desired_order = desired_order)

###################
library(orderedcldlist)
set.seed(12345)
df <- data.frame(program = rep(c("AA", "BB", "CC", "DD", "EE", "FF"), each = 10),
                 weight_loss = c(runif(10, 0, 10),
                                 runif(10, 0, 4),
                                 runif(10, 1, 7),
                                 runif(10, 0, 3),
                                 runif(10, 0, 3),
                                 runif(10, 3, 8)),
                 group = factor(sample(c(0,1), replace = TRUE, size = 10)))
model = aov(weight_loss ~ program * group, data = df)
summary(model)
TUK = TukeyHSD(model, ordered = TRUE)
TUK = as.data.frame(TUK[[1]])
HSD = data.frame(Comparison=row.names(TUK),
                 diff=TUK$diff, lwr=TUK$lwr, upr=TUK$upr, p.adj=TUK$`p adj`)
HSD

desired_order = c("CC", "FF", "BB", "EE", "DD", "AA")
orderedcldlist(data = HSD,
         comparison = "Comparison",
         p.value = "p.adj",
         threshold = 0.05,
         desired_order = desired_order)
