## Script for illustrating contig distribution using base R (simple as possible)
## Jared Mamrot  jared.mamrot@hudson.org.au  22/07/2016

#Trial run
#Import data
MA <- read.table("Master_Assembly.txt")
TB <- read.table("Trinity_best.txt")
TS <- read.table("Trinity_Seifert.txt")
SA <- read.table("SOAP_tr2aacds.txt")
OA <- read.table("Oases_tr2aacds.txt")
TC <- read.table("dist.trans_lengths_trinity_corrected.txt")
TV2 <- read.table("Trinity_v2.3.2.txt")
TR2 <- read.table("tr2aacds_ult.txt")
#Assign to variables
a <- MA$V1
b <- MA$V2
c <- TB$V1
d <- TB$V2
e <- TS$V1
f <- TS$V2
g <- SA$V1
h <- SA$V2
i <- OA$V1
j <- OA$V2
k <- TC$V1
l <- TC$V2
m <- TV2$V1
n <- TV2$V2
o <- TR2$V1
p <- TR2$V2
#Specify the filename/type of plot
png(file="Contig_distribution.png",
    width=750,height=500)
#Set margins
par(mar= c(6,8,4,4), las=1)
#Stop R from converting to scientific notation
options("scipen"=10, "digits"=4)
#Plot the data for 'MA'
plot(a,b,log="xy",type="l",
     col="purple", ylab = 'Log number contigs\n',
     lwd = 2, xlab = 'Size of contig (log bp)',
     main = "Distribution of contigs for each assembly",
     cex.main  = 1.6, cex.lab = 1.4, cex.axis = 1.2,
     xlim=c(200,50000), ylim=c(0.9,1000000))
#Add lines for the other variables
lines(c,d,type="l", col="blue", lwd = 2)
lines(e,f,type="l", col="green", lwd = 2)
lines(g,h,type="l", col="orange", lwd = 2)
lines(i,j,type="l", col="red", lwd = 2)
lines(k,l,type="l", col="yellow", lwd = 2)
lines(m,n,type="l", col="navyblue", lwd = 2)
lines(o,p,type="l", col="turquoise1", lwd = 2)
#Add the legend
leg <- c("Tr2aacds_v1", "Trinity_r20140413",
         "Trinity_Gawriluk et al.","SOAPdenovo-Trans",
         "Velvet/Oases", "Trinity_corrected",
         "Trinity_v2.3.2", "Tr2aacds_v2")
#Legend colours
cols_for_leg <- c("purple","blue","green","orange",
                  "red","yellow", "navyblue", "turquoise1")
#Position the legend
legend("topright", legend = leg,
       col=cols_for_leg, lwd = 2,
       bty = "n", cex = 1.15,
       y.intersp = 1.1, inset = 0.01,
       lty = c(1,1,1,1,1,1,1,1))
#Close the device (save the plot)
dev.off()


## Contig distribution panel
## Velvet/Oases
png(file="Contig_distribution_panel.png",width=1200,height=1500)
par(mfrow=c(5,2))
dir_025 <- read.table("dist.trans_lengths_dir_25.txt")
dir_027 <- read.table("dist.trans_lengths_dir_27.txt")
dir_029 <- read.table("dist.trans_lengths_dir_29.txt")
dir_031 <- read.table("dist.trans_lengths_dir_31.txt")
dir_035 <- read.table("dist.trans_lengths_dir_35.txt")
dir_041 <- read.table("dist.trans_lengths_dir_41.txt")
dir_051 <- read.table("dist.trans_lengths_dir_51.txt")
dir_061 <- read.table("dist.trans_lengths_dir_61.txt")
dir_071 <- read.table("dist.trans_lengths_dir_71.txt")
dir_081 <- read.table("dist.trans_lengths_dir_81.txt")
dir_091 <- read.table("dist.trans_lengths_dir_91.txt")
dir_101 <- read.table("dist.trans_lengths_dir_101.txt")
dir_111 <- read.table("dist.trans_lengths_dir_111.txt")
dir_121 <- read.table("dist.trans_lengths_dir_121.txt")
x01 <- dir_025$V1
x02 <- dir_027$V1
x03 <- dir_029$V1
x04 <- dir_031$V1
x05 <- dir_035$V1
x06 <- dir_041$V1
x07 <- dir_051$V1
x08 <- dir_061$V1
x09 <- dir_071$V1
x10 <- dir_081$V1
x11 <- dir_091$V1
x12 <- dir_101$V1
x13 <- dir_111$V1
x14 <- dir_121$V1
y01 <- dir_025$V2
y02 <- dir_027$V2
y03 <- dir_029$V2
y04 <- dir_031$V2
y05 <- dir_035$V2
y06 <- dir_041$V2
y07 <- dir_051$V2
y08 <- dir_061$V2
y09 <- dir_071$V2
y10 <- dir_081$V2
y11 <- dir_091$V2
y12 <- dir_101$V2
y13 <- dir_111$V2
y14 <- dir_121$V2
par(mar=c(3,10,5,2), las=1)
options(scipen = 10)
plot(x01, y01, log = "x", type = "o", col = "red",
     xlab = "", ylab = "Number of Contigs Velvet/Oases \n\n",
     main = "Contig distribution log(x)",cex.main = 2,
     cex.lab = 2, cex.axis = 1.25, ylim = c(0, 250000),
     xlim = c(100,50000), pch = ".")
lines(x02, y02, type = "o", col = "blue", pch = ".")
lines(x03, y03, type = "o", col = "green", pch = ".")
lines(x04, y04, type = "o", col = "yellow", pch = ".")
lines(x05, y05, type = "o", col = "blueviolet", pch = ".")
lines(x06, y06, type = "o", col = "orange", pch = ".")
lines(x07, y07, type = "o", col = "navyblue", pch = ".")
lines(x08, y08, type = "o", col = "aquamarine", pch = ".")
lines(x09, y09, type = "o", col = "darkcyan", pch = ".")
lines(x10, y10, type = "o", col = "darkmagenta", pch = ".")
lines(x11, y11, type = "o", col = "darkorchid", pch = ".")
lines(x12, y12, type = "o", col = "deeppink", pch = ".")
lines(x13, y13, type = "o", col = "goldenrod", pch = ".")
lines(x14, y14, type = "o", col = "lightsteelblue", pch = ".")
leg <- c("kmer = 25","kmer = 27","kmer = 29","kmer = 31",
         "kmer = 35","kmer = 41","kmer = 51","kmer = 61",
         "kmer = 71","kmer = 81","kmer = 91","kmer = 101",
         "kmer = 111","kmer = 121")
cols_for_leg <- c("red", "blue", "green", "yellow",
                  "blueviolet", "orange", "navyblue",
                  "aquamarine", "darkcyan", "darkmagenta",
                  "darkorchid", "deeppink", "goldenrod",
                  "lightsteelblue")
legend("right", legend = leg, col=cols_for_leg,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.3, inset = 0.05)
## Velvet/Oases log
par(mar=c(3,4,5,4), las=1)
options(scipen = 10)
plot(x01, y01, log = "xy", type = "o", col = "red",
     ylab = "", xlab = "", main = "Contig distribution log(xy)",
     cex.main = 2, cex.lab = 2, cex.axis = 1.25, pch = ".",
     xlim = c(100, 50000), ylim = c(0.9, 1000000))
lines(x02, y02, type = "o", col = "blue", pch = ".")
lines(x03, y03, type = "o", col = "green", pch = ".")
lines(x04, y04, type = "o", col = "yellow", pch = ".")
lines(x05, y05, type = "o", col = "blueviolet", pch = ".")
lines(x06, y06, type = "o", col = "orange", pch = ".")
lines(x07, y07, type = "o", col = "navyblue", pch = ".")
lines(x08, y08, type = "o", col = "aquamarine", pch = ".")
lines(x09, y09, type = "o", col = "darkcyan", pch = ".")
lines(x10, y10, type = "o", col = "darkmagenta", pch = ".")
lines(x11, y11, type = "o", col = "darkorchid", pch = ".")
lines(x12, y12, type = "o", col = "deeppink", pch = ".")
lines(x13, y13, type = "o", col = "goldenrod", pch = ".")
lines(x14, y14, type = "o", col = "lightsteelblue", pch = ".")
leg <- c("kmer = 25","kmer = 27","kmer = 29","kmer = 31",
         "kmer = 35","kmer = 41","kmer = 51","kmer = 61",
         "kmer = 71","kmer = 81","kmer = 91","kmer = 101",
         "kmer = 111","kmer = 121")
cols_for_leg <- c("red", "blue", "green", "yellow",
                  "blueviolet", "orange", "navyblue",
                  "aquamarine", "darkcyan", "darkmagenta",
                  "darkorchid", "deeppink", "goldenrod",
                  "lightsteelblue")
legend("topright", legend = leg, col=cols_for_leg,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.1, inset = 0.05)
## SOAP clean
SOAPclean_021 <- read.table("dist.trans_lengths_SOAP_clean_21.txt")
SOAPclean_023 <- read.table("dist.trans_lengths_SOAP_clean_23.txt")
SOAPclean_025 <- read.table("dist.trans_lengths_SOAP_clean_25.txt")
SOAPclean_027 <- read.table("dist.trans_lengths_SOAP_clean_27.txt")
SOAPclean_029 <- read.table("dist.trans_lengths_SOAP_clean_29.txt")
SOAPclean_031 <- read.table("dist.trans_lengths_SOAP_clean_31.txt")
SOAPclean_035 <- read.table("dist.trans_lengths_SOAP_clean_35.txt")
SOAPclean_041 <- read.table("dist.trans_lengths_SOAP_clean_41.txt")
SOAPclean_051 <- read.table("dist.trans_lengths_SOAP_clean_51.txt")
SOAPclean_061 <- read.table("dist.trans_lengths_SOAP_clean_61.txt")
SOAPclean_071 <- read.table("dist.trans_lengths_SOAP_clean_71.txt")
SOAPclean_081 <- read.table("dist.trans_lengths_SOAP_clean_81.txt")
SOAPclean_091 <- read.table("dist.trans_lengths_SOAP_clean_91.txt")
v01 <- SOAPclean_021$V1
v02 <- SOAPclean_023$V1
v03 <- SOAPclean_025$V1
v04 <- SOAPclean_027$V1
v05 <- SOAPclean_029$V1
v06 <- SOAPclean_031$V1
v07 <- SOAPclean_035$V1
v08 <- SOAPclean_041$V1
v09 <- SOAPclean_051$V1
v10 <- SOAPclean_061$V1
v11 <- SOAPclean_071$V1
v12 <- SOAPclean_081$V1
v13 <- SOAPclean_091$V1
w01 <- SOAPclean_021$V2
w02 <- SOAPclean_023$V2
w03 <- SOAPclean_025$V2
w04 <- SOAPclean_027$V2
w05 <- SOAPclean_029$V2
w06 <- SOAPclean_031$V2
w07 <- SOAPclean_035$V2
w08 <- SOAPclean_041$V2
w09 <- SOAPclean_051$V2
w10 <- SOAPclean_061$V2
w11 <- SOAPclean_071$V2
w12 <- SOAPclean_081$V2
w13 <- SOAPclean_091$V2
par(mar=c(3,10,2,2), las=1)
options(scipen = 10)
plot(v01, w01, log = "x", type = "o", col = "red",
     xlab = "", ylab = "Number of Contigs SOAPdenovo-Trans \n\n",
     cex.main = 2, cex.lab = 2, cex.axis = 1.25, pch = ".",
     ylim = c(0, 2000000), xlim = c(100, 50000))
lines(v02, w02, type = "o", col = "blue", pch = ".")
lines(v03, w03, type = "o", col = "green", pch = ".")
lines(v04, w04, type = "o", col = "yellow", pch = ".")
lines(v05, w05, type = "o", col = "blueviolet", pch = ".")
lines(v06, w06, type = "o", col = "orange", pch = ".")
lines(v07, w07, type = "o", col = "navyblue", pch = ".")
lines(v08, w08, type = "o", col = "aquamarine", pch = ".")
lines(v09, w09, type = "o", col = "darkcyan", pch = ".")
lines(v10, w10, type = "o", col = "darkmagenta", pch = ".")
lines(v11, w11, type = "o", col = "darkorchid", pch = ".")
lines(v12, w12, type = "o", col = "deeppink", pch = ".")
lines(v13, w13, type = "o", col = "goldenrod", pch = ".")
legSOAPclean <- c("kmer = 21","kmer = 23","kmer = 25",
                  "kmer = 27","kmer = 29","kmer = 31",
                  "kmer = 35","kmer = 41","kmer = 51",
                  "kmer = 61","kmer = 71","kmer = 81",
                  "kmer = 91")
cols_for_legSOAPclean <- c("red", "blue", "green", "yellow",
                           "blueviolet","orange", "navyblue",
                           "aquamarine", "darkcyan", "darkmagenta",
                           "darkorchid", "deeppink", "goldenrod")
legend("topright", legend = legSOAPclean,
       col=cols_for_legSOAPclean, pch=19, bty = "n",
       cex = 1.2, xjust = 1, y.intersp = 1.1, inset = 0.05)
## SOAP clean log
par(mar=c(3,4,2,4), las=1)
options(scipen = 10)
plot(v01, w01, log = "xy", type = "o", col = "red",
     xlab = "", ylab = "", cex.main = 2, cex.lab = 2,
     cex.axis = 1.25, pch = ".", xlim = c(100, 50000))
lines(v02, w02, type = "o", col = "blue", pch = ".")
lines(v03, w03, type = "o", col = "green", pch = ".")
lines(v04, w04, type = "o", col = "yellow", pch = ".")
lines(v05, w05, type = "o", col = "blueviolet", pch = ".")
lines(v06, w06, type = "o", col = "orange", pch = ".")
lines(v07, w07, type = "o", col = "navyblue", pch = ".")
lines(v08, w08, type = "o", col = "aquamarine", pch = ".")
lines(v09, w09, type = "o", col = "darkcyan", pch = ".")
lines(v10, w10, type = "o", col = "darkmagenta", pch = ".")
lines(v11, w11, type = "o", col = "darkorchid", pch = ".")
lines(v12, w12, type = "o", col = "deeppink", pch = ".")
lines(v13, w13, type = "o", col = "goldenrod", pch = ".")
legSOAPclean <- c("kmer = 21","kmer = 23","kmer = 25",
                  "kmer = 27","kmer = 29","kmer = 31",
                  "kmer = 35","kmer = 41","kmer = 51",
                  "kmer = 61","kmer = 71","kmer = 81",
                  "kmer = 91")
cols_for_legSOAPclean <- c("red", "blue", "green", "yellow",
                           "blueviolet", "orange", "navyblue",
                           "aquamarine", "darkcyan", "darkmagenta",
                           "darkorchid", "deeppink", "goldenrod")
legend("topright", legend = legSOAPclean,
       col=cols_for_legSOAPclean, pch=19,
       bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.1, inset = 0.05)
## SOAP norm
SOAP_norm_021 <- read.table("dist.trans_lengths_SOAP_norm_kmer21.txt")
SOAP_norm_023 <- read.table("dist.trans_lengths_SOAP_norm_kmer23.txt")
SOAP_norm_025 <- read.table("dist.trans_lengths_SOAP_norm_kmer25.txt")
SOAP_norm_027 <- read.table("dist.trans_lengths_SOAP_norm_kmer27.txt")
SOAP_norm_029 <- read.table("dist.trans_lengths_SOAP_norm_kmer29.txt")
SOAP_norm_031 <- read.table("dist.trans_lengths_SOAP_norm_kmer31.txt")
SOAP_norm_035 <- read.table("dist.trans_lengths_SOAP_norm_kmer35.txt")
SOAP_norm_041 <- read.table("dist.trans_lengths_SOAP_norm_kmer41.txt")
SOAP_norm_051 <- read.table("dist.trans_lengths_SOAP_norm_kmer51.txt")
SOAP_norm_061 <- read.table("dist.trans_lengths_SOAP_norm_kmer61.txt")
SOAP_norm_071 <- read.table("dist.trans_lengths_SOAP_norm_kmer71.txt")
SOAP_norm_081 <- read.table("dist.trans_lengths_SOAP_norm_kmer81.txt")
SOAP_norm_091 <- read.table("dist.trans_lengths_SOAP_norm_kmer91.txt")
t01 <- SOAP_norm_021$V1
t02 <- SOAP_norm_023$V1
t03 <- SOAP_norm_025$V1
t04 <- SOAP_norm_027$V1
t05 <- SOAP_norm_029$V1
t06 <- SOAP_norm_031$V1
t07 <- SOAP_norm_035$V1
t08 <- SOAP_norm_041$V1
t09 <- SOAP_norm_051$V1
t10 <- SOAP_norm_061$V1
t11 <- SOAP_norm_071$V1
t12 <- SOAP_norm_081$V1
t13 <- SOAP_norm_091$V1
u01 <- SOAP_norm_021$V2
u02 <- SOAP_norm_023$V2
u03 <- SOAP_norm_025$V2
u04 <- SOAP_norm_027$V2
u05 <- SOAP_norm_029$V2
u06 <- SOAP_norm_031$V2
u07 <- SOAP_norm_035$V2
u08 <- SOAP_norm_041$V2
u09 <- SOAP_norm_051$V2
u10 <- SOAP_norm_061$V2
u11 <- SOAP_norm_071$V2
u12 <- SOAP_norm_081$V2
u13 <- SOAP_norm_091$V2
par(mar=c(3,10,2,2), las=1)
options(scipen = 10)
plot(t01, u01, log = "x", type = "o", col = "red",
     xlab = "", ylab = "Contig number SOAP normalized \n\n",
     cex.main = 2, cex.lab = 2, cex.axis = 1.2,
     ylim = c(0, 2000000), xlim = c(100, 50000), pch = ".")
lines(t02, u02, type = "o", col = "blue", pch = ".")
lines(t03, u03, type = "o", col = "green", pch = ".")
lines(t04, u04, type = "o", col = "yellow", pch = ".")
lines(t05, u05, type = "o", col = "blueviolet", pch = ".")
lines(t06, u06, type = "o", col = "orange", pch = ".")
lines(t07, u07, type = "o", col = "navyblue", pch = ".")
lines(t08, u08, type = "o", col = "aquamarine", pch = ".")
lines(t09, u09, type = "o", col = "darkcyan", pch = ".")
lines(t10, u10, type = "o", col = "darkmagenta", pch = ".")
lines(t11, u11, type = "o", col = "darkorchid", pch = ".")
lines(t12, u12, type = "o", col = "deeppink", pch = ".")
lines(t13, u13, type = "o", col = "goldenrod", pch = ".")
legSOAPnorm <- c("kmer = 21","kmer = 23","kmer = 25",
                 "kmer = 27","kmer = 29","kmer = 31",
                 "kmer = 35","kmer = 41","kmer = 51",
                 "kmer = 61","kmer = 71","kmer = 81",
                 "kmer = 91")
cols_for_leg_SOAPnorm <- c("red", "blue", "green", "yellow",
                           "blueviolet", "orange", "navyblue",
                           "aquamarine", "darkcyan", "darkmagenta",
                           "darkorchid", "deeppink", "goldenrod")
legend("right", legend = legSOAPnorm, col=cols_for_leg_SOAPnorm,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.1, inset = 0.05)
## SOAP norm log
par(mar=c(3,4,2,4), las=1)
options(scipen = 10)
plot(t01, u01, log = "xy", type = "o", col = "red",
     xlab = "", ylab = "", cex.main = 2, cex.lab = 2,
     cex.axis = 1.2, pch = ".", xlim = c(100, 50000))
lines(t02, u02, type = "o", col = "blue", pch = ".")
lines(t03, u03, type = "o", col = "green", pch = ".")
lines(t04, u04, type = "o", col = "yellow", pch = ".")
lines(t05, u05, type = "o", col = "blueviolet", pch = ".")
lines(t06, u06, type = "o", col = "orange", pch = ".")
lines(t07, u07, type = "o", col = "navyblue", pch = ".")
lines(t08, u08, type = "o", col = "aquamarine", pch = ".")
lines(t09, u09, type = "o", col = "darkcyan", pch = ".")
lines(t10, u10, type = "o", col = "darkmagenta", pch = ".")
lines(t11, u11, type = "o", col = "darkorchid", pch = ".")
lines(t12, u12, type = "o", col = "deeppink", pch = ".")
lines(t13, u13, type = "o", col = "goldenrod", pch = ".")
legSOAPnorm <- c("kmer = 21","kmer = 23","kmer = 25",
                 "kmer = 27","kmer = 29","kmer = 31",
                 "kmer = 35","kmer = 41","kmer = 51",
                 "kmer = 61","kmer = 71","kmer = 81",
                 "kmer = 91")
cols_for_leg_SOAPnorm <- c("red", "blue", "green", "yellow",
                           "blueviolet", "orange", "navyblue",
                           "aquamarine", "darkcyan", "darkmagenta",
                           "darkorchid", "deeppink", "goldenrod")
legend("topright", legend = legSOAPnorm, col=cols_for_leg_SOAPnorm,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.1, inset = 0.05)
## SOAP raw
SOAPraw_021 <- read.table("dist.trans_lengths_SOAP_raw_kmer21.txt")
SOAPraw_023 <- read.table("dist.trans_lengths_SOAP_raw_kmer23.txt")
SOAPraw_025 <- read.table("dist.trans_lengths_SOAP_raw_kmer25.txt")
SOAPraw_027 <- read.table("dist.trans_lengths_SOAP_raw_kmer27.txt")
SOAPraw_029 <- read.table("dist.trans_lengths_SOAP_raw_kmer29.txt")
SOAPraw_031 <- read.table("dist.trans_lengths_SOAP_raw_kmer31.txt")
r01 <- SOAPraw_021$V1
r02 <- SOAPraw_023$V1
r03 <- SOAPraw_025$V1
r04 <- SOAPraw_027$V1
r05 <- SOAPraw_029$V1
r06 <- SOAPraw_031$V1
s01 <- SOAPraw_021$V2
s02 <- SOAPraw_023$V2
s03 <- SOAPraw_025$V2
s04 <- SOAPraw_027$V2
s05 <- SOAPraw_029$V2
s06 <- SOAPraw_031$V2
par(mar=c(3,10,2,2), las=1)
options(scipen = 10)
plot(r01, s01, log = "x", type = "o", col = "red",
     xlab = "", ylab = "Contig number SOAP untrimmed reads \n\n",
     cex.main = 2, cex.lab = 2, cex.axis = 1.25,
     ylim = c(0, 3500000), pch = ".", xlim = c(100, 50000))
lines(r02, s02, type = "o", col = "blue", pch = ".")
lines(r03, s03, type = "o", col = "green", pch = ".")
lines(r04, s04, type = "o", col = "yellow", pch = ".")
lines(r05, s05, type = "o", col = "blueviolet", pch = ".")
lines(r06, s06, type = "o", col = "orange", pch = ".")
legSOAPraw <- c("kmer = 21","kmer = 23","kmer = 25",
                "kmer = 27","kmer = 29","kmer = 31")
cols_for_leg_SOAPraw <- c("red", "blue", "green",
                          "yellow", "blueviolet",
                          "orange")
legend("right", legend = legSOAPraw, col=cols_for_leg_SOAPraw,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.4, inset = 0.05)
## SOAP raw log
par(mar=c(3,4,2,4), las=1)
options(scipen = 10)
plot(r01, s01, log = "xy", type = "o", col = "red",
     xlab = "", ylab = "", cex.main = 2, cex.lab = 2,
     cex.axis = 1.25, ylim = c(0.9, 3500000),
     xlim = c(100, 50000), pch = ".")
lines(r02, s02, type = "o", col = "blue", pch = ".")
lines(r03, s03, type = "o", col = "green", pch = ".")
lines(r04, s04, type = "o", col = "yellow", pch = ".")
lines(r05, s05, type = "o", col = "blueviolet", pch = ".")
lines(r06, s06, type = "o", col = "orange", pch = ".")
legSOAPraw <- c("kmer = 21","kmer = 23","kmer = 25",
                "kmer = 27","kmer = 29","kmer = 31")
cols_for_leg_SOAPraw <- c("red", "blue", "green",
                          "yellow", "blueviolet",
                          "orange")
legend("right", legend = legSOAPraw, col=cols_for_leg_SOAPraw,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.4, inset = 0.05)
## Trinity & tr2aacds merged
Trin_best <- read.table("dist.Trinity_best copy.txt")
Trin_v2.3.2 <- read.table("dist.Trinity_v2.3.2 copy.txt")
Trinity_error_corrected <- read.table("dist.Trinity_corrected.txt")
tr2aacds_v1 <- read.table("dist.tr2aacds_v1.txt")
tr2aacds_v2 <- read.table("dist.tr2aacds_v2 copy.txt")
t01 <- Trin_best$V1
t02 <- Trin_v2.3.2$V1
t03 <- Trinity_error_corrected$V1
t04 <- tr2aacds_v1$V1
t05 <- tr2aacds_v2$V1
u01 <- Trin_best$V2
u02 <- Trin_v2.3.2$V2
u03 <- Trinity_error_corrected$V2
u04 <- tr2aacds_v1$V2
u05 <- tr2aacds_v2$V2
par(mar=c(5,10,2,2), las=1)
options(scipen = 10)
plot(t01, u01, log = "x", type = "o", col = "red",
     xlab = "Contig size (log)",
     ylab = "Contig number Trinity /tr2aacds \n\n",
     cex.main = 2, cex.lab = 2, cex.axis = 1.25,
     ylim = c(0, 1000000), pch = ".", xlim = c(100, 50000))
lines(t02, u02, type = "o", col = "blue", pch = ".")
lines(t03, u03, type = "o", col = "green", pch = ".")
lines(t04, u04, type = "o", col = "yellow", pch = ".")
lines(t05, u05, type = "o", col = "orange", pch = ".")
legTrin <- c("Trinity_r20140414","Trinity_v2.3.2",
             "Trinity_error_corrected","tr2aacds_v1","tr2aacds_v2")
cols_for_leg_Trin <- c("red", "blue", "green", "yellow", "orange")
legend("right", legend = legTrin, col=cols_for_leg_Trin,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.4, inset = 0.025)
## Trinity & tr2aacds merged log
par(mar=c(5,4,2,4), las=1)
options(scipen = 10)
plot(t01, u01, log = "xy", type = "o", col = "red",
     xlab = "Contig size (log)", ylab = "",
     cex.main = 2, cex.lab = 2, cex.axis = 1.25,
     ylim = c(0.9, 1000000), xlim = c(100, 50000),
     pch = ".")
lines(t02, u02, type = "o", col = "blue", pch = ".")
lines(t03, u03, type = "o", col = "green", pch = ".")
lines(t04, u04, type = "o", col = "yellow", pch = ".")
lines(t05, u05, type = "o", col = "orange", pch = ".")
legTrin <- c("Trinity_r20140414","Trinity_v2.3.2",
             "Trinity_error_corrected","tr2aacds_v1","tr2aacds_v2")
cols_for_leg_Trin <- c("red", "blue", "green", "yellow","orange")
legend("right", legend = legTrin, col=cols_for_leg_Trin,
       pch=19, bty = "n", cex = 1.2, xjust = 1,
       y.intersp = 1.4, inset = 0.025)
dev.off()