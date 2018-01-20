### To beging with, make sure you have installed the following packages.
install.packages("ggplot2")
install.packages("Lock5Data")
install.packages("pastecs")
install.packages("boot")
install.packages("extrafont")

library(boot)
library(ggplot2)
library(Lock5Data)
library(MASS)
library(pastecs)
library(extrafont)
font_import()
loadfonts()

### You will also need to have ghostscript locally installed on your machine and enable its executable as follows 
### Note that ghostscript is not an R package. The proper version for your operative system should be downloaded 
Sys.setenv(R_GSCMD = "/usr/local/bin/gs")

######
### YOU NEED TO EDIT THIS SECTION FOR THE CODE TO RUN ###
### Give the pathnames on your computer
######

### EDIT THE PATH TO WHERE THE SURVEY DATA IS: ### 
path <- "/Users/melissa/Dropbox (ASU)/Courses_assessments/2016_Spring_GeneticDrift/Teaching_GeneticDrift/"


### EVERYTHING ELSE SHOULD RUN SO LONG AS YOU HAVE NOT EDITED THE NAME OF THE SURVEY DATA ###

# This will automatincally make the path to the survey data in tab-delimited format
# If you have edited the name of the survey data file on your computer, you will need to edit it below.
data <- paste(path,"data/genetic_drift_data.txt", sep="")

# These will make a directory for printing plots
# Later in the program, it will generate additional directories in this one
# to help keep similar plots together.
plots_dir <- paste(path,"plots",sep="")
dir.create(plots_dir) 


# Read in data
drift <- read.table(data, header=T)

# Remove entries that don’t have a Pre-test or a Post-test
drift_pre <- drift[complete.cases(drift$Pre_1),]
drift_pre_post <- drift_pre[complete.cases(drift_pre$Post_1),]

# If we want to also examine the students that took the third exam
drift_pre_post_3rd <- drift_pre_post[complete.cases(drift_pre_post$Exam3_Q23),]

# If we want to also examine the students that took the final
drift_pre_post_3rd_final <- drift_pre_post_3rd[complete.cases(drift_pre_post_3rd$Final_Q25),]

# Calculate the number of elements in each datasets and print to the screen.
length(drift[[1]])
length(drift_pre[[1]])
length(drift_pre_post[[1]])
length(drift_pre_post_3rd[[1]])
length(drift_pre_post_3rd_final[[1]])

# Student's who agreed to participate on the study:	269
# Student's who presented a pre_lesson assessment: 250
# Student's who presented a post_lesson assessment: 203
# Student's who presented both pre_lesson and post_lesson assessments: 188
# Student's who presented the pre_lesson assessment, post_lesson assessment, and final exam: 78

# Calculate Sums of Pre and Post lesson assessments
Pre_Sum <- drift_pre_post$Pre_1 + drift_pre_post$Pre_2 + drift_pre_post$Pre_3 + drift_pre_post$Pre_4 + drift_pre_post$Pre_5 + drift_pre_post$Pre_6 + drift_pre_post$Pre_7 + drift_pre_post$Pre_8 + drift_pre_post$Pre_9 + drift_pre_post$Pre_10 + drift_pre_post$Pre_11 + drift_pre_post$Pre_12 + drift_pre_post$Pre_13 + drift_pre_post$Pre_14 + drift_pre_post$Pre_15 + drift_pre_post$Pre_16 + drift_pre_post$Pre_17 + drift_pre_post$Pre_18 + drift_pre_post$Pre_19 + drift_pre_post$Pre_20 + drift_pre_post$Pre_21 + drift_pre_post$Pre_22
Post_Sum <- drift_pre_post$Post_1 + drift_pre_post$Post_2 + drift_pre_post$Post_3 + drift_pre_post$Post_4 + drift_pre_post$Post_5 + drift_pre_post$Post_6 + drift_pre_post$Post_7 + drift_pre_post$Post_8 + drift_pre_post$Post_9 + drift_pre_post$Post_10 + drift_pre_post$Post_11 + drift_pre_post$Post_12 + drift_pre_post$Post_13 + drift_pre_post$Post_14 + drift_pre_post$Post_15 + drift_pre_post$Post_16 + drift_pre_post$Post_17 + drift_pre_post$Post_18 + drift_pre_post$Post_19 + drift_pre_post$Post_20 + drift_pre_post$Post_21 + drift_pre_post$Post_22

#Calculate the difference between Pre and Post lesson assessments
Diff <- Post_Sum - Pre_Sum

# Add these columns back to the appropriate data partitions
drift_pre_post$Pre_assessment_Sum <- Pre_Sum
drift_pre_post$Post_assessment_Sum <- Post_Sum
drift_pre_post$Diff <- Diff

# Calculate summary statistics on each data partition
summary_pre_post <- cbind(as.matrix(stat.desc(drift_pre_post$Pre_assessment_Sum)),
                          as.matrix(stat.desc(drift_pre_post$Post_assessment_Sum)))

# Assigns column name to a vector
colnames(summary_pre_post) <- c("Pre_Summary","Post_Summary")

# Organizes table by assigned gender_vector
t(summary_pre_post)

# nbr.val nbr.null nbr.na min max range  sum median     mean   SE.mean CI.mean.0.95      var  std.dev  coef.var
# Pre_Summary      203        0      0   6  22    16 2878     14 14.17734 0.2540035    0.5008383 13.09711 3.618993 0.2552660
# Post_Summary     203        0      0   6  22    16 3341     17 16.45813 0.2724750    0.5372601 15.07126 3.882172 0.2358817

#write.table(t(summary_pre_post), pre_post_outputFile, col.names=NA, sep="\t")
t_test_all <- t.test(Pre_Sum,Post_Sum)


# Creates a data frame of the mean and standard error
dataframe_plot <- data.frame(
  Pre_Post_assessment = factor(c("Pre_lesson","Post_lesson"), levels=c("Pre_lesson","Post_lesson")),
  Summary = summary_pre_post[9,1:2],
  SE.mean = summary_pre_post[10,1:2]
)

# Creates and exports a pdf file with the ggplot of the results
pdf(paste(plots_dir,'/Overall_pre-post.pdf', sep=""),width=6,height=4)
group.colors <- c(Pre_lesson = "gold", 
                  Post_lesson ="goldenrod3")
ggplot(data = dataframe_plot, 
       aes(x=Pre_Post_assessment, y=Summary, fill=Pre_Post_assessment)) + 
  xlab("Assessment") +
  ylab("Mean") +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=Summary-SE.mean, ymax=Summary+SE.mean), width=.2) +
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(text=element_text(size=18,  family="Helvetica"),legend.position="none") +
  theme(panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
dev.off()

#Add the complete path to the destination pdf
embedFonts(path.expand("~/Desktop/EEO/Teaching_GeneticDrift/plots_final/Overall_pre-post.pdf"))

### Next, let’s study the results by demographic group ###

# Reported gender: Gender
# First generation college student: First_Gen
# Student tool a genetics class at the same institution (ASU): Bio340_at_ASU
# Reported ethnicity: Ethnicity
# Assigned TA pair and recitation time: RecitationPair_Time
# Student's final letter grade: Letter_Grade


### First, let’s break up each group for all of pre_post assessnents ###

#################
### By gender ###
#################

# Divide the data set by reported gender 
out <- split(drift_pre_post, drift_pre_post$Gender)

length(out$F[[1]])
length(out$M[[1]])

# Number of female students (F): 115
# Number of male students (M): 88

# Creates a summary table of each assessment per reported gender
summary_gender <- cbind(as.matrix(stat.desc(out$F$Pre_assessment_Sum)),
                        as.matrix(stat.desc(out$F$Post_assessment_Sum)),
                        as.matrix(stat.desc(out$M$Pre_assessment_Sum)),
                        as.matrix(stat.desc(out$M$Post_assessment_Sum)),
                        as.matrix(stat.desc(out$F$Diff)),
                        as.matrix(stat.desc(out$M$Diff)))


# Assigns column name to a vector (gender_vector)
colnames(summary_gender) <- c("F_Pre","F_Post","M_Pre","M_Post","F_Diff","M_Diff")

# Organizes table by assigned gender_vector
t(summary_gender)

# Results
# nbr.val nbr.null nbr.na min max range  sum median      mean   SE.mean CI.mean.0.95       var  std.dev  coef.var
# F_Pre      115        0      0   6  22    16 1611     14 14.008696 0.3193471    0.6326241 11.727994 3.424616 0.2444636
# F_Post     115        0      0   8  22    14 1855     16 16.130435 0.3520717    0.6974514 14.254767 3.775549 0.2340637
# M_Pre       88        0      0   7  22    15 1267     14 14.397727 0.4122408    0.8193732 14.954937 3.867161 0.2685953
# M_Post      88        0      0   6  22    16 1486     18 16.886364 0.4262274    0.8471730 15.986938 3.998367 0.2367808
# F_Diff     115       16      0  -6  13    19  244      2  2.121739 0.2904921    0.5754625  9.704348 3.115180 1.4682202
# M_Diff      88       12      0  -5  13    18  219      2  2.488636 0.3845210    0.7642771 13.011364 3.607127 1.4494391

# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$F$Pre_assessment_Sum,out$F$Post_assessment_Sum)
tests[[2]] <- t.test(out$M$Pre_assessment_Sum,out$M$Post_assessment_Sum)
tests[[3]] <- t.test(out$F$Pre_assessment_Sum,out$M$Pre_assessment_Sum)
tests[[4]] <- t.test(out$F$Post_assessment_Sum,out$M$Post_assessment_Sum)
tests[[5]] <- t.test(out$F$Diff,out$M$Diff)

tests_gender <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    #ci.lower = x$conf.int[1],
    #ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

# Assigns column name to a vector (gender_vector)
colnames(tests_gender) <- c("F_Pre-Post", "M_Pre-Post", "Pre_F-M","Post_F-M","Diff_F-M")

# Organizes pre and post results table by assigned gender_vector
t(tests_gender)

#            mean of x mean of y      p.value
# F_Pre-Post 14.008696 16.130435 1.271624e-05
# M_Pre-Post 14.397727 16.886364 4.309951e-05
# Pre_F-M    14.008696 14.397727 4.566476e-01
# Post_F-M   16.130435 16.886364 1.731977e-01
# Diff_F-M    2.121739  2.488636 4.475020e-01

# Creates a data frame of the mean and standard error for each group
dataframe_plot_gender <- data.frame(
  Gender_Assessment = factor(c("F_Pre","F_Post","M_Pre","M_Post"), levels=c("F_Pre","F_Post","M_Pre","M_Post")),
  Summary = summary_gender[9,1:4],
  SE.mean = summary_gender[10,1:4]
)

#Adding a new column to the data frame to bin the results
dataframe_plot_gender$bins=factor(c("Female","Female","Male","Male"))

# Creates and exports a pdf file with the ggplot of the results, tabulated by gender
pdf(paste(plots_dir,'/Gender_pre-post.pdf', sep=""),width=6,height=4)
group.colors <- c(F_Pre = "deepskyblue3", 
                  F_Post = "deepskyblue4", 
                  M_Pre ="olivedrab3", 
                  M_Post = "olivedrab4")
ggplot(data = dataframe_plot_gender, 
       aes(x=factor(bins), y=Summary, fill=Gender_Assessment)) + 
  xlab("Reported gender by assessment") +
  ylab("Mean") +
  geom_bar(stat="identity", position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=Summary-SE.mean, ymax=Summary+SE.mean), position = position_dodge(0.9), width=.2) +
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(text=element_text(size=18,  family="Helvetica"),legend.position="none") + 
  theme(panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
dev.off()

#Add the complete path to the destination pdf
embedFonts(path.expand("~/Desktop/EEO/Teaching_GeneticDrift/plots_final/Gender_pre-post.pdf"))

####################
### By First_Gen ###
####################

# Divide the data set by reported generation in college 
out <- split(drift_pre_post, drift_pre_post$First_Gen)

length(out$Y[[1]])
length(out$N[[1]])

# Number of first generation at college students (Y): 63
# Number of not first generation at college students (N): 140


# Creates a summary table of each assessment per reported generation in college
summary_first <- cbind(as.matrix(stat.desc(out$Y$Pre_assessment_Sum)),
                       as.matrix(stat.desc(out$Y$Post_assessment_Sum)),
                       as.matrix(stat.desc(out$N$Pre_assessment_Sum)),
                       as.matrix(stat.desc(out$N$Post_assessment_Sum)),
                       as.matrix(stat.desc(out$Y$Diff)),
                       as.matrix(stat.desc(out$N$Diff)))

# Assigns column name to a vector (generation_vector)
colnames(summary_first) <- c("Y_Pre","Y_Post","N_Pre","N_Post","Y_Diff","N_Diff")

# Organizes table by assigned generation_vector
t(summary_first)

# Results
# nbr.val nbr.null nbr.na min max range  sum median      mean   SE.mean CI.mean.0.95      var  std.dev  coef.var
# Y_Pre       63        0      0   6  22    16  846     13 13.428571 0.4657849    0.9310908 13.66820 3.697053 0.2753125
# Y_Post      63        0      0   6  22    16  967     15 15.349206 0.4801000    0.9597062 14.52125 3.810676 0.2482653
# N_Pre      140        0      0   7  22    15 2032     15 14.514286 0.2996205    0.5924029 12.56814 3.545157 0.2442530
# N_Post     140        0      0   8  22    14 2374     18 16.957143 0.3231193    0.6388644 14.61686 3.823200 0.2254625
# Y_Diff      63        8      0  -6  12    18  121      1  1.920635 0.4555521    0.9106356 13.07424 3.615833 1.8826235
# N_Diff     140       20      0  -4  13    17  342      2  2.442857 0.2703717    0.5345729 10.23412 3.199081 1.3095655


# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$Y$Pre_assessment_Sum,out$Y$Post_assessment_Sum)
tests[[2]] <- t.test(out$N$Pre_assessment_Sum,out$N$Post_assessment_Sum)
tests[[3]] <- t.test(out$Y$Pre_assessment_Sum,out$N$Pre_assessment_Sum)
tests[[4]] <- t.test(out$Y$Post_assessment_Sum,out$N$Post_assessment_Sum)
tests[[5]] <- t.test(out$Y$Diff,out$N$Diff)

tests_first <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    #ci.lower = x$conf.int[1],
    #ci.upper = x$conf.int[2],
    p.value = x$p.value)
})


# Assigns column name to a vector (gender_vector)
colnames(tests_first) <- c("Y_Pre-Post", "N_Pre-Post", "Pre_Y-N","Post_Y-N","Diff_Y-N")

# Organizes pre and post results table by assigned gender_vector
t(tests_first)

#              mean of x mean of y      p.value
# Y_Pre-Post 13.428571 15.349206 4.810281e-03
# N_Pre-Post 14.514286 16.957143 6.914017e-08
# Pre_Y-N    13.428571 14.514286 5.236779e-02
# Post_Y-N   15.349206 16.957143 6.341017e-03
# Diff_Y-N    1.920635  2.442857 3.264457e-01


# Creates a data frame of the mean and standard error for each group
dataframe_plot_generation <- data.frame(
  Generation_Assessment = factor(c("Y_Pre", "Y_Post", "N_Pre","N_Post"), levels=c("Y_Pre", "Y_Post", "N_Pre","N_Post")),
  Summary = summary_first[9,1:4],
  SE.mean = summary_first[10,1:4]
)

#Adding a new column to the data frame to bin the results
dataframe_plot_generation$bins=factor(c("First generation","First generation","Not first generation","Not first generation"))

# Creates and exports a pdf file with the ggplot of the results, tabulated by generation
pdf(paste(plots_dir,'/First_pre-post.pdf', sep=""),width=6,height=4)
group.colors <- c(Y_Pre = "deepskyblue3", Y_Post = "deepskyblue4", N_Pre ="olivedrab3", N_Post = "olivedrab4")
ggplot(data = dataframe_plot_generation, 
       aes(x=factor(bins), y=Summary, fill=Generation_Assessment)) + 
  xlab("Reported generation by assessment") +
  ylab("Mean") +
  geom_bar(stat="identity", position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=Summary-SE.mean, ymax=Summary+SE.mean), position = position_dodge(0.9), width=.2) +
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(text=element_text(size=18,  family="Helvetica"),legend.position="none") + 
  theme(panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
dev.off()

#Add the complete path to the destination pdf
embedFonts(path.expand("~/Desktop/EEO/Teaching_GeneticDrift/plots_final/First_pre-post.pdf"))

########################
### By Bio340_at_ASU ###
########################

# Divide the data set by genetics course taken at the same institution or not
out <- split(drift_pre_post, drift_pre_post$Bio340_at_ASU)

length(out$Y[[1]])
length(out$N[[1]])

# Genetics course taken at the same institution (Y): 198
# Genetics course taken at the same institution (N): 5

# Not enough resolution to continue analysis


####################
### By Ethnicity ###
####################

# Divide the data set by ethnicity
out <- split(drift_pre_post, drift_pre_post$Ethnicity)

length(out$'1'[[1]])
length(out$'2'[[1]])
length(out$'3'[[1]])
length(out$'4'[[1]])
length(out$'5'[[1]])
length(out$'6'[[1]])

# Ethnicity category (1): 2
# Ethnicity category (2): 28
# Ethnicity category (3): 2
# Ethnicity category (4): 26
# Ethnicity category (5): 2
# Ethnicity category (6): 119
# Ethnicity category (More_than_one): 24 


# Creates a summary table of each assessment per reported ethnicity
summary_ethnicity <- cbind(as.matrix(stat.desc(out$'2'$Pre_assessment_Sum)),
                           as.matrix(stat.desc(out$'2'$Post_assessment_Sum)),
                           as.matrix(stat.desc(out$'4'$Pre_assessment_Sum)),
                           as.matrix(stat.desc(out$'4'$Post_assessment_Sum)),
                           as.matrix(stat.desc(out$'6'$Pre_assessment_Sum)),
                           as.matrix(stat.desc(out$'6'$Post_assessment_Sum)),
                           as.matrix(stat.desc(out$'2'$Diff)),
                           as.matrix(stat.desc(out$'4'$Diff)),
                           as.matrix(stat.desc(out$'6'$Diff)))

# Assigns column name to a vector (ethnicity_vector)
colnames(summary_ethnicity) <- c("2_Pre","2_Post","4_Pre","4_Post","6_Pre","6_Post","2_Diff","4_Diff","6_Diff")

# Organizes table by assigned ethnicity_vector
t(summary_ethnicity)

# Results
# nbr.val nbr.null nbr.na min max range  sum median      mean   SE.mean CI.mean.0.95       var  std.dev  coef.var
# 2_Pre       28        0      0   7  22    15  374   12.0 13.357143 0.7259628    1.4895525 14.756614 3.841434 0.2875940
# 2_Post      28        0      0   8  22    14  450   16.5 16.071429 0.7290796    1.4959478 14.883598 3.857927 0.2400488
# 4_Pre       26        0      0   6  19    13  339   13.0 13.038462 0.6621196    1.3636608 11.398462 3.376161 0.2589386
# 4_Post      26        0      0  10  22    12  410   16.0 15.769231 0.6134585    1.2634415  9.784615 3.128037 0.1983633
# 6_Pre      119        0      0   7  22    15 1754   15.0 14.739496 0.3260652    0.6456978 12.651901 3.556951 0.2413211
# 6_Post     119        0      0   8  22    14 2004   17.0 16.840336 0.3558339    0.7046479 15.067512 3.881689 0.2304995
# 2_Diff      28        3      0  -3   9    12   76    2.0  2.714286 0.6441785    1.3217452 11.619048 3.408672 1.2558267
# 4_Diff      26        2      0  -6  13    19   71    3.0  2.730769 0.8237955    1.6966386 17.644615 4.200549 1.5382294
# 6_Diff     119       19      0  -4  13    17  250    2.0  2.100840 0.2970191    0.5881786 10.498220 3.240096 1.5422855


# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$'2'$Pre_assessment_Sum,out$'2'$Post_assessment_Sum)
tests[[2]] <- t.test(out$'4'$Pre_assessment_Sum,out$'4'$Post_assessment_Sum)
tests[[3]] <- t.test(out$'6'$Pre_assessment_Sum,out$'6'$Post_assessment_Sum)

tests[[4]] <- t.test(out$'2'$Pre_assessment_Sum,out$'4'$Pre_assessment_Sum)
tests[[5]] <- t.test(out$'2'$Pre_assessment_Sum,out$'6'$Pre_assessment_Sum)
tests[[6]] <- t.test(out$'4'$Pre_assessment_Sum,out$'6'$Pre_assessment_Sum)

tests[[7]] <- t.test(out$'2'$Post_assessment_Sum,out$'4'$Post_assessment_Sum)
tests[[8]] <- t.test(out$'2'$Post_assessment_Sum,out$'6'$Post_assessment_Sum)
tests[[9]] <- t.test(out$'4'$Post_assessment_Sum,out$'6'$Post_assessment_Sum)

tests[[10]] <- t.test(out$'2'$Diff,out$'4'$Diff)
tests[[11]] <- t.test(out$'2'$Diff,out$'6'$Diff)
tests[[12]] <- t.test(out$'4'$Diff,out$'6'$Diff)

tests_ethnicity <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

# Assigns column name to a vector (ethnicity_vector)
colnames(tests_ethnicity) <- c("2_Pre-Post","4_Pre-Post","6_Pre-Post","Pre_2-4","Pre_2-6","Pre_4-6","Post_2-4","Post_2-6","Post_4-6","Diff_2-4","Diff_2-6","Diff_4-6")

# Organizes pre and post results table by assigned gender_vector
t(tests_ethnicity)

#            mean of x mean of y      p.value
# 2_Pre-Post 13.357143 16.071429 1.086908e-02
# 4_Pre-Post 13.038462 15.769231 3.924778e-03
# 6_Pre-Post 14.739496 16.840336 2.008336e-05
# Pre_2-4    13.357143 13.038462 7.469871e-01
# Pre_2-6    13.357143 14.739496 9.035510e-02
# Pre_4-6    13.038462 14.739496 2.671870e-02
# Post_2-4   16.071429 15.769231 7.524163e-01
# Post_2-6   16.071429 16.840336 3.488173e-01
# Post_4-6   15.769231 16.840336 1.381736e-01
# Diff_2-4    2.714286  2.730769 9.874891e-01
# Diff_2-6    2.714286  2.100840 3.924021e-01
# Diff_4-6    2.730769  2.100840 4.771841e-01


########
## POC vs White
########

# Ethnicities are recoded into POC vs white
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==1] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==2] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==3] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==4] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==5] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==6] <- 2

# Students who reported more than one ethnicity are also recoded on the same manner
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity!=2] <- 1

# Divide the data set by POC vs White
out <- split(drift_pre_post, drift_pre_post$Ethnicity)

length(out$'1'[[1]])
length(out$'2'[[1]])

# POC Ethnicity (1): 84 
# White Ethnicity (2): 119 

# Creates a summary table of each assessment per reported ethnicity
summary_ethnicity <- cbind(as.matrix(stat.desc(out$'1'$Pre_assessment_Sum)),
                           as.matrix(stat.desc(out$'1'$Post_assessment_Sum)),
                           as.matrix(stat.desc(out$'2'$Pre_assessment_Sum)),
                           as.matrix(stat.desc(out$'2'$Post_assessment_Sum)),
                           as.matrix(stat.desc(out$'1'$Diff)),
                           as.matrix(stat.desc(out$'2'$Diff)))

# Assigns column name to a vector (ethnicity_vector)
colnames(summary_ethnicity) <- c("POC_Pre","POC_Post","White_Pre","White_Post","POC_Diff","White_Diff")

# Organizes table by assigned ethnicity_vector
t(summary_ethnicity)

# Results
# nbr.val nbr.null nbr.na min max range  sum median      mean   SE.mean CI.mean.0.95      var  std.dev  coef.var
# POC_Pre         84        0      0   6  22    16 1124     13 13.380952 0.3902515    0.7761946 12.79289 3.576714 0.2672989
# POC_Post        84        0      0   6  22    16 1337     16 15.916667 0.4190694    0.8335121 14.75201 3.840834 0.2413090
# White_Pre      119        0      0   7  22    15 1754     15 14.739496 0.3260652    0.6456978 12.65190 3.556951 0.2413211
# White_Post     119        0      0   8  22    14 2004     17 16.840336 0.3558339    0.7046479 15.06751 3.881689 0.2304995
# POC_Diff        84        9      0  -6  13    19  213      2  2.535714 0.3781338    0.7520930 12.01076 3.465654 1.3667368
# White_Diff     119       19      0  -4  13    17  250      2  2.100840 0.2970191    0.5881786 10.49822 3.240096 1.5422855

# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$'1'$Pre_assessment_Sum,out$'1'$Post_assessment_Sum)
tests[[2]] <- t.test(out$'2'$Pre_assessment_Sum,out$'2'$Post_assessment_Sum)

tests[[3]] <- t.test(out$'1'$Pre_assessment_Sum,out$'2'$Pre_assessment_Sum)
tests[[4]] <- t.test(out$'1'$Post_assessment_Sum,out$'2'$Post_assessment_Sum)

tests[[5]] <- t.test(out$'1'$Diff,out$'2'$Diff)

tests_ethnicity <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

# Assigns column name to a vector (ethnicity_vector)
colnames(tests_ethnicity) <- c("POC_Pre","POC_Post","Pre_White","Post_White","Diff_POC-White")

# Organizes pre and post results table by assigned gender_vector
t(tests_ethnicity)

# Results
# mean of x mean of y      p.value
# POC_Pre        13.380952  15.91667 1.722874e-05
# POC_Post       14.739496  16.84034 2.008336e-05
# Pre_White      13.380952  14.73950 8.252112e-03
# Post_White     15.916667  16.84034 9.466662e-02
# Diff_POC-White  2.535714   2.10084 3.670503e-01


# Creates a data frame of the mean and standard error for each group
dataframe_plot_ethnicity <- data.frame(
  Ethnicity_Assessment = factor(c("POC_Pre","POC_Post","White_Pre","White_Post"), levels=c("POC_Pre","POC_Post","White_Pre","White_Post")),
  Summary = summary_ethnicity[9,1:4],
  SE.mean = summary_ethnicity[10,1:4]
)

#Adding a new column to the data frame to bin the results
dataframe_plot_ethnicity$bins=factor(c("POC","POC","White","White"))

# Creates and exports a pdf file with the ggplot of the results, tabulated by ethnicity
pdf(paste(plots_dir,'/Ethnicity_pre-post.pdf', sep=""),width=6,height=4)
group.colors <- c(POC_Pre = "deepskyblue3", POC_Post = "deepskyblue4", White_Pre ="olivedrab3", White_Post = "olivedrab4")
ggplot(data = dataframe_plot_ethnicity, 
       aes(x=factor(bins), y=Summary, fill=Ethnicity_Assessment)) + 
  xlab("Reported ethnicity by assessment") +
  ylab("Mean") +
  geom_bar(stat="identity", position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=Summary-SE.mean, ymax=Summary+SE.mean), position = position_dodge(0.9), width=.2) +
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(text=element_text(size=18,  family="Helvetica"),legend.position="none") + 
  theme(panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
dev.off()

#Add the complete path to the destination pdf
embedFonts(path.expand("~/Desktop/EEO/Teaching_GeneticDrift/plots_final/Ethnicity_pre-post.pdf"))

#######################
### By Letter_Grade ###
#######################

# Divide the data set by letter grade
out <- split(drift_pre_post, drift_pre_post$Letter_Grade)

length(out$A[[1]])
length(out$B[[1]])
length(out$C[[1]])
length(out$D[[1]])
length(out$E[[1]])

# Letter grade A: 78
# Letter grade B: 84
# Letter grade C: 41
# Letter grade D: 0
# Letter grade E: 0

# Creates a summary table of each assessment per final grade
summary_grades <- cbind(as.matrix(stat.desc(out$A$Pre_assessment_Sum)),
                        as.matrix(stat.desc(out$A$Post_assessment_Sum)),
                        as.matrix(stat.desc(out$B$Pre_assessment_Sum)),
                        as.matrix(stat.desc(out$B$Post_assessment_Sum)),
                        as.matrix(stat.desc(out$C$Pre_assessment_Sum)),
                        as.matrix(stat.desc(out$C$Post_assessment_Sum)),
                        as.matrix(stat.desc(out$A$Diff)),
                        as.matrix(stat.desc(out$B$Diff)),
                        as.matrix(stat.desc(out$C$Diff)))

# Assigns column name to a vector (grade_vector)
colnames(summary_grades) <- c("A_Pre","A_Post","B_Pre","B_Post","C_Pre","C_Post","A_Diff","B_Diff","C_Diff")

# Organizes table by assigned grade_vector
t(summary_grades)

# Results
# nbr.val nbr.null nbr.na min max range  sum median      mean   SE.mean CI.mean.0.95       var  std.dev  coef.var
# A_Pre       78        0      0   6  22    16 1206     16 15.461538 0.3804638    0.7576002 11.290709 3.360165 0.2173241
# A_Post      78        0      0   8  22    14 1420     19 18.205128 0.3863984    0.7694174 11.645688 3.412578 0.1874515
# B_Pre       84        0      0   7  22    15 1191     14 14.178571 0.3859111    0.7675616 12.509897 3.536933 0.2494562
# B_Post      84        0      0   6  22    16 1327     16 15.797619 0.4199486    0.8352608 14.813970 3.848892 0.2436375
# C_Pre       41        0      0   7  22    15  481     11 11.731707 0.4711486    0.9522268  9.101220 3.016823 0.2571512
# C_Post      41        0      0   8  22    14  594     14 14.487805 0.5433881    1.0982284 12.106098 3.479382 0.2401593
# A_Diff      78        8      0  -3  12    15  214      2  2.743590 0.3674375    0.7316615 10.530803 3.245120 1.1828007
# B_Diff      84       13      0  -5  12    17  136      1  1.619048 0.3301214    0.6565982  9.154332 3.025613 1.8687607
# C_Diff      41        7      0  -6  13    19  113      3  2.756098 0.6106579    1.2341856 15.289024 3.910118 1.4187155


# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$A$Pre_assessment_Sum,out$A$Post_assessment_Sum)
tests[[2]] <- t.test(out$B$Pre_assessment_Sum,out$B$Post_assessment_Sum)
tests[[3]] <- t.test(out$C$Pre_assessment_Sum,out$C$Post_assessment_Sum)

tests[[4]] <- t.test(out$A$Pre_assessment_Sum,out$B$Pre_assessment_Sum)
tests[[5]] <- t.test(out$A$Pre_assessment_Sum,out$C$Pre_assessment_Sum)
tests[[6]] <- t.test(out$B$Pre_assessment_Sum,out$C$Pre_assessment_Sum)

tests[[7]] <- t.test(out$A$Post_assessment_Sum,out$B$Post_assessment_Sum)
tests[[8]] <- t.test(out$A$Post_assessment_Sum,out$C$Post_assessment_Sum)
tests[[9]] <- t.test(out$B$Post_assessment_Sum,out$C$Post_assessment_Sum)

tests[[10]] <- t.test(out$A$Diff,out$B$Diff)
tests[[11]] <- t.test(out$A$Diff,out$C$Diff)
tests[[12]] <- t.test(out$B$Diff,out$C$Diff)

tests_grades <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

# Assigns column name to a vector (ethnicity_vector)
colnames(tests_grades) <- c("A_Pre-Post","B_Pre-Post","C_Pre-Post","Pre_A-B","Pre_A-C","Pre_B-C","Post_A-B","Post_A-C","Post_B-C","Diff_A-B","Diff_A-C","Diff_B-C")

# Organizes pre and post results table by assigned gender_vector
t(tests_grades)

#            mean of x mean of y      p.value
# A_Pre-Post 15.461538 18.205128 1.183279e-06
# B_Pre-Post 14.178571 15.797619 5.098950e-03
# C_Pre-Post 11.731707 14.487805 2.547500e-04
# Pre_A-B    15.461538 14.178571 1.910780e-02
# Pre_A-C    15.461538 11.731707 2.046140e-08
# Pre_B-C    14.178571 11.731707 1.201891e-04
# Post_A-B   18.205128 15.797619 4.105917e-05
# Post_A-C   18.205128 14.487805 3.236837e-07
# Post_B-C   15.797619 14.487805 5.978335e-02
# Diff_A-B    2.743590  1.619048 2.416262e-02
# Diff_A-C    2.743590  2.756098 9.860478e-01
# Diff_B-C    1.619048  2.756098 1.063217e-01


# Creates a data frame of the mean and standard error for each group
dataframe_plot_grade <- data.frame(
  Grade_Assessment = factor(c("A_Pre","A_Post","B_Pre","B_Post","C_Pre","C_Post"),
                            levels=c("A_Pre","A_Post","B_Pre","B_Post","C_Pre","C_Post")),
  Summary = summary_grades[9,1:6],
  SE.mean = summary_grades[10,1:6]
)

#Adding a new column to the data frame to bin the results
dataframe_plot_grade$bins=factor(c("A","A","B","B","C","C"))

# Creates and exports a pdf file with the ggplot of the results, tabulated by final letter grade
pdf(paste(plots_dir,'/Grades_pre-post.pdf', sep=""),width=6,height=4)
group.colors <- c(A_Pre = "deepskyblue3", A_Post = "deepskyblue4", B_Pre ="olivedrab3", B_Post = "olivedrab4", C_Pre = "firebrick3", C_Post = "firebrick4")
ggplot(data = dataframe_plot_grade, 
       aes(x=factor(bins), y=Summary, fill=Grade_Assessment)) + 
  xlab("Final grade by assessment") +
  ylab("Mean") +
  geom_bar(stat="identity", position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=Summary-SE.mean, ymax=Summary+SE.mean), position = position_dodge(0.9), width=.2) +
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(panel.background = element_blank(), panel.border = element_blank()) +
  theme(text=element_text(size=18,  family="Helvetica"),legend.position="none") + 
  theme(axis.line = element_line(colour = "black"))
dev.off()

#Add the complete path to the destination pdf
embedFonts(path.expand("~/Desktop/EEO/Teaching_GeneticDrift/plots_final/Grades_pre-post.pdf"))

##############################
### By RecitationPair_Time ###
##############################

# Divide the data set by assigned TA pair and time
out <- split(drift_pre_post, drift_pre_post$RecitationPair_Time)

length(out$Pair1_300[[1]])
length(out$Pair1_430[[1]])
length(out$Pair1_600[[1]])
length(out$Pair1_730[[1]])

length(out$Pair2_300[[1]])
length(out$Pair2_430[[1]])
length(out$Pair2_600[[1]])
length(out$Pair2_730[[1]])

# Pair1_300: 31
# Pair1_430: 28
# Pair1_600: 25
# Pair1_730: 11
# Pair2_300: 28
# Pair2_430: 38
# Pair2_600: 21
# Pair2_730: 21

# Creates a summary table of each assessment per final grade
summary_pair <- cbind(as.matrix(stat.desc(out$Pair1_300$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_300$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_430$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_430$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_600$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_600$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_730$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_730$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_300$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_300$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_430$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_430$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_600$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_600$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_730$Pre_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair2_730$Post_assessment_Sum)),
                      as.matrix(stat.desc(out$Pair1_300$Diff)),
                      as.matrix(stat.desc(out$Pair1_430$Diff)),
                      as.matrix(stat.desc(out$Pair1_600$Diff)),
                      as.matrix(stat.desc(out$Pair1_730$Diff)),
                      as.matrix(stat.desc(out$Pair2_300$Diff)),
                      as.matrix(stat.desc(out$Pair2_430$Diff)),
                      as.matrix(stat.desc(out$Pair2_600$Diff)),
                      as.matrix(stat.desc(out$Pair2_730$Diff)))

# Assigns column name to a vector (pairandtime_vector)
colnames(summary_pair) <- c("Pair1_300_Pre","Pair1_300_Post","Pair1_430_Pre","Pair1_430_Post",
                            "Pair1_600_Pre","Pair1_600_Post","Pair1_730_Pre","Pair1_730_Post",
                            "Pair2_300_Pre","Pair2_300_Post","Pair2_430_Pre","Pair2_430_Post",
                            "Pair2_600_Pre","Pair2_600_Post","Pair2_730_Pre","Pair2_730_Post",
                            "Pair1_300_Diff","Pair1_430_Diff","Pair1_600_Diff","Pair1_730_Diff",
                            "Pair2_300_Diff","Pair2_430_Diff","Pair2_600_Diff","Pair2_730_Diff")

# Organizes table by assigned pairandtime_vector
t(summary_pair)

# Results
# nbr.val nbr.null nbr.na min max range sum median       mean   SE.mean CI.mean.0.95       var  std.dev   coef.var
# Pair1_300_Pre       31        0      0   8  22    14 458   14.0 14.7741935 0.6336609    1.2941083 12.447312 3.528075  0.2387998
# Pair1_300_Post      31        0      0   8  22    14 536   18.0 17.2903226 0.6834346    1.3957597 14.479570 3.805203  0.2200770
# Pair1_430_Pre       28        0      0   7  20    13 352   12.0 12.5714286 0.5828877    1.1959869  9.513228 3.084352  0.2453462
# Pair1_430_Post      28        0      0   9  22    13 407   14.0 14.5357143 0.7004952    1.4372974 13.739418 3.706672  0.2550045
# Pair1_600_Pre       25        0      0  11  21    10 409   16.0 16.3600000 0.5127703    1.0583058  6.573333 2.563851  0.1567146
# Pair1_600_Post      25        0      0   8  22    14 457   19.0 18.2800000 0.6416645    1.3243304 10.293333 3.208323  0.1755100
# Pair1_730_Pre       11        0      0   9  22    13 160   14.0 14.5454545 1.2237323    2.7266454 16.472727 4.058661  0.2790329
# Pair1_730_Post      11        0      0   6  21    15 163   15.0 14.8181818 1.2707777    2.8314692 17.763636 4.214693  0.2844271
# Pair2_300_Pre       28        0      0   7  22    15 383   14.5 13.6785714 0.7418937    1.5222401 15.411376 3.925732  0.2869987
# Pair2_300_Post      28        0      0  10  22    12 474   16.0 16.9285714 0.7522956    1.5435830 15.846561 3.980774  0.2351512
# Pair2_430_Pre       38        0      0   6  22    16 534   13.5 14.0526316 0.6693316    1.3561946 17.024182 4.126037  0.2936131
# Pair2_430_Post      38        0      0   9  22    13 628   16.5 16.5263158 0.6019608    1.2196885 13.769559 3.710736  0.2245350
# Pair2_600_Pre       21        0      0  10  19     9 282   13.0 13.4285714 0.6346031    1.3237589  8.457143 2.908117  0.2165619
# Pair2_600_Post      21        0      0   8  22    14 321   16.0 15.2857143 0.8169131    1.7040508 14.014286 3.743566  0.2449062
# Pair2_730_Pre       21        0      0   8  21    13 300   14.0 14.2857143 0.7781016    1.6230916 12.714286 3.565710  0.2495997
# Pair2_730_Post      21        0      0  10  22    12 355   18.0 16.9047619 0.8726117    1.8202362 15.990476 3.998809  0.2365493
# Pair1_300_Diff      31        6      0  -4  12    16  78    2.0  2.5161290 0.6322361    1.2911984 12.391398 3.520142  1.3990307
# Pair1_430_Diff      28        2      0  -6   8    14  55    2.0  1.9642857 0.6351314    1.3031820 11.294974 3.360800  1.7109525
# Pair1_600_Diff      25        2      0  -3   7    10  48    1.0  1.9200000 0.4722993    0.9747779  5.576667 2.361497  1.2299462
# Pair1_730_Diff      11        2      0  -5   6    11   3   -1.0  0.2727273 1.0188310    2.2700968 11.418182 3.379080 12.3899601
# Pair2_300_Diff      28        3      0  -3  13    16  91    2.5  3.2500000 0.7285286    1.4948173 14.861111 3.855011  1.1861573
# Pair2_430_Diff      38        6      0  -2  13    15  94    1.5  2.4736842 0.5616183    1.1379468 11.985775 3.462048  1.3995513
# Pair2_600_Diff      21        3      0  -4   7    11  39    1.0  1.8571429 0.7568283    1.5787162 12.028571 3.468223  1.8675047
# Pair2_730_Diff      21        4      0  -1   9    10  55    3.0  2.6190476 0.5955237    1.2422408  7.447619 2.729033  1.0419943

# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$Pair1_300$Pre_assessment_Sum,out$Pair1_300$Post_assessment_Sum)
tests[[2]] <- t.test(out$Pair1_430$Pre_assessment_Sum,out$Pair1_430$Post_assessment_Sum)
tests[[3]] <- t.test(out$Pair1_600$Pre_assessment_Sum,out$Pair1_600$Post_assessment_Sum)
tests[[4]] <- t.test(out$Pair1_730$Pre_assessment_Sum,out$Pair1_730$Post_assessment_Sum)

tests[[5]] <- t.test(out$Pair2_300$Pre_assessment_Sum,out$Pair2_300$Post_assessment_Sum)
tests[[6]] <- t.test(out$Pair2_430$Pre_assessment_Sum,out$Pair2_430$Post_assessment_Sum)
tests[[7]] <- t.test(out$Pair2_600$Pre_assessment_Sum,out$Pair2_600$Post_assessment_Sum)
tests[[8]] <- t.test(out$Pair2_730$Pre_assessment_Sum,out$Pair2_730$Post_assessment_Sum)

tests[[9]] <- t.test(out$Pair1_300$Pre_assessment_Sum,out$Pair2_300$Pre_assessment_Sum)
tests[[10]] <- t.test(out$Pair1_430$Pre_assessment_Sum,out$Pair2_430$Pre_assessment_Sum)
tests[[11]] <- t.test(out$Pair1_600$Pre_assessment_Sum,out$Pair2_600$Pre_assessment_Sum)
tests[[12]] <- t.test(out$Pair1_730$Pre_assessment_Sum,out$Pair2_730$Pre_assessment_Sum)

tests[[13]] <- t.test(out$Pair1_300$Post_assessment_Sum,out$Pair2_300$Post_assessment_Sum)
tests[[14]] <- t.test(out$Pair1_430$Post_assessment_Sum,out$Pair2_430$Post_assessment_Sum)
tests[[15]] <- t.test(out$Pair1_600$Post_assessment_Sum,out$Pair2_600$Post_assessment_Sum)
tests[[16]] <- t.test(out$Pair1_730$Post_assessment_Sum,out$Pair2_730$Post_assessment_Sum)

tests[[17]] <- t.test(out$Pair1_300$Diff,out$Pair2_300$Diff)
tests[[18]] <- t.test(out$Pair1_430$Diff,out$Pair2_430$Diff)
tests[[19]] <- t.test(out$Pair1_600$Diff,out$Pair2_600$Diff)
tests[[20]] <- t.test(out$Pair1_730$Diff,out$Pair2_730$Diff)

tests_pair <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

# Assigns column name to a vector (ethnicity_vector)
colnames(tests_pair) <- c("Pair1_300_Pre-Post","Pair1_430_Pre-Post","Pair1_600_Pre-Post","Pair1_730_Pre-Post","Pair2_300_Pre-Post","Pair2_430_Pre-Post","Pair2_600_Pre-Post","Pair2_730_Pre-Post","Pair1-Pair2_300_Pre","Pair1-Pair2_430_Pre","Pair1-Pair2_600_Pre","Pair1-Pair2_730_Pre","Pair1-Pair2_300_Post","Pair1-Pair2_430_Post","Pair1-Pair2_600_Post","Pair1-Pair2_730_Post","Pair1-Pair2_300_Diff","Pair1-Pair2_430_Diff","Pair1-Pair2_600_Diff","Pair1-Pair2_730_Diff")

# Organizes pre and post results table by assigned gender_vector
t(tests_pair)

#            mean of x mean of y      p.value
# Pair1_300_Pre-Post    14.77419  17.29032 0.009015568
# Pair1_430_Pre-Post    12.57143  14.53571 0.035747852
# Pair1_600_Pre-Post    16.36000  18.28000 0.023840535
# Pair1_730_Pre-Post    14.54545  14.81818 0.878696230    #This is the smallest group - 11 students
# Pair2_300_Pre-Post    13.67857  16.92857 0.003290523
# Pair2_430_Pre-Post    14.05263  16.52632 0.007547250
# Pair2_600_Pre-Post    13.42857  15.28571 0.080625019
# Pair2_730_Pre-Post    14.28571  16.90476 0.030779804
# Pair1-Pair2_300_Pre   14.77419  13.67857 0.266374633
# Pair1-Pair2_430_Pre   12.57143  14.05263 0.100034595
# Pair1-Pair2_600_Pre   16.36000  13.42857 0.000879890   # Pair1_600 is the honor's section
# Pair1-Pair2_730_Pre   14.54545  14.28571 0.859825746
# Pair1-Pair2_300_Post  17.29032  16.92857 0.723242880
# Pair1-Pair2_430_Post  14.53571  16.52632 0.035278460
# Pair1-Pair2_600_Post  18.28000  15.28571 0.006339062   # Pair1_600 is the honor's section
# Pair1-Pair2_730_Post  14.81818  16.90476 0.191365100
# Pair1-Pair2_300_Diff  2.5161290  3.250000 0.450033327
# Pair1-Pair2_430_Diff  1.9642857  2.473684 0.550243717
# Pair1-Pair2_600_Diff  1.9200000  1.857143 0.944237674
# Pair1-Pair2_730_Diff  0.2727273  2.619048 0.063128081


# Creates a data frame of the mean and standard error for each group
dataframe_plot_pair <- data.frame(
  Pair_Assessment = factor(c("Pair1_300_Pre","Pair1_300_Post","Pair1_430_Pre","Pair1_430_Post",
                             "Pair1_600_Pre","Pair1_600_Post","Pair1_730_Pre","Pair1_730_Post",
                             "Pair2_300_Pre","Pair2_300_Post","Pair2_430_Pre","Pair2_430_Post",
                             "Pair2_600_Pre","Pair2_600_Post","Pair2_730_Pre","Pair2_730_Post"),
                           levels=c("Pair1_300_Pre","Pair1_300_Post","Pair1_430_Pre","Pair1_430_Post",
                                    "Pair1_600_Pre","Pair1_600_Post","Pair1_730_Pre","Pair1_730_Post",
                                    "Pair2_300_Pre","Pair2_300_Post","Pair2_430_Pre","Pair2_430_Post",
                                    "Pair2_600_Pre","Pair2_600_Post","Pair2_730_Pre","Pair2_730_Post")),
  Summary = summary_pair[9,1:16],
  SE.mean = summary_pair[10,1:16]
)


#Adding a new column to the data frame to bin the results_V2
dataframe_plot_pair$bins=factor(c("3:00PM","3:00PM","4:30PM","4:30PM","6:00PM","6:00PM","7:30PM","7:30PM",
                                  " 3:00PM "," 3:00PM "," 4:30PM "," 4:30PM "," 6:00PM "," 6:00PM "," 7:30PM "," 7:30PM "))

dataframe_plot_pair$bins <- factor(dataframe_plot_pair$bins, levels=unique(dataframe_plot_pair$bins))

# Creates and exports a pdf file with the ggplot of the results, tabulated by recitation group V2
pdf(paste(plots_dir,'/Pair_pre-post.pdf', sep=""),width=6,height=4) #REMEMBER TO CHANGE THE NAME ONCE DECIDED THE VERSION
group.colors <- c(Pair1_300_Pre = "deepskyblue3", 
                  Pair1_300_Post = "deepskyblue4", 
                  Pair1_430_Pre ="olivedrab3", 
                  Pair1_430_Post = "olivedrab4", 
                  Pair1_600_Pre = "firebrick3", 
                  Pair1_600_Post = "firebrick4",
                  Pair1_730_Pre = "hotpink3",
                  Pair1_730_Post = "hotpink4",
                  Pair2_300_Pre = "cadetblue3", 
                  Pair2_300_Post = "cadetblue4", 
                  Pair2_430_Pre ="darkseagreen2", 
                  Pair2_430_Post = "darkseagreen", 
                  Pair2_600_Pre = "indianred1", 
                  Pair2_600_Post = "indianred3",
                  Pair2_730_Pre = "plum1",
                  Pair2_730_Post = "plum")


ggplot(data = dataframe_plot_pair, 
       aes(x=factor(bins), y=Summary, fill=Pair_Assessment)) + 
  xlab("TA pair and time by assessment") +
  ylab("Mean") +
  ggtitle("TA pair 1                        TA pair 2") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_bar(stat="identity", position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=Summary-SE.mean, ymax=Summary+SE.mean), position = position_dodge(0.9), width=.2) +
  geom_vline(xintercept = 4.5, colour = "gray40", linetype = "dashed") + 
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(text=element_text(size=15, family="Helvetica"), legend.position="none")+#, axis.text.x=element_text(angle = 60, hjust = 1)) + 
  theme(panel.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
dev.off()

#Add the complete path to the destination pdf
embedFonts(path.expand("~/Desktop/EEO/Teaching_GeneticDrift/plots_final/Pair_pre-post.pdf"))


# Gender
# First_Gen
# Bio340_at_ASU - not enough resolution # Y: 198; N: 5
# Ethnicity
# Letter_Grade
# RecitationPair_Time

# t(summary_pre_post)
# t(summary_gender)
# t(summary_first)
# t(summary_ethnicity)
# t(tests_grades)


######################################################
##### Now test how students did on each question #####
######################################################

# First compute the difference between the post-test and pre-test
Q1 <- drift_pre_post$Post_1-drift_pre_post$Pre_1
Q2 <- drift_pre_post$Post_2-drift_pre_post$Pre_2
Q3 <- drift_pre_post$Post_3-drift_pre_post$Pre_3
Q4 <- drift_pre_post$Post_4-drift_pre_post$Pre_4
Q5 <- drift_pre_post$Post_5-drift_pre_post$Pre_5
Q6 <- drift_pre_post$Post_6-drift_pre_post$Pre_6
Q7 <- drift_pre_post$Post_7-drift_pre_post$Pre_7
Q8 <- drift_pre_post$Post_8-drift_pre_post$Pre_8
Q9 <- drift_pre_post$Post_9-drift_pre_post$Pre_9
Q10 <- drift_pre_post$Post_10-drift_pre_post$Pre_10
Q11 <- drift_pre_post$Post_11-drift_pre_post$Pre_11
Q12 <- drift_pre_post$Post_12-drift_pre_post$Pre_12
Q13 <- drift_pre_post$Post_13-drift_pre_post$Pre_13
Q14 <- drift_pre_post$Post_14-drift_pre_post$Pre_14
Q15 <- drift_pre_post$Post_15-drift_pre_post$Pre_15
Q16 <- drift_pre_post$Post_16-drift_pre_post$Pre_16
Q17 <- drift_pre_post$Post_17-drift_pre_post$Pre_17
Q18 <- drift_pre_post$Post_18-drift_pre_post$Pre_18
Q19 <- drift_pre_post$Post_19-drift_pre_post$Pre_19
Q20 <- drift_pre_post$Post_20-drift_pre_post$Pre_20
Q21 <- drift_pre_post$Post_21-drift_pre_post$Pre_21
Q22 <- drift_pre_post$Post_22-drift_pre_post$Pre_22

# But the difference will be ambiguous, so also compute the sum
Q1p <- drift_pre_post$Post_1+drift_pre_post$Pre_1
Q2p <- drift_pre_post$Post_2+drift_pre_post$Pre_2
Q3p <- drift_pre_post$Post_3+drift_pre_post$Pre_3
Q4p <- drift_pre_post$Post_4+drift_pre_post$Pre_4
Q5p <- drift_pre_post$Post_5+drift_pre_post$Pre_5
Q6p <- drift_pre_post$Post_6+drift_pre_post$Pre_6
Q7p <- drift_pre_post$Post_7+drift_pre_post$Pre_7
Q8p <- drift_pre_post$Post_8+drift_pre_post$Pre_8
Q9p <- drift_pre_post$Post_9+drift_pre_post$Pre_9
Q10p <- drift_pre_post$Post_10+drift_pre_post$Pre_10
Q11p <- drift_pre_post$Post_11+drift_pre_post$Pre_11
Q12p <- drift_pre_post$Post_12+drift_pre_post$Pre_12
Q13p <- drift_pre_post$Post_13+drift_pre_post$Pre_13
Q14p <- drift_pre_post$Post_14+drift_pre_post$Pre_14
Q15p <- drift_pre_post$Post_15+drift_pre_post$Pre_15
Q16p <- drift_pre_post$Post_16+drift_pre_post$Pre_16
Q17p <- drift_pre_post$Post_17+drift_pre_post$Pre_17
Q18p <- drift_pre_post$Post_18+drift_pre_post$Pre_18
Q19p <- drift_pre_post$Post_19+drift_pre_post$Pre_19
Q20p <- drift_pre_post$Post_20+drift_pre_post$Pre_20
Q21p <- drift_pre_post$Post_21+drift_pre_post$Pre_21
Q22p <- drift_pre_post$Post_22+drift_pre_post$Pre_22

# Now we bind the results
out <- cbind(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20,Q21,Q22)
outp <- cbind(Q1p,Q2p,Q3p,Q4p,Q5p,Q6p,Q7p,Q8p,Q9p,Q10p,Q11p,Q12p,Q13p,Q14p,Q15p,Q16p,Q17p,Q18p,Q19p,Q20p,Q21p,Q22p)

# And compute table with unambigous response
test <- out/outp

# Now rename so we can compute the total of how students performed on each question
test[is.nan(test)] <- 2
test[test==-1] <-3
test[test==0] <-4

########
## 1 = incorrect on pre-test, correct on post-test
## 2 = no change, incorrect both pre/post test
## 3 = correct on pre-test, incorrect on post-test
## 4 = no change, correct on both pre/post test
########

q1 <- as.data.frame(table(test[,1]))
q2 <- as.data.frame(table(test[,2]))
q3 <- as.data.frame(table(test[,3]))
q4 <- as.data.frame(table(test[,4]))
q5 <- as.data.frame(table(test[,5]))
q6 <- as.data.frame(table(test[,6]))
q7 <- as.data.frame(table(test[,7]))
q8 <- as.data.frame(table(test[,8]))
q9 <- as.data.frame(table(test[,9]))
q10 <- as.data.frame(table(test[,10]))
q11 <- as.data.frame(table(test[,11]))
q12 <- as.data.frame(table(test[,12]))
q13 <- as.data.frame(table(test[,13]))
q14 <- as.data.frame(table(test[,14]))
q15 <- as.data.frame(table(test[,15]))
q16 <- as.data.frame(table(test[,16]))
q17 <- as.data.frame(table(test[,17]))
q18 <- as.data.frame(table(test[,18]))
q19 <- as.data.frame(table(test[,19]))
q20 <- as.data.frame(table(test[,20]))
q21 <- as.data.frame(table(test[,21]))
q22 <- as.data.frame(table(test[,22]))

# This created a summary table of the students anwers for each question
test_table <- cbind(q1[,2],q2[,2],q3[,2],q4[,2],q5[,2],q6[,2],q7[,2],q8[,2],q9[,2],q10[,2],q11[,2],q12[,2],q13[,2],q14[,2],q14[,2],q15[,2],q16[,2],q17[,2],q18[,2],q19[,2],q20[,2],q21[,2],q22[,2])

rownames(test_table) <- c("incorrect->correct","no_change_incorrect","correct->incorrect","no_change_correct")
colnames(test_table) <- c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q14","q15","q16","q17","q18","q19","q20","q21","q22")
test_table

# q1 q2  q3  q4 q5 q6  q7 q8  q9 q10 q11 q12 q13 q14 q14 q15 q16 q17 q18 q19 q20 q21 q22
# incorrect->correct   22 41  53  44 51 53  32 46  28  28  50  60  86  46  46  26  28  58  29  36  40  24  30
# no_change_incorrect   8 39  10  28 38 69  10 71   8  21  17  36  18  61  61   9  58  49  12  28  22  20  45
# correct->incorrect   14 27   7  13 15 19  19 22  21  18  17  22   9  32  32  15  43  12   8  39  18  28  30
# no_change_correct   159 96 133 118 99 62 142 64 146 136 119  85  90  64  64 153  74  84 154 100 123 131  98

######## 
## Add analysis of Genetic Drift question from third exam and the final
########
drift_pre_post_3rd <- drift_pre_post[complete.cases(drift_pre_post$Exam3_Q23),]
drift_pre_post_3rd_final <- drift_pre_post_3rd[complete.cases(drift_pre_post_3rd$Final_Q25),]

########
## First for the third exam
########

out <- split(drift_pre_post_3rd, drift_pre_post_3rd$Exam3_Q25)

length(out$'0'[[1]])
length(out$'1'[[1]])

# 1: 124 (Incorrect)
# 2: 64 (Correct)

summary_third <- cbind(as.matrix(summary(out$'0'$Pre_assessment_Sum)),
                       as.matrix(summary(out$'0'$Post_assessment_Sum)),
                       as.matrix(summary(out$'1'$Pre_assessment_Sum)),
                       as.matrix(summary(out$'1'$Post_assessment_Sum)),
                       as.matrix(summary(out$'0'$Diff)),
                       as.matrix(summary(out$'1'$Diff)))

colnames(summary_third) <- c("Incorrect_Pre","Incorrect_Post","Correct_Pre","Correct_Post","Incorrect_Diff","Correct_Diff")

t(summary_third)

# Results
#                  Min. 1st Qu. Median   Mean 3rd Qu. Max.
# Incorrect_Pre     7      11     13 13.760      16   22
# Incorrect_Post    6      13     16 16.050      19   22
# Correct_Pre       6      12     15 14.910      18   22
# Correct_Post      8      14     18 17.020      21   22
# Incorrect_Diff   -6       0      2  2.290       4   13
# Correct_Diff     -4       0      2  2.109       4   12

# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$'0'$Pre_assessment_Sum,out$'0'$Post_assessment_Sum)
tests[[2]] <- t.test(out$'1'$Pre_assessment_Sum,out$'1'$Post_assessment_Sum)

tests[[3]] <- t.test(out$'0'$Pre_assessment_Sum,out$'1'$Pre_assessment_Sum)
tests[[4]] <- t.test(out$'0'$Post_assessment_Sum,out$'1'$Post_assessment_Sum)

tests[[5]] <- t.test(out$'0'$Diff,out$'1'$Diff)


tests_third <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

colnames(tests_third) <- c("Incorrect_Pre-Post","Correct_Pre-Post","Pre_Incorrect-Correct","Post_Incorrect-Correct","Diff_Incorrect-Correct")

t(tests_third)

#                        mean of x mean of y      p.value
# Incorrect_Pre-Post     13.758065 16.048387 2.483368e-06
# Correct_Pre-Post       14.906250 17.015625 2.290405e-03
# Pre_Incorrect-Correct  13.758065 14.906250 4.216545e-02
# Post_Incorrect-Correct 16.048387 17.015625 1.151100e-01
# Diff_Incorrect-Correct  2.290323  2.109375 7.277420e-01


########
## If we want to also examine the students that took the final exam
########
out <- split(drift_pre_post_3rd_final, drift_pre_post_3rd_final$Final_Q25)

length(out$'0'[[1]])
length(out$'1'[[1]])

# 1: 22 (Incorrect)
# 2: 56 (Correct)

summary_final <- cbind(as.matrix(summary(out$'0'$Pre_assessment_Sum)),
                       as.matrix(summary(out$'0'$Post_assessment_Sum)),
                       as.matrix(summary(out$'1'$Pre_assessment_Sum)),
                       as.matrix(summary(out$'1'$Post_assessment_Sum)),
                       as.matrix(summary(out$'0'$Diff)),
                       as.matrix(summary(out$'1'$Diff)))

colnames(summary_final) <- c("Incorrect_Pre","Incorrect_Post","Correct_Pre","Correct_Post","Incorrect_Diff","Correct_Diff")

t(summary_final)

# Results
#                  Min. 1st Qu. Median   Mean 3rd Qu. Max.
# Incorrect_Pre     8   10.25   13.0 12.640   14.00   17
# Incorrect_Post    8   13.00   15.0 15.410   18.00   22
# Correct_Pre       6   11.00   13.0 13.160   16.00   22
# Correct_Post      8   12.00   15.5 15.620   19.25   22
# Incorrect_Diff   -4    0.00    3.0  2.773    4.00   13
# Correct_Diff     -4   -1.00    2.0  2.464    5.00   13


# t-test between pre/post and between pre and post of category
tests <- list()
tests[[1]] <- t.test(out$'0'$Pre_assessment_Sum,out$'0'$Post_assessment_Sum)
tests[[2]] <- t.test(out$'1'$Pre_assessment_Sum,out$'1'$Post_assessment_Sum)

tests[[3]] <- t.test(out$'0'$Pre_assessment_Sum,out$'1'$Pre_assessment_Sum)
tests[[4]] <- t.test(out$'0'$Post_assessment_Sum,out$'1'$Post_assessment_Sum)

tests[[5]] <- t.test(out$'0'$Diff,out$'1'$Diff)


tests_final <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

colnames(tests_final) <- c("Incorrect_Pre-Post","Correct_Pre-Post","Pre_Incorrect-Correct","Post_Incorrect-Correct","Diff_Incorrect-Correct")

t(tests_final)

#                        mean of x mean of y      p.value
# Incorrect_Pre-Post     12.636364 15.409091 0.004850748
# Correct_Pre-Post       13.160714 15.625000 0.001247745
# Pre_Incorrect-Correct  12.636364 13.160714 0.475523179
# Post_Incorrect-Correct 15.409091 15.625000 0.818813004
# Diff_Incorrect-Correct  2.772727  2.464286 0.740174982
