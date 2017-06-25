## First make sure you have ggplot and Lock5Data
# install.packages("ggplot2")
# install.packages("Lock5Data")

library(ggplot2)
library(Lock5Data)
library(MASS)

###
# YOU NEED TO EDIT THIS SECTION FOR THE CODE TO RUN
# Give the pathnames on your computer
###

# EDIT THE PATH TO WHERE THE SURVEY DATA IS: 
path <- "/Users/melissa/Dropbox (ASU)/Courses_assessments/2016_Spring_GeneticDrift/Teaching_GeneticDrift/"


# EVERYTHING ELSE SHOULD RUN SO LONG AS YOU HAVE NOT EDITED THE NAME OF THE SURVEY DATA

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

# all_agreed:	269
# pre_only:	250
# (1) pre_post: 	203
# (2) pre_post_3rd:	188
# (3) pp_3rd_final:	78

# Calculate Sums of Pre and Post
Pre <-  drift_pre_post$Pre_1 + drift_pre_post$Pre_2 + drift_pre_post$Pre_3 + drift_pre_post$Pre_4 + drift_pre_post$Pre_5 + drift_pre_post$Pre_6 + drift_pre_post$Pre_7 + drift_pre_post$Pre_8 + drift_pre_post$Pre_9 + drift_pre_post$Pre_10 + drift_pre_post$Pre_11 + drift_pre_post$Pre_12 + drift_pre_post$Pre_13 + drift_pre_post$Pre_14 + drift_pre_post$Pre_15 + drift_pre_post$Pre_16 + drift_pre_post$Pre_17 + drift_pre_post$Pre_18 + drift_pre_post$Pre_19 + drift_pre_post$Pre_20 + drift_pre_post$Pre_21 + drift_pre_post$Pre_22
Post <-  drift_pre_post$Post_1 + drift_pre_post$Post_2 + drift_pre_post$Post_3 + drift_pre_post$Post_4 + drift_pre_post$Post_5 + drift_pre_post$Post_6 + drift_pre_post$Post_7 + drift_pre_post$Post_8 + drift_pre_post$Post_9 + drift_pre_post$Post_10 + drift_pre_post$Post_11 + drift_pre_post$Post_12 + drift_pre_post$Post_13 + drift_pre_post$Post_14 + drift_pre_post$Post_15 + drift_pre_post$Post_16 + drift_pre_post$Post_17 + drift_pre_post$Post_18 + drift_pre_post$Post_19 + drift_pre_post$Post_20 + drift_pre_post$Post_21 + drift_pre_post$Post_22

#Calculate the difference
Diff <- Post - Pre

# Add these columns back to the appropriate data partitions
drift_pre_post$Pre_test <- Pre
drift_pre_post$Post_test <- Post
drift_pre_post$Diff <- Diff

# Calculate summary statistics on each data partition
summary_pre_post <- cbind(as.matrix(summary(drift_pre_post$Pre_test)),as.matrix(summary(drift_pre_post$Post_test)))

colnames(summary_pre_post) <- c("Pre","Post")

#write.table(t(summary_pre_post), pre_post_outputFile, col.names=NA, sep="\t")
t_test_all <- t.test(Pre,Post)

# Next, let’s study the results by demographic group
# First, let’s break up each group by reported gender

# Gender
# First_Gen
# Bio340_at_ASU
# Ethnicity
# RecitationPair_Time
# Letter_Grade

########
# First all of pre_post
########

########################
### By gender ###
########################
out <- split(drift_pre_post, drift_pre_post$Gender)

length(out$F[[1]])
length(out$M[[1]])

# F: 115
# M: 88

summary_gender <- cbind(as.matrix(summary(out$F$Pre_test)),as.matrix(summary(out$F$Post_test)),as.matrix(summary(out$M$Pre_test)),as.matrix(summary(out$M$Post_test)),as.matrix(summary(out$F$Diff)),as.matrix(summary(out$M$Diff)))

colnames(summary_gender) <- c("F_Pre","F_Post","M_Pre","M_Post","F_Diff","M_Diff")

t(summary_gender)

# Results
#        Min. 1st Qu. Median  Mean 3rd Qu. Max.
# F_Pre     6      11     14 14.01      17   22
# F_Post    8      13     16 16.13      20   22
# M_Pre     7      11     14 14.40      17   22
# M_Post    6      14     18 16.89      20   22
# F_Diff   -6       0      2  2.122       4   13
# M_Diff   -5       0      2  2.489       5   13

# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$F$Pre_test,out$F$Post_test)
tests[[2]] <- t.test(out$M$Pre_test,out$M$Post_test)
tests[[3]] <- t.test(out$F$Pre_test,out$M$Pre_test)
tests[[4]] <- t.test(out$F$Post_test,out$M$Post_test)
tests[[5]] <- t.test(out$F$Diff,out$M$Diff)

tests_gender <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

colnames(tests_gender) <- c("F_Pre-Post", "M_Pre-Post", "Pre_F-M","Post_F-M","Diff_F-M")

t(tests_gender)

#            mean of x mean of y      p.value
# F_Pre-Post 14.008696 16.130435 1.271624e-05
# M_Pre-Post 14.397727 16.886364 4.309951e-05
# Pre_F-M    14.008696 14.397727 4.566476e-01
# Post_F-M   16.130435 16.886364 1.731977e-01
# Diff_F-M    2.121739  2.488636 4.475020e-01

plot_gender <- summary_gender[4,1:4]
pdf(paste(plots_dir,'/Gender_pre-post.pdf', sep=""),width=6,height=4)
barplot(plot_gender, col=c("blue","darkblue","green","darkgreen"),ylim=c(0,22))
dev.off()



########################
### By First_Gen ###
########################

out <- split(drift_pre_post, drift_pre_post$First_Gen)

length(out$Y[[1]])
length(out$N[[1]])

# Y: 63
# N: 140

summary_first <- cbind(as.matrix(summary(out$Y$Pre_test)),as.matrix(summary(out$Y$Post_test)),as.matrix(summary(out$N$Pre_test)),as.matrix(summary(out$N$Post_test)),as.matrix(summary(out$Y$Diff)),as.matrix(summary(out$N$Diff)))

colnames(summary_first) <- c("Y_Pre","Y_Post","N_Pre","N_Post","Y_Diff","N_Diff")

t(summary_first)

# Results
#          Min. 1st Qu. Median  Mean 3rd Qu. Max.
# Y_Pre     6      11     13 13.43      16   22
# Y_Post    6      12     15 15.35      18   22
# N_Pre     7      12     15 14.51      17   22
# N_Post    8      14     18 16.96      20   22
# Y_Diff   -6      -1      1  1.921    4.00   12
# N_Diff   -4       0      2  2.443    4.25   13


# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$Y$Pre_test,out$Y$Post_test)
tests[[2]] <- t.test(out$N$Pre_test,out$N$Post_test)
tests[[3]] <- t.test(out$Y$Pre_test,out$N$Pre_test)
tests[[4]] <- t.test(out$Y$Post_test,out$N$Post_test)
tests[[5]] <- t.test(out$Y$Diff,out$N$Diff)

tests_first <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

colnames(tests_first) <- c("Y_Pre-Post", "N_Pre-Post", "Pre_Y-N","Post_Y-N","Diff_Y-N")

t(tests_first)

#              mean of x mean of y      p.value
# Y_Pre-Post 13.428571 15.349206 4.810281e-03
# N_Pre-Post 14.514286 16.957143 6.914017e-08
# Pre_Y-N    13.428571 14.514286 5.236779e-02
# Post_Y-N   15.349206 16.957143 6.341017e-03
# Diff_Y-N    1.920635  2.442857 3.264457e-01


plot_first <- summary_first[4,1:4]
pdf(paste(plots_dir,'/First_pre-post.pdf', sep=""),width=6,height=4)
barplot(plot_first, col=c("blue","darkblue","green","darkgreen"),ylim=c(0,22))
dev.off()


########################
### By Bio340_at_ASU ###
########################

out <- split(drift_pre_post, drift_pre_post$Bio340_at_ASU)

length(out$Y[[1]])
length(out$N[[1]])

# Y: 198
# N: 5

# Not enough resolution to continue analysis


########################
### By Ethnicity ###
########################

out <- split(drift_pre_post, drift_pre_post$Ethnicity)

length(out$'1'[[1]])
length(out$'2'[[1]])
length(out$'3'[[1]])
length(out$'4'[[1]])
length(out$'5'[[1]])
length(out$'6'[[1]])

# 1: 2
# 2: 28
# 3: 2
# 4: 26
# 5: 2
# 6: 119
# More_than_one: 24 


summary_ethnicity <- cbind(as.matrix(summary(out$'2'$Pre_test)),as.matrix(summary(out$'2'$Post_test)),as.matrix(summary(out$'4'$Pre_test)),as.matrix(summary(out$'4'$Post_test)),as.matrix(summary(out$'6'$Pre_test)),as.matrix(summary(out$'6'$Post_test)),as.matrix(summary(out$'2'$Diff)),as.matrix(summary(out$'4'$Diff)),as.matrix(summary(out$'6'$Diff)))

colnames(summary_ethnicity) <- c("2_Pre","2_Post","4_Pre","4_Post","6_Pre","6_Post","2_Diff","4_Diff","6_Diff")

t(summary_ethnicity)

# Results
#        Min. 1st Qu. Median   Mean 3rd Qu. Max.
# 2_Pre     7   11.00   12.0 13.360   15.00   22
# 2_Post    8   13.75   16.5 16.070   19.00   22
# 4_Pre     6   10.00   13.0 13.040   15.75   19
# 4_Post   10   13.00   16.0 15.770   18.00   22
# 6_Pre     7   12.00   15.0 14.740   17.00   22
# 6_Post    8   14.00   17.0 16.840   20.00   22
# 2_Diff   -3    0.00    2.0  2.714    5.25    9
# 4_Diff   -6   -0.75    3.0  2.731    4.75   13
# 6_Diff   -4    0.00    2.0  2.101    4.00   13


# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$'2'$Pre_test,out$'2'$Post_test)
tests[[2]] <- t.test(out$'4'$Pre_test,out$'4'$Post_test)
tests[[3]] <- t.test(out$'6'$Pre_test,out$'6'$Post_test)

tests[[4]] <- t.test(out$'2'$Pre_test,out$'4'$Pre_test)
tests[[5]] <- t.test(out$'2'$Pre_test,out$'6'$Pre_test)
tests[[6]] <- t.test(out$'4'$Pre_test,out$'6'$Pre_test)

tests[[7]] <- t.test(out$'2'$Post_test,out$'4'$Post_test)
tests[[8]] <- t.test(out$'2'$Post_test,out$'6'$Post_test)
tests[[9]] <- t.test(out$'4'$Post_test,out$'6'$Post_test)

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

colnames(tests_ethnicity) <- c("2_Pre-Post","4_Pre-Post","6_Pre-Post","Pre_2-4","Pre_2-6","Pre_4-6","Post_2-4","Post_2-6","Post_4-6","Diff_2-4","Diff_2-6","Diff_4-6")

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


#####
# POC vs White
####

#recode into POC vs white
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==1] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==2] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==3] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==4] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==5] <- 1
drift_pre_post$Ethnicity[drift_pre_post$Ethnicity==6] <- 2

#also do this for people who reported more than one ethnicity

drift_pre_post$Ethnicity[drift_pre_post$Ethnicity!=2] <- 1



out <- split(drift_pre_post, drift_pre_post$Ethnicity)

length(out$'1'[[1]])
length(out$'2'[[1]])

# 1: 84 (POC)
# 2: 119 (white)

summary_ethnicity <- cbind(as.matrix(summary(out$'1'$Pre_test)),as.matrix(summary(out$'1'$Post_test)),as.matrix(summary(out$'2'$Pre_test)),as.matrix(summary(out$'2'$Post_test)),as.matrix(summary(out$'1'$Diff)),as.matrix(summary(out$'2'$Diff)))

colnames(summary_ethnicity) <- c("POC_Pre","POC_Post","White_Pre","White_Post","POC_Diff","White_Diff")

t(summary_ethnicity)

# Results
#             Min. 1st Qu. Median   Mean 3rd Qu. Max.
# POC_Pre       6      11     13 13.380      15   22
# POC_Post      6      13     16 15.920      19   22
# White_Pre     7      12     15 14.740      17   22
# White_Post    8      14     17 16.840      20   22
# POC_Diff     -6       0      2  2.536       5   13
# White_Diff   -4       0      2  2.101       4   13

# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$'1'$Pre_test,out$'1'$Post_test)
tests[[2]] <- t.test(out$'2'$Pre_test,out$'2'$Post_test)

tests[[3]] <- t.test(out$'1'$Pre_test,out$'2'$Pre_test)
tests[[4]] <- t.test(out$'1'$Post_test,out$'2'$Post_test)

tests[[5]] <- t.test(out$'1'$Diff,out$'2'$Diff)


tests_ethnicity <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    # ci.lower = x$conf.int[1],
    # ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

colnames(tests_ethnicity) <- c("POC_Pre-Post","White_Pre-Post","Pre_POC-White","Post_POC-White","Diff_POC-White")

t(tests_ethnicity)

# Results
#               mean of x mean of y      p.value
# POC_Pre-Post   13.380952  15.91667 1.722874e-05
# White_Pre-Post 14.739496  16.84034 2.008336e-05
# Pre_POC-White  13.380952  14.73950 8.252112e-03
# Post_POC-White 15.916667  16.84034 9.466662e-02
# Diff_POC-White  2.535714   2.10084 3.670503e-01


plot_ethnicity <- summary_ethnicity[4,1:4]
pdf(paste(plots_dir,'/Ethnicity_pre-post.pdf', sep=""),width=6,height=4)
barplot(plot_ethnicity, col=c("blue","darkblue","green","darkgreen"),ylim=c(0,22))
dev.off()



########################
### By Letter_Grade ###
########################

out <- split(drift_pre_post, drift_pre_post$Letter_Grade)

length(out$A[[1]])
length(out$B[[1]])
length(out$C[[1]])
length(out$D[[1]])
length(out$E[[1]])

# A: 78
# B: 84
# C: 41
# D: 0
# E: 0

summary_grades <- cbind(as.matrix(summary(out$A$Pre_test)),as.matrix(summary(out$A$Post_test)),as.matrix(summary(out$B$Pre_test)),as.matrix(summary(out$B$Post_test)),as.matrix(summary(out$C$Pre_test)),as.matrix(summary(out$C$Post_test)),as.matrix(summary(out$A$Diff)),as.matrix(summary(out$B$Diff)),as.matrix(summary(out$C$Diff)))

colnames(summary_grades) <- c("A_Pre","A_Post","B_Pre","B_Post","C_Pre","C_Post","A_Diff","B_Diff","C_Diff")

t(summary_grades)

# Results
#        Min. 1st Qu. Median   Mean 3rd Qu. Max.
# A_Pre     6   13.00     16 15.460   18.00   22
# A_Post    8   16.00     19 18.210   21.00   22
# B_Pre     7   11.00     14 14.180   17.00   22
# B_Post    6   13.00     16 15.800   19.00   22
# C_Pre     7   10.00     11 11.730   13.00   22
# C_Post    8   12.00     14 14.490   17.00   22
# A_Diff   -3    0.25      2  2.744    4.75   12
# B_Diff   -5    0.00      1  1.619    4.00   12
# C_Diff   -6    0.00      3  2.756    5.00   13


# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$A$Pre_test,out$A$Post_test)
tests[[2]] <- t.test(out$B$Pre_test,out$B$Post_test)
tests[[3]] <- t.test(out$C$Pre_test,out$C$Post_test)

tests[[4]] <- t.test(out$A$Pre_test,out$B$Pre_test)
tests[[5]] <- t.test(out$A$Pre_test,out$C$Pre_test)
tests[[6]] <- t.test(out$B$Pre_test,out$C$Pre_test)

tests[[7]] <- t.test(out$A$Post_test,out$B$Post_test)
tests[[8]] <- t.test(out$A$Post_test,out$C$Post_test)
tests[[9]] <- t.test(out$B$Post_test,out$C$Post_test)

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

colnames(tests_grades) <- c("A_Pre-Post","B_Pre-Post","C_Pre-Post","Pre_A-B","Pre_A-C","Pre_B-C","Post_A-B","Post_A-C","Post_B-C","Diff_A-B","Diff_A-C","Diff_B-C")

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


plot_grades <- summary_grades[4,1:6]
pdf(paste(plots_dir,'/Grades_pre-post.pdf', sep=""),width=6,height=4)
barplot(plot_grades, col=c("blue","darkblue","green","darkgreen","red","darkred"),ylim=c(0,22))
dev.off()


##############################
### By RecitationPair_Time ###
##############################

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

summary_pair <- cbind(as.matrix(summary(out$Pair1_300$Pre_test)),as.matrix(summary(out$Pair1_300$Post_test)),
                      as.matrix(summary(out$Pair1_430$Pre_test)),as.matrix(summary(out$Pair1_430$Post_test)),
                      as.matrix(summary(out$Pair1_600$Pre_test)),as.matrix(summary(out$Pair1_600$Post_test)),
                      as.matrix(summary(out$Pair1_730$Pre_test)),as.matrix(summary(out$Pair1_730$Post_test)),
                      as.matrix(summary(out$Pair2_300$Pre_test)),as.matrix(summary(out$Pair2_300$Post_test)),
                      as.matrix(summary(out$Pair2_430$Pre_test)),as.matrix(summary(out$Pair2_430$Post_test)),
                      as.matrix(summary(out$Pair2_600$Pre_test)),as.matrix(summary(out$Pair2_600$Post_test)),
                      as.matrix(summary(out$Pair2_730$Pre_test)),as.matrix(summary(out$Pair2_730$Post_test)),
                      as.matrix(summary(out$Pair1_300$Diff)),as.matrix(summary(out$Pair1_430$Diff)),as.matrix(summary(out$Pair1_600$Diff)),as.matrix(summary(out$Pair1_730$Diff)),
                      as.matrix(summary(out$Pair2_300$Diff)),as.matrix(summary(out$Pair2_430$Diff)),as.matrix(summary(out$Pair2_600$Diff)),as.matrix(summary(out$Pair2_730$Diff)))


colnames(summary_pair) <- c("Pair1_300_Pre","Pair1_300_Post","Pair1_430_Pre","Pair1_430_Post",
                            "Pair1_600_Pre","Pair1_600_Post","Pair1_730_Pre","Pair1_730_Post",
                            "Pair2_300_Pre","Pair2_300_Post","Pair2_430_Pre","Pair2_430_Post",
                            "Pair2_600_Pre","Pair2_600_Post","Pair2_730_Pre","Pair2_730_Post",
                            "Pair1_300_Diff","Pair1_430_Diff","Pair1_600_Diff","Pair1_730_Diff",
                            "Pair2_300_Diff","Pair2_430_Diff","Pair2_600_Diff","Pair2_730_Diff")


t(summary_pair)

# Results
# Min. 1st Qu. Median    Mean 3rd Qu. Max.
# Pair1_300_Pre     8   12.50   14.0 14.7700   17.00   22
# Pair1_300_Post    8   15.50   18.0 17.2900   20.00   22
# Pair1_430_Pre     7   10.75   12.0 12.5700   14.00   20
# Pair1_430_Post    9   12.00   14.0 14.5400   18.00   22
# Pair1_600_Pre    11   15.00   16.0 16.3600   18.00   21
# Pair1_600_Post    8   17.00   19.0 18.2800   20.00   22
# Pair1_730_Pre     9   11.50   14.0 14.5500   16.00   22
# Pair1_730_Post    6   13.00   15.0 14.8200   17.00   21
# Pair2_300_Pre     7   10.75   14.5 13.6800   16.00   22
# Pair2_300_Post   10   13.75   16.0 16.9300   21.00   22
# Pair2_430_Pre     6   11.00   13.5 14.0500   16.00   22
# Pair2_430_Post    9   14.00   16.5 16.5300   20.00   22
# Pair2_600_Pre    10   11.00   13.0 13.4300   15.00   19
# Pair2_600_Post    8   12.00   16.0 15.2900   17.00   22
# Pair2_730_Pre     8   12.00   14.0 14.2900   17.00   21
# Pair2_730_Post   10   13.00   18.0 16.9000   21.00   22
# Pair1_300_Diff   -4    0.00    2.0  2.5160    4.50   12
# Pair1_430_Diff   -6   -0.25    2.0  1.9640    4.25    8
# Pair1_600_Diff   -3    1.00    1.0  1.9200    3.00    7
# Pair1_730_Diff   -5   -1.00   -1.0  0.2727    2.00    6
# Pair2_300_Diff   -3    0.75    2.5  3.2500    5.25   13
# Pair2_430_Diff   -2    0.00    1.5  2.4740    4.00   13
# Pair2_600_Diff   -4   -1.00    1.0  1.8570    5.00    7
# Pair2_730_Diff   -1    0.00    3.0  2.6190    4.00    9

# t-test between pre/post and between pre and post of category

tests <- list()
tests[[1]] <- t.test(out$Pair1_300$Pre_test,out$Pair1_300$Post_test)
tests[[2]] <- t.test(out$Pair1_430$Pre_test,out$Pair1_430$Post_test)
tests[[3]] <- t.test(out$Pair1_600$Pre_test,out$Pair1_600$Post_test)
tests[[4]] <- t.test(out$Pair1_730$Pre_test,out$Pair1_730$Post_test)

tests[[5]] <- t.test(out$Pair2_300$Pre_test,out$Pair2_300$Post_test)
tests[[6]] <- t.test(out$Pair2_430$Pre_test,out$Pair2_430$Post_test)
tests[[7]] <- t.test(out$Pair2_600$Pre_test,out$Pair2_600$Post_test)
tests[[8]] <- t.test(out$Pair2_730$Pre_test,out$Pair2_730$Post_test)

tests[[9]] <- t.test(out$Pair1_300$Pre_test,out$Pair2_300$Pre_test)
tests[[10]] <- t.test(out$Pair1_430$Pre_test,out$Pair2_430$Pre_test)
tests[[11]] <- t.test(out$Pair1_600$Pre_test,out$Pair2_600$Pre_test)
tests[[12]] <- t.test(out$Pair1_730$Pre_test,out$Pair2_730$Pre_test)

tests[[13]] <- t.test(out$Pair1_300$Post_test,out$Pair2_300$Post_test)
tests[[14]] <- t.test(out$Pair1_430$Post_test,out$Pair2_430$Post_test)
tests[[15]] <- t.test(out$Pair1_600$Post_test,out$Pair2_600$Post_test)
tests[[16]] <- t.test(out$Pair1_730$Post_test,out$Pair2_730$Post_test)

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

colnames(tests_pair) <- c("Pair1_300_Pre-Post","Pair1_430_Pre-Post","Pair1_600_Pre-Post","Pair1_730_Pre-Post","Pair2_300_Pre-Post","Pair2_430_Pre-Post","Pair2_600_Pre-Post","Pair2_730_Pre-Post","Pair1-Pair2_300_Pre","Pair1-Pair2_430_Pre","Pair1-Pair2_600_Pre","Pair1-Pair2_730_Pre","Pair1-Pair2_300_Post","Pair1-Pair2_430_Post","Pair1-Pair2_600_Post","Pair1-Pair2_730_Post","Pair1-Pair2_300_Diff","Pair1-Pair2_430_Diff","Pair1-Pair2_600_Diff","Pair1-Pair2_730_Diff")

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


plot_pair <- summary_pair[4,1:16]
#colnames(plot_pair)<-c("Pre1_1","Post1_1","Pre2_1","Post2_1","Pre3_1","Post3_1","Pre4_1","Post4_1","Pre1_2","Post1_2","Pre2_2","Post2_2","Pre3_2","Post3_2","Pre4_2","Post4_2")
pdf(paste(plots_dir,'/Pair_pre-post.pdf', sep=""),width=12,height=4)
barplot(plot_pair, col=c("blue","darkblue","green","darkgreen","red","darkred","violet","purple"),ylim=c(0,22))
dev.off()



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

######
# Now test how students did on each question
#####

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

# Now bind
out <- cbind(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20,Q21,Q22)
outp <- cbind(Q1p,Q2p,Q3p,Q4p,Q5p,Q6p,Q7p,Q8p,Q9p,Q10p,Q11p,Q12p,Q13p,Q14p,Q15p,Q16p,Q17p,Q18p,Q19p,Q20p,Q21p,Q22p)

# And compute table with unambigous response
test <- out/outp

# Now rename so we can compute the total of how students performed on each question
test[is.nan(test)] <- 2
test[test==-1] <-3
test[test==0] <-4

###
# 1 = incorrect on pre-test, correct on post-test
# 2 = no change, incorrect both pre/post test
# 3 = correct on pre-test, incorrect on post-test
# 4 = no change, correct on both pre/post test

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

test_table <- cbind(q1[,2],q2[,2],q3[,2],q4[,2],q5[,2],q6[,2],q7[,2],q8[,2],q9[,2],q10[,2],q11[,2],q12[,2],q13[,2],q14[,2],q14[,2],q15[,2],q16[,2],q17[,2],q18[,2],q19[,2],q20[,2],q21[,2],q22[,2])

rownames(test_table) <- c("incorrect->correct","no_change_incorrect","correct->incorrect","no_change_correct")
colnames(test_table) <- c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q14","q15","q16","q17","q18","q19","q20","q21","q22")
test_table


#### 
# Add analysis of Genetic Drift question from third exam and the final
####
drift_pre_post_3rd <- drift_pre_post[complete.cases(drift_pre_post$Exam3_Q23),]
drift_pre_post_3rd_final <- drift_pre_post_3rd[complete.cases(drift_pre_post_3rd$Final_Q25),]

###
# First for the third exam
###

out <- split(drift_pre_post_3rd, drift_pre_post_3rd$Exam3_Q25)

length(out$'0'[[1]])
length(out$'1'[[1]])

# 1: 124 (Incorrect)
# 2: 64 (Correct)

summary_third <- cbind(as.matrix(summary(out$'0'$Pre_test)),as.matrix(summary(out$'0'$Post_test)),as.matrix(summary(out$'1'$Pre_test)),as.matrix(summary(out$'1'$Post_test)),as.matrix(summary(out$'0'$Diff)),as.matrix(summary(out$'1'$Diff)))

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
tests[[1]] <- t.test(out$'0'$Pre_test,out$'0'$Post_test)
tests[[2]] <- t.test(out$'1'$Pre_test,out$'1'$Post_test)

tests[[3]] <- t.test(out$'0'$Pre_test,out$'1'$Pre_test)
tests[[4]] <- t.test(out$'0'$Post_test,out$'1'$Post_test)

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


#####
# If we want to also examine the students that took the final
#####

out <- split(drift_pre_post_3rd_final, drift_pre_post_3rd_final$Final_Q25)

length(out$'0'[[1]])
length(out$'1'[[1]])

# 1: 22 (Incorrect)
# 2: 56 (Correct)

summary_final <- cbind(as.matrix(summary(out$'0'$Pre_test)),as.matrix(summary(out$'0'$Post_test)),as.matrix(summary(out$'1'$Pre_test)),as.matrix(summary(out$'1'$Post_test)),as.matrix(summary(out$'0'$Diff)),as.matrix(summary(out$'1'$Diff)))

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
tests[[1]] <- t.test(out$'0'$Pre_test,out$'0'$Post_test)
tests[[2]] <- t.test(out$'1'$Pre_test,out$'1'$Post_test)

tests[[3]] <- t.test(out$'0'$Pre_test,out$'1'$Pre_test)
tests[[4]] <- t.test(out$'0'$Post_test,out$'1'$Post_test)

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
