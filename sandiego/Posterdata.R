conference <- read.csv("SANDIEGODATA.csv", header = TRUE)
View(conference)
attach(conference)
#librarys
library("plyr")
library("ggplot2")
library("ggpubr")
library("tidyverse")
library("dslabs")
library(dplyr)
install.packages("psych")
library("psych")



ggviolin(data = conference, x = "Control.Experimental", y = "Escalatotal", fill = "Control.Experimental",
         palette = c("#008080", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))

#dotplot with exercises

meanexperimental <- c(76.5, 84.9, 77.0, 81.3, 75.4)
meancontrol <- c(77.7, 88.4, 84.0, 87.4, 79.7) 
unit <- c("VCI", "PRI", "WMI", "PSI", "FSIQ")
long <- data.frame(meanclinical, meancontrol, unit)
  
gg_dot <-  ggplot() +
  # remove axes and superfluous grids
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  
  
  # add a dummy point for scaling purposes
  geom_point(aes(x = 10, y = unit), 
             size = 0, col = "white") + 
  
  # add the horizontal discipline lines
  geom_hline(yintercept = 1:10, col = "grey80") +
  
  # add a point for each male success rate
  geom_point(aes(x = meanexperimental, y = unit), 
             size = 11, col = "#9DBEBB") +
  # add a point for each female success rate
  geom_point(aes(x = meancontrol, y = unit),
             size = 11, col = "#468189") 
gg_dot


gg_dot <- gg_dot + 
  # add the text (%) for each male success rate
  geom_text(aes(x = meanexperimental, y = unit, 
                label = paste0(round(meanexperimental, 1))),
            col = "black") +
  # add the text (%) for each female success rate
  geom_text(aes(x = meancontrol, y = unit, 
                label = paste0(round(meancontrol, 1))),
            col = "white") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label, col = label),
            data.frame(x = c(87.4, 77.7), y = 5, 
                       label = c("control", "experimental")), size = 6) +
  scale_color_manual(values = c("#9DBEBB", "#468189"), guide = "none") +
  scale_x_continuous(expand_scale()) + 
                     
  scale_y_discrete(expand = c(0.2, 0))

gg_dot


# manually specify the x-axis

# manually set the spacing above and below the plot
 


exercisesexperimental <- c(5.7, 6.6, 5.4, 8.8, 6.5, 7.6, 6.4, 4.5, 7.2, 7.2)
exercisescontrol <- c(4.3, 7.6, 6.6, 9.2, 6.7, 7.8, 8.9, 5.1, 8.0, 7.5)
  names <- c("Similarities", "Vocabulary", "Comprehension", "Block Design", 
             "Pictue Concepts", "Matrix Reasoning", "Digit Span", "Letter-Number Sequencing", "Coding", "Symbol Search") 
allexercises <- data.frame(exercisesexperimental, exercisescontrol, names)

  gg_dot <-  ggplot() +
    # remove axes and superfluous grids
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank()) +
    # add a dummy point for scaling purposes
    geom_point(aes(x = 10, y = names), 
               size = 0, col = "white") + 
    # add the horizontal discipline lines
    geom_hline(yintercept = 1:10, col = "grey80") +
    # add a point for each male success rate
    geom_point(aes(x = exercisesexperimental, y = names), 
               size = 10, col = "#9DBEBB") +
    # add a point for each female success rate
    geom_point(aes(x = exercisescontrol, y = names),
               size = 10, col = "#468189") 
  gg_dot
  
  gg_dot <- gg_dot + 
    geom_text(aes(x = exercisesexperimental, y = names, 
                  label = paste0(round(exercisesexperimental, 1))),
              col = "black") +
    geom_text(aes(x = exercisescontrol, y = names, 
                  label = paste0(round(exercisescontrol, 1))),
              col = "white") +
    geom_text(aes(x = x, y = y, label = label, col = label),
              data.frame(x = c(6.6, 7.6), y = 10,
                                    label = c("experimental", "control")), vjust = -1.2,  size = 6) +
    scale_color_manual(values = c("#468189", "#9DBEBB"), guide = "none") +
    scale_x_continuous(expand_scale())  +
    scale_x_continuous(breaks = c(4, 5, 6, 7, 8, 9, 10), 
                       labels = c("4", "5", "6", "7", "8", "9", "10")) +
    scale_y_discrete(expand = c(0.2, 0)) 
  gg_dot  
    


  
  
  #testing normal distribution of sample
  install.packages("dplyr")
  install.packages("ggpubr")
  library(ggpubr)
  #normal distrubution stats
  shapiro.test(conference$Age)
  shapiro.test(conference$Escalatotal)
  shapiro.test(conference$Edu_ParentYR)
  shapiro.test(conference$ICV)
  shapiro.test(conference$IVP)
  shapiro.test(conference$IRP)
  shapiro.test(conference$IMT)
  shapiro.test(conference$Claves)
  shapiro.test(conference$ClavesESCOLAR)
  shapiro.test(conference$Cubos)
  shapiro.test(conference$CubosESCOLAR)
  
  
  #nonnormal distribution variables
  ggqqplot(conference$Age)
  ggqqplot(conference$Edu_ParentYR)
  ggqqplot(conference$Cubos)
  
  #normal distribution variables in graph form
  ggqqplot(conference$IMT)
  ggqqplot(conference$Grade)
  ggqqplot(conference$CubosESCOLAR)
  
 
  #scatterplot parent edu and FSIQ
  install.packages("ggplot2")
  library(ggplot2)
  
  ggplot(data = conference, 
         aes(Edu_ParentYR, Escalatotal, color=group)) + geom_point () + geom_smooth(method = "lm", fill= NA)
  
  ggplot(data = conference, 
         aes(Escalatotal, Edu_ParentYR, color=group)) + geom_point () + geom_smooth(method = "lm", fill= NA)
  
  #transform male/female to 1/2
  install.packages("plyr")
  library(plyr)
  groupgender <- mapvalues(conference$Gender, from = 
                             c("Male", "Female", "Male ", "Female "), to = c("1", "2", "1", "2"))
 
  groupgender1 <- mapvalues(groupgender, from = c("1", "2"), to = c("male", "female"))
   table(groupgender1)
   
   
  ggplot(data = conference, aes(groupgender1, Escalatotal, color=group)) +
    geom_point() + geom_smooth(method= "lm", fill=NA)
  
  
  #Basic stats#
  table(Age)
  mean(Age)
  #grouping data by control/experimental for demographic info#
  install.packages("Rcpp")
  install.packages("dplyr")
  library(dplyr)
  
  #mean and SD age by group
  controlexperimental <- group_by(conference, Control.Experimental)
  demobycontrolexperimental <- summarise(controlexperimental, mean(Age), sd(Age))
  head(demobycontrolexperimental)
  
  
  #confounding variables age/education parents
  cor(Escalatotal, Edu_ParentYR, use = "pairwise", method=c("spearman"))
  cor(Escalatotal, Age, method = "spearman")
  
  conference %>% group_by(Control.Experimental, Gender) %>% mutate(count = n())
  conference %>% count(mean(Age), sd(Age))
  
  #Tables#
  install.packages("knitr")
  install.packages("xtable")
  library(knitr)
  library(xtable)
  
  #simple regression gender/age/parental education
  simpleregression_gender <- lm(Escalatotal ~ groupgender, data = conference)
  summary(simpleregression_gender)
  #age
  simpleregression_age <- lm(Escalatotal ~ Age, data = conference)
  summary(simpleregression_age)
  
  #parentaleducation
  simpleregression_parentedu <- lm(Escalatotal ~ Edu_ParentYR, data = conference)
  summary(simpleregression_parentedu)
  
  #multiple regression with all three variables
  multipleregression <- lm(Escalatotal ~ Age + groupgender + parenteducationyr, data=conference)
  summary(multipleregression)
  
  #multiple regression with indices and total IQ score
  multipleregression_indices <- lm(Escalatotal ~ IVP + IRP + ICV + IMT, data = conference)
  summary(multipleregression_indices)
  
  #regression IMT/Escala total
  plot(Escalatotal, IMT)
  regression_imt <- lm(Escalatotal ~ IMT, data=conference)
  summary(regression_imt)
  abline(regression_imt, col="red", lwd=2)
  #regression ICV/Escalatotal
  plot(Escalatotal, ICV)
  regression_icv <- lm(Escalatotal ~ ICV, data = conference)
  summary(regression_icv)
  abline(regression_icv, col="red", lwd=2)
  #regression IRP 
  plot(Escalatotal, IRP)
  regression_irp <- lm(Escalatotal ~ IRP, data = conference)
  summary(regression_irp)
  abline(regression_irp, col="red", lwd=2)
  #regression IVP
  plot(Escalatotal, IVP)
  regression_ivp <- lm(Escalatotal ~ IVP, data = conference)
  summary(regression_ivp)
  abline(regression_ivp, col="red", lwd=2)
  
  #regression parent edu and IMT
  plot(IMT, Edu_ParentYR)
  regression_eduIMT <- lm(IMT ~ Edu_ParentYR, data = conference)
  summary(regression_eduIMT)
  abline(regression_eduIMT, col="red", lwd=2)
  #regression parent edu and IVP
  plot(IVP, Edu_ParentYR)
  regression_eduIVP <- lm(IVP ~ Edu_ParentYR, data = conference)
  summary(regression_eduIVP)
  abline(regression_eduIVP, col="red", lwd=2)
 
  TukeyHSD(aov.data1)
  
  #demographic table
  
  experimental_group <- c(9.26, 14.5 , 20, 15 )
  control_group <- c(7.88, 10.2, 34, 17 )
  names <- c("Age", "Parent Education", "Males", "Females") 
  allexercises <- data.frame(names, control_group, experimental_group)
print(allexercises)

xtable(allexercises, digits = 0)
  
  install.packages("jmv")
  library("jmv")  
 newtable <- jmv::descriptives(
    formula = ICV + IRP + IVP + IMT + Escalatotal ~ `Control.Experimental`,
    data = conference,
    sd = TRUE)
xtable(newtable) 


#Bar graph Pre y Post test exercises#
control1 <- c(77.6, 88.4, 88.3, 87.3, 79.9)
experimental1 <- c(76.5, 84.9, 76.9, 81.3, 75.4)
unit <- c("Verbal Comprehension","Perceptual Reasoning","Working Memory", "Processing Speed", "Full-Scale IQ")
df <- data.frame(unit, experimental1 , control1)
df.long <- gather(df, variable, value, -unit)

#changing variable labels#
df.long$unit <- factor(df.long$unit, levels =
                         c("Verbal Comprehension","Perceptual Reasoning","Working Memory", "Processing Speed", "Full-Scale IQ"))

#tidyr#
Ejercicios <- ggplot(data = df.long, aes(x = unit, y = value, fill = variable)) +
  geom_col(position = position_dodge()) 

#changing theme#

Ejercicios <- ggplot(data = df.long, aes(x = unit, y = value, fill = variable)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values=c("#008080", "#0b4545")) + theme_void()

#Graph without X and Y values#

Ejercicios + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(size = 10, angle = 5, hjust = 1))


Control <- c(77.6, 88.4, 88.3, 87.3, 79.9)
Experimental <- c(76.5, 84.9, 76.9, 81.3, 75.4)
t <- c(-0.40, -1.15, -2.23, -2.19, -1.80)
p <- c(0.69, 0.25, 0.03, 0.03, 0.08)
Index <- c("Verbal Comprehension"
          ,"Perceptual Reasoning","Working Memory", "Processing Speed", "Full-Scale IQ")
df <- data.frame(Index, Experimental, Control, t, p)
View(df)
