library(readxl)
EchoCardiograms_data_for_R <- read_excel("Echocardiogram_Data/EchoCardiograms data for R.xls", 
                                         sheet = "Sheet3")

View(EchoCardiograms_data_for_R)

EchoC_data <- EchoCardiograms_data_for_R
names(EchoC_data)
#install.packages("tidyverse")
library(tidyverse)

EchoC_data %>%
  select(ID, Genotype, Vol_s, Vol_d, EF, FS, starts_with('Dia'), starts_with('mean'), starts_with('LV'), Peak_grad, Peak_vel, starts_with('Aor'), brach., -starts_with('RV'), -starts_with('PA'))

anyNA(EchoC_data)
apply(EchoC_data, 2, function(x) sum(is.na(x)))

#install.packages("ggplot2")
library(ggplot2)

ggplot(EchoC_data, aes(x = Genotype, y = Vol_s)) + geom_boxplot()

LV_Sys_Vol <- ggplot(EchoC_data, aes(x = Genotype, y = Vol_s)) + geom_violin() + theme_bw() + ylab("Left Ventricular Systolic Volume (uL)") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + ggtitle("June 2018 Systolic Volume Variation") + theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3) + stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color="blue") 

LV_Diameter <- ggplot(EchoC_data, aes(x = Genotype, y = Dia_s)) + geom_violin() + theme_bw() + ylab("Left Ventricular Diameter (mm)") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + ggtitle("June 2018 Diameter Variation During Systole") + theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3) + stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color="blue") 

LVOT_PEAK_VEL <- ggplot(EchoC_data, aes(x = Genotype, y = Peak_vel)) + geom_violin() + theme_bw() + ylab("Left Ventricular Outflow Tract Peak Velocity") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + ggtitle("June 2018 LVOT Peak Velocity Variation") + theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3) + stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color="blue") 

LV_Sys_Vol
LV_Diameter
LVOT_PEAK_VEL


#install.packages("readxl")
library(readxl)
edited_new_echo_r <- read_excel("Echocardiogram_Data/edited_new_echo_r.xls", 
                                sheet = "Sept 2018")
View(new_echo_r)
head(new_echo_r)
anyNA(new_echo_r)
names(new_echo_r)
apply(edited_new_echo_r, 2, function(x) sum(is.na(x)))
library(tidyverse)
fixed_new_echo <- new_echo_r %>% drop_na()
anyNA(fixed_new_echo)
ttest <- t.test(PWV ~ Genotype, data=fixed_new_echo)
ttest$p.value
lapply(fixed_new_echo[,3:24], function(x) t.test(x ~ fixed_new_echo$Genotype, var.equal = TRUE))
lapply(new_echo_r[,3:23], function(x) t.test(x ~ new_echo_r$Genotype, var.equal = TRUE))
lapply(EchoC_data[,3:24], function(x) t.test(x ~ EchoC_data$Genotype, var.equal = TRUE))

LV_Sys_Vol_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LV_Vol_s)) + geom_violin() + theme_bw() + 
  ylab("Left Ventricular Systolic Volume (uL)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Systolic Volume Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3) + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LV_Sys_Vol_Sept
 
LV_Diameter_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = Dia_s)) + geom_violin() + theme_bw() + 
  ylab("Left Ventricular Diameter (mm)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Diameter Variation During Systole") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3) + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LV_Diameter_Sept


#plot that I am playing with:
LVOT_Peak_Vel_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LVOT_Peak_vel)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.1) + #can use geom_jitter to introduce a small amount of random variation to the location of each point to handle overplotting 
  ylab("Left Ventricular Outflow Tract Peak Velocity") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 LVOT Peak Velocity Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LVOT_Peak_Vel_Sept
LV_Sys_Vol_Sept_2 <- ggplot(new_echo_r, aes(x = Genotype, y = LV_Vol_s)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) +
  ylab("Left Ventricular Systolic Volume (uL)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Systolic Volume Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LV_Sys_Vol_Sept_2


#New versions 
#June:
LV_Sys_Vol <- ggplot(EchoC_data, aes(x = Genotype, y = Vol_s)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + #can use geom_jitter to introduce a small amount of random variation to the location of each point to handle overplott
  ylab("Left Ventricular Systolic Volume (uL)") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Systolic Volume Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color="blue") 
LV_Sys_Vol

#September:
LV_Sys_Vol_Sept_2 <- ggplot(new_echo_r, aes(x = Genotype, y = LV_Vol_s)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Systolic Volume (uL)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Systolic Volume Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LV_Sys_Vol_Sept_2

#June EF
LV_EF <- ggplot(EchoC_data, aes(x = Genotype, y = EF)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + #can use geom_jitter to introduce a small amount of random variation to the location of each point to handle overplott
  ylab("Ejection Fraction %") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Ejection Fraction Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LV_EF

#September EF
LV_EF_sept <- ggplot(new_echo_r, aes(x = Genotype, y = EF)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Ejection Fraction %") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Ejection Fraction Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LV_EF_sept


#June LV Diam Sys
LV_Diam_Sys <- ggplot(EchoC_data, aes(x = Genotype, y = Dia_s)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + #can use geom_jitter to introduce a small amount of random variation to the location of each point to handle overplott
  ylab("Left Ventricular Diameter in Systole") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Left Ventricular Diameter Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LV_Diam_Sys

#September LV Diam Sys
LV_Diam_Sys_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = Dia_s)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Diameter in Systole") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Left Ventricular Diameter Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LV_Diam_Sys_Sept


#June LVAW Dia
LVAW_Dia <- ggplot(EchoC_data, aes(x = Genotype, y = LVAW_d)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricle Anterior Wall Thickness in Diastole (mm)") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Left Ventricular Wall Thickness") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LVAW_Dia

LVAW_Dia_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LVAW_d)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricul Anterior Wall Thickness in Diastole (mm)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Left Ventricular Wall Thickness") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LVAW_Dia_Sept


#LVOT Mean Gradient
LVOT_Mean_grad <- ggplot(EchoC_data, aes(x = Genotype, y = mean_grad)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Mean Gradient") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Left Ventricular Outflow Tract Mean Gradient Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LVOT_Mean_grad


LVOT_Mean_Grad_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LVOT_mean_grad)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Mean Gradient") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Left Ventricular Outflow Tract Mean Gradient Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LVOT_Mean_Grad_Sept

#LVOT Peak Grad
LVOT_Peak_grad <- ggplot(EchoC_data, aes(x = Genotype, y = Peak_grad)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Peak Gradient") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Left Ventricular Outflow Tract Peak Gradient Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LVOT_Peak_grad

LVOT_Peak_Grad_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LVOT_Peak_grad)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Peak Gradient") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Left Ventricular Outflow Tract Peak Gradient Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LVOT_Peak_Grad_Sept


#LVOT Mean Vel
LVOT_Mean_Vel <- ggplot(EchoC_data, aes(x = Genotype, y = mean_vel)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Mean Velocity") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Left Ventricular Outflow Tract Mean Velocity Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LVOT_Mean_Vel

LVOT_Mean_Vel_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LVOT_mean_vel)) + geom_violin(scale = "area", adjust = 0.6) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Mean Velocity") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Left Ventricular Outflow Tract Mean Velocity Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LVOT_Mean_Vel_Sept


#LVOT  Peak Vel
LVOT_Peak_Vel <- ggplot(EchoC_data, aes(x = Genotype, y = Peak_vel)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Peak Velocity") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Left Ventricular Outflow Tract Peak Velocity Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
LVOT_Peak_Vel

LVOT_Peak_Vel_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = LVOT_Peak_vel)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Left Ventricular Outflow Tract Peak Velocity") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Left Ventricular Outflow Tract Peak Velocity Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
LVOT_Peak_Vel_Sept

#Aorta in Sys
Aor_systole <- ggplot(EchoC_data, aes(x = Genotype, y = Aor_sys)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Aorta Diameter (mm)") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Aorta Internal Diameter Systolic Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median, geom = "point", shape = 3, size = 3, color = "blue") 
Aor_systole

Aor_systole_sept <- ggplot(new_echo_r, aes(x = Genotype, y = Aor_sys)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Aorta Diameter (mm)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Aorta Internal Diameter Systolic Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
Aor_systole_sept


#Brachiocephalic trunk
brachiocephalic_trunk <- ggplot(EchoC_data, aes(x = Genotype, y = brach.)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Brachiocephalic Trunk Diameter (mm)") + 
  scale_x_discrete(label = c("Wild Type", "Mutant")) + 
  ggtitle("June 2018 Brachiocephalic Trunk Diameter Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y=median,  geom = "point", shape = 3, size = 3, color = "blue") 
brachiocephalic_trunk

brachiocephalic_trunk_sept <- ggplot(new_echo_r, aes(x = Genotype, y = brach.)) + geom_violin(scale = "area", adjust = 1) + theme_bw() + 
  geom_point(size = .5, height = 0, width = 0.05) + 
  ylab("Brachiocephalic Trunk Diameter (mm)") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 Brachiocephalic Trunk Diameter Variation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
brachiocephalic_trunk_sept


PWV_Sept <- ggplot(new_echo_r, aes(x = Genotype, y = PWV)) + geom_violin(scale = "area", adjust = .6) + theme_bw() + 
  geom_jitter(size = .5, height = 0, width = 0.05) + 
  ylab("PWV") + 
  scale_x_discrete(limits = c("Wild_Type", "Mutant"), label = c("Wild Type", "Mutant")) +
  ggtitle("September 2018 PWV") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y = mean, geom="point", shape = 18, size  = 3, color = "red") + 
  stat_summary(fun.y = median,  geom = "point", shape = 3, size = 3, color = "blue")
PWV_Sept
summary(PWV_Sept)

##Make a figure with both june and september on the same figure!!!
