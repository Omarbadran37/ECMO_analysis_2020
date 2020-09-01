###########################################################################
## UNIT1 ECMO Anticoagulation Analysis 
##By: Omar Badran
##Start: 12/1/2018
##Edited: 3/23/2018
############################################################################


## Load Library 

library(ggplot2)
library(tidyr)
library(dplyr)
library(psych)
library(stringr)
library(qwraps2)
library(plotly)
library(viridis)
library(RColorBrewer)
library(forcats)
library(readxl)

## Load Data 
UNIT1_data <- read_excel("C:/Users/Badrano/OneDrive - UPMC/UNIT1_ECMO.xlsx")

## Look at data

head(UNIT1_data)
dim(UNIT1_data)
view(UNIT1_data)
summary(UNIT1_data)
str(UNIT1_data)

### replace the spaces in the column names 
UNIT1_data <-str_replace_all(names(UNIT1_data)," ","_")


###########################################################
##Check distribution of variables being measured
## PTT, Fibrinogen, AT3, Anti-Xa, INR, Plasma HB, 
##Platelets, Heparin infusion rate, and Blood products
###########################################################

ptt_dis <-gather(select(UNIT1_data,starts_with("PTT")),key = "Key",value = "Value")
qplot(Value, data=ptt_dis, geom="density", fill=I("red"))
shapiro.test(ptt_dis$Value)
skew(ptt_dis$Value)

fib_dis <-gather(select(UNIT1_data,starts_with("FIB")),key = "Key",value = "Value")
qplot(Value, data=fib_dis, geom="density", fill=I("red"))
shapiro.test(fib_dis$Value)
skew(fib_dis$Value)

at3_dis <-gather(select(UNIT1_data,starts_with("AT3")),key = "Key",value = "Value")
qplot(Value, data=at3_dis, geom="density", fill=I("red"))
shapiro.test(at3_dis$Value)
skew(at3_dis$Value)

axa_dis <-gather(select(UNIT1_data,starts_with("AXA")),key = "Key",value = "Value")
qplot(Value, data=axa_dis, geom="density", fill=I("red"))
shapiro.test(axa_dis$Value)
skew(axa_dis$Value)

inr_dis <-gather(select(UNIT1_data,starts_with("INR")),key = "Key",value = "Value")
qplot(Value, data=inr_dis, geom="density", fill=I("red"))
shapiro.test(inr_dis$Value)
skew(inr_dis$Value)

phb_dis <-gather(select(UNIT1_data,starts_with("PHB")),key = "Key",value = "Value")
qplot(Value, data=phb_dis, geom="density", fill=I("red"))
shapiro.test(phb_dis$Value)

plts_dis <-gather(select(UNIT1_data,starts_with("PLT")),key = "Key",value = "Value")
qplot(Value, data=plts_dis, geom="density", fill=I("red"))
shapiro.test(plts_dis$Value)
skew(plts_dis$Value)

hr_dis <-gather(select(UNIT1_data,starts_with("HR")),key = "Key",value = "Value")
qplot(Value, data=hr_dis, geom="density", fill=I("red"))
shapiro.test(hr_dis$Value)

prbc_dis <-gather(select(UNIT1_data,starts_with("PRBC")),key = "Key",value = "Value")
qplot(Value, data=prbc_dis, geom="density", fill=I("red"))
shapiro.test(prbc_dis$Value)
skew(prbc_dis$Value)

ffp_dis <-gather(select(UNIT1_data,starts_with("FFP")),key = "Key",value = "Value")
qplot(Value, data=ffp_dis, geom="density", fill=I("red"))
shapiro.test(ffp_dis$Value)
skew(ffp_dis$Value)

totalplts_dis <-gather(select(UNIT1_data,starts_with("Plts")),key = "Key",value = "Value")
qplot(Value, data=totalplts_dis, geom="density", fill=I("red"))
shapiro.test(totalplts_dis$Value)
skew(totalplts_dis$Value)

totalat3_dis <-gather(select(UNIT1_data,starts_with("total_AT3")),key = "Key",value = "Value")
qplot(Value, data=totalat3_dis, geom="density", fill=I("red"))
shapiro.test(totalat3_dis$Value)
skew(totalat3_dis$Value)

cryo_dis <-gather(select(UNIT1_data,starts_with("Cryo")),key = "Key",value = "Value")
qplot(Value, data=cryo_dis, geom="density", fill=I("red"))
shapiro.test(cryo_dis$Value)
skew(cryo_dis$Value)

###################################################
## Split the dataset between pre and post protocol
## and run analysis 
###################################################


PostP <-UNIT1_data %>%
  filter(Received_Protocol==1)
PreP <- UNIT1_data %>%
  filter(Received_Protocol==0)

##PTT test

ptt_post <- select(PostP, starts_with("PTT"))
ptt_pre <- select(PreP, starts_with("PTT"))


The_mean_postptt <- data.frame(Mean=apply(ptt_post,1, mean, na.rm = T),Median=apply(ptt_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preptt <- data.frame(Mean=apply(ptt_pre, 1, mean, na.rm=T),Median=apply(ptt_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postptt$Mean,The_mean_preptt$Mean, exact= NULL)
t.test(The_mean_postptt$Mean,The_mean_preptt$Mean)
wilcox.test(The_mean_postptt$Median,The_mean_preptt$Median, exact= NULL)
t.test(The_mean_postptt$Median,The_mean_preptt$Median)


summary(The_mean_preptt$Median)
summary(The_mean_postptt$Median)

##PTT graph
median_postptt_column <-data.frame(Median=apply(ptt_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_preptt_column <- data.frame(Median=apply(ptt_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_ptt_column <- median_postptt_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_preptt_column%>%
              mutate(Protocol ="Pre"))
median_ptt_row <- select(The_mean_postptt, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preptt,2)%>% 
              mutate(Protocol = "Pre"))

median_ptt_column$Protocol <- factor(median_ptt_column$Protocol, levels = c("Pre", "Post"))
median_ptt_row$Protocol <- factor(median_ptt_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_ptt_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="PTT",breaks=seq(40,200,20))+ ggtitle("PTT Level First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_ptt_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="PTT",breaks=seq(40,200,20))+ ggtitle("PTT by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##FIB test
fib_post <- select(PostP, starts_with("FIB"))
fib_pre <- select(PreP, starts_with("FIB"))
The_mean_postfib <- data.frame(Mean=apply(fib_post,1, mean, na.rm = T),Median=apply(fib_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_prefib <- data.frame(Mean=apply(fib_pre, 1, mean, na.rm=T),Median=apply(fib_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postfib$Mean,The_mean_prefib$Mean, exact= NULL)
t.test(The_mean_postfib$Mean,The_mean_prefib$Mean)
wilcox.test(The_mean_postfib$Median,The_mean_prefib$Median)
t.test(The_mean_postfib$Median,The_mean_prefib$Median)

summary(The_mean_prefib$Median)
summary(The_mean_postfib$Median)

##FIB graph
median_postfib_column <-data.frame(Median=apply(fib_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_prefib_column <- data.frame(Median=apply(fib_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_fib_column <- median_postfib_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_prefib_column%>%
              mutate(Protocol ="Pre"))
median_fib_row <- select(The_mean_postfib, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_prefib,2)%>% 
              mutate(Protocol = "Pre"))

median_fib_column$Protocol <- factor(median_fib_column$Protocol, levels = c("Pre", "Post"))
median_fib_row$Protocol <- factor(median_fib_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_fib_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="Fibrinogen",breaks=seq(40,600,20))+ ggtitle("Fibrinogen Level First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_fib_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="Fibrinogen",breaks=seq(40,600,20))+ ggtitle("Fibrinogen by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##AT3 test
at3_post <- select(PostP, starts_with("AT3"))
at3_pre <- select(PreP, starts_with("AT3"))
The_mean_postat3 <- data.frame(Mean=apply(at3_post,1, mean, na.rm = T),Median=apply(at3_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preat3 <- data.frame(Mean=apply(at3_pre, 1, mean, na.rm=T),Median=apply(at3_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postat3$Mean,The_mean_preat3$Mean, exact= NULL)
t.test(The_mean_postat3$Mean,The_mean_preat3$Mean)
wilcox.test(The_mean_postat3$Median,The_mean_preat3$Median)
t.test(The_mean_postat3$Median,The_mean_preat3$Median)

summary(The_mean_preat3$Median)
summary(The_mean_postat3$Median)

##AT3 graphs
median_postat3_column <-data.frame(Median=apply(at3_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_preat3_column <- data.frame(Median=apply(at3_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_at3_column <- median_postat3_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_preat3_column%>%
              mutate(Protocol ="Pre"))
median_at3_row <- select(The_mean_postat3, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preat3,2)%>% 
              mutate(Protocol = "Pre"))

median_at3_column$Protocol <- factor(median_at3_column$Protocol, levels = c("Pre", "Post"))
median_at3_row$Protocol <- factor(median_at3_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_at3_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="AT-III",breaks=seq(35,100,5), limits=c(35,80))+ ggtitle("AT-III Level First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_at3_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(40,85))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="AT-III",breaks=seq(30,100,5), limits=c(30,100))+ ggtitle("AT-III by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##AXA
axa_post <- select(PostP, starts_with("AXA"))
axa_pre <- select(PreP, starts_with("AXA"))
The_mean_postaxa <- data.frame(Mean=apply(axa_post,1, mean, na.rm = T),Median=apply(axa_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preaxa <- data.frame(Mean=apply(axa_pre, 1, mean, na.rm=T),Median=apply(axa_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postaxa$Mean,The_mean_preaxa$Mean, exact= NULL)
t.test(The_mean_postaxa$Mean,The_mean_preaxa$Mean)
wilcox.test(The_mean_postaxa$Median,The_mean_preaxa$Median)
t.test(The_mean_postaxa$Median,The_mean_preaxa$Median)

summary(The_mean_preaxa$Median)
summary(The_mean_postaxa$Median)

##AXA Graph
median_postaxa_column <-data.frame(Median=apply(axa_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_preaxa_column <- data.frame(Median=apply(axa_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_axa_column <- median_postaxa_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_preaxa_column%>%
              mutate(Protocol ="Pre"))
median_axa_row <- select(The_mean_postaxa, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preaxa,2)%>% 
              mutate(Protocol = "Pre"))

median_axa_column$Protocol <- factor(median_axa_column$Protocol, levels = c("Pre", "Post"))
median_axa_row$Protocol <- factor(median_axa_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_axa_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="Anti-Xa",breaks=seq(0,0.5,0.05), limits=c(0,0.45))+ ggtitle("Anti-Xa Level First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_axa_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="Anti-Xa",breaks=seq(0,0.5,0.05), limits=c(0,0.45))+ ggtitle("Anti-Xa by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##Max Phb
mxphb_post <- select(PostP, starts_with("Max"))
mxphb_pre <- select(PreP, starts_with("Max"))
The_mean_postmxphb <- data.frame(Mean=apply(mxphb_post,1, mean, na.rm = T),Median=apply(mxphb_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_premxphb <- data.frame(Mean=apply(mxphb_pre, 1, mean, na.rm=T),Median=apply(mxphb_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
wilcox.test(mxphb_post$Max_PHB,mxphb_pre$Max_PHB, exact= NULL)
t.test(mxphb_post$Max_PHB,mxphb_pre$Max_PHB)


##Avg PHB
phb_post <- select(PostP, starts_with("PHB"))
phb_pre <- select(PreP, starts_with("PHB"))
The_mean_postphb <- data.frame(Mean=apply(phb_post,1, mean, na.rm = T),Median=apply(phb_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_prephb <- data.frame(Mean=apply(phb_pre, 1, mean, na.rm=T),Median=apply(phb_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postphb$Mean,The_mean_prephb$Mean, exact= NULL)
t.test(The_mean_postphb$Mean,The_mean_prephb$Mean)
wilcox.test(The_mean_postphb$Median,The_mean_prephb$Median)
t.test(The_mean_postphb$Median,The_mean_prephb$Median)

summary(The_mean_prephb$Median)
summary(The_mean_postphb$Median)

##PHB graph
median_postphb_column <-data.frame(Median=apply(phb_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_prephb_column <- data.frame(Median=apply(phb_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_phb_column <- median_postphb_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_prephb_column%>%
              mutate(Protocol ="Pre"))
median_phb_row <- select(The_mean_postphb, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_prephb,2)%>% 
              mutate(Protocol = "Pre"))
median_phb_column <- na.omit(median_phb_column)
median_phb_column$Protocol <- factor(median_phb_column$Protocol, levels = c("Pre", "Post"))
median_phb_row$Protocol <- factor(median_phb_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_phb_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="Plasma-Hemoglobin",breaks=seq(0,325,25), limits=c(0,325))+ ggtitle("Plasma-Hemoglobin Level First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_phb_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(0,100))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="Plasma-Hemogolbin",breaks=seq(0,1000,20), limits=c(0,1000))+ ggtitle("Plasma-Hemoglobin by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##Plts
plt_post <- select(PostP, starts_with("PLT"))
plt_pre <- select(PreP, starts_with("PLT"))
plt1_post <- select(plt_post, 2:31)
plt1_pre <- select(plt_pre, 2:31)
The_mean_postplt <- data.frame(Mean=apply(plt1_post,1, mean, na.rm = T),Median=apply(plt1_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preplt <- data.frame(Mean=apply(plt1_pre, 1, mean, na.rm=T),Median=apply(plt1_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postplt$Mean,The_mean_preplt$Mean, exact= NULL)
t.test(The_mean_postplt$Mean,The_mean_preplt$Mean)
wilcox.test(The_mean_postplt$Median,The_mean_preplt$Median)
t.test(The_mean_postplt$Median,The_mean_preplt$Median)

summary(The_mean_preplt$Median)
summary(The_mean_postplt$Median)

##plts graph
median_postplt_column <-data.frame(Median=apply(plt1_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_preplt_column <- data.frame(Median=apply(plt1_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_plt_column <- median_postplt_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_preplt_column%>%
              mutate(Protocol ="Pre"))
median_plt_row <- select(The_mean_postplt, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preplt,2)%>% 
              mutate(Protocol = "Pre"))
median_plt_column <- na.omit(median_plt_column)
median_plt_column$Protocol <- factor(median_plt_column$Protocol, levels = c("Pre", "Post"))
median_plt_row$Protocol <- factor(median_plt_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_plt_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="Platelets",breaks=seq(40,160,20), limits=c(40,140))+ ggtitle("Platelet Level First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_plt_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(60,160))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="Platelets",breaks=seq(0,200,20), limits=c(0,200))+ ggtitle("Platelet level by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##INR
inr_post <- select(PostP, starts_with("INR"))
inr_pre <- select(PreP, starts_with("INR"))
The_mean_postinr <- data.frame(Mean=apply(inr_post,1, mean, na.rm = T),Median=apply(inr_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preinr <- data.frame(Mean=apply(inr_pre, 1, mean, na.rm=T),Median=apply(inr_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_postinr$Mean,The_mean_preinr$Mean, exact= NULL)
t.test(The_mean_postinr$Mean,The_mean_preinr$Mean)
wilcox.test(The_mean_postinr$Median,The_mean_preinr$Median)
t.test(The_mean_postinr$Median,The_mean_preinr$Median)

summary(The_mean_preinr$Median)
summary(The_mean_postinr$Median)

##INR graph
median_postinr_column <-data.frame(Median=apply(inr_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_preinr_column <- data.frame(Median=apply(inr_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_inr_column <- median_postinr_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_preinr_column%>%
              mutate(Protocol ="Pre"))
median_inr_row <- select(The_mean_postinr, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preinr,2)%>% 
              mutate(Protocol = "Pre"))
median_inr_column <- na.omit(median_inr_column)
median_inr_column$Protocol <- factor(median_inr_column$Protocol, levels = c("Pre", "Post"))
median_inr_row$Protocol <- factor(median_inr_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_inr_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(0,124,12))+scale_y_continuous(name="INR",breaks=seq(1,2,0.1), limits=c(1.2,1.8))+ ggtitle("INR First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_inr_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(1.0,1.7))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="INR",breaks=seq(0,4,0.1), limits=c(0,4))+ ggtitle("INR by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##heparin rate
hr_post <- select(PostP, starts_with("HR"))
hr_pre <- select(PreP, starts_with("HR"))
The_mean_posthr <- data.frame(Mean=apply(hr_post,1, mean, na.rm = T),Median=apply(hr_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_prehr <- data.frame(Mean=apply(hr_pre, 1, mean, na.rm=T),Median=apply(hr_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

wilcox.test(The_mean_posthr$Mean,The_mean_prehr$Mean, exact= NULL)
t.test(The_mean_posthr$Mean,The_mean_prehr$Mean)
wilcox.test(The_mean_posthr$Median,The_mean_prehr$Median)
t.test(The_mean_posthr$Median,The_mean_prehr$Median)

summary(The_mean_prehr$Median)
summary(The_mean_posthr$Median)

##heparin rate graph
median_posthr_column <-data.frame(Median=apply(hr_post,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120))
median_prehr_column <- data.frame(Median=apply(hr_pre,2,median, na.rm=T),hour=c(4,8,12,16,20,24,28,32	,36,	40,	44,	48,	52,	56,	60,	64,	68,	72,	76,	80,	84,	88,	92,	96,	100,	104,	108,	112,	116,	120))
median_hr_column <- median_posthr_column%>%
  mutate(Protocol ="Post")%>%
  bind_rows(median_prehr_column%>%
              mutate(Protocol ="Pre"))
median_hr_row <- select(The_mean_posthr, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_prehr,2)%>% 
              mutate(Protocol = "Pre"))
median_hr_column <- na.omit(median_hr_column)
median_hr_column$Protocol <- factor(median_hr_column$Protocol, levels = c("Pre", "Post"))
median_hr_row$Protocol <- factor(median_hr_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_hr_column,aes(y=Median,x=hour,col=Protocol))+geom_line()+scale_color_manual(values = brewer.pal(3, "Set1"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_continuous(name= "Hour Since Going on ECMO", breaks=seq(4,124,12))+scale_y_continuous(name="Heparin Rate",breaks=seq(10,50,5), limits=c(10,50))+ ggtitle("Heparin Rate First 120 Hours by Protocol")+theme(plot.title = element_text(hjust = 0.5))
ggplot(median_hr_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="Heparin Rate",breaks=seq(5,50,5), limits=c(5,50))+ ggtitle("Heparin by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##PRBC per Kilo
prbc_post <- select(PostP, starts_with("PRBC_Per"))
prbc_pre <- select(PreP, starts_with("PRBC_Per"))
The_mean_postprbc <- data.frame(Mean=apply(prbc_post,1, mean, na.rm = T),Median=apply(prbc_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preprbc <- data.frame(Mean=apply(prbc_pre, 1, mean, na.rm=T),Median=apply(prbc_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
wilcox.test(The_mean_postprbc$Mean,The_mean_preprbc$Mean, exact= NULL)
t.test(The_mean_postprbc$Mean,The_mean_preprbc$Mean)
wilcox.test(The_mean_postprbc$Median,The_mean_preprbc$Median)
t.test(The_mean_postprbc$Median,The_mean_preprbc$Median)
summary(The_mean_preprbc$Median)
summary(The_mean_postprbc$Median)

##PRBC per kilo graph
median_prbc_row <- select(The_mean_postprbc, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preprbc,2)%>% 
              mutate(Protocol = "Pre"))
median_prbc_row <- na.omit(median_prbc_row)
median_prbc_row$Protocol <- factor(median_prbc_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_prbc_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim=c(0,70))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="PRBC PER Kilo",breaks=seq(0,200,10), limits=c(0,200))+ ggtitle("PRBC Per Kilo by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##FFP per kilo
ffp_post <- select(PostP, starts_with("FFP_Per"))
ffp_pre <- select(PreP, starts_with("FFP_Per"))
The_mean_postffp <- data.frame(Mean=apply(ffp_post,1, mean, na.rm = T),Median=apply(ffp_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preffp <- data.frame(Mean=apply(ffp_pre, 1, mean, na.rm=T),Median=apply(ffp_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
wilcox.test(The_mean_postffp$Mean,The_mean_preffp$Mean, exact= NULL)
t.test(The_mean_postffp$Mean,The_mean_preffp$Mean)
wilcox.test(The_mean_postffp$Median,The_mean_preffp$Median)
t.test(The_mean_postffp$Median,The_mean_preffp$Median)
summary(The_mean_preffp$Median)
summary(The_mean_postffp$Median)

##FFP per kilo graph
median_ffp_row <- select(The_mean_postffp, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preffp,2)%>% 
              mutate(Protocol = "Pre"))
median_ffp_row <- na.omit(median_ffp_row)
median_ffp_row$Protocol <- factor(median_ffp_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_ffp_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol), outlier.shape = NA)+coord_cartesian(ylim=c(0,80))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="FFP PER Kilo",breaks=seq(0,200,10), limits=c(0,200))+ ggtitle("FFP Per Kilo by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##Plt per kilo
pltsper_post <- select(PostP, starts_with("plts_Per"))
pltsper_pre <- select(PreP, starts_with("plts_Per"))
The_mean_postpltsper <- data.frame(Mean=apply(pltsper_post,1, mean, na.rm = T),Median=apply(pltsper_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_prepltsper <- data.frame(Mean=apply(pltsper_pre, 1, mean, na.rm=T),Median=apply(pltsper_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
wilcox.test(The_mean_postpltsper$Mean,The_mean_prepltsper$Mean, exact= NULL)
t.test(The_mean_postpltsper$Mean,The_mean_prepltsper$Mean)
wilcox.test(The_mean_postpltsper$Median,The_mean_prepltsper$Median)
t.test(The_mean_postpltsper$Median,The_mean_prepltsper$Median)
summary(The_mean_prepltsper$Median)
summary(The_mean_postpltsper$Median)

##plts per kill graph
median_pltsper_row <- select(The_mean_postpltsper, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_prepltsper,2)%>% 
              mutate(Protocol = "Pre"))


median_pltsper_row$Protocol <- factor(median_pltsper_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_pltsper_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol), outlier.shape = NA)+coord_cartesian(ylim=c(0,125))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="Platelets PER Kilo",breaks=seq(0,500,25), limits=c(0,500))+ ggtitle("Platelets Per Kilo by Protocol")+theme(plot.title = element_text(hjust = 0.5))


##AT3 given
at3dose_post <- select(PostP, starts_with("total_AT3"))
at3dose_pre <- select(PreP, starts_with("total_AT3"))
The_mean_postat3dose <- data.frame(Mean=apply(at3dose_post,1, mean, na.rm = T),Median=apply(at3dose_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_preat3dose <- data.frame(Mean=apply(at3dose_pre, 1, mean, na.rm=T),Median=apply(at3dose_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
wilcox.test(The_mean_postat3dose$Mean,The_mean_preat3dose$Mean, exact= NULL)
t.test(The_mean_postat3dose$Mean,The_mean_preat3dose$Mean)
wilcox.test(The_mean_postat3dose$Median,The_mean_preat3dose$Median)
t.test(The_mean_postat3dose$Median,The_mean_preat3dose$Median)
summary(The_mean_preat3dose$Median)
summary(The_mean_postat3dose$Median)

##At3 graph
median_at3dose_row <- select(The_mean_postat3dose, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_preat3dose,2)%>% 
              mutate(Protocol = "Pre"))


median_at3dose_row$Protocol <- factor(median_at3dose_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_at3dose_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(0,8))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name="AT-III Dose",breaks=seq(0,15,1), limits=c(0,12))+ ggtitle("AT-III Dose by Protocol")+theme(plot.title = element_text(hjust = 0.5))                                                                                                                                                                                                                                                                                                                                                              

##Cryo per kilo
cryo_post <- select(PostP, starts_with("cryo_Per"))
cryo_pre <- select(PreP, starts_with("cryo_Per"))
The_mean_postcryo <- data.frame(Mean=apply(cryo_post,1, mean, na.rm = T),Median=apply(cryo_post,1, median, na.rm = T),postpatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
The_mean_precryo <- data.frame(Mean=apply(cryo_pre, 1, mean, na.rm=T),Median=apply(cryo_pre,1, median, na.rm = T),prepatient=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
wilcox.test(The_mean_postcryo$Mean,The_mean_precryo$Mean, exact= NULL)
t.test(The_mean_postcryo$Mean,The_mean_precryo$Mean)
wilcox.test(The_mean_postcryo$Median,The_mean_precryo$Median)
t.test(The_mean_postcryo$Median,The_mean_precryo$Median)
summary(The_mean_precryo$Median)
summary(The_mean_postcryo$Median)

##cryo per kilo graph
median_cryo_row <- select(The_mean_postcryo, 2)%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(The_mean_precryo,2)%>% 
              mutate(Protocol = "Pre"))


median_cryo_row$Protocol <- factor(median_cryo_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_cryo_row,aes(y=Median,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(0,6))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name=" Cryoprecipitate Per Kilo",breaks=seq(0,21,1), limits=c(0,21))+ ggtitle("Cryoprecipitate Per Kilo by Protocol")+theme(plot.title = element_text(hjust = 0.5))                                                                                                                                                                                                                                                                                                                                                              


##LOS test
wilcox.test(PostP$LOS,PreP$LOS)
t.test(PostP$LOS,PreP$LOS)
summary(PostP$LOS)
summary(PreP$LOS)

##LOS graph
median_los_row <- select(PostP, starts_with("LOS"))%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(PreP,starts_with("LOS"))%>% 
              mutate(Protocol = "Pre"))


median_los_row$Protocol <- factor(median_los_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_los_row,aes(y=LOS,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(0,110))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name=" Length of Stay",breaks=seq(0,400,10), limits=c(0,400))+ ggtitle("Length of Stay by Protocol")+theme(plot.title = element_text(hjust = 0.5))                                                                                                                                                                                                                                                                                                                                                              


##Weight test
wilcox.test(PostP$`Weight_(KG)`,PreP$`Weight_(KG)`)
t.test(PostP$`Weight_(KG)`,PreP$`Weight_(KG)`)

##weight graph
median_weight_row <- select(PostP, starts_with("Weight"))%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(PreP,starts_with("Weight"))%>% 
              mutate(Protocol = "Pre"))


median_weight_row$Protocol <- factor(median_weight_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_weight_row,aes(y=`Weight_(KG)`,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(1,6))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name=" Weight Per Kilogram",breaks=seq(0,6,1), limits=c(0,6))+ ggtitle("Weight by Protocol")+theme(plot.title = element_text(hjust = 0.5))                                                                                                                                                                                                                                                                                                                                                              


##Hours on ecmo
wilcox.test(PostP$Hours_on_ECMO,PreP$Hours_on_ECMO)
t.test(PostP$Hours_on_ECMO,PreP$Hours_on_ECMO)

##House on ECMO graph
median_hours_row <- select(PostP, starts_with("Hours_on_ECMO"))%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(PreP,starts_with("Hours_on_ECMO"))%>% 
              mutate(Protocol = "Pre"))


median_hours_row$Protocol <- factor(median_hours_row$Protocol, levels = c("Pre", "Post"))
ggplot(median_hours_row,aes(y=Hours_on_ECMO,x= Protocol))+geom_boxplot(aes(fill=Protocol),outlier.shape = NA)+coord_cartesian(ylim = c(0,220))+scale_fill_manual(values = brewer.pal(3, "Dark2"), guide = guide_legend(title = "Protocol"))+theme_classic(base_size = 14)+scale_x_discrete(name= "Protocol")+scale_y_continuous(name=" Hours on ECMO",breaks=seq(0,600,20), limits=c(0,600))+ ggtitle("Hours on ECMO by Protocol")+theme(plot.title = element_text(hjust = 0.5))                                                                                                                                                                                                                                                                                                                                                              


##Hours till start of heparin
till_heparin_pre <- c(2:28,2:02,3:20,0:50,0:42,4:00,0:55,1:28,3:19,3:41,1:19,2:52,1:39, 0:15,1:00,3:45, 0:49, 3:01, 0:49)
till_heparin_post <-c(1:26,1:34,6:00, 2:03, 0:59, 0:57, 2:30, 0:15, 1:40,0:59, 0:53,  0:43,1:20,1:42,1:26)
str(till_heparin_pre)
Pro_heparin_post <- data.frame(till_heparin_pre)

median_hours_row <- till_heparin_pre%>%
  mutate(Protocol = "Post")%>%
  bind_rows(select(PreP,starts_with("Hours_on_ECMO"))%>% 
              mutate(Protocol = "Pre"))
##wilcox.test(PostP$Hours_till_start_of_heparin,PreP$Hours_till_start_of_heparin)

##Plasma_phereis 
Plasma_phereis <- matrix(c(1,8,14,11), nrow=2, ncol=2)
fisher.test(Plasma_phereis)

#Circuit change
Circuit_change <-matrix(c(7,0,12,15), nrow=2,ncol=2)
fisher.test(Circuit_change)
summary(PreP$Hours_Till_circuit_change)
wilcox.test(PreP$Hours_Till_circuit_change,PostP$Hours_Till_circuit_change)

summary(PreP$`Weight_(KG)`)
summary(PostP$`Weight_(KG)`)
wilcox.test(PreP$`Weight_(KG)`,PostP$`Weight_(KG)`)

##Need for ECMO 
Need_ECMO <- matrix(c(16,15,3,1), nrow=2, ncol=2)
fisher.test(Need_ECMO)

##Gender
Gender <- matrix(c(10,9,9,6), nrow=2, ncol=2)
fisher.test(Gender) 

##Mortality
Morality <- matrix(c(4,3,15,11),nrow=2, ncol=2)
fisher.test(Morality)
