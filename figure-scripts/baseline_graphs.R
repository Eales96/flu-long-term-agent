# Set location
setwd('/flu-long-term-agent/figure_scripts')

# Load plotting functions
source('plot_functions.R')

# Load packages
library(ggdist)
library(dplyr)
library(ggridges)

# Specify simulation parameters (obtained from main.py)
pop_size <- 80000 
N_years <- 160
N_iter <- 256 


#Load in data
dat1 <- read.table('input_data/AttackRates/baseline.csv', header=F, sep=',')



################################################################################################################################
# Mean dynamics by time figure 
################################################################################################################################
# Overall
AR1 <- attack_rate_over_time(dat = dat1, pop_size, N_years, N_iter)
AR_1 <- AR1[[2]][AR1[[2]]$TSI<=160,]
ARD <- attack_rate_over_decade(dat = dat1, pop_size, N_years, N_iter)
AR_D <- ARD[[2]][ARD[[2]]$TSI<=160,]

# By age-groups
A_AR_00 <- age_attack_rate_by_tsi(dat = dat1,pop_size, N_years, N_iter, tsi=1)
A_AR_01 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=11)
A_AR_02 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=31)
A_AR_03 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=51)
A_AR_04 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=71)
A_AR_05 <- age_attack_rate_by_decade(dat = dat1,pop_size, N_years, N_iter, tsi=151)

A_AR_00[[2]]$tsi <- "Pandemic year"
A_AR_01[[2]]$tsi <- "10-19 years"
A_AR_02[[2]]$tsi <- "30-39 years"
A_AR_03[[2]]$tsi <- "50-59 years"
A_AR_04[[2]]$tsi <- "70-79 years"
A_AR_05[[2]]$tsi <- "150-159 years"

A_AR_00[[3]]$tsi <- "Pandemic year"
A_AR_01[[3]]$tsi <- "10-19 years"
A_AR_02[[3]]$tsi <- "30-39 years"
A_AR_03[[3]]$tsi <- "50-59 years"
A_AR_04[[3]]$tsi <- "70-79 years"
A_AR_05[[3]]$tsi <- "150-159 years"

age_df <- rbind(A_AR_00[[2]],A_AR_01[[2]],A_AR_01[[2]],A_AR_02[[2]],A_AR_03[[2]],A_AR_04[[2]],A_AR_05[[2]])
age_df$tsi <- factor(age_df$tsi, levels=c("Pandemic year","10-19 years","30-39 years","50-59 years","70-79 years","150-159 years"))
age_df_grp <- rbind(A_AR_00[[3]],A_AR_01[[3]],A_AR_01[[3]],A_AR_02[[3]],A_AR_03[[3]],A_AR_04[[3]],A_AR_05[[3]])
age_df_grp$tsi <- factor(age_df_grp$tsi, levels=c("Pandemic year","10-19 years","30-39 years","50-59 years","70-79 years","150-159 years"))

age_df_comp <- age_df_grp
age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR

age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp <- age_df_comp[age_df_comp$tsi!="Pandemic year",]



age_df_comp <- age_df
age_df_comp[age_df_comp$tsi=="5-9 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="5-9 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$mn_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR

age_df_comp[age_df_comp$tsi=="5-9 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="5-9 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="10-19 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="30-39 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="50-59 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="70-79 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR <- age_df_comp[age_df_comp$tsi=="150-159 years",]$sem_AR/age_df_comp[age_df_comp$tsi=="Pandemic year",]$mn_AR
age_df_comp <- age_df_comp[age_df_comp$tsi!="Pandemic year",]


# Plot of attack rate over time by year
AR_1$TSI <- AR_1$TSI-1
plt1A<-ggplot()+
  geom_line(data = AR_1, aes(x=TSI, y=mn_AR), alpha=0.5)+
  geom_point(data = AR_1, aes(x=TSI, y=mn_AR))+
  geom_ribbon(data = AR_1, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR), alpha=0.5)+
  #geom_jitter(data=AR1[[1]], aes(x=TSI, y=AR), alpha=0.2, width=0.2, height=0)+
  #geom_errorbar(data=AR_1, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme_bw(base_size = 12)

# Plot of attack rate over time by decade
AR_D$TSI <- AR_D$TSI-1




ARD_8yr <- attack_rate_over_eightyr(dat = dat1, pop_size, N_years, N_iter)
ARD_8yr[[2]]$TSI[1] <-2
ts1a <- AR_1[1,]
ts1b <- AR_1[1,]
ts1b$TSI<-1

ts2a <- AR_1[2,]
ts2b <- AR_1[2,]
ts2b$TSI<-2

ts <- rbind(ts1a, ts1b, ts2a, ts2b)
ts$dec <- c(-1,-1,0,0)
AR_D$TSI[seq(2,32,by=2)] <- AR_D$TSI[seq(2,32,by=2)]+1.0
AR_D_ts <- rbind(AR_D[3:nrow(AR_D),], ARD_8yr[[2]])

plt1B<- ggplot()+
  geom_line(data = AR_D_ts, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec))+
  geom_ribbon(data = AR_D_ts, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec), alpha=0.5)+
  #geom_jitter(data=AR1[[1]], aes(x=TSI, y=AR), alpha=0.2, width=0.2, height=0)+
  #geom_errorbar(data=ARD[[2]], aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100, 120, 140,160))+
  theme_bw(base_size = 12)

# Plot of attack rate by age group
plt1C<-ggplot()+
  geom_line(data=age_df, aes(x=age, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tsi, color=tsi))+
  geom_ribbon(data=age_df, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, fill=tsi),alpha=0.5)+
  scale_color_brewer("Years since pandemic",palette='Dark2')+
  scale_fill_brewer("Years since pandemic",palette="Dark2")+
  xlab("Age (years)")+
  ylab("Average annual attack rate")+
  theme_bw(base_size = 12)+
  theme(legend.position = c(0.79,0.77),
        legend.background = element_rect(colour = 'black'))

# Plot of porportional difference in attack rate by age-group compared to pandemic year
col_custom <- RColorBrewer::brewer.pal(7, "Dark2")[2:7]
plt1D<-ggplot()+
  geom_line(data=age_df_comp, aes(x=age, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tsi, color=tsi))+
  geom_ribbon(data=age_df_comp, aes(x=age, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tsi, fill=tsi),alpha=0.5)+
  scale_color_manual("Years since pandemic",values=col_custom)+
  scale_fill_manual("Years since pandemic",values=col_custom)+
  xlab("Age (years)")+
  ylab("Attack rate (proportion of value in pandemic year)")+
  theme_bw(base_size = 12)+
  theme(legend.position = c(0.27,0.8),
        legend.background = element_rect(colour = 'black'))

plt1A<-plt1A+labs(tag="A")+
  theme(plot.tag.position=c(0.005,0.95))
plt1B<-plt1B+labs(tag="B")+
  theme(plot.tag.position=c(0.02,1))
plt1C<-plt1C+labs(tag="C")+
  theme(plot.tag.position=c(0.02,1),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8))
plt1D<-plt1D+labs(tag="D")+
  theme(plot.tag.position=c(0.02,1),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8))


mid_row <- plot_grid(plt1B, plt1C, plt1D, nrow=1, rel_widths = c(1,1,1))
plt_final1 <- plot_grid(plt1A, mid_row, nrow=2, rel_heights = c(1,1.2))

ggsave('figures/Figure3.pdf', width=12, height=8.5)


##########################################################################################################################################
### Variation in attack rates figure/s
###########################################################################################################################################

df <- AR1[[1]]
df$TSI <- df$TSI-1
plt2D <- ggplot()+
  geom_line(data=df[df$sim<4,],aes(x = TSI, y = AR, color=factor(sim)),position = position_dodge(width=0.5), alpha=0.5) +
  geom_point(data=df[df$sim<4,],aes(x = TSI, y = AR, color=factor(sim)),position = position_dodge(width=0.5))+
  scale_color_brewer(palette='Dark2') +
  xlab("Years since pandemic")+
  ylab("Annual attack rate")+
  theme_bw()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  coord_cartesian(xlim=c(5,155))+
  theme(legend.position="none",
        legend.background = element_rect(color="black"))


df <- AR1[[1]]
df <- df[df$TSI<=160,]
df$TSI <- df$TSI-1
df <- group_by(df, TSI)
df <- point_interval(df, AR, .width = c(.5, .8, .95), .point = mean)


plt2A <- ggplot() +
  geom_lineribbon(data=df,aes(x = TSI, y = AR, ymin = .lower, ymax = .upper), size=0) +
  geom_point(data=df,aes(x = TSI, y = AR, ymin = .lower, ymax = .upper)) +
  scale_fill_brewer(name="Pointwise\nconfidence interval") +
  #geom_line(data=AR1[[2]], aes(x=TSI, y=mn_AR), col='red')+
  xlab("Years since pandemic")+
  ylab("Annual attack rate")+
  theme_bw()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  coord_cartesian(xlim=c(5,155))+
  theme(legend.position="none",
        legend.background = element_rect(color="black"))



df <- AR1[[1]]
df_plt<- data.frame()
df_pltT<- data.frame()
for(i in seq(0,0.6,0.02)){
  for(j in seq_len(N_iter)){
    tmp <- df[df$sim==j,]
    dns<-nrow(tmp[tmp$AR>=i & tmp$AR<i+0.02,])
    row_df1 <- data.frame(sim = j,
                          bin=i,
                          density=dns/N_years/0.02)
    row_df2 <- data.frame(sim = j,
                          bin=i+0.0199,
                          density=dns/N_years/0.02)
    df_plt <- rbind(df_plt, row_df1, row_df2)
  }
  
  
  dns<-nrow(df[df$AR>=i & df$AR<i+0.02,])
  row_df1 <- data.frame(bin=i,
                        density=dns/N_years/N_iter/0.02)
  row_df2 <- data.frame(bin=i+0.0199,
                        density=dns/N_years/N_iter/0.02)
  df_pltT <- rbind(df_pltT, row_df1, row_df2)
  
  
}
df_plt <- group_by(df_plt, bin)
df_plt <- point_interval(df_plt, density, .width = c(.5, .8, .95), .point=mean)

plt2B <- ggplot() +
  geom_lineribbon(data=df_plt,aes(x = bin, y = density, ymin = .lower, ymax = .upper)) +
  scale_fill_brewer(name="Pointwise\nconfidence interval") +
  #geom_line(data=df_pltT, aes(x=bin, y=density), col="red")+
  xlab("Annual attack rate")+
  ylab("Density")+
  theme_bw()+
  theme(legend.position=c(0.8,0.8),
        legend.background = element_rect(color="black"))




df <- AR1[[1]]
df$dec<-as.factor((df$TSI-1) %/%10)

levels(df$dec) <- list("0-9" = "0",
                       "10-19" = "1",
                       "20-29" = "2",
                       "30-39" = "3",
                       "40-49" = "4",
                       "50-59" = "5",
                       "60-69" = "6",
                       "70-79" = "7",
                       "80-89" = "8",
                       "90-99" = "9",
                       "100-109" = "10",
                       "110-119" = "11",
                       "120-129" = "12",
                       "130-139" = "13",
                       "140-149" = "14",
                       "150-159" = "15")

plt2C <- ggplot(df, aes(x = AR, y = dec)) +
  geom_density_ridges(stat = 'binline', binwidth=0.01) +
  theme_ridges() + 
  geom_point(data=ARD[[2]], aes(x=mn_AR, y=dec))+
  geom_path(data=ARD[[2]], aes(y=dec,x=mn_AR, group=1))+
  geom_errorbarh(data=ARD[[2]], aes(x=mn_AR,xmin=mn_AR-1.96*sem_AR,xmax=mn_AR+1.96*sem_AR, y=dec), height=0)+
  theme_bw()+
  theme(legend.position = "none")+
  coord_cartesian(ylim=c(1,17))+
  ylab("Years since pandemic")+
  xlab("Annual attack rate")




plt2D<-plt2D+labs(tag="A")+
  theme(plot.tag.position=c(0.005,0.95))
plt2A<-plt2A+labs(tag="B")+
  theme(plot.tag.position=c(0.005,0.95))
plt2B<-plt2B+labs(tag="C")+
  theme(plot.tag.position=c(0.01,0.98))
plt2C<-plt2C+labs(tag="D")+
  theme(plot.tag.position=c(0.03,0.98))



bot_row <- plot_grid(plt2B, plt2C, nrow=1, rel_widths = c(1,1.5))
plt_final2 <- plot_grid(plt2D, plt2A, bot_row, nrow=3, rel_heights = c(1, 1,1.5))
ggsave('figures/Variation_sup.pdf', width=11, height=9)

#########################################################################################################################################################
# Mean dynamics by cohort figures
#########################################################################################################################################################
dbc1 <- dynamics_by_cohort(dat1, pop_size, N_years, N_iter)

dbc1_01 <- dynamics_by_cohort_age_range(dat1, pop_size, N_years, N_iter, 0, 4, "Early childhood infections (0-4 years)")
dbc1_02 <- dynamics_by_cohort_age_range(dat1, pop_size, N_years, N_iter, 5, 17, "School age infections (5-17 years)")
dbc1_03 <- dynamics_by_cohort_age_range(dat1, pop_size, N_years, N_iter, 18, 64, "Working age infections (18-64 years)")
dbc1_04 <- dynamics_by_cohort_age_range(dat1, pop_size, N_years, N_iter, 65, 79, "Old age infections (65-79 years)")
dbc1_05 <- dynamics_by_cohort_age_range(dat1, pop_size, N_years, N_iter, 0, 79, "Total infections")

dbc <- rbind(dbc1_01, dbc1_02, dbc1_03, dbc1_04, dbc1_05)

dbc1$age_grp <- factor(dbc1$age_grp)

levels(dbc1$age_grp) <- list("Ages 0-4 years" = "1",
                                 "Ages 5-9 years" = "2",
                                 "Ages 10-14 years" = "3",
                                 "Ages 15-19 years" = "4",
                                 "Ages 20-24 years" = "5",
                                 "Ages 25-29 years" = "6",
                                 "Ages 30-34 years" = "7",
                                 "Ages 35-39 years" = "8",
                                 "Ages 40-44 years" = "9",
                                 "Ages 45-49 years" = "10",
                                 "Ages 50-54 years" = "11",
                                 "Ages 55-59 years" = "12",
                                 "Ages 60-64 years" = "13",
                                 "Ages 65-69 years" = "14",
                                 "Ages 70-74 years" = "15",
                                 "Ages 75-79 years" = "16")


dbc1 <- dbc1[dbc1$birth_year<81,]
dbc1 <- dbc1[dbc1$yrs>4,]

data_min1 <- aggregate(ar ~ age_grp,             
                       data = dbc1,
                       FUN = min)

dbc1$min <-data_min1$ar[sapply(dbc1$age_grp, function(x) which(x == data_min1$age_grp)[[1]])]


dbc1_mn1 <- rbind(dbc1[dbc1$age_grp=="Ages 0-4 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 5-9 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 10-14 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 15-19 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 20-24 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 25-29 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 30-34 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 35-39 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 40-44 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 45-49 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 50-54 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 55-59 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 60-64 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 65-69 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 70-74 years",][1,],
                  dbc1[dbc1$age_grp=="Ages 75-79 years",][1,])

dbc1_mn3 <- rbind(dbc1[dbc1$age_grp=="Ages 0-4 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 5-9 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 10-14 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 15-19 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 20-24 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 25-29 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 30-34 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 35-39 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 40-44 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 45-49 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 50-54 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 55-59 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 60-64 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 65-69 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 70-74 years",][3,],
                  dbc1[dbc1$age_grp=="Ages 75-79 years",][3,])



############################### For all age-groups #############################

cohort1<-ggplot()+
  scale_x_continuous(breaks=c(-80,-60,-40,-20,0,20,40,60,80), labels = c(-80,-60,-40,-20,"0\nPandemic\nyear",20,40,60,80))+
  xlab("Birth year")+
  ylab("Expected number of infections whilst in age-group")+
  geom_hline(aes(yintercept = ar), data=dbc1_mn1, linetype='dashed', col='red')+
  geom_hline(aes(yintercept = min), data=dbc1, linetype='dashed', col='black')+
  geom_line(data=dbc1, aes(x=birth_year, y=ar,color=age_grp))+
  geom_ribbon(data=dbc1, aes(x=birth_year, y=ar,fill=age_grp, ymin=ar-1.96*sem_ar, ymax=ar+1.96*sem_ar), alpha=0.3)+
  facet_wrap(.~age_grp, nrow=4, scales = 'free_y')+
  scale_color_viridis_d(option='turbo')+
  scale_fill_viridis_d(option='turbo')+
  coord_cartesian(xlim=c(-80,80))+
  theme_bw()+
  theme(legend.position = "none")

levels(dbc1$age_grp) <- list("0-4 years" = "Ages 0-4 years",
                             "5-9 years" = "Ages 5-9 years",
                             "10-14 years" = "Ages 10-14 years",
                             "15-19 years" = "Ages 15-19 years",
                             "20-24 years" = "Ages 20-24 years",
                             "25-29 years" = "Ages 25-29 years",
                             "30-34 years" = "Ages 30-34 years",
                             "35-39 years" = "Ages 35-39 years",
                             "40-44 years" = "Ages 40-44 years",
                             "45-49 years" = "Ages 45-49 years",
                             "50-54 years" = "Ages 50-54 years",
                             "55-59 years" = "Ages 55-59 years",
                             "60-64 years" = "Ages 60-64 years",
                             "65-69 years" = "Ages 65-69 years",
                             "70-74 years" = "Ages 70-74 years",
                             "75-79 years" = "Ages 75-79 years")
cohort2<-ggplot()+
  scale_x_continuous(breaks=c(-80,-60,-40,-20,0,20,40,60,80), labels = c(-80,-60,-40,-20,"0\nPandemic\nyear",20,40,60,80))+
  scale_y_continuous(breaks=c(1.0,1.1,1.2,1.3,1.4,1.5,1.6))+
  xlab("Birth year")+
  ylab("Expected number of infections whilst in age-group relative to minimum")+
  geom_line(data=dbc1, aes(x=birth_year, y=ar/min, color=age_grp))+
  geom_ribbon(data=dbc1, aes(x=birth_year, y=ar/min, fill=age_grp, ymin=(ar-1.96*sem_ar)/min, ymax=(ar+1.96*sem_ar)/min), alpha=0.3)+
  #facet_wrap(.~age_grp, nrow=4, scales = 'fixed')+
  geom_hline(yintercept = 1.0, linetype="dashed", col='black')+
  scale_color_viridis_d('Age-group',option='turbo')+
  scale_fill_viridis_d('Age-group',option='turbo')+
  coord_cartesian(xlim=c(-80,80))+
  theme_bw()+
  theme(legend.position = c(0.4,0.9),
        legend.background = element_rect(color='black'))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),
         color=guide_legend(nrow=4))



cohort1<-cohort1+labs(tag="A")+
  theme(plot.tag.position=c(0.005,0.98))
cohort2<-cohort2+labs(tag="B")+
  theme(plot.tag.position=c(0.005,0.98))

plot_grid(cohort1, cohort2, nrow=1, rel_widths=c(1.5,1))
ggsave('figures/CohortSup.pdf', width=16, height=8)


############################### For a few age groups #############################
test <- dbc[dbc$yrs>0,]

test[test$age_grp=="Early childhood infections (0-4 years)" & test$yrs<5,]$yrs <- 0
test[test$age_grp=="School age infections (5-17 years)" & test$yrs<13,]$yrs <- 0
test[test$age_grp=="Working age infections (18-64 years)" & test$yrs<47,]$yrs <- 0
test[test$age_grp=="Old age infections (65-79 years)" & test$yrs<15,]$yrs <- 0
test[test$age_grp=="Total infections" & test$yrs<80,]$yrs <- 0
test <- test[test$yrs>0,]

test <- test[test$birth_year<81,]

#test[test$age_grp == "Early childhood infections (0-4 years)",]$ar <- test[test$age_grp == "Early childhood infections (0-4 years)",]$ar/5
#test[test$age_grp == "School age infections (5-17 years)",]$ar <- test[test$age_grp == "School age infections (5-17 years)",]$ar/13
#test[test$age_grp == "Working age infections (18-64 years)",]$ar <- test[test$age_grp == "Working age infections (18-64 years)",]$ar/47
#test[test$age_grp == "Old age infections (65-79 years)",]$ar <- test[test$age_grp == "Old age infections (65-79 years)",]$ar/15
#test[test$age_grp == "Total infections",]$ar <- test[test$age_grp == "Total infections",]$ar/80

#test[test$age_grp == "Early childhood infections (0-4 years)",]$sem_ar <- test[test$age_grp == "Early childhood infections (0-4 years)",]$sem_ar/5
#test[test$age_grp == "School age infections (5-17 years)",]$sem_ar <- test[test$age_grp == "School age infections (5-17 years)",]$sem_ar/13
#test[test$age_grp == "Working age infections (18-64 years)",]$sem_ar <- test[test$age_grp == "Working age infections (18-64 years)",]$sem_ar/47
#test[test$age_grp == "Old age infections (65-79 years)",]$sem_ar <- test[test$age_grp == "Old age infections (65-79 years)",]$sem_ar/15
#test[test$age_grp == "Total infections",]$sem_ar <- test[test$age_grp == "Total infections",]$sem_ar/80

test$age_grp <- factor(test$age_grp, levels = c("Early childhood infections (0-4 years)",
                                                "School age infections (5-17 years)",
                                                "Working age infections (18-64 years)",
                                                "Old age infections (65-79 years)",
                                                "Total infections"))

mn1 <- rbind(test[test$age_grp=="Early childhood infections (0-4 years)",][1,],
             test[test$age_grp=="School age infections (5-17 years)",][1,],
             test[test$age_grp=="Working age infections (18-64 years)",][1,],
             test[test$age_grp=="Old age infections (65-79 years)",][1,],
             test[test$age_grp=="Total infections",][1,])

mn3 <- rbind(test[test$age_grp=="Early childhood infections (0-4 years)",][3,],
             test[test$age_grp=="School age infections (5-17 years)",][3,],
             test[test$age_grp=="Working age infections (18-64 years)",][3,],
             test[test$age_grp=="Old age infections (65-79 years)",][3,],
             test[test$age_grp=="Total infections",][3,])


plt3A<-ggplot()+
  geom_line(data=test, aes(x=birth_year, y=ar))+
  geom_ribbon(data=test, aes(x=birth_year, y=ar, ymin=ar-1.96*sem_ar, ymax=ar+1.96*sem_ar), alpha=0.5)+
  scale_x_continuous(breaks=c(-80,-60,-40,-20,0,20,40,60,80), labels = c(-80,-60,-40,-20,"0\nPandemic\nyear",20,40,60,80))+
  geom_vline(aes(xintercept = birth_year), data=mn1, linetype='dashed', col='red', alpha=0.5)+
  geom_vline(xintercept = 40, linetype='dashed', col='blue', alpha=0.5)+
  #geom_vline(aes(xintercept = birth_year), data=mn3, linetype='dashed', col='blue', alpha=0.5)+
  #geom_hline(aes(yintercept = ar), data=mn1, linetype='dashed', col='red')+
  #geom_hline(aes(yintercept = ar), data=mn3, linetype='dashed', col='blue')+
  xlab("Cohort (birth year)")+
  ylab("Expected number of infections over lifetime")+
  facet_wrap(.~age_grp, nrow=5, scales = 'free_y')+
  coord_cartesian(xlim=c(-80,80))+
  theme_bw(base_size = 15)



test[test$age_grp == "Early childhood infections (0-4 years)",]$sem_ar <- test[test$age_grp == "Early childhood infections (0-4 years)",]$sem_ar/min(test[test$age_grp == "Early childhood infections (0-4 years)",]$ar)
test[test$age_grp == "School age infections (5-17 years)",]$sem_ar <- test[test$age_grp == "School age infections (5-17 years)",]$sem_ar/min(test[test$age_grp == "School age infections (5-17 years)",]$ar[3])
test[test$age_grp == "Working age infections (18-64 years)",]$sem_ar <- test[test$age_grp == "Working age infections (18-64 years)",]$sem_ar/min(test[test$age_grp == "Working age infections (18-64 years)",]$ar)
test[test$age_grp == "Old age infections (65-79 years)",]$sem_ar <- test[test$age_grp == "Old age infections (65-79 years)",]$sem_ar/min(test[test$age_grp == "Old age infections (65-79 years)",]$ar)
test[test$age_grp == "Total infections",]$sem_ar <- test[test$age_grp == "Total infections",]$sem_ar/min(test[test$age_grp == "Total infections",]$ar)


test[test$age_grp == "Early childhood infections (0-4 years)",]$ar <- test[test$age_grp == "Early childhood infections (0-4 years)",]$ar/min(test[test$age_grp == "Early childhood infections (0-4 years)",]$ar)
test[test$age_grp == "School age infections (5-17 years)",]$ar <- test[test$age_grp == "School age infections (5-17 years)",]$ar/min(test[test$age_grp == "School age infections (5-17 years)",]$ar)
test[test$age_grp == "Working age infections (18-64 years)",]$ar <- test[test$age_grp == "Working age infections (18-64 years)",]$ar/min(test[test$age_grp == "Working age infections (18-64 years)",]$ar)
test[test$age_grp == "Old age infections (65-79 years)",]$ar <- test[test$age_grp == "Old age infections (65-79 years)",]$ar/min(test[test$age_grp == "Old age infections (65-79 years)",]$ar)
test[test$age_grp == "Total infections",]$ar <- test[test$age_grp == "Total infections",]$ar/min(test[test$age_grp == "Total infections",]$ar)



plt3B <- ggplot()+
  geom_line(data=test, aes(x=birth_year, y=ar, group=age_grp, col =age_grp))+
  geom_ribbon(data=test, aes(x=birth_year, y=ar, ymin=ar-1.96*sem_ar, ymax=ar+1.96*sem_ar,group=age_grp, fill=age_grp), alpha=0.5)+
  scale_x_continuous(breaks=c(-80,-60,-40,-20,0,20,40,60,80), labels = c(-80,-60,-40,-20,"0\nPandemic\nyear",20,40,60,80))+
  #geom_vline(aes(xintercept = birth_year), data=mn1, linetype='dashed', col='red', alpha=0.5)+
  #geom_vline(aes(xintercept = birth_year), data=mn3, linetype='dashed', col='blue', alpha=0.5)+
  #geom_hline(aes(yintercept = ar), data=mn1, linetype='dashed', col='red')+
  #geom_hline(aes(yintercept = ar), data=mn3, linetype='dashed', col='blue')+
  xlab("Cohort (birth year)")+
  ylab("Relative number of infections over lifetime")+
  coord_cartesian(xlim=c(-80,80))+
  theme_bw(base_size = 15)+
  geom_hline(yintercept = 1.0, linetype="dashed")+
  scale_color_brewer("Age group",palette = 'Dark2')+
  scale_fill_brewer("Age group",palette = 'Dark2')+
  theme(legend.background = element_rect(color='black'),
        legend.position = c(0.24,0.85),
        legend.title = element_blank())

plot_grid(plt3A, plt3B, nrow=1, rel_widths = c(0.5,1))
ggsave('figures/CohortGraph_Figure4.pdf', width=14, height=8)



#####################################################################################################################################
# Auto-correlation structure
#####################################################################################################################################
fishers_z<- function(r){
  log((1+r) / (1-r)) / 2
  
}

inv_fishers_z <- function(z){
  
  (exp(2*z)-1)/(exp(2*z)+1)
}
correlation_between_adjacent_alternative <- function(df, pop_size, N_iter, N_years, yr_dif){
  
  cor_df <- data.frame()
  for(i in seq_len(N_iter)){
    tmp <- df[df$sim==(i-1),]
    
    tmp$AR2 <- -1.0
    tmp$AR2[1:(nrow(tmp)-yr_dif)] <- tmp$AR[(1+yr_dif):nrow(tmp)]
    cor_df <- rbind(cor_df, tmp[tmp$AR2!=-1.0,])
  }
  
  cor_df <- cor_df[cor_df$TSI>20,]
  comp_df <- data.frame()
  for(i in seq_len(N_iter)){
    ct <- cor.test(cor_df[cor_df$sim==(i-1),]$AR, cor_df[cor_df$sim==(i-1),]$AR2)
    #sd = abs(ct$conf.int[2]-ct$conf.int[1])/(3.92)
    row_df <- data.frame(cor = ct$estimate,
                         upr=ct$conf.int[2],
                         lwr=ct$conf.int[1])
    
    comp_df <- rbind(comp_df, row_df)
  }
  
  
  return(comp_df)
  
}
cor_alternative_analysis <- function(dat, pop_size, N_iter, N_years, label){
  
  dfnew <- data.frame()
  for(i in seq_len(nrow(dat))){
    
    AR = sum(dat[i,])/pop_size
    TSI = (i-1) %% N_years +1
    sim = (i-1) %/% N_years
    
    
    row_df <- data.frame(AR = AR,
                         TSI = TSI,
                         sim = sim)
    dfnew <- rbind(dfnew, row_df)
  }
  
  
  df <- data.frame()
  
  for(i in seq_len(30)){
    
    comp_df <- correlation_between_adjacent_alternative(dfnew, pop_size, N_iter, N_years, yr_dif =i)
    comp_df$new_mn <- fishers_z(comp_df$cor)
    comp_df$new_lwr <- fishers_z(comp_df$lwr)
    comp_df$new_upr <- fishers_z(comp_df$upr)
    comp_df$new_sd <- abs(comp_df$new_lwr- comp_df$new_upr)/3.92
    
    cor <- mean(comp_df$new_mn)
    cor_sd <- sqrt(sum(comp_df$new_sd**2))/N_iter
    
    
    
    rndm <- rnorm(10000, cor, cor_sd)
    
    
    row_df <- data.frame(cor = inv_fishers_z(cor),
                         cor_lwr = inv_fishers_z(cor-1.96*cor_sd),
                         cor_upr = inv_fishers_z(cor+1.96*cor_sd),
                         p_val = length(inv_fishers_z(rndm[rndm>0]))/10000,
                         yr_dif = i,
                         label = label)
    
    df <- rbind(df, row_df)
    
    
  }
  
  return(df)
  
  
  
  
}


acs <- autocorrelation_over_time(dat1, pop_size, N_iter, N_years)

new_analytics <- function(acs, label){
  
  cor_df <- data.frame()
  for(i in seq_len(length(unique(acs$dif)))){
    comp_df <- acs[acs$dif==i & acs$year>20 & acs$year<(161-i),]
    print(i)
    
    
    comp_df$new_mn <- fishers_z(comp_df$cor)
    comp_df$new_lwr <- fishers_z(comp_df$cor_lwr)
    comp_df$new_upr <- fishers_z(comp_df$cor_upr)
    comp_df$new_sd <- abs(comp_df$new_lwr- comp_df$new_upr)/3.92
    
    cor <- mean(comp_df$new_mn)
    cor_sd <- sqrt(sum(comp_df$new_sd**2))/nrow(comp_df)
    
    
    
    rndm <- rnorm(10000, cor, cor_sd)
    
    
    row_df <- data.frame(cor = inv_fishers_z(cor),
                         cor_lwr = inv_fishers_z(cor-1.96*cor_sd),
                         cor_upr = inv_fishers_z(cor+1.96*cor_sd),
                         p_val = length(inv_fishers_z(rndm[rndm>0]))/10000,
                         yr_dif = i,
                         label = label)
    
    cor_df<-rbind(cor_df, row_df)
    
    
  }
  
  return(cor_df)
  
}

corW <- new_analytics(acs, 'test')

acs1 <- acs[acs$dif<7,]

acs1$dif <- c("1 year between epidemics",
                 "2 years between epidemics",
                 "3 years between epidemics",
                 "4 years between epidemics",
                 "5 years between epidemics",
                 "6 years between epidemics")


Cplt1<-ggplot()+
  geom_errorbar(data=acs1, aes(x=year, y=cor,ymin=cor_lwr, ymax=cor_upr), width=0.0, color='black',alpha=0.5, size=0.05)+
  geom_point(data=acs1, aes(x=year, y=cor),color='black', size=0.2)+
  xlab("Years since pandemic")+
  ylab("Correlation between annual attack rates")+
  geom_hline(yintercept = 0.0, linetype="dashed", col='red')+
  theme_bw(base_size = 13)+
  facet_wrap(.~dif, nrow=2)+
  coord_cartesian(ylim=c(-0.5,0.5), xlim=c(0,160))+
  scale_x_continuous(breaks=seq(0,160,20), minor_breaks = seq(0,160,20) )+
  theme(legend.position = c(0.7,0.25),
        legend.background = element_rect(color='black'))

Cplt2 <-ggplot()+
  geom_line(data=corW, aes(x=yr_dif, y=cor), alpha=0.6)+
  geom_errorbar(data=corW, aes(x=yr_dif, y=cor,ymin=cor_lwr, ymax=cor_upr), width=0.0)+
  geom_point(data=corW, aes(x=yr_dif, y=cor))+
  xlab("Years between epidemics")+
  ylab("Correlation between annual attack rates")+
  geom_hline(yintercept = 0.0, linetype="dashed", color='red')+
  theme_bw(base_size = 13)+
  scale_y_continuous(breaks=c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15), minor_breaks = seq(-0.2,0.2,0.01))+
  scale_x_continuous(breaks=seq(0,20,2))+
  geom_label(label="Average correlation\n20-159 years since pandemic", aes(x=12, y=0.1))



Cplt1<-Cplt1+labs(tag="A")+
  theme(plot.tag.position=c(0.005,0.98))
Cplt2<-Cplt2+labs(tag="B")+
  theme(plot.tag.position=c(0.005,0.98))

plot_grid(Cplt1, Cplt2, nrow=1, rel_widths=c(2,1))
ggsave('figures/Baseline/AutoCorSup.pdf', width=12, height=6)

