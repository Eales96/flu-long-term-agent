# Set location
setwd('/flu-long-term-agent/figure_scripts')

# Load plotting functions and packages
source('plot_functions.R')

# Specify simulation parameters (obtained from main.py)
pop_size <- 80000
N_years <- 160
N_iter <- 256

#Load in baseline scenario
datB <- read.table('input_data/AttackRates/baseline.csv', header=F, sep=',')

# Load in different waning immunity parameter values
dat0 <- read.table('input_data/AttackRates/05_waning_immunity.csv', header=F, sep=',')
dat1 <- read.table('input_data/AttackRates/1_waning_immunity.csv', header=F, sep=',')
dat2 <- read.table('input_data/AttackRates/2_waning_immunity.csv', header=F, sep=',')
dat3 <- read.table('input_data/AttackRates/3_waning_immunity.csv', header=F, sep=',')
dat4 <- read.table('input_data/AttackRates/4_waning_immunity.csv', header=F, sep=',')
dat5 <- read.table('input_data/AttackRates/5_waning_immunity.csv', header=F, sep=',')


################################################################################################################################
# Waning Immunity figure (Main)
################################################################################################################################
# Dynamics over time
AR1 <- attack_rate_over_time(dat = datB, pop_size, N_years, N_iter)
AR2 <- attack_rate_over_time(dat = dat0, pop_size, N_years, N_iter)
AR3 <- attack_rate_over_time(dat = dat5, pop_size, N_years, N_iter)
AR4 <- attack_rate_over_time(dat = dat3, pop_size, N_years, N_iter)

AR1[[2]]$tau <- "1.27 (empirical estimate)"
AR2[[2]]$tau <- "0.5"
AR3[[2]]$tau <- "5.0"
AR4[[2]]$tau <- "3.0"

AR_reg <- rbind(AR1[[2]], AR2[[2]], AR3[[2]], AR4[[2]])
AR_reg$tau <- factor(AR_reg$tau)

ARD1 <- attack_rate_over_decade(dat = datB, pop_size, N_years, N_iter)
ARD2 <- attack_rate_over_decade(dat = dat0, pop_size, N_years, N_iter)
ARD3 <- attack_rate_over_decade(dat = dat5, pop_size, N_years, N_iter)
ARD4 <- attack_rate_over_decade(dat = dat3, pop_size, N_years, N_iter)

ARD1[[2]]$tau <- "1.27 (empirical estimate)"
ARD2[[2]]$tau <- "0.5"
ARD3[[2]]$tau <- "5.0"
ARD4[[2]]$tau <- "3.0"

ARD_reg <- rbind(ARD1[[2]], ARD2[[2]], ARD3[[2]], ARD4[[2]])
ARD_reg$tau <- factor(ARD_reg$tau)

ARD_reg <- ARD_reg[ARD_reg$TSI<=160,]
AR_reg <- AR_reg[AR_reg$TSI<=160,]

#########################################################################################
AR_reg$TSI <- AR_reg$TSI-1
ARD_reg$TSI <- ARD_reg$TSI-1

pltAS_R1A <- ggplot()+
  geom_line(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_point(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_ribbon(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tau, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme(legend.position = c(0.85,0.8),
        legend.background = element_rect(color='black'))


pltAS_R1B <- ggplot()+
  geom_line(data = AR_reg[AR_reg$TSI<11,], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_point(data = AR_reg[AR_reg$TSI<11,], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=tau, color=tau))+
  geom_ribbon(data = AR_reg[AR_reg$TSI<11,], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=tau, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual\nattack rate")+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,10,1))+
  theme(legend.position = "none",
        legend.background = element_rect(color='black'),
        plot.background = element_rect(color = 'black'))


plt1B<- ggplot()+
  geom_line(data = AR_D, aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec))+
  geom_ribbon(data = AR_D, aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec), alpha=0.5)+
  #geom_jitter(data=AR1[[1]], aes(x=TSI, y=AR), alpha=0.2, width=0.2, height=0)+
  #geom_errorbar(data=ARD[[2]], aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual attack rate")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100, 120, 140,160))+
  theme_bw(base_size = 12)


pltAS_R1C <- ggplot()+
  geom_line(data = ARD_reg[ARD_reg$tau=="0.5",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="0.5",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  geom_line(data = ARD_reg[ARD_reg$tau=="3.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="3.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  geom_line(data = ARD_reg[ARD_reg$tau=="5.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="5.0",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  geom_line(data = ARD_reg[ARD_reg$tau=="1.27 (empirical estimate)",], aes(x=TSI, y=mn_AR, ymin=mn_AR-sem_AR, ymax=mn_AR+sem_AR, group=dec, color=tau))+
  geom_ribbon(data = ARD_reg[ARD_reg$tau=="1.27 (empirical estimate)",], aes(x=TSI, y=mn_AR, ymin=mn_AR-1.96*sem_AR, ymax=mn_AR+1.96*sem_AR, group=dec, fill=tau), alpha=0.5)+
  #geom_errorbar(data = AR_reg, aes(x=TSI, y=mn_AR, ymin=lwr_AR, ymax=upr_AR, group=tau), alpha=0.2, width=0.0)+
  xlab("Years since pandemic")+
  ylab("Average annual\nattack rate")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+
  coord_cartesian(ylim=c(0.1,0.27))+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme(legend.position = "none",
        legend.background = element_rect(color='black'),
        legend.title = element_blank())

pltW_A <- pltAS_R1A + annotation_custom(ggplotGrob(pltAS_R1B), xmin = 10, xmax = 110, 
                              ymin = 0.35, ymax = 0.55)


####################################################################################################################
# Correlation plot

acs <- autocorrelation_over_time(datB, pop_size, N_iter, N_years)

acs05 <- autocorrelation_over_time(dat0, pop_size, N_iter, N_years)
acs1 <- autocorrelation_over_time(dat1, pop_size, N_iter, N_years)
acs2 <- autocorrelation_over_time(dat2, pop_size, N_iter, N_years)
acs3 <- autocorrelation_over_time(dat3, pop_size, N_iter, N_years)
acs4 <- autocorrelation_over_time(dat4, pop_size, N_iter, N_years)
acs5 <- autocorrelation_over_time(dat5, pop_size, N_iter, N_years)

corWB <- new_analytics(acs, "1.27 (empirical estimate)")
corW05 <- new_analytics(acs05, "0.5")
corW1 <- new_analytics(acs1, "1")
corW2 <- new_analytics(acs2, "2")
corW3 <- new_analytics(acs3, "3")
corW4 <- new_analytics(acs4, "4")
corW5 <- new_analytics(acs5, "5")

corW_df <- rbind(corWB,corW05, corW1, corW2, corW3, corW4, corW5)

pltW_B <- ggplot()+
  geom_line(data=corW_df, aes(x=yr_dif, y=cor, color=label), position=position_dodge(width = 0.2), alpha=0.6)+
  geom_errorbar(data=corW_df, aes(x=yr_dif, y=cor,ymin=cor_lwr, ymax=cor_upr, color=label), width=0.0, position=position_dodge(width = 0.2))+
  geom_point(data=corW_df, aes(x=yr_dif, y=cor, color=label),position=position_dodge(width = 0.2))+
  xlab("Years between epidemics")+
  ylab("Correlation between\nannual attack rates")+
  geom_hline(yintercept = 0.0, linetype="dashed")+
  scale_color_brewer("Duration of short-term\nimmunity (years)", palette = "Set1")+
  scale_fill_brewer("Duration of short-term\nimmunity (years)", palette = "Set1")+
  theme_bw()+
  coord_cartesian(xlim=c(1,10))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), minor_breaks =c(1,2,3,4,5,6,7,8,9,10) )+
  theme(legend.position = c(0.7,0.23),
        legend.background = element_rect(color='black'))


pltW_A<-pltW_A+labs(tag="A")+
  theme(plot.tag.position=c(0.01,0.99),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13))
pltW_B<-pltW_B+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.99),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13))
pltW_C<-pltAS_R1C+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.99))

grd <- plot_grid(pltW_C, pltW_B, nrow=2, rel_heights = c(0.6,1))

plot_grid(pltW_A, pltW_B, ncol=2, rel_widths = c(1,0.6))
ggsave('figures/WaningImmunity_main.pdf', width=12, height=6)
