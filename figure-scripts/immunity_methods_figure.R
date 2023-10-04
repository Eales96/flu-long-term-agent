# load packages
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(grid)
library(cowplot)
library(gridExtra)
# Set location
setwd('/flu-long-term-agent/figure_scripts')

#Read in the data and the posterior parameters from model
post <- readRDS('input_data/posterior.rds')
dat <- read.csv('input_data/antigenic_data.csv')

lambda <- mean(post$lambda)
theta <- mean(post$theta)
sigma <- mean(post$sigma)

yr_1 <- min(dat$year)
yr_n <- max(dat$year)

mod_df <- data.frame()

for(i in seq_len(yr_n - yr_1+1)){
  quan_x <- quantile(post$mu_x[,i],c(0.025,0.5, 0.975))
  quan_y <- quantile(post$mu_y[,i],c(0.025,0.5, 0.975))
  
  row_df <- data.frame(year = yr_1-1 +i,
                       mu_x = quan_x[[2]],
                       lwr_x = quan_x[[1]],
                       upr_x = quan_x[[3]],
                       mu_y = quan_y[[2]],
                       lwr_y = quan_y[[1]],
                       upr_y = quan_y[[3]])
  
  mod_df <- rbind(mod_df, row_df)
}


mod_df$year <- as.factor(mod_df$year)
dat$year <- factor(dat$year, levels= levels(mod_df$year))
factor()

cols<-RColorBrewer::brewer.pal(10, 'Paired')

cols <- c(cols, cols, cols, cols,cols,cols)

plt1<-ggplot()+
  geom_point(data = dat, aes(x= ag1, y=ag2, col=year))+
  #geom_point(data=mod_df, aes(x= mu_x, y=mu_y, col=year), shape=4)+
  geom_errorbar(data=mod_df, aes(x= mu_x, y=mu_y,ymin=lwr_y,ymax=upr_y, col=year), width=0)+
  geom_errorbarh(data=mod_df, aes(x= mu_x, y=mu_y,xmin=lwr_x,xmax=upr_x, col=year), width=0)+
  geom_line(data=mod_df, aes(x= mu_x, y=mu_y), col='grey')+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  coord_cartesian(ylim=c(-4.2, 4.2), xlim=c(0.5, 45.5))+
  scale_color_manual(values = cols, levels(mod_df$year))+
  scale_y_continuous(minor_breaks=seq(-5,5,1), breaks=seq(-4,4,2))+
  scale_x_continuous(minor_breaks=seq(-1,50,1), breaks=seq(0,50,10))+
  theme_bw()+
  theme(legend.position = "none")

for(i in seq_len(nrow(dat))){
  t_dat <- dat[i,]
  t_mod <- mod_df[mod_df$year == t_dat$year,]
  
  t_df <- rbind(data.frame(ag_x = t_dat$ag1, ag_y = t_dat$ag2, year=factor(t_dat$year, levels=levels(mod_df$year)) ),
                data.frame(ag_x = t_mod$mu_x, ag_y = t_mod$mu_y, year=factor(t_mod$year, levels=levels(mod_df$year))))
  
  plt1<-plt1+geom_line(data=t_df, aes(x=ag_x, y=ag_y, col=year), linetype='dashed', alpha=0.9)
  
}


index<-c(1,6,14,24,31,41,44)
plt1 <- plt1+geom_label_repel(aes(x=mod_df$mu_x[index],y=mod_df$mu_y[index],label = mod_df$year[index], col=mod_df$year[index]), nudge_x =-1., nudge_y = 1.5)


ggsave('figures/global_drift_model_fit.pdf', width=12, height=4)



#################################################################################################################
# Code for simulating and plotting genetic drift model
#################################################################################################################
set.seed(520)

yr = factor(seq(1968, 2011, 1))
mnx1 = c(0)
mnx2 = c(0)
mnx3 = c(0)
mnx4 = c(0)

mny1 = c(0)
mny2 = c(0)
mny3 = c(0)
mny4 = c(0)
for(i in seq_len(length(yr)-1)){
  mnx1 = c(mnx1, mnx1[i]+rexp(1, rate=lambda))
  mnx2 = c(mnx2, mnx2[i]+rexp(1, rate=lambda))
  mnx3 = c(mnx3, mnx3[i]+rexp(1, rate=lambda))
  mnx4 = c(mnx4, mnx4[i]+rexp(1, rate=lambda))
  
  mny1 = c(mny1, mny1[i]+rnorm(1,0, theta))
  mny2 = c(mny2, mny2[i]+rnorm(1,0, theta))
  mny3 = c(mny3, mny3[i]+rnorm(1,0, theta))
  mny4 = c(mny4, mny4[i]+rnorm(1,0, theta))
  
  
}

mn_df1 <- data.frame(year=yr, mu_x = mnx1, mu_y = mny1)
mn_df2 <- data.frame(year=yr, mu_x = mnx2, mu_y = mny2)
mn_df3 <- data.frame(year=yr, mu_x = mnx3, mu_y = mny3)
mn_df4 <- data.frame(year=yr, mu_x = mnx4, mu_y = mny4)

sim1 = data.frame()
sim2 = data.frame()
sim3 = data.frame()
sim4 = data.frame()


for(i in seq_len(length(yr))){
  
  for(j in seq_len(10)){
    
    row_df1 <- data.frame(year = mn_df1$year[i],
                          ag_x = mn_df1$mu_x[i] + rnorm(1,0, sigma),
                          ag_y = mn_df1$mu_y[i] + rnorm(1,0, sigma))
    row_df2 <- data.frame(year = mn_df2$year[i],
                          ag_x = mn_df2$mu_x[i] + rnorm(1,0, sigma),
                          ag_y = mn_df2$mu_y[i] + rnorm(1,0, sigma))
    row_df3 <- data.frame(year = mn_df3$year[i],
                          ag_x = mn_df3$mu_x[i] + rnorm(1,0, sigma),
                          ag_y = mn_df3$mu_y[i] + rnorm(1,0, sigma))
    row_df4 <- data.frame(year = mn_df4$year[i],
                          ag_x = mn_df4$mu_x[i] + rnorm(1,0, sigma),
                          ag_y = mn_df4$mu_y[i] + rnorm(1,0, sigma))
    
    sim1 <- rbind(sim1, row_df1)
    sim2 <- rbind(sim2, row_df2)
    sim3 <- rbind(sim3, row_df3)
    sim4 <- rbind(sim4, row_df4)
  }
}

sim1a <-sim1


plt2a<-ggplot()+
  geom_point(data = sim1, aes(x= ag_x, y=ag_y, col=year))+
  geom_point(data=mn_df1, aes(x= mu_x, y=mu_y, col=year), shape=4, size=2)+
  geom_line(data=mn_df1, aes(x= mu_x, y=mu_y), col='grey')+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  coord_cartesian(ylim=c(-12, 12), xlim=c(0, 48))+
  scale_color_manual(values = cols, levels(mn_df1$year))+
  scale_y_continuous(minor_breaks=seq(-20,100,1), breaks=seq(-10,100,10))+
  scale_x_continuous(minor_breaks=seq(-2,50,1), breaks=seq(0,50,10))+
  theme_bw()+
  theme(legend.position = "none")

plt2b<-ggplot()+
  geom_point(data = sim2, aes(x= ag_x, y=ag_y, col=year))+
  geom_point(data=mn_df2, aes(x= mu_x, y=mu_y, col=year), shape=4, size=2)+
  geom_line(data=mn_df2, aes(x= mu_x, y=mu_y), col='grey')+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  coord_cartesian(ylim=c(-12, 12), xlim=c(0, 48))+
  scale_color_manual(values = cols, levels(mn_df2$year))+
  scale_y_continuous(minor_breaks=seq(-20,100,1), breaks=seq(-10,100,10))+
  scale_x_continuous(minor_breaks=seq(-2,50,1), breaks=seq(0,50,10))+
  theme_bw()+
  theme(legend.position = "none")

plt2c<-ggplot()+
  geom_point(data = sim3, aes(x= ag_x, y=ag_y, col=year))+
  geom_point(data=mn_df3, aes(x= mu_x, y=mu_y, col=year), shape=4, size=2)+
  geom_line(data=mn_df3, aes(x= mu_x, y=mu_y), col='grey')+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  coord_cartesian(ylim=c(-12, 12), xlim=c(0, 48))+
  scale_color_manual(values = cols, levels(mn_df3$year))+
  scale_y_continuous(minor_breaks=seq(-20,100,1), breaks=seq(-10,100,10))+
  scale_x_continuous(minor_breaks=seq(-2,50,1), breaks=seq(0,50,10))+
  theme_bw()+
  theme(legend.position = "none")

plt2d<-ggplot()+
  geom_point(data = sim4, aes(x= ag_x, y=ag_y, col=year))+
  geom_point(data=mn_df4, aes(x= mu_x, y=mu_y, col=year), shape=4, size=2)+
  geom_line(data=mn_df4, aes(x= mu_x, y=mu_y), col='grey')+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  coord_cartesian(ylim=c(-12, 12), xlim=c(0, 48))+
  scale_color_manual(values = cols, levels(mn_df4$year))+
  scale_y_continuous(minor_breaks=seq(-20,100,1), breaks=seq(-10,100,10))+
  scale_x_continuous(minor_breaks=seq(-2,50,1), breaks=seq(0,50,10))+
  theme_bw()+
  theme(legend.position = "none")


for(i in seq_len(nrow(sim1))){
  print(i)
  t_sim1 <- sim1[i,]
  t_sim2 <- sim2[i,]
  t_sim3 <- sim3[i,]
  t_sim4 <- sim4[i,]
  
  t_mn1 <- mn_df1[mn_df1$year == t_sim1$year,]
  t_mn2 <- mn_df2[mn_df2$year == t_sim2$year,]
  t_mn3 <- mn_df3[mn_df3$year == t_sim3$year,]
  t_mn4 <- mn_df4[mn_df4$year == t_sim4$year,]
  
  t_df1 <- rbind(data.frame(ag_x = t_sim1$ag_x, ag_y = t_sim1$ag_y, year=t_sim1$year),
                 data.frame(ag_x = t_mn1$mu_x, ag_y = t_mn1$mu_y, year=t_mn1$year))
  t_df2 <- rbind(data.frame(ag_x = t_sim2$ag_x, ag_y = t_sim2$ag_y, year=t_sim2$year),
                 data.frame(ag_x = t_mn2$mu_x, ag_y = t_mn2$mu_y, year=t_mn2$year))
  t_df3 <- rbind(data.frame(ag_x = t_sim3$ag_x, ag_y = t_sim3$ag_y, year=t_sim3$year),
                 data.frame(ag_x = t_mn3$mu_x, ag_y = t_mn3$mu_y, year=t_mn3$year))
  t_df4 <- rbind(data.frame(ag_x = t_sim4$ag_x, ag_y = t_sim4$ag_y, year=t_sim4$year),
                 data.frame(ag_x = t_mn4$mu_x, ag_y = t_mn4$mu_y, year=t_mn4$year))
  
  plt2a<-plt2a+geom_line(data=t_df1, aes(x=ag_x, y=ag_y, col=year), linetype='dashed', alpha=0.9)
  plt2b<-plt2b+geom_line(data=t_df2, aes(x=ag_x, y=ag_y, col=year), linetype='dashed', alpha=0.9)
  plt2c<-plt2c+geom_line(data=t_df3, aes(x=ag_x, y=ag_y, col=year), linetype='dashed', alpha=0.9)
  plt2d<-plt2d+geom_line(data=t_df4, aes(x=ag_x, y=ag_y, col=year), linetype='dashed', alpha=0.9)
  
}

library(cowplot)
plt_grd <- plot_grid(plt2a, plt2b, plt2c, plt2d, nrow=2)

ggsave("figures/global_drift_sim10.pdf", height = 8, width =14)


#################################################################################################################
# Plot antibody titre/immunity model
##################################################################################################################
immunity <- function(x,centre, mu, sig){
  
  
  ls <-  1 - sig * abs(x-centre)
  
  ls[ls<0] <- 0
  
  mu * ls
  
}

waning <- function(t, omega){
  max(c(0, 1 - omega * t))
}


alpha = 2.844
beta = 1.299

seniority = 0.039


titre_immunity <- function(Tit, alpha, beta){
  Tit = 10 * (2**(Tit-1))
  1 - (1 / (1 + exp(beta * (log(Tit) - alpha) )))
  
  #1 - 1 / (1 + np.exp(beta * (np.log(hiat) - alpha)))
}

x <- seq(-5, 5, 0.1)
st_y <- immunity(x,5 ,2.02, 0.130)
lt_y <- immunity(x,5 ,2.69, 0.031)


x <- seq(0, 40, 0.1)

st_y1 <- immunity(x,10 ,2.02, 0.130)
lt_y1 <- immunity(x,10 ,2.69, 0.031)

st_y2 <- immunity(x,30 ,2.02, 0.130)
lt_y2 <- immunity(x,30 ,2.69, 0.031)

st_y3 <- immunity(x,50 ,2.02, 0.130)
lt_y3 <- immunity(x,50 ,2.69, 0.031)

st_y4 <- immunity(x,70 ,2.02, 0.130)
lt_y4 <- immunity(x,70 ,2.69, 0.031)

df1 <- data.frame(x=x, infection = 'first',
                  sty = st_y1, lty = lt_y1)
df2 <- data.frame(x=x, infection = 'second',
                  sty = st_y2, lty = lt_y2)
df3 <- data.frame(x=x, infection = 'third',
                  sty = st_y3, lty = lt_y3)
df4 <- data.frame(x=x, infection = 'fourth',
                  sty = st_y4, lty = lt_y4)



x <- seq(-50, 50, 0.1)
lt_y1 <- immunity(x, 0 ,2.02, 0.130)
st_y1 <- immunity(x, 0 ,2.69, 0.031)

lt_y2 <- immunity(x, 90 ,2.02, 0.130)
st_y2 <- immunity(x, 90 ,2.69, 0.031)

df1a <- data.frame(x=x, grp = factor('Short-term'), tsi = "0 years since infection",
                   sty = st_y1*waning(0, 0.79))
df1b <- data.frame(x=x, grp = factor('Long-term'), tsi = "0 years since infection",
                   sty = lt_y1)

df2a <- data.frame(x=x, grp = factor('Short-term'), tsi = "0.5 years since infection ",
                   sty = st_y1*waning(0.5, 0.79))
df2b <- data.frame(x=x, grp = factor('Long-term'), tsi = "0.5 years since infection ",
                   sty = lt_y1)

df3a <- data.frame(x=x, grp = factor('Short-term'), tsi = "1 years since infection ",
                   sty = st_y1*waning(1., 0.79))
df3b <- data.frame(x=x, grp = factor('Long-term'), tsi = "1 years since infection ",
                   sty = lt_y1)

df4a <- data.frame(x=x, grp = factor('Short-term'), tsi = "1.27 years since infection ",
                   sty = st_y1*waning(1.5, 0.79))
df4b <- data.frame(x=x, grp = factor('Long-term'), tsi = "1.27 years since infection ",
                   sty = lt_y1)


df <- rbind(df1b, df1a,
            df2b, df2a,
            df3b, df3a,
            df4b, df4a)
df$grp <- factor(df$grp, levels=c('Short-term', "Long-term"))

dfTa <- rbind(df1a, df2a, df3a, df4a)
dfTb <- rbind(df1b, df2b, df3b, df4b)
dfTa$lty <- dfTa$sty + dfTb$sty

plt4<-ggplot(data=df)+
  geom_area(data = df, aes(x=x, y= sty, fill = grp))+
  ylab("Log-titre")+
  xlab("Antigenic distance")+
  theme_bw()+
  facet_wrap(.~tsi, nrow=1)+
  geom_hline(yintercept = 2.02, linetype='dashed', color='black')+
  geom_hline(yintercept = 2.02+2.69, linetype='dashed', color='black')+
  scale_fill_brewer("Immunity",palette = 'Set1')+
  theme(legend.position = c(0.9,0.7),
        legend.background = element_rect(color='black'),
        panel.grid = element_blank())

plt4_alt<-ggplot(data=df)+
  geom_area(data = df, aes(x=x, y= sty, fill = grp),position = "identity",alpha=0.6)+
  geom_line(data = dfTa, aes(x=x, y= lty, color='black'), alpha=1.0)+
  ylab("Log-titre component")+
  xlab("Antigenic distance")+
  theme_bw()+
  facet_wrap(.~tsi, nrow=1)+
  geom_hline(yintercept = 2.02, linetype='dashed', color='black')+
  geom_hline(yintercept = 2.69, linetype='dashed', color='black')+
  geom_hline(yintercept = 2.02+2.69, linetype='dashed', color='black')+
  scale_fill_brewer("Immunity",palette = 'Set1')+
  scale_color_manual("Total log-titre", values = c("black"), label="")+
  theme(legend.position = c(0.9,0.7),
        legend.background = element_rect(color='black'),
        panel.grid = element_blank())


x <- seq(0, 50, 0.1)
lt_y1 <- immunity(x, 10 ,2.02, 0.130)
lt_y2 <- immunity(x, 15 ,2.02, 0.130)
lt_y3 <- immunity(x, 20 ,2.02, 0.130)
lt_y4 <- immunity(x, 25 ,2.02, 0.130)
lt_y5 <- immunity(x, 30 ,2.02, 0.130)
lt_y6 <- immunity(x, 35 ,2.02, 0.130)

df1 <- data.frame(x=x, grp = factor('1'),
                  lty = lt_y1*waning(1-1, 0.039))
df2 <- data.frame(x=x, grp = factor('2'),
                  lty = lt_y2*waning(2-1, 0.039))
df3 <- data.frame(x=x, grp = factor('3'),
                  lty = lt_y3*waning(3-1, 0.039))
df4 <- data.frame(x=x, grp = factor('4'),
                  lty = lt_y4*waning(4-1, 0.039))
df5 <- data.frame(x=x, grp = factor('5'),
                  lty = lt_y5*waning(5-1, 0.039))
df6 <- data.frame(x=x, grp = factor('6'),
                  lty = lt_y6*waning(6-1, 0.039))


df <- rbind(df1, df2, df3, df4, df5, df6)

dfT <- df1
dfT$lty <- df1$lty +df2$lty+df3$lty+df4$lty+df5$lty+df6$lty


plt5<-ggplot(data=df)+
  geom_area(data = df, aes(x=x, y= lty, fill = grp), position = "identity", alpha=0.6)+
  ylab("Log-titre component")+
  xlab("Antigenic distance")+
  theme_bw()+
  geom_hline(yintercept = 2.02, linetype='dashed', color='black')+
  scale_fill_brewer("Infection",palette = 'Dark2')+
  theme(legend.position = c(0.9,0.5),
        legend.background = element_rect(color='black'),
        panel.grid = element_blank())


plt5_alt<-ggplot(data=df)+
  geom_line(data = dfT, aes(x=x, y= lty, color='black'), alpha=1.0)+
  geom_area(data = df, aes(x=x, y= lty, fill = grp), position = "identity", alpha=0.6)+
  ylab("Log-titre component")+
  xlab("Antigenic distance")+
  theme_bw()+
  geom_hline(yintercept = 2.02, linetype='dashed', color='black')+
  scale_color_manual("Total log-titre", values = c("black"), label="")+
  scale_fill_brewer("Infection",palette = 'Dark2')+
  theme(legend.position = c(0.9,0.5),
        legend.background = element_rect(color='black'),
        panel.grid = element_blank())


x = seq(-1, 8, 0.1)
y = titre_immunity(x, alpha, beta)

df_im <- data.frame(x=x, y=y)
plt6<-ggplot(data=df_im, aes(x=x, y=y))+
  geom_line()+
  theme_bw()+
  xlab("Log-titre")+
  ylab("Protection")+
  coord_cartesian(xlim=c(0.25,8), ylim=c(0,1))

im_a <- plt4_alt
im_b <- plt5_alt
im_c <- plt6


im_a <- im_a+labs(tag="A")+
  theme(plot.tag.position=c(0.02,0.95),
        legend.position = c(0.9,0.75))+
  guides(col="none")
im_b <- im_b+labs(tag="B")+
  theme(plot.tag.position=c(0.01,0.95),
        legend.position = c(0.9,0.6))+
  guides(col="none")+
  ylab("Antigenic dimension 1")
im_c <- im_c+labs(tag="C")+
  theme(plot.tag.position=c(0.01,0.95))

im_grd<-plot_grid(im_b, im_c, nrow=1, rel_widths = c(2,1))
im_abc <- plot_grid(im_a, im_grd, nrow=2)
ggsave('figures/immunity_model.pdf', width=8.3, height=6)

#################################################################################################################
# 2d immunity
##################################################################################################################

sim1 <-sim1a
sim1$ag_y <- sim1$ag_y-55
infections <- c(1,47, 116, 162,208, 277, 346, 369, 415)

sim1$strain <- seq(1,20,1)
sim1$ag1 <- sim1$ag_x
sim1$ag2 <- sim1$ag_y


## Colours to show different years
col0 <- rev(colorRampPalette(brewer.pal(9, 'Blues')[3:9])(N_strains))
col1 <- rev(colorRampPalette(brewer.pal(9, 'Oranges')[3:9])(N_strains))
col2 <- rev(colorRampPalette(brewer.pal(9, 'Purples')[3:9])(N_strains))
col3 <- rev(colorRampPalette(brewer.pal(9, 'Greens')[3:9])(N_strains))
col4 <- rev(colorRampPalette(brewer.pal(9, 'Reds')[3:9])(N_strains))
col5 <- rev(colorRampPalette(c('paleturquoise1','turquoise1','turquoise4'))(N_strains))
col6 <- rev(colorRampPalette(c('khaki1','gold1','darkgoldenrod4'))(N_strains))
col7 <- rev(colorRampPalette(c('pink1','deeppink1','deeppink4'))(N_strains))

hcol0 <- col0[N_strains/2]
hcol1 <- col1[N_strains/2]
hcol2 <- col2[N_strains/2]
hcol3 <- col3[N_strains/2]
hcol4 <- col4[N_strains/2]
hcol5 <- col5[N_strains/2]
hcol6 <- col6[N_strains/2]
hcol7 <- col7[N_strains/2]

hcol <- c(hcol0, hcol1, hcol2, hcol3, hcol4 , hcol5, hcol6, hcol7)
overall_cols <- c(hcol, hcol, hcol, hcol, hcol, hcol, hcol, hcol, hcol)[1:44]


df_res <- sim1
# Some infections set manually to plot 2d immune landscape
infections <- c(1,47, 116, 162,208, 277, 346, 369, 415)
df_inf <- df_res[infections,]
im2d_A <- ggplot()+
  geom_point(data = df_res, aes(x= ag1, y=ag2, col=factor(year)), size=2, alpha=0.2)+
  geom_point(data = df_inf, aes(x= ag1, y=ag2, col=factor(year)),shape=4, size=10)+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  scale_color_manual(values=overall_cols)+
  scale_y_continuous(minor_breaks=seq(-10,10,1), breaks=seq(-10,10,5))+
  scale_x_continuous(minor_breaks=seq(-10,50,1), breaks=seq(0,50,5), position="top")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_cartesian(xlim=c(-2, 45), ylim=c(-10, 5.5))+
  geom_label(
    label = "Strains in a 2D antigenic strain space",
    mapping = aes(x = -Inf, y = 4.5, label = label),
    hjust   = -0.02,
    vjust   = 0,
    fill = 'white')


####################################################################################################

x=seq(-10,50,0.1)
y=seq(-12,7,0.1)
dfz<-expand.grid(x,y)
dfz$lt <- 0
dfz$st <- 0

for(i in seq_len(nrow(dfz))){
  for(j in seq_len(length(infections))){
    dist = sqrt((df_inf$ag1[j]-dfz$Var1[i])**2 + (df_inf$ag2[j]-dfz$Var2[i])**2)
    
    dfz$lt[i] <- dfz$lt[i]+ immunity(dist, 0, 2.02, 0.130) * waning(j-1, 0.039)
    
    if(j==length(infections)){
      dfz$st[i] <- dfz$st[i]+ immunity(dist, 0, 2.69, 0.031) * waning(j-1, 0.039)
      
    }
  }
}

alpha = 2.844
beta = 1.299


dfz1 <- dfz
dfz2 <- dfz
dfz2$lt <- dfz2$lt + dfz2$st

dfz1$tsi <- "old infections"
dfz2$tsi <- "just infected"

dfz <- rbind(dfz1, dfz2)

dfz$pro <- titre_immunity(dfz$lt,alpha, beta)


df_fake <- data.frame(name = c("Past infections", "Most recent infection"),
                      x= c(1,2),
                      y= c(1,2))

plt_fake<-ggplot(df_fake, aes(x=x, y=y, col=name, shape=name),size=4)+
  geom_point()+
  scale_color_manual(values=c('red','black'))+
  scale_shape_manual(values=c(4,4))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.background = element_rect(color='black'))



im2D_B<-ggplot()+
  #stat_contour_filled(data=dfz, aes(x=Var1, y=Var2, z=pro), binwidth = 0.05)+
  geom_tile(data=dfz, aes(x=Var1, y=Var2, fill=pro))+
  geom_point(data = df_inf, aes(x= ag1, y=ag2),shape=4, size=10, alpha=1)+
  geom_point(data = df_inf[7,], aes(x= ag1, y=ag2),shape=4, size=10, alpha=1, col='red')+
  coord_cartesian(xlim=c(-2, 45), ylim=c(-10, 5.5))+
  theme_bw()+
  facet_wrap(.~tsi, nrow=2)+
  theme(panel.grid = element_blank(),
        strip.text.x = element_blank(),
        legend.background = element_rect(color='black'),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  scale_y_continuous(minor_breaks=seq(-10,10,1), breaks=seq(-10,10,5))+
  scale_x_continuous(minor_breaks=seq(-10,50,1), breaks=seq(0,50,5), position="bottom")+
  labs(fill="Protection\nagainst\ninfection")+
  scale_fill_viridis_c(limits=c(0.15,1))


leg <- get_legend(im2D_B)

leg2 <- get_legend(plt_fake)

dat_text <- data.frame(
  label = c("Protection immediately following recent infection (red)", "Long-term protection"),
  tsi   = c("just infected","old infections")
)

im2D_B<-ggplot()+
  stat_contour_filled(data=dfz, aes(x=Var1, y=Var2, z=pro), binwidth = 0.05)+
  geom_point(data = df_inf, aes(x= ag1, y=ag2),shape=4, size=10, alpha=1)+
  geom_point(data = df_inf[9,], aes(x= ag1, y=ag2),shape=4, size=10, alpha=1, col='red')+
  coord_cartesian(xlim=c(-2, 45), ylim=c(-10, 5.5))+
  theme_bw()+
  facet_wrap(.~tsi, nrow=2)+
  theme(panel.grid = element_blank(),
        strip.text.x = element_blank(),
        legend.background = element_rect(color='black'),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  scale_y_continuous(minor_breaks=seq(-10,10,1), breaks=seq(-10,10,5))+
  scale_x_continuous(minor_breaks=seq(-10,50,1), breaks=seq(0,50,5), position="bottom")+
  labs(fill="Protection\nagainst\ninfection")+
  geom_label(
    data    = dat_text,
    mapping = aes(x = -Inf, y = 4.5, label = label),
    hjust   = -0.02,
    vjust   = -0.,
    fill = 'white')


grid<-plot_grid(im2d_A, im2D_B, nrow=2, rel_heights=c(1,2))

y.grob <- textGrob("Antigenic dimension 2", 
                   gp=gpar(fontsize=11), rot=90)

x.grob <- textGrob("Antigenic dimension 1", 
                   gp=gpar(fontsize=11))

lab1 <- textGrob("Example 2D strain space",
                 gp=gpar(fontsize=11))

lab1 <- grobTree( rectGrob(gp=gpar(fill="white", col='black')), textGrob("Testing title background", gp=gpar(fontsize=11)))


# add legends to plot
sim_grd<-arrangeGrob(grid, left = y.grob, bottom = x.grob)
sim_grd<-ggdraw()+draw_grob(sim_grd)+
  draw_grob(leg, x=0.42, y=-0.3)+
  draw_grob(leg2,x=0.39, y=-0.07)


ggsave('figures/ExampleImmunity.pdf',sim_grd, width=8.3, height=8, units = 'in')

