# Example simulations
# Set location
setwd('C:/Users/EALESO/PycharmProjects/pythonProject/figure_scripts')

# Load plotting functions
source('plot_functions.R')


# Specify simulation parameters (obtained from main.py)
pop_size <- 80000 #160000
N_years <- 20 #160
N_iter <- 1 #128
N_strains <- 20

#read data

df0 <- read.table('input_data/StrainCurves/inc_year_0.csv', header=F, sep=',')
df1 <- read.table('input_data/StrainCurves/inc_year_1.csv', header=F, sep=',')
df2 <- read.table('input_data/StrainCurves/inc_year_2.csv', header=F, sep=',')
df3 <- read.table('input_data/StrainCurves/inc_year_3.csv', header=F, sep=',')
df4 <- read.table('input_data/StrainCurves/inc_year_4.csv', header=F, sep=',')
df5 <- read.table('input_data/StrainCurves/inc_year_5.csv', header=F, sep=',')
df6 <- read.table('input_data/StrainCurves/inc_year_6.csv', header=F, sep=',')
df7 <- read.table('input_data/StrainCurves/inc_year_7.csv', header=F, sep=',')
df8 <- read.table('input_data/StrainCurves/inc_year_8.csv', header=F, sep=',')
df9 <- read.table('input_data/StrainCurves/inc_year_9.csv', header=F, sep=',')
df10 <- read.table('input_data/StrainCurves/inc_year_10.csv', header=F, sep=',')
df11 <- read.table('input_data/StrainCurves/inc_year_11.csv', header=F, sep=',')
df12 <- read.table('input_data/StrainCurves/inc_year_12.csv', header=F, sep=',')
df13 <- read.table('input_data/StrainCurves/inc_year_13.csv', header=F, sep=',')
df14 <- read.table('input_data/StrainCurves/inc_year_14.csv', header=F, sep=',')
df15 <- read.table('input_data/StrainCurves/inc_year_15.csv', header=F, sep=',')
df16 <- read.table('input_data/StrainCurves/inc_year_16.csv', header=F, sep=',')
df17<- read.table('input_data/StrainCurves/inc_year_17.csv', header=F, sep=',')
df18<- read.table('input_data/StrainCurves/inc_year_18.csv', header=F, sep=',')
df19<- read.table('input_data/StrainCurves/inc_year_19.csv', header=F, sep=',')

res <- read.table('input_data/StrainCurves/res_year_0.csv', header=F, sep=',')
res_mean <- read.table('input_data/StrainCurves/res_mean_0.csv', header=F, sep=',')


#############################################################################################

df <- df0
yr <- "0"

get_inc_curve <- function(df, yr){
  
  df_inc <- data.frame()
  for(i in seq_len(nrow(df))){
    row_df <- data.frame(year=yr,
                         day= seq(1,365,1),
                         inc = as.numeric(df[i,]),
                         strain = factor(i))
    df_inc <- rbind(df_inc, row_df)
  }
  
  return(df_inc)
  
}

cur0 <- get_inc_curve(df0, 0)
cur1 <- get_inc_curve(df1, 1)
cur2 <- get_inc_curve(df2, 2)
cur3 <- get_inc_curve(df3, 3)
cur4 <- get_inc_curve(df4, 4)
cur5 <- get_inc_curve(df5, 5)
cur6 <- get_inc_curve(df6, 6)
cur7 <- get_inc_curve(df7, 7)
cur8 <- get_inc_curve(df8, 8)
cur9 <- get_inc_curve(df9, 9)
cur10<- get_inc_curve(df10, 10)
cur11<- get_inc_curve(df11, 11)
cur12<- get_inc_curve(df12, 12)
cur13<- get_inc_curve(df13, 13)
cur14<- get_inc_curve(df14, 14)
cur15<- get_inc_curve(df15, 15)
cur16<- get_inc_curve(df16, 16)
cur17<- get_inc_curve(df17, 17)
cur18<- get_inc_curve(df18, 18)
cur19<- get_inc_curve(df19, 19)


df <- rbind(cur0, cur1, cur2, cur3, cur4, cur5, cur6, cur7, cur8, cur9,
            cur10, cur11, cur12, cur13, cur14, cur15, cur16, cur17, cur18, cur19)

df


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



col_all <- c(col0, col1, col2, col3, col4, col5, col6, col7)
hcol <- c(hcol0, hcol1, hcol2, hcol3, hcol4 , hcol5, hcol6, hcol7)


plt2<-ggplot()+
  geom_area(data=df, aes(x=(day+year*365)/365, y=inc/10000, fill=interaction( strain, year)), position="stack")+
  theme_bw()+
  xlab("Years since pandemic")+
  ylab("Daily infection incidence")+
  scale_fill_manual(values = c(col_all,col_all,col_all,col_all))+
  scale_x_continuous(breaks=seq(0,20,5),minor_breaks=c())+
  geom_vline(xintercept = seq(0,19,1), linetype="dashed",col=c(hcol, hcol, hcol, hcol)[1:20])+
  theme(legend.position = "none")+
  coord_cartesian(xlim=c(0.75,19.25))
##############################################################################################
# Global resovior
df_res <- data.frame()
df_mn <- data.frame()
for(i in seq_len(N_years)){
  row_df <- data.frame(year=rep(i, N_strains),
                       strain = seq(1,N_strains,1),
                       ag2 = res[,(i*2)],
                       ag1 = res[,(i*2-1)])
  
  df_res <- rbind(df_res, row_df)
  
  row_df <- data.frame(year=i,
                       ag2 = res_mean[2,i],
                       ag1 = res_mean[1,i])
  df_mn <- rbind(df_mn, row_df)
}

df_res$inc <- c(rowSums(df0), rowSums(df1), rowSums(df2), rowSums(df3), rowSums(df4), rowSums(df5),rowSums(df6), rowSums(df7),rowSums(df8),rowSums(df9),
                rowSums(df10), rowSums(df11), rowSums(df12), rowSums(df13), rowSums(df14), rowSums(df15),rowSums(df16), rowSums(df17),rowSums(df18),rowSums(df19))




plt1 <- ggplot()+
  geom_point(data = df_res, aes(x= ag1, y=ag2, col=interaction(strain, year)))+
  geom_line(data=df_mn, aes(x= ag1, y=ag2), col='grey')+
  geom_point(data=df_mn, aes(x= ag1, y=ag2), shape=4, col=c(hcol, hcol, hcol, hcol)[1:20], size=2)+
  xlab("Antigenic dimension 1")+
  ylab("Antigenic dimension 2")+
  scale_color_manual(values=c(col_all, col_all, col_all, col_all))+
  scale_y_continuous(minor_breaks=seq(-10,10,1), breaks=seq(-10,10,5))+
  scale_x_continuous(minor_breaks=seq(-10,50,1), breaks=seq(0,50,5), position="top")+
  #geom_vline(xintercept = df_mn$ag1, linetype='dashed', col=c(hcol, hcol, hcol, hcol)[1:20])+
  theme_bw()+
  theme(legend.position = "none")

plt1
xmin <- -3
xmax <- 25
df_lines1 <- df_mn
df_lines2 <- df_mn
df_lines3 <- df_mn
df_lines2$ag2 <- -7
df_lines3$ag2 <- -8

df_lines <- rbind(df_lines1, df_lines2)

df_lines3$ag1 <- xmin+(xmax-xmin)*seq(0,19,1)/20
df_con <- rbind(df_lines2, df_lines3)

hcols2 <- c(hcol0,hcol0, hcol1,hcol1,hcol2, hcol2,hcol3, hcol3,hcol4, hcol4 ,hcol5, hcol5,hcol6, hcol6,hcol7, hcol7)
mn_cols<-c(hcols2, hcols2, hcols2, hcols2)[1:40]

plt1<-plt1+
  geom_line(data=df_lines, aes(x= ag1, y=ag2, group=year), linetype='dashed', col=mn_cols)+
  geom_line(data=df_con, aes(x= ag1, y=ag2, group=year), linetype='dashed', col=mn_cols)+
  coord_cartesian(xlim=c(xmin+1, xmax-1), ylim=c(-7.2, 4))
  


for(i in seq_len(nrow(df_res))){
  print(i)
  t_sim1 <- df_res[i,]
  
  
  t_mn1 <- df_mn[df_mn$year == t_sim1$year,]
  
  t_df1 <- rbind(data.frame(ag_x = t_sim1$ag1, ag_y = t_sim1$ag2, year=t_sim1$year, strain=t_sim1$strain),
                 data.frame(ag_x = t_mn1$ag1, ag_y = t_mn1$ag2, year=t_mn1$year, strain=t_sim1$strain))
  
  
  plt1<-plt1+geom_line(data=t_df1, aes(x=ag_x, y=ag_y),col=c(hcol, hcol, hcol, hcol)[t_sim1$year], linetype='dotted', alpha=0.5)
  #plt1<-plt1+geom_line(data=t_df1, aes(x=ag_x, y=ag_y, col=interaction(strain, year)),col=c(hcol, hcol, hcol, hcol)[t_sim1$year], linetype='dashed', alpha=0.9)
  
  
}
plt1 <- plt1+
  geom_line(data=df_mn, aes(x= ag1, y=ag2), col='grey')+
  geom_point(data = df_res, aes(x= ag1, y=ag2, col=interaction(strain, year)))+
  geom_point(data=df_mn, aes(x= ag1, y=ag2), shape=4, col=c(hcol, hcol, hcol, hcol)[1:20], size=2)


plt_grid<-plot_grid(plt1, plt2, nrow=2, rel_heights = c(1,0.8))
  
ggsave('figures/ExampleSimulation/Figure2.pdf', width=9, height=7)



##############################################################################################################
im_a <- readRDS('FigureComponents/immunity_a.rds')
im_b <- readRDS('FigureComponents/immunity_b.rds')
im_c <- readRDS('FigureComponents/immunity_c.rds')


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

ggsave('figures/ExampleSimulation/immunity_model.pdf', width=8.3, height=6)

####################################################################################################
# 2D immunity panels

overall_cols <- c(hcol, hcol, hcol, hcol)[1:20]


df_res[df_res$year==20,]
infections = c(54,100,164,246,306, 395)

infections = c(10, 10+3*20, 10+5*20, 10+8*20, 10+13*20, 10+15*20, 10+19*20)

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
  coord_cartesian(xlim=c(xmin+1, xmax+2), ylim=c(-5.5, 5.5))+
  geom_label(
    label = "Strains in a 2D antigenic strain space",
    mapping = aes(x = -Inf, y = 4.5, label = label),
    hjust   = -0.02,
    vjust   = 0,
    fill = 'white')


####################################################################################################






x=seq(-10,29,0.1)
y=seq(-6,6,0.1)
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


im2D_B<-ggplot()+
  #stat_contour_filled(data=dfz, aes(x=Var1, y=Var2, z=pro), binwidth = 0.05)+
  geom_tile(data=dfz, aes(x=Var1, y=Var2, fill=pro))+
  geom_point(data = df_inf, aes(x= ag1, y=ag2),shape=4, size=10, alpha=1)+
  geom_point(data = df_inf[7,], aes(x= ag1, y=ag2),shape=4, size=10, alpha=1, col='red')+
  coord_cartesian(xlim=c(xmin+1, xmax+2), ylim=c(-5.5, 5.5))+
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

dat_text <- data.frame(
  label = c("Protection immediately following recent infection (red)", "Long-term protection"),
  tsi   = c("just infected","old infections")
)

im2D_B<-ggplot()+
  stat_contour_filled(data=dfz, aes(x=Var1, y=Var2, z=pro), binwidth = 0.05)+
  geom_point(data = df_inf, aes(x= ag1, y=ag2),shape=4, size=10, alpha=1)+
  geom_point(data = df_inf[7,], aes(x= ag1, y=ag2),shape=4, size=10, alpha=1, col='red')+
  coord_cartesian(xlim=c(xmin+1, xmax+2), ylim=c(-5.5, 5.5))+
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
grid <- grid+labs(tag = "D")+
  theme(plot.tag.position=c(0.0,0.98),
        plot.tag = element_text(face = 'plain'))

y.grob <- textGrob("Antigenic dimension 2", 
                   gp=gpar(fontsize=11), rot=90)

x.grob <- textGrob("Antigenic dimension 1", 
                   gp=gpar(fontsize=11))

lab1 <- textGrob("Example 2D strain space",
                 gp=gpar(fontsize=11))

lab1 <- grobTree( rectGrob(gp=gpar(fill="white", col='black')), textGrob("Testing title background", gp=gpar(fontsize=11)))


#add to plot
library(grid)
library(gridExtra)
sim_grd<-arrangeGrob(grid, left = y.grob, bottom = x.grob)
sim_grd<-ggdraw()+draw_grob(sim_grd)+
  draw_grob(leg, x=0.4, y=-0.3)
  

ggsave('figures/ExampleSimulation/ExampleImmunity.pdf',sim_grd, width=12.3, height=6, units = 'in')




final1<-plot_grid(im_abc,sim_grd, ncol=2, rel_widths = c(1,0.8))
ggsave('figures/ExampleSimulation/MethodsFigure1.pdf',final1, width=12, height=7.5,  units = 'in')

final <- plot_grid(plt_grid, final1, ncol=2, rel_widths = c(1,0.5))

ggsave('figures/ExampleSimulation/SimulationFig2.pdf',plt_grid, width=12.3, height=6)


################################################################################################################################################################################
# Version 2
sim1 <- readRDS('FigureComponents/sim1.rds')

####################################################################################################
# 2D immunity panels
sim1$strain <- seq(1,20,1)
sim1$ag1 <- sim1$ag_x
sim1$ag2 <- sim1$ag_y


overall_cols <- c(hcol, hcol, hcol, hcol, hcol, hcol, hcol, hcol, hcol)[1:44]



df_res <- sim1
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
#grid <- grid+labs(tag = "D")+
#  theme(plot.tag.position=c(0.0,0.98),
#        plot.tag = element_text(face = 'plain'))

y.grob <- textGrob("Antigenic dimension 2", 
                   gp=gpar(fontsize=11), rot=90)

x.grob <- textGrob("Antigenic dimension 1", 
                   gp=gpar(fontsize=11))

lab1 <- textGrob("Example 2D strain space",
                 gp=gpar(fontsize=11))

lab1 <- grobTree( rectGrob(gp=gpar(fill="white", col='black')), textGrob("Testing title background", gp=gpar(fontsize=11)))


#add to plot
library(grid)
library(gridExtra)
sim_grd<-arrangeGrob(grid, left = y.grob, bottom = x.grob)
sim_grd<-ggdraw()+draw_grob(sim_grd)+
  draw_grob(leg, x=0.42, y=-0.3)+
  draw_grob(leg2,x=0.39, y=-0.07)


ggsave('figures/ExampleSimulation/ExampleImmunity.pdf',sim_grd, width=8.3, height=8, units = 'in')




final1<-plot_grid(im_abc,sim_grd, ncol=2, rel_widths = c(1,0.8))
ggsave('figures/ExampleSimulation/MethodsFigure1.pdf',final1, width=12, height=7.5,  units = 'in')

final <- plot_grid(plt_grid, final1, ncol=2, rel_widths = c(1,0.5))

ggsave('figures/ExampleSimulation/SimulationFig2.pdf',plt_grid, width=12.3, height=6)


