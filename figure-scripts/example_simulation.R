# Set location
setwd('/flu-long-term-agent/figure_scripts')

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

