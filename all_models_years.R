library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ANTÁRTICA FEVEREIRO
#-----------------------------------------------------------------------------------------------------------------
i1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/gdfl_antar.txt')
i2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/mpi_antar.txt')
i3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/ncar_antar.txt')
i4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/besm_antar.txt')
#-------------------------------------
i5 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/gdfl_antar.txt')
i6 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/mpi_antar.txt')
i7 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/ncar_antar.txt')
i8 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/satelite/cmip6/sate_all_years_antar.txt')
i9 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/sate_V4/historical/antar_years.txt')
#---------------
j <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/ACCESS/historical/antar_years.txt")
j1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/ACCESS/historical/antar_years.txt")
j2 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/CANESM/historical/antar_years.txt")
j3 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/CANESM/historical/antar_years.txt")
j4 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/FGOALS/historical/antar_years.txt")
j5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/FGOALS/historical/antar_years.txt")
j6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/IPSL/historical/antar_years.txt")
j7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/IPSL/historical/antar_years.txt")
j8 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MIROC/historical/antar_years.txt")
j9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MIROC/historical/antar_years.txt")
j10 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/EC-EARTH/antar_years.txt")
j11 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/EC-EARTH/antar_years.txt")
j12 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/giss/antar_years.txt")
j13 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/giss/antar_years.txt")
j14 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/hadgem/antar_years.txt")
j15 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/hadgem/antar_years.txt")
#---- RETIRANDO O NOME DAS COLUNAS
i1 <- i1[-1,]
i2 <- i2[-1,]
i3 <- i3[-1,]
i4 <- i4[-1,]
i5 <- i5[-1,]
i6 <- i6[-1,]
i7 <- i7[-1,]
i8 <- i8[-1,]
i9 <- i9[-1,]
j <- j[-1,]
j1 <- j1[-1,]
j2 <- j2[-1,]
j3 <- j3[-1,]
j4 <- j4[-1,]
j5 <- j5[-1,]
j6 <- j6[-1,]
j7 <- j7[-1,]
j8 <- j8[-1,]
j9 <- j9[-1,]
j10 <- j10[-1,]
j11 <- j11[-1,]
j12 <- j12[-1,]
j13 <- j13[-1,]
j14 <- j14[-1,]
j15 <- j15[-1,]
#---- FEVEREIRO CMIP5
ic <- data.frame(ano = i4$V2,mes = i4$V1, 
                 BESM5 = i4$V4,
                 GDFL5 = i1$V4, 
                 MPI5 = i2$V4, 
                 NCAR5 = i3$V4, 
                 MIROC5 = j8$V4,
                 IPSL5 = j6$V4,
                 ACCESS5 = j$V4,
                 CANESM5 = j2$V4,
                 FGOALS5 = j4$V4,
                 giss5= j12$V4,
                 had5 = j14$V4,
                 earth5 = j10$V4)
ice_fev <- ic[ic$mes == "FEB",]
ice_fev$mes <- NULL
#---- FEVEREIRO CMIP6
ic1 <- data.frame(ano = i5$V2, mes = i5$V1,
                  GDFL6 = i5$V4, 
                  MPI6 = i6$V4, 
                  NCAR6 = i7$V4,
                  MIROC6 = j9$V4,
                  IPSL6 = j7$V4,
                  ACCESS6 = j1$V4,
                  CANESM6 = j3$V4,
                  FGOALS6 = j5$V4,
                  giss6 = j13$V4,
                  had6 = j15$V4,
                  earth6 = j11$V4,
                  SATE = i9$V4)
ice_fev1 <- ic1[ic1$mes == "FEB",]
ice_fev1$mes <- NULL
#----
data <- merge(ice_fev1,ice_fev, all.x = TRUE)
data
#data[-c(27:35),]

#----
ice <- gather(data = data, key = meses, 
              value = extensao,
              BESM5,
              GDFL5,MPI5,NCAR5,
              GDFL6,MPI6,NCAR6,MIROC5,MIROC6, IPSL5, IPSL6,
              ACCESS5,ACCESS6,CANESM5,CANESM6,FGOALS5,FGOALS6,giss5,giss6,had5,had6,earth5,earth6,SATE)
ice
#----
ice$extensao <- as.numeric(as.character(ice$extensao))
ice$ano <- as.numeric(as.character(ice$ano))
#----
#sd(as.numeric(as.character(data$ncar_fev)), na.rm = T)
#l <- lm(as.numeric(as.character(data$besm_fev)) ~ as.numeric(as.character(data$ano)))
#summary(l)

#trendline(as.numeric(as.character(ice_fev1$ano)), as.numeric(as.character(ice_fev1$gdflcm4_fev)), model="line2P", ePos.x = "topleft", summary=TRUE, eDigit=5)
#---- PLOT
hist <- ggplot(ice, aes(x=ano,y=extensao)) +
  geom_line(aes(group = meses, linetype=meses,colour = meses),size=0.6)+
  #geom_abline(intercept = 23, slope = 2.6)+
  stat_smooth(aes(x = ano, y = extensao, group = meses), 
            method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed",
             colour = "black", size =0.3, na.rm = TRUE) +
  scale_colour_manual(values = c('slateblue1','slateblue3',"orange",'royalblue1','royalblue4','green','green4',
                                 " darkorchid1","darkorchid4","gold","gold3","deeppink","deeppink3","gray70",
                                 "gray48",'chartreuse2', 'chartreuse3','darkolivegreen2','darkolivegreen3', 
                                 "darkslategray2","darkslategray3",'chocolate1','chocolate3','black'),
                      labels = c('ACCESS1-0','ACCESS-ESM1-5','BESM-OA2.5', 'CanESM2','CanESM5',
                                 'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                 'GISS-E2-H','GISS-E2-1-H', 'HadCM3','HadGEM3-GC31-LL',
                                 'IPSL-CM5A', 'IPSL-CM6A', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                 'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2', 'Observado (Satélite)'),
                      name="") +
  scale_linetype_manual(values =  c("dashed","solid","dashed","dashed","solid",
                                    "dashed","solid","dashed","solid","dashed","solid","dashed","solid",
                                    "dashed","solid","dashed","solid","dashed","solid","dashed","solid","dashed","solid","solid"),
                        labels = c('ACCESS1-0','ACCESS-ESM1-5','BESM-OA2.5', 'CanESM2','CanESM5',
                                   'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                   'GISS-E2-H','GISS-E2-1-H', 'HadCM3','HadGEM3-GC31-LL',
                                   'IPSL-CM5A', 'IPSL-CM6A', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                   'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2', 'Observado (Satélite)'),
                        name="")+
  
  labs(title = "",
       x = "", 
       y = "SIA (x10⁶km²)",size = 2) + 
  ylim(-1,30) + 
  xlab("a) Antártica - Fevereiro")+
  theme_classic(base_size = 16) +
  theme(legend.position="none",
        legend.text=element_text(size=8),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                    linetype="solid"))

hist

#library(stats)


#histi <- ggscatter(ice, x = "ano", y="extensao", color = "meses", palette = "jco",
#                   add = "reg.line", conf.int = TRUE)+
#  stat_cor(aes(color=ice$meses),label.x.npc = 0.53,label.y.npc = 0.33)+
# stat_regline_equation(aes(color=ice$meses), label.x.npc = 0.10,label.y.npc = 0.33)
#histi
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ANTÁRTICA SETEMBRO
#-----------------------------------------------------------------------------------------------------------------
i1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/gdfl_antar.txt')
i2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/mpi_antar.txt')
i3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/ncar_antar.txt')
i4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/besm_antar.txt')
#-------------------------------------
i5 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/gdfl_antar.txt')
i6 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/mpi_antar.txt')
i7 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/ncar_antar.txt')
i8 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/satelite/cmip6/sate_all_years_antar.txt')
i9 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/sate_V4/historical/antar_years.txt')
#---------------
j <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/ACCESS/historical/antar_years.txt")
j2 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/CANESM/historical/antar_years.txt")
j4 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/FGOALS/historical/antar_years.txt")
j6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/IPSL/historical/antar_years.txt")
j8 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MIROC/historical/antar_years.txt")
#---------------
j1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/ACCESS/historical/antar_years.txt")
j3 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/CANESM/historical/antar_years.txt")
j5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/FGOALS/historical/antar_years.txt")
j7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/IPSL/historical/antar_years.txt")
j9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MIROC/historical/antar_years.txt")
#---- RETIRANDO O NOME DAS COLUNAS
i1 <- i1[-1,]
i2 <- i2[-1,]
i3 <- i3[-1,]
i4 <- i4[-1,]
i5 <- i5[-1,]
i6 <- i6[-1,]
i7 <- i7[-1,]
i8 <- i8[-1,]
i9 <- i9[-1,]
j <- j[-1,]
j1 <- j1[-1,]
j2 <- j2[-1,]
j3 <- j3[-1,]
j4 <- j4[-1,]
j5 <- j5[-1,]
j6 <- j6[-1,]
j7 <- j7[-1,]
j8 <- j8[-1,]
j9 <- j9[-1,]
#---- FEVEREIRO CMIP5
ic <- data.frame(ano = i4$V2,mes = i4$V1, 
                 BESM5 = i4$V4,
                 GDFL5 = i1$V4, 
                 MPI5 = i2$V4, 
                 NCAR5 = i3$V4, 
                 MIROC5 = j8$V4,
                 IPSL5 = j6$V4,
                 ACCESS5 = j$V4,
                 CANESM5 = j2$V4,
                 FGOALS5 = j4$V4,
                 giss5= j12$V4,
                 had5 = j14$V4,
                 earth5 = j10$V4)
ice_fev <- ic[ic$mes == "SEP",]
ice_fev$mes <- NULL
#---- FEVEREIRO CMIP6
ic1 <- data.frame(ano = i5$V2, mes = i5$V1,
                  GDFL6 = i5$V4, 
                  MPI6 = i6$V4, 
                  NCAR6 = i7$V4,
                  MIROC6 = j9$V4,
                  IPSL6 = j7$V4,
                  ACCESS6 = j1$V4,
                  CANESM6 = j3$V4,
                  FGOALS6 = j5$V4,
                  giss6 = j13$V4,
                  had6 = j15$V4,
                  earth6 = j11$V4,
                  SATE = i9$V4)
ice_fev1 <- ic1[ic1$mes == "SEP",]
ice_fev1$mes <- NULL
ice_fev1$mes <- NULL
#----
data <- merge(ice_fev1,ice_fev, all.x = TRUE)
data
#data[-c(27:35),]

#----
ice <- gather(data = data, key = meses, 
              value = extensao,
              BESM5,
              GDFL5,MPI5,NCAR5,
              GDFL6,MPI6,NCAR6,MIROC5,MIROC6, IPSL5, IPSL6,
              ACCESS5,ACCESS6,CANESM5,CANESM6,FGOALS5,FGOALS6,giss5,giss6,had5,had6,earth5,earth6,
              SATE)
ice
#----
ice$extensao <- as.numeric(as.character(ice$extensao))
ice$ano <- as.numeric(as.character(ice$ano))


#sd(as.numeric(as.character(data$ncar_fev)), na.rm = T)
#l <- lm(as.numeric(as.character(data$besm_fev)) ~ as.numeric(as.character(data$ano)))
#summary(l)

#---- PLOT
hist_antar6 <- ggplot(ice, aes(x=ano,y=extensao)) +
  geom_line(aes(group = meses, linetype=meses,colour = meses),size=0.6)+
   stat_smooth(aes(x = ano, y = extensao, group = meses), 
              method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed",
             colour = "black", size =0.3, na.rm = TRUE) +
  scale_colour_manual(values = c('slateblue1','slateblue3',"orange",'royalblue1','royalblue4','green','green4',
                                 " darkorchid1","darkorchid4","gold","gold3","deeppink","deeppink3","gray70",
                                 "gray48",'chartreuse2', 'chartreuse3','darkolivegreen2','darkolivegreen3', 
                                 "darkslategray2","darkslategray3",'chocolate1','chocolate3','black'),
                      labels = c('ACCESS1-0','ACCESS-ESM1-5','BESM-OA2.5', 'CanESM2','CanESM5',
                                 'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                 'GISS-E2-H','GISS-E2-1-H', 'HadCM3','HadGEM3-GC31-LL',
                                 'IPSL-CM5A', 'IPSL-CM6A', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                 'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2', 'Observado (Satélite)'),
                      name="") +
  scale_linetype_manual(values =  c("dashed","solid","dashed","dashed","solid",
                                    "dashed","solid","dashed","solid","dashed","solid","dashed","solid",
                                    "dashed","solid","dashed","solid","dashed","solid","dashed","solid","dashed","solid","solid"),
                        labels = c('ACCESS1-0','ACCESS-ESM1-5','BESM-OA2.5', 'CanESM2','CanESM5',
                                   'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                   'GISS-E2-H','GISS-E2-1-H', 'HadCM3','HadGEM3-GC31-LL',
                                   'IPSL-CM5A', 'IPSL-CM6A', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                   'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2', 'Observado (Satélite)'),
                        name="")+
  
  labs(title = "",
       x = "", 
       y = "",size = 2) + 
  ylim(0,30) + 
  xlab("b) Antártica - Setembro")+
  theme_classic(base_size = 16) +
  theme(#legend.position=c(.75,.80),
    legend.text=element_text(size=8),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                linetype="solid"))
hist_antar6
######## R quadrado, inclinação
#histi <- ggscatter(ice, x = "ano", y="extensao", color = "meses", palette = "jco",
#                  add = "reg.line", conf.int = TRUE)+
#stat_cor(aes(color=ice$meses),label.x.npc = 0.53,label.y.npc = 0.33)+
# stat_regline_equation(aes(color=ice$meses), label.x.npc = 0.10,label.y.npc = 0.33)
#histi
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ÁRTICO MARÇO
#-----------------------------------------------------------------------------------------------------------------
i1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/gdfl_arct.txt')
i2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/mpi_arct.txt')
i3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/ncar_arct.txt')
i4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/besm_arct.txt')
#-------------------------------------
j1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/gdfl_arct.txt')
j2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/mpi_arct.txt')
j3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/ncar_arct.txt')
j4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/satelite/cmip6/arct_all_years_arct.txt')
j5 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/extent/sate_1980_2014_arct.txt')
#---------------
#-----
i1 <- i1[-1,] ; i2 <- i2[-1,] ; i3 <- i3[-1,] ; i4 <- i4[-1,]
j1 <- j1[-1,] ; j2 <- j2[-1,] ; j3 <- j3[-1,] ; j4 <- j4[-1,]; j5 <- j5[-1,]
#---- FEVEREIRO CMIP5
ic <- data.frame(ano = i1$V2, mes=i1$V1, gdfl_fev=i1$V4, mpi_fev=i2$V4, ncar_fev = i3$V4, besm_fev = i4$V4)
ice_fev <- ic[ic$mes == "MAR",]
ice_fev$mes <- NULL
#---- FEVEREIRO CMIP6
ic1 <- data.frame(ano = j1$V2, mes=j1$V1, gdflcm4_fev=j1$V4, mpiesm_fev=j2$V4, ncarcesm_fev = j3$V4,sate=j5$V3)
ice_fev1 <- ic1[ic1$mes == "MAR",]
ice_fev1$mes <- NULL
#----
data <- merge(ice_fev1,ice_fev, all.x = TRUE)
data
#----
ice <- gather(data = data, key = meses, 
              value = extensao, besm_fev, gdfl_fev, mpi_fev, ncar_fev, gdflcm4_fev,mpiesm_fev,ncarcesm_fev,sate)
ice
#----
ice$extensao <- as.numeric(as.character(ice$extensao))
ice$ano <- as.numeric(as.character(ice$ano))
#---- PLOT
hist_5 <- ggplot(ice, aes(x=ano,y=extensao)) +
  geom_line(aes(group = meses, linetype=meses,colour = meses),size=0.6)+
  # stat_smooth(aes(x = ano, y = extensao, group = meses), 
  #           method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed",
  #           colour = "black", size =0.3, na.rm = TRUE) +
  scale_colour_manual(values = c('orange','red1','red4','royalblue1','royalblue4','green','green4',"black"), 
                      labels = c('BESM-OA2.5 ','GFDL-CM3 ','GFDL-CM4 ',
                                 'MPI-ESM-LR ','MPI-ESM1.2-LR ',
                                 'NCAR-CCSM4 ','NCAR-CESM2 ',"Satélite"), name="")+ 
  scale_linetype_manual(values=c("dashed","dashed","solid","dashed",
                                 "solid","dashed","solid","solid"),
                        labels = c('BESM-OA2.5 ','GFDL-CM3 ','GFDL-CM4 ',
                                   'MPI-ESM-LR ','MPI-ESM1.2-LR ',
                                   'NCAR-CCSM4 ','NCAR-CESM2 ',"Satélite"),name="") +
  
  labs(title = "",
       x = "", 
       y = "SIA (x10⁶km²)",size = 2) + 
  ylim(5,16) + 
  xlab("c) Ártico - Março")+
  theme_classic(base_size = 16) +
  theme(#legend.position=c(.75,.80),
    legend.text=element_text(size=8),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                linetype="solid"))
hist_5

#histi <- ggscatter(ice, x = "ano", y="extensao", color = "meses", palette = "jco",
#                  add = "reg.line", conf.int = TRUE)+
#stat_cor(aes(color=ice$meses),label.x.npc = 0.53,label.y.npc = 0.53)+
#stat_regline_equation(aes(color=ice$meses), label.x.npc = 0.10,label.y.npc = 0.53)
#histi
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ÁRTICO SETEMBRO
#-----------------------------------------------------------------------------------------------------------------
i1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/gdfl_arct.txt')
i2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/mpi_arct.txt')
i3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/ncar_arct.txt')
i4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/historical/besm_arct.txt')
#-------------------------------------
j1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/gdfl_arct.txt')
j2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/mpi_arct.txt')
j3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/historical/ncar_arct.txt')
j4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/satelite/cmip6/arct_all_years_arct.txt')
j5 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/extent/sate_1980_2014_arct.txt')
#---------------
#-----
i1 <- i1[-1,] ; i2 <- i2[-1,] ; i3 <- i3[-1,] ; i4 <- i4[-1,]
j1 <- j1[-1,] ; j2 <- j2[-1,] ; j3 <- j3[-1,] ; j4 <- j4[-1,]; j5 <- j5[-1,]
#---- FEVEREIRO CMIP5
ic <- data.frame(ano = i1$V2, mes=i1$V1, gdfl_fev=i1$V4, mpi_fev=i2$V4, ncar_fev = i3$V4, besm_fev = i4$V4)
ice_fev <- ic[ic$mes == "SEP",]
ice_fev$mes <- NULL
#---- FEVEREIRO CMIP6
ic1 <- data.frame(ano = j1$V2, mes=j1$V1, gdflcm4_fev=j1$V4, mpiesm_fev=j2$V4, ncarcesm_fev = j3$V4,sate=j5$V3)
ice_fev1 <- ic1[ic1$mes == "SEP",]
ice_fev1$mes <- NULL
#----
data <- merge(ice_fev1,ice_fev, all.x = TRUE)
data
#----
ice <- gather(data = data, key = meses, 
              value = extensao, besm_fev, gdfl_fev, mpi_fev, ncar_fev, gdflcm4_fev,mpiesm_fev,ncarcesm_fev,sate)
ice
#----
data <- merge(ice_fev1,ice_fev, all.x = TRUE)
data
#----
ice <- gather(data = data, key = meses, 
              value = extensao, besm_fev, gdfl_fev, mpi_fev, ncar_fev, gdflcm4_fev,mpiesm_fev,ncarcesm_fev,sate)
ice
#----
ice$extensao <- as.numeric(as.character(ice$extensao))
ice$ano <- as.numeric(as.character(ice$ano))
#---- PLOT
hist_6 <- ggplot(ice, aes(x=ano,y=extensao)) +
  geom_line(aes(group = meses, linetype=meses,colour = meses),size=0.6)+
  #stat_smooth(aes(x = ano, y = extensao, group = meses), 
  #          method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed",
  #         colour = "black", size =0.3, na.rm = TRUE) +
  scale_colour_manual(values = c('orange','red1','red4','royalblue1','royalblue4','green','green4',"black"), 
                      labels = c('BESM-OA2.5 ','GFDL-CM3 ','GFDL-CM4 ',
                                 'MPI-ESM-LR ','MPI-ESM1.2-LR ',
                                 'NCAR-CCSM4 ','NCAR-CESM2 ',"Satélite"), name="")+ 
  scale_linetype_manual(values=c("dashed","dashed","solid","dashed",
                                 "solid","dashed","solid","solid"),
                        labels = c('BESM-OA2.5 ','GFDL-CM3 ','GFDL-CM4 ',
                                   'MPI-ESM-LR ','MPI-ESM1.2-LR ',
                                   'NCAR-CCSM4 ','NCAR-CESM2 ',"Satélite"),name="") +
  
  labs(title = "",
       x = "", 
       y = "SIA (x10⁶km²)",size = 2) + 
  ylim(-1,16) + 
  xlab("d) Ártico - Setembro")+
  theme_classic(base_size = 16) +
  theme(#legend.position=c(.75,.80),
    legend.text=element_text(size=8),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                linetype="solid"))
hist_6
#histi <- ggscatter(ice, x = "ano", y="extensao", color = "meses", palette = "jco",
#                  add = "reg.line", conf.int = TRUE)+
#stat_cor(aes(color=ice$meses),label.x.npc = 0.53,label.y.npc = 0.53)+
#stat_regline_equation(aes(color=ice$meses), label.x.npc = 0.10,label.y.npc = 0.53)
#histi

