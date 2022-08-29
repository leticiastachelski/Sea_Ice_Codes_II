library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library("RColorBrewer")
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ANTÁRTICA
#-----------------------------------------------------------------------------------------------------------------
# ---- abrindo dados satélite ----
#ice_sate <- read_excel(
# "/home/leticia/Documentos/recuperação/Sea_Ice_Index_Monthly_Data_by_Year_G02135_v3.0.xlsx", sheet = 3)
#ice_sate <- ice_sate[c(3:37),]
#---- ABRINDO ARQUIVOS
i1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/GDFL/grads/GFDL_CM3_antar.txt") 
i2 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MPI/grads/mpi_antar.txt")
i3 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/NCAR/grads/ncar_antar.txt")
i4 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/GDFL/grads/ciclo_sazonal111.txt")
i5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MPI/grads/ciclo_sazonal111.txt")
i6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/NCAR/grads/ncar_antar.txt")
#i7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/satelite/sate_antar.txt")
#i8 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/BESM/grads/besm_antar.txt")
#i7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/extent/sate_1980_2014_mean.txt")
#i9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/sate_V4/historical/ciclo_sazonalNSIDC.txt")
#i9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_artigo_programs/todos netcdfs/diferença CMIPs/ciclo_sazonalSATE.txt")
i9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_artigo_programs/todos netcdfs/diferença CMIPs/ciclo_sazonalSATE.txt")
## ----------------------------------------
j <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/ACCESS/historical/ciclo_sazonal.txt")
j1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/ACCESS/historical/ciclo_sazonal.txt")
j2 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/CANESM/historical/ciclo_sazonal.txt")
j3 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/CANESM/historical/ciclo_sazonal.txt")
j4 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/FGOALS/historical/ciclo_sazonal.txt")
j5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/FGOALS/historical/ciclo_sazonal.txt")
j6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/IPSL/historical/ciclo_sazonal.txt")
j7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/IPSL/historical/ciclo_sazonal.txt")
j8 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MIROC/historical/ciclo_sazonal.txt")
j9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MIROC/historical/ciclo_sazonal.txt")
j10 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/EC-EARTH/ciclo_sazonal.txt")
j11 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/EC-EARTH/ciclo_sazonal.txt")
j12 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/giss/ciclo_sazonal.txt")
j13 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/giss/ciclo_sazonal.txt")
j14 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/hadgem/ciclo_sazonal.txt")
j15 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/hadgem/ciclo_sazonal.txt")
## ----------------------------------------
u <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_artigo_programs/todos netcdfs/CMIP5/ciclo_sazonalCMIP5.txt")
u1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_artigo_programs/todos netcdfs/CMIP6/ciclo_sazonalCMIP6.txt")
#---- RETIRANDO O NOME DAS COLUNAS
u <- u[-1,] 
u1 <- u1[-1,]
#---- RETIRANDO O NOME DAS COLUNAS
i1 <- i1[-1,]
i2 <- i2[-1,]
i3 <- i3[-1,]
i4 <- i4[-1,]
i5 <- i5[-1,]
i6 <- i6[-1,]
i7 <- i7[-1,]
#i8 <- i8[-1,]
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
#---- CRIANDO DATA.FRAME
ice_sic <- data.frame(mes = i1$V1, 
                   #   BESM5 = i8$V4,
                      GDFL5 = i1$V4, 
                      GDFL6 = i4$V4, 
                      MPI5 = i2$V4, 
                      MPI6 = i5$V4, 
                      NCAR5 = i3$V4, 
                      NCAR6 = i6$V4,
                      MIROC5 = j8$V4,
                      MIROC6 = j9$V4,
                      IPSL5 = j6$V4,
                      IPSL6 = j7$V4,
                      ACCESS5 = j$V4,
                      ACCESS6 = j1$V4,
                      CANESM5 = j2$V4,
                      CANESM6 = j3$V4,
                      FGOALS5 = j4$V4,
                      FGOALS6 = j5$V4,
                      giss5= j12$V4,
                      giss6 = j13$V4,
                      had5 = j14$V4,
                      had6 = j15$V4,
                      earth5 = j10$V4,
                      earth6 = j11$V4,
                      CMIP5 = u$V4,
                      CMIP6 = u1$V4,
                      SATE = i9$V4)
ice_sic

#ice_sic$BESM5 <- as.numeric(as.character(ice_sic$BESM5))
ice_sic$GDFL5 <- as.numeric(as.character(ice_sic$GDFL5))
ice_sic$GDFL6 <- as.numeric(as.character(ice_sic$GDFL6))
ice_sic$MPI5 <- as.numeric(as.character(ice_sic$MPI5))
ice_sic$MPI6 <- as.numeric(as.character(ice_sic$MPI6))
ice_sic$NCAR5 <- as.numeric(as.character(ice_sic$NCAR5))
ice_sic$NCAR6 <- as.numeric(as.character(ice_sic$NCAR6))
ice_sic$MIROC5 <- as.numeric(as.character(ice_sic$MIROC5))
ice_sic$MIROC6 <- as.numeric(as.character(ice_sic$MIROC6))
ice_sic$IPSL5 <- as.numeric(as.character(ice_sic$IPSL5))
ice_sic$IPSL6 <- as.numeric(as.character(ice_sic$IPSL6))
ice_sic$ACCESS5 <- as.numeric(as.character(ice_sic$ACCESS5))
ice_sic$ACCESS6 <- as.numeric(as.character(ice_sic$ACCESS6))
ice_sic$CANESM5 <- as.numeric(as.character(ice_sic$CANESM5))
ice_sic$CANESM6 <- as.numeric(as.character(ice_sic$CANESM6))
ice_sic$FGOALS5 <- as.numeric(as.character(ice_sic$FGOALS5))
ice_sic$FGOALS6 <- as.numeric(as.character(ice_sic$FGOALS6))
ice_sic$giss5 <- as.numeric(as.character(ice_sic$giss5))
ice_sic$giss6 <- as.numeric(as.character(ice_sic$giss6))
ice_sic$had5 <- as.numeric(as.character(ice_sic$had5))
ice_sic$had6 <- as.numeric(as.character(ice_sic$had6))
ice_sic$earth5 <- as.numeric(as.character(ice_sic$earth5))
ice_sic$earth6 <- as.numeric(as.character(ice_sic$earth6))
ice_sic$CMIP5 <- as.numeric(as.character(ice_sic$CMIP5))
ice_sic$CMIP6 <- as.numeric(as.character(ice_sic$CMIP6))
ice_sic$SATE <- as.numeric(as.character(ice_sic$SATE))

#---- MANIPULANDO DATA.FRAME PARA O PLOT
sic <- gather(data = ice_sic, key = meses, 
              value = extensao,#BESM5,
              GDFL5,MPI5,NCAR5,
              GDFL6,MPI6,NCAR6,MIROC5,MIROC6, IPSL5, IPSL6,
              ACCESS5,ACCESS6,CANESM5,CANESM6,FGOALS5,FGOALS6,giss5,giss6,had5,had6,earth5,earth6,CMIP5,CMIP6,
              SATE)
sic$extensao <- as.numeric(as.character(sic$extensao))
#---- AJUSTE EIXO X
ice_sic$mes <- factor(ice_sic$mes, 
                  levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                             "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
  ice_sic

#---- PLOT
sea_ice <- ggplot(ice_sic,aes(group = 1)) +
  geom_line(aes(x=mes,y=ACCESS5, colour= "ACCESS1-0",linetype= "ACCESS1-0"))+
    geom_line(aes(x=mes,y=ACCESS6, colour= "ACCESS-ESM1-5",linetype= "ACCESS-ESM1-5"))+
   #   geom_line(aes(x=mes,y=BESM5, colour= "BESM-OA2.5",linetype= "BESM-OA2.5"))+
        geom_line(aes(x=mes,y=CANESM5, colour= "CanESM2",linetype= "CanESM2"))+
          geom_line(aes(x=mes,y=CANESM6, colour= "CanESM5",linetype= "CanESM5"))+
            geom_line(aes(x=mes,y=earth5, colour= "EC-EARTH",linetype= "EC-EARTH"))+
              geom_line(aes(x=mes,y=earth6, colour= "EC-Earth3",linetype= "EC-Earth3"))+
                geom_line(aes(x=mes,y=FGOALS5, colour= "FGOALS-g2",linetype= "FGOALS-g2"))+
                   geom_line(aes(x=mes,y=FGOALS6, colour= "FGOALS-g3",linetype= "FGOALS-g3"))+
                      geom_line(aes(x=mes,y=GDFL5, colour= "GFDL-CM3",linetype= "GFDL-CM3"))+
                          geom_line(aes(x=mes,y=GDFL6, colour= "GFDL-CM4",linetype= "GFDL-CM4"))+
                             geom_line(aes(x=mes,y=giss5, colour= "GISS-E2-H",linetype= "GISS-E2-H"))+
                                geom_line(aes(x=mes,y=giss6, colour= "GISS-E2-1-H",linetype= "GISS-E2-1-H"))+
                                    geom_line(aes(x=mes,y=had5, colour= "HadCM3",linetype= "HadCM3"))+
                                        geom_line(aes(x=mes,y=had6, colour= "HadGEM3-GC31-LL",linetype= "HadGEM3-GC31-LL"))+
                                          geom_line(aes(x=mes,y=IPSL5, colour= "IPSL-CM5A-LR",linetype= "IPSL-CM5A-LR"))+
                                            geom_line(aes(x=mes,y=IPSL6, colour= "IPSL-CM6A-LR",linetype= "IPSL-CM6A-LR"))+
                                                geom_line(aes(x=mes,y=MIROC5, colour= "MIROC5",linetype= "MIROC5"))+
                                                  geom_line(aes(x=mes,y=MIROC6, colour= "MIROC6",linetype= "MIROC6"))+
                                                   geom_line(aes(x=mes,y=MPI5, colour= "MPI-ESM-LR",linetype= "MPI-ESM-LR"))+
                                                    geom_line(aes(x=mes,y=MPI6, colour= "MPI-ESM1.2-LR",linetype= "MPI-ESM1.2-LR"))+
                                                      geom_line(aes(x=mes,y=NCAR5, colour= "NCAR-CCSM4",linetype= "NCAR-CCSM4"))+
                                            geom_line(aes(x=mes,y=NCAR6, colour= "NCAR-CESM2",linetype= "NCAR-CESM2"))+
                                          geom_line(aes(x=mes,y=CMIP5, colour= 'CMIP5 mean',linetype= 'CMIP5 mean'),size=1)+
                                            geom_line(aes(x=mes,y=CMIP6, colour= "CMIP6 mean",linetype= "CMIP6 mean"),size=1)+
                                              geom_line(aes(x=mes,y=SATE, colour= "Observado (Satélite)",linetype= "Observado (Satélite)"),size=1)+
                          
  # stat_smooth(aes(x = mes, y = extensao, group = meses),
  #            method = "loess", formula = y ~ x, se = FALSE, linetype = "dashed",
  #           colour = "black", size =0.3,na.rm=TRUE)+
  scale_colour_manual(breaks = c('ACCESS1-0','ACCESS-ESM1-5',#'BESM-OA2.5',
                                 'CanESM2','CanESM5',
                                 'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                 'GISS-E2-H','GISS-E2-1-H', 'HadCM3','HadGEM3-GC31-LL',
                                 'IPSL-CM5A-LR', 'IPSL-CM6A-LR', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                 'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2'
                                 ,'CMIP5 mean','CMIP6 mean', 'Observado (Satélite)'),
                      values = c('ACCESS1-0'='hotpink1',
                                 'ACCESS-ESM1-5'='hotpink2',
                                # 'BESM-OA2.5'="orange",
                                 'CanESM2'="gold",
                                 'CanESM5'="gold3",
                                 'EC-EARTH'='green',
                                 'EC-Earth3'='green4',
                                 'FGOALS-g2'=" blue1",
                                 'FGOALS-g3'="blue2",
                                 'GFDL-CM3'='yellow1',
                                 'GFDL-CM4'='yellow2',
                                 'GISS-E2-H'="deeppink",
                                 'GISS-E2-1-H'="deeppink3",
                                 'HadCM3'="gray49",
                                 'HadGEM3-GC31-LL'="gray29",
                                 'IPSL-CM5A-LR'='darkorange', 
                                 'IPSL-CM6A-LR'='darkorange3',
                                 'MIROC5'='purple2',
                                 'MIROC6'='purple4', 
                                 'MPI-ESM-LR'="darkslategray2",
                                 'MPI-ESM1.2-LR'="darkslategray3",
                                 'NCAR-CCSM4'='springgreen1',
                                 'NCAR-CESM2'='springgreen2',
                                'CMIP5 mean'='red',
                                'CMIP6 mean'='red2', 
                                'Observado (Satélite)'='black'),
                      name="") +
  scale_linetype_manual(breaks = c('ACCESS1-0','ACCESS-ESM1-5',#'BESM-OA2.5', 
                                   'CanESM2','CanESM5',
                                   'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                   'GISS-E2-H','GISS-E2-1-H', 'HadCM3','HadGEM3-GC31-LL',
                                   'IPSL-CM5A-LR', 'IPSL-CM6A-LR', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                   'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2',
                                   'CMIP5 mean','CMIP6 mean', 'Observado (Satélite)'),
                       values =  c('ACCESS1-0'='dashed',
                                   'ACCESS-ESM1-5'='solid',
                                   #'BESM-OA2.5'="dashed",
                                   'CanESM2'='dashed',
                                   'CanESM5'='solid',
                                   'EC-EARTH'='dashed',
                                   'EC-Earth3'='solid',
                                   'FGOALS-g2'="dashed",
                                   'FGOALS-g3'="solid",
                                   'GFDL-CM3'="dashed",
                                   'GFDL-CM4'="solid",
                                   'GISS-E2-H'="dashed",
                                   'GISS-E2-1-H'="solid",
                                   'HadCM3'="dashed",
                                   'HadGEM3-GC31-LL'="solid",
                                   'IPSL-CM5A-LR'='dashed', 
                                   'IPSL-CM6A-LR'='solid',
                                   'MIROC5'='dashed',
                                   'MIROC6'='solid', 
                                   'MPI-ESM-LR'="dashed",
                                   'MPI-ESM1.2-LR'="solid",
                                   'NCAR-CCSM4'='dashed',
                                   'NCAR-CESM2'='solid',
                                   'CMIP5 mean'='dashed',
                                   'CMIP6 mean'='solid', 
                                   'Observado (Satélite)'='solid'),
               name="")+
  guides(linetype = guide_legend(override.aes = list(size = 0.3)))+
  ylim(-1,25)+
  labs(x = "", 
       y = "SIA x 10⁶ km²",size = 10) + 
 # ylim(-1,25) +
  theme_classic(base_size = 16) +
  theme(legend.text=element_text(size=8),panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                                                     linetype="solid")
  )
sea_ice
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ÁRTICO
#-----------------------------------------------------------------------------------------------------------------
#ice_sate <- read_excel(
# "/home/leticia/Documentos/recuperação/Sea_Ice_Index_Monthly_Data_by_Year_G02135_v3.0.xlsx", sheet = 1)
#ice_sate <- ice_sate[c(3:37),]
#---- ABRINDO ARQUIVOS
i1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/GDFL/grads/GFDL_CM3_arct.txt") 
i2 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MPI/grads/mpi_arct.txt")
i3 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/NCAR/grads/ncar_arct.txt")
i4 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/GDFL/grads/gdfl_arct.txt")
i5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MPI/grads/mpi_arct.txt")
i6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/NCAR/grads/ncar_arct.txt")
i7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/satelite/sate_arct.txt")
i8 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/BESM/grads/besm_arct.txt")
i9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/extent/sate_mean_arct.txt")
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
#---- CRIANDO DATA.FRAME
ice_sic1 <- data.frame(mes = i1$V1, 
                       BESM_CMIP5 = i8$V4,
                       GDFL_CMIP5 = i1$V4, 
                       MPI_CMIP5 = i2$V4, 
                       NCAR_CMIP5 = i3$V4, 
                       GDFL_CMIP6 = i4$V4, 
                       MPI_CMIP6 = i5$V4, 
                       NCAR_CMIP6 = i6$V4,
                       SATE = i9$V2)
ice_sic1
#---- MANIPULANDO DATA.FRAME PARA O PLOT
sic1 <- gather(data = ice_sic1, key = meses, 
               value = extensao,BESM_CMIP5,
               GDFL_CMIP5,MPI_CMIP5,NCAR_CMIP5,
               GDFL_CMIP6,MPI_CMIP6,NCAR_CMIP6,
               SATE)
sic1$extensao <- as.numeric(as.character(sic1$extensao))
#---- AJUSTE EIXO X
sic1$mes <- factor(sic1$mes, 
                   levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                              "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
sic1
#---- PLOT
sea_ice1 <- ggplot(sic1, aes(x=mes,y=extensao)) +
  geom_line(aes(group = meses,linetype=meses, colour = meses),size=0.6)+
  scale_colour_manual(values = c("orange",'red1','red4','royalblue1','royalblue4','green','green4','black'), 
                      labels = c("BESM-OA2.5",'GFDL-CM3','GFDL-CM4',
                                 'MPI-ESM-LR','MPI-ESM1.2-LR',
                                 'NCAR-CCSM4','NCAR-CESM2','Satélite'),
                      name="") +
  scale_linetype_manual(values =  c("dashed","dashed","solid","dashed","solid",
                                    "dashed","solid","solid"),
                        labels = c("BESM-OA2.5",'GFDL-CM3','GFDL-CM4',
                                   'MPI-ESM-LR','MPI-ESM1.2-LR',
                                   'NCAR-CCSM4','NCAR-CESM2','Satélite'),
                        name="")+
  labs(x = "b) Ártico", 
       y = "SIA x 10⁶ km²",size = 10) + 
  ylim(-1,15) +
  theme_classic(base_size = 14) +
  theme( legend.text=element_text(size=8),panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                                                      linetype="solid")
  )
sea_ice1

