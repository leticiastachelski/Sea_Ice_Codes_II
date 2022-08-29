library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library("RColorBrewer")
#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ANTÁRTICA FEVEREIRO RCP8.5 E SSP585
#-----------------------------------------------------------------------------------------------------------------
#i1 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/rcp_85/besm_antar_rcp_85.txt')
i2 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/GDFL/rcp/RCP_45/dados/gdfl_antar_rcp_45.txt')
i3 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MPI/rcp/rcp_45/mpi_antar_rcp_45.txt')
i4 <- read.table('/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/NCAR/rcp/rcp_45/ncar_antar_rcp_85.txt')
i5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/GDFL/futuro_ssp_245/GFDL_fev_ssp_245.txt") 
i6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MPI/futuro_ssp_245/MPI_fev_ssp_245.txt")
i7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/NCAR/futuro_ssp_245/fev_ssp_245.txt")
i8 <- read.table("/media/leticia/Expansion Drive/dados futuro/mean RCP SSP 45/cmip5/antar_years.txt")
i9 <- read.table("/media/leticia/Expansion Drive/dados futuro/mean RCP SSP 45/cmip6/fev_ssp_245.txt")
#-----------------------------------------------------------------------------------------------------------------
j <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/ACCESS/rcp/45/antar_years.txt")
j1 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/ACCESS/rcp/antar_years.txt")
j2 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/CANESM/rcp/45/antar_years.txt")
j3 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/CANESM/rcp/antar_years.txt")
j4 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/FGOALS/rcp/45/antar_years.txt")
j5 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/FGOALS/rcp/fev_ssp_245.txt")
j6 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/IPSL/rcp/antar_years.txt")
j7 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/IPSL/rcp/fev_ssp_245.txt")
j8 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/MIROC/rcp/antar_years.txt")
j9 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/MIROC/rcp/fev_ssp_245.txt")
j10 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/EC-EARTH/rcp/45/antar_years.txt")
j11 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/EC-EARTH/rcp/antar_years.txt")
j12 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/giss/rcp/45/antar_years.txt")
j13 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/giss/rcp/45/antar_years.txt")
j14 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP5/hadgem/rcp/45/antar_years.txt")
j15 <- read.table("/home/leticia/Documentos/leticia_dados_CMIP/dados_CMIP6/hadgem/rcp/45/fev_ssp_245.txt")

#-----------------------------------------------------------------------------------------------------------------
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
ic <- data.frame(ano = i2$V2, mes=i2$V1,#BESM5 = i1$V4,
                 GDFL5 = i2$V4, 
                 MPI5 = i3$V4, 
                 NCAR5 = i4$V4, 
                 MIROC5 = j8$V4,
                 IPSL5 = j6$V4,
                 ACCESS5 = j$V4,
                 CANESM5 = j2$V4,
                 FGOALS5 = j4$V4,
                 giss5= j12$V4,
                 had5 = j14$V4,
                 earth5 = j10$V4,
                 cmip5 = i8$V4)
ice_fev <- ic[ic$mes == "FEB",]
ice_fev$mes <- NULL
#---- FEVEREIRO CMIP6
ic1 <- data.frame(ano = i5$V2, mes=i5$V1, 
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
                  cmip6 = i9$V4)
ice_fev1 <- ic1[ic1$mes == "FEB",]
ice_fev1$mes <- NULL
ice_sic <- merge(ice_fev,ice_fev1, all.x = TRUE)

ice_sic

#----
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
ice_sic$cmip5 <- as.numeric(as.character(ice_sic$cmip5))
ice_sic$cmip6 <- as.numeric(as.character(ice_sic$cmip6))
ice_sic$ano <-as.numeric(as.character(ice_sic$ano))
#ice_sic$extensao <- as.numeric(as.character(ice_sic$extensao))

sea_ice <- ggplot(ice_sic,aes(group = 1)) +
  geom_line(aes(x=ano,y=ACCESS5, colour= "ACCESS1-0",linetype= "ACCESS1-0"))+
  geom_line(aes(x=ano,y=ACCESS6, colour= "ACCESS-ESM1-5",linetype= "ACCESS-ESM1-5"))+
  geom_line(aes(x=ano,y=CANESM5, colour= "CanESM2",linetype= "CanESM2"))+
  geom_line(aes(x=ano,y=CANESM6, colour= "CanESM5",linetype= "CanESM5"))+
  geom_line(aes(x=ano,y=earth5, colour= "EC-EARTH",linetype= "EC-EARTH"))+
  geom_line(aes(x=ano,y=earth6, colour= "EC-Earth3",linetype= "EC-Earth3"))+
  geom_line(aes(x=ano,y=FGOALS5, colour= "FGOALS-g2",linetype= "FGOALS-g2"))+
  geom_line(aes(x=ano,y=FGOALS6, colour= "FGOALS-g3",linetype= "FGOALS-g3"))+
  geom_line(aes(x=ano,y=GDFL5, colour= "GFDL-CM3",linetype= "GFDL-CM3"))+
  geom_line(aes(x=ano,y=GDFL6, colour= "GFDL-CM4",linetype= "GFDL-CM4"))+
  geom_line(aes(x=ano,y=giss5, colour= "GISS-E2-H",linetype= "GISS-E2-H"))+
  geom_line(aes(x=ano,y=giss6, colour= "GISS-E2-1-H",linetype= "GISS-E2-1-H"))+
  geom_line(aes(x=ano,y=had5, colour= "HadGEM2-CC",linetype= "HadGEM2-CC"))+
  geom_line(aes(x=ano,y=had6, colour= "HadGEM3-GC31-LL",linetype= "HadGEM3-GC31-LL"))+
  geom_line(aes(x=ano,y=IPSL5, colour= "IPSL-CM5A-LR",linetype= "IPSL-CM5A-LR"))+
  geom_line(aes(x=ano,y=IPSL6, colour= "IPSL-CM6A-LR",linetype= "IPSL-CM6A-LR"))+
  geom_line(aes(x=ano,y=MIROC5, colour= "MIROC5",linetype= "MIROC5"))+
  geom_line(aes(x=ano,y=MIROC6, colour= "MIROC6",linetype= "MIROC6"))+
  geom_line(aes(x=ano,y=MPI5, colour= "MPI-ESM-LR",linetype= "MPI-ESM-LR"))+
  geom_line(aes(x=ano,y=MPI6, colour= "MPI-ESM1.2-LR",linetype= "MPI-ESM1.2-LR"))+
  geom_line(aes(x=ano,y=NCAR5, colour= "NCAR-CCSM4",linetype= "NCAR-CCSM4"))+
  geom_line(aes(x=ano,y=NCAR6, colour= "NCAR-CESM2",linetype= "NCAR-CESM2"))+
  geom_line(aes(x=ano,y=cmip5, colour= 'CMIP5 ensemble',linetype= 'CMIP5 ensemble'),size=1)+
  geom_line(aes(x=ano,y=cmip6, colour= "CMIP6 ensemble",linetype= "CMIP6 ensemble"),size=1)+
  # geom_line(aes(group = meses, linetype=meses,colour = meses),size=0.6)+
  scale_colour_manual(breaks = c('ACCESS1-0','ACCESS-ESM1-5',
                                 'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                 'GISS-E2-H','GISS-E2-1-H', 'HadGEM2-CC','HadGEM3-GC31-LL',
                                 'IPSL-CM5A-LR', 'IPSL-CM6A-LR', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                 'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2'
                                 ,'CMIP5 ensemble','CMIP6 ensemble'),
                      values = c('ACCESS1-0'='hotpink1',
                                 'ACCESS-ESM1-5'='hotpink2',
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
                                 'HadGEM2-CC'="gray49",
                                 'HadGEM3-GC31-LL'="gray29",
                                 'IPSL-CM5A-LR'='darkorange', 
                                 'IPSL-CM6A-LR'='darkorange3',
                                 'MIROC5'='purple2',
                                 'MIROC6'='purple4', 
                                 'MPI-ESM-LR'="darkslategray2",
                                 'MPI-ESM1.2-LR'="darkslategray3",
                                 'NCAR-CCSM4'='springgreen1',
                                 'NCAR-CESM2'='springgreen2',
                                 'CMIP5 ensemble'='black',
                                 'CMIP6 ensemble'='black'),
                      name="") +
  scale_linetype_manual(breaks = c('ACCESS1-0','ACCESS-ESM1-5',
                                   'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                   'GISS-E2-H','GISS-E2-1-H', 'HadGEM2-CC','HadGEM3-GC31-LL',
                                   'IPSL-CM5A-LR', 'IPSL-CM6A-LR', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                   'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2'
                                   ,'CMIP5 ensemble','CMIP6 ensemble'),
                        values =  c('ACCESS1-0'='dashed',
                                    'ACCESS-ESM1-5'='solid',
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
                                    'HadGEM2-CC'="dashed",
                                    'HadGEM3-GC31-LL'="solid",
                                    'IPSL-CM5A-LR'='dashed', 
                                    'IPSL-CM6A-LR'='solid',
                                    'MIROC5'='dashed',
                                    'MIROC6'='solid', 
                                    'MPI-ESM-LR'="dashed",
                                    'MPI-ESM1.2-LR'="solid",
                                    'NCAR-CCSM4'='dashed',
                                    'NCAR-CESM2'='solid',
                                    'CMIP5 ensemble'='dashed',
                                    'CMIP6 ensemble'='solid'),
                        name="")+
  guides(linetype = guide_legend(override.aes = list(size = 0.3)))+
  labs(x = "a) Fevereiro", 
       y = "") + 
  scale_y_continuous(limits=c(-1,10),breaks = c(0,5,10)) + 
  scale_x_continuous(limits=c(2005,2100),breaks = c(2025,2050,2075,2100)) +
  theme_classic(base_size = 16) +
  theme(legend.text=element_text(size=8),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                    linetype="solid"))
sea_ice

#-----------------------------------------------------------------------------------------------------------------
#-------------------------------------ANTÁRTICA FEVEREIRO RCP8.5 E SSP585
#-----------------------------------------------------------------------------------------------------------------

#---- FEVEREIRO CMIP5
#---- FEVEREIRO CMIP5
ic <- data.frame(ano = i2$V2, mes=i2$V1,#BESM5 = i1$V4,
                 GDFL5 = i2$V4, 
                 MPI5 = i3$V4, 
                 NCAR5 = i4$V4, 
                 MIROC5 = j8$V4,
                 IPSL5 = j6$V4,
                 ACCESS5 = j$V4,
                 CANESM5 = j2$V4,
                 FGOALS5 = j4$V4,
                 giss5= j12$V4,
                 had5 = j14$V4,
                 earth5 = j10$V4,
                 cmip5 = i8$V4)
ice_fev <- ic[ic$mes == "SEP",]
ice_fev$mes <- NULL
#---- FEVEREIRO CMIP6
ic1 <- data.frame(ano = i5$V2, mes=i5$V1, 
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
                  cmip6 = i9$V4)
ice_fev1 <- ic1[ic1$mes == "SEP",]
ice_fev1$mes <- NULL
ice_sic <- merge(ice_fev,ice_fev1, all.x = TRUE)

ice_sic

#----
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
ice_sic$cmip5 <- as.numeric(as.character(ice_sic$cmip5))
ice_sic$cmip6 <- as.numeric(as.character(ice_sic$cmip6))
ice_sic$ano <-as.numeric(as.character(ice_sic$ano))
#ice_sic$extensao <- as.numeric(as.character(ice_sic$extensao))

sea_ice1 <- ggplot(ice_sic,aes(group = 1)) +
  geom_line(aes(x=ano,y=ACCESS5, colour= "ACCESS1-0",linetype= "ACCESS1-0"))+
  geom_line(aes(x=ano,y=ACCESS6, colour= "ACCESS-ESM1-5",linetype= "ACCESS-ESM1-5"))+
  geom_line(aes(x=ano,y=CANESM5, colour= "CanESM2",linetype= "CanESM2"))+
  geom_line(aes(x=ano,y=CANESM6, colour= "CanESM5",linetype= "CanESM5"))+
  geom_line(aes(x=ano,y=earth5, colour= "EC-EARTH",linetype= "EC-EARTH"))+
  geom_line(aes(x=ano,y=earth6, colour= "EC-Earth3",linetype= "EC-Earth3"))+
  geom_line(aes(x=ano,y=FGOALS5, colour= "FGOALS-g2",linetype= "FGOALS-g2"))+
  geom_line(aes(x=ano,y=FGOALS6, colour= "FGOALS-g3",linetype= "FGOALS-g3"))+
  geom_line(aes(x=ano,y=GDFL5, colour= "GFDL-CM3",linetype= "GFDL-CM3"))+
  geom_line(aes(x=ano,y=GDFL6, colour= "GFDL-CM4",linetype= "GFDL-CM4"))+
  geom_line(aes(x=ano,y=giss5, colour= "GISS-E2-H",linetype= "GISS-E2-H"))+
  geom_line(aes(x=ano,y=giss6, colour= "GISS-E2-1-H",linetype= "GISS-E2-1-H"))+
  geom_line(aes(x=ano,y=had5, colour= "HadGEM2-CC",linetype= "HadGEM2-CC"))+
  geom_line(aes(x=ano,y=had6, colour= "HadGEM3-GC31-LL",linetype= "HadGEM3-GC31-LL"))+
  geom_line(aes(x=ano,y=IPSL5, colour= "IPSL-CM5A-LR",linetype= "IPSL-CM5A-LR"))+
  geom_line(aes(x=ano,y=IPSL6, colour= "IPSL-CM6A-LR",linetype= "IPSL-CM6A-LR"))+
  geom_line(aes(x=ano,y=MIROC5, colour= "MIROC5",linetype= "MIROC5"))+
  geom_line(aes(x=ano,y=MIROC6, colour= "MIROC6",linetype= "MIROC6"))+
  geom_line(aes(x=ano,y=MPI5, colour= "MPI-ESM-LR",linetype= "MPI-ESM-LR"))+
  geom_line(aes(x=ano,y=MPI6, colour= "MPI-ESM1.2-LR",linetype= "MPI-ESM1.2-LR"))+
  geom_line(aes(x=ano,y=NCAR5, colour= "NCAR-CCSM4",linetype= "NCAR-CCSM4"))+
  geom_line(aes(x=ano,y=NCAR6, colour= "NCAR-CESM2",linetype= "NCAR-CESM2"))+
  geom_line(aes(x=ano,y=cmip5, colour= 'CMIP5 ensemble',linetype= 'CMIP5 ensemble'),size=1)+
  geom_line(aes(x=ano,y=cmip6, colour= "CMIP6 ensemble",linetype= "CMIP6 ensemble"),size=1)+
  # geom_line(aes(group = meses, linetype=meses,colour = meses),size=0.6)+
  scale_colour_manual(breaks = c('ACCESS1-0','ACCESS-ESM1-5',
                                 'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                 'GISS-E2-H','GISS-E2-1-H', 'HadGEM2-CC','HadGEM3-GC31-LL',
                                 'IPSL-CM5A-LR', 'IPSL-CM6A-LR', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                 'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2'
                                 ,'CMIP5 ensemble','CMIP6 ensemble'),
                      values = c('ACCESS1-0'='hotpink1',
                                 'ACCESS-ESM1-5'='hotpink2',
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
                                 'HadGEM2-CC'="gray49",
                                 'HadGEM3-GC31-LL'="gray29",
                                 'IPSL-CM5A-LR'='darkorange', 
                                 'IPSL-CM6A-LR'='darkorange3',
                                 'MIROC5'='purple2',
                                 'MIROC6'='purple4', 
                                 'MPI-ESM-LR'="darkslategray2",
                                 'MPI-ESM1.2-LR'="darkslategray3",
                                 'NCAR-CCSM4'='springgreen1',
                                 'NCAR-CESM2'='springgreen2',
                                 'CMIP5 ensemble'='black',
                                 'CMIP6 ensemble'='black'),
                      name="") +
  scale_linetype_manual(breaks = c('ACCESS1-0','ACCESS-ESM1-5',
                                   'EC-EARTH','EC-Earth3','FGOALS-g2','FGOALS-g3','GFDL-CM3','GFDL-CM4',
                                   'GISS-E2-H','GISS-E2-1-H', 'HadGEM2-CC','HadGEM3-GC31-LL',
                                   'IPSL-CM5A-LR', 'IPSL-CM6A-LR', 'MIROC5','MIROC6', 'MPI-ESM-LR',
                                   'MPI-ESM1.2-LR', 'NCAR-CCSM4','NCAR-CESM2'
                                   ,'CMIP5 ensemble','CMIP6 ensemble'),
                        values =  c('ACCESS1-0'='dashed',
                                    'ACCESS-ESM1-5'='solid',
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
                                    'HadGEM2-CC'="dashed",
                                    'HadGEM3-GC31-LL'="solid",
                                    'IPSL-CM5A-LR'='dashed', 
                                    'IPSL-CM6A-LR'='solid',
                                    'MIROC5'='dashed',
                                    'MIROC6'='solid', 
                                    'MPI-ESM-LR'="dashed",
                                    'MPI-ESM1.2-LR'="solid",
                                    'NCAR-CCSM4'='dashed',
                                    'NCAR-CESM2'='solid',
                                    'CMIP5 ensemble'='dashed',
                                    'CMIP6 ensemble'='solid'),
                        name="")+
  guides(linetype = guide_legend(override.aes = list(size = 0.3)))+
  labs(x = "b) Setembro", 
       y = "") + 
  scale_y_continuous(limits=c(-1,25),breaks = c(0,5,10,15,20,25)) + 
  scale_x_continuous(limits=c(2005,2100),breaks = c(2025,2050,2075,2100)) +
  theme_classic(base_size = 16) +
  theme(legend.text=element_text(size=8),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(fill=NA,color="black", size=1.1, 
                                    linetype="solid"))
sea_ice1

legend <- get_legend(sea_ice1)












