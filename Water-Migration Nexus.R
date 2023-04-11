## Attaching Required Libraries

library (readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
library(plm)
library(Hmisc)
library(ggstatsplot)
library(data.table)
library(xtable)


## Loading the final dataset for water-migration relationship across the countries

WMNCA_final <- read_excel("D:\\HSE\\R Projects\\Water-Migration Nexus\\Final Dataset_Water-Migration Nexus.xlsx",
                          sheet = "FinalImpdata", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric", "numeric", "numeric"))

view(WMNCA_final)


## Developing the function for getting correlation matrix of the final imputted dataset

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  require(xtable)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

Cormat_final <- corstars(WMNCA_final[,6:17], method = "pearson", removeTriangle = "upper")

print(Cormat_final)
write.csv(Cormat_final, file = "Correlation Table_FinalImputted.csv")

## Doing the Correlation Analysis with the control variables. As the control variables are qualitative data, spearman method should be applied

Cormat_final_sp <- corstars(WMNCA_final[,4:17], method = "spearman", removeTriangle = "upper")

print(Cormat_final_sp)
write.csv(Cormat_final_sp, file = "Correlation Table_FinalImputted_spearman.csv")


## Now exploring the dataset by filtering the dataset on the basis of control variables

# No Stress, Origin, Least Developed

NOL <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "0", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NOL)

NOL_PD_NE_plot <-  ggplot(data=NOL, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL_PD_NE_plot)

NOL_GDPPC_NE_plot <-  ggplot(data=NOL, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL_GDPPC_NE_plot)

NOL_HDI_NE_plot <-  ggplot(data=NOL, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL_HDI_NE_plot)

NOL_NRI_NE_plot <-  ggplot(data=NOL, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL_NRI_NE_plot)

NOL_TPASDW_NE_plot <-  ggplot(data=NOL, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL_TPASDW_NE_plot)

NOL_TAPND_NE_plot <-  ggplot(data=NOL, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(NOL_PD_NE_plot, NOL_GDPPC_NE_plot, NOL_HDI_NE_plot, NOL_NRI_NE_plot, NOL_TPASDW_NE_plot, NOL_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# No Stress, Origin, Less Developed

NOL1 <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "0", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NOL1)

NOL1_PD_NE_plot <-  ggplot(data=NOL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL1_PD_NE_plot)

NOL1_GDPPC_NE_plot <-  ggplot(data=NOL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL1_GDPPC_NE_plot)

NOL1_HDI_NE_plot <-  ggplot(data=NOL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL1_HDI_NE_plot)

NOL1_NRI_NE_plot <-  ggplot(data=NOL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL1_NRI_NE_plot)

NOL1_TPASDW_NE_plot <-  ggplot(data=NOL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL1_TPASDW_NE_plot)

NOL1_TAPND_NE_plot <-  ggplot(data=NOL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average Number of Total Affected People by Natural Disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(NOL1_PD_NE_plot, NOL1_GDPPC_NE_plot, NOL1_HDI_NE_plot, NOL1_NRI_NE_plot, NOL1_TPASDW_NE_plot, NOL1_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# No Stress, Origin, More Developed

NOM <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "0", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))

View(NOM)

NOM_PD_NE_plot <-  ggplot(data=NOM, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOM_PD_NE_plot)

NOM_GDPPC_NE_plot <-  ggplot(data=NOM, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOM_GDPPC_NE_plot)

NOM_HDI_NE_plot <-  ggplot(data=NOM, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOM_HDI_NE_plot)

NOM_NRI_NE_plot <-  ggplot(data=NOM, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOM_NRI_NE_plot)

NOM_TPASDW_NE_plot <-  ggplot(data=NOM, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (JMP) (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOM_TPASDW_NE_plot)

NOM_TAPND_NE_plot <-  ggplot(data=NOM, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average Number of Total Affected People by Natural Disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NOM_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(NOM_PD_NE_plot, NOM_GDPPC_NE_plot, NOM_HDI_NE_plot, NOM_NRI_NE_plot, NOM_TPASDW_NE_plot, NOM_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# No Stress, Destination, Least Developed

# NI

NDL <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "1", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NDL)

NDL_PD_NI_plot <-  ggplot(data=NDL, aes(x= PD
                                        ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_PD_NI_plot)

NDL_GDPPC_NI_plot <-  ggplot(data=NDL, aes(x= GDPPC
                                           ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_GDPPC_NI_plot)

NDL_HDI_NI_plot <-  ggplot(data=NDL, aes(x= HDI
                                         ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_HDI_NI_plot)

NDL_NRI_NI_plot <-  ggplot(data=NDL, aes(x= NRI
                                         ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_NRI_NI_plot)

NDL_TPASDW_NI_plot <-  ggplot(data=NDL, aes(x= TPASDW
                                            ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_TPASDW_NI_plot)

NDL_TAPND_NI_plot <-  ggplot(data=NDL, aes(x= TAPND
                                           ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(NDL_PD_NI_plot, NDL_GDPPC_NI_plot, NDL_HDI_NI_plot, NDL_NRI_NI_plot, NDL_TPASDW_NI_plot, NDL_TAPND_NI_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# IMS_BS

NDL_PD_IMS_plot <-  ggplot(data=NDL, aes(x= PD
                                         ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_PD_IMS_plot)

NDL_GDPPC_IMS_plot <-  ggplot(data=NDL, aes(x= GDPPC
                                            ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_GDPPC_IMS_plot)

NDL_HDI_IMS_plot <-  ggplot(data=NDL, aes(x= HDI
                                          ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_HDI_IMS_plot)

NDL_NRI_IMS_plot <-  ggplot(data=NDL, aes(x= NRI
                                          ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_NRI_IMS_plot)

NDL_TPASDW_IMS_plot <-  ggplot(data=NDL, aes(x= TPASDW
                                             ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_TPASDW_IMS_plot)

NDL_TAPND_IMS_plot <-  ggplot(data=NDL, aes(x= TAPND
                                            ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL_TAPND_IMS_plot)


ggstatsplot::combine_plots(
  plotlist = list(
    NDL_PD_IMS_plot, NDL_GDPPC_IMS_plot, NDL_HDI_IMS_plot, NDL_NRI_IMS_plot, NDL_TPASDW_IMS_plot, NDL_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

## No Stress, Destination, Less Developed

# NI

NDL1 <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "1", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NDL1)

NDL1_PD_NI_plot <-  ggplot(data=NDL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_PD_NI_plot)

NDL1_GDPPC_NI_plot <-  ggplot(data=NDL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_GDPPC_NI_plot)

NDL1_HDI_NI_plot <-  ggplot(data=NDL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_HDI_NI_plot)

NDL1_NRI_NI_plot <-  ggplot(data=NDL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_NRI_NI_plot)

NDL1_TPASDW_NI_plot <-  ggplot(data=NDL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_TPASDW_NI_plot)

NDL1_TAPND_NI_plot <-  ggplot(data=NDL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(NDL1_PD_NI_plot, NDL1_GDPPC_NI_plot, NDL1_HDI_NI_plot, NDL1_NRI_NI_plot, NDL1_TPASDW_NI_plot, NDL1_TAPND_NI_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# IMS_BS

NDL1_PD_IMS_plot <-  ggplot(data=NDL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_PD_IMS_plot)

NDL1_GDPPC_IMS_plot <-  ggplot(data=NDL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_GDPPC_IMS_plot)

NDL1_HDI_IMS_plot <-  ggplot(data=NDL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_HDI_IMS_plot)

NDL1_NRI_IMS_plot <-  ggplot(data=NDL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_NRI_IMS_plot)

NDL1_TPASDW_IMS_plot <-  ggplot(data=NDL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_TPASDW_IMS_plot)

NDL1_TAPND_IMS_plot <-  ggplot(data=NDL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    NDL1_PD_IMS_plot, NDL1_GDPPC_IMS_plot, NDL1_HDI_IMS_plot, NDL1_NRI_IMS_plot, NDL1_TPASDW_IMS_plot, NDL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


## No Stress, Destination, More Developed

# NI

NDM <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "1", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NDM)

NDM_PD_NI_plot <-  ggplot(data=NDM, aes(x= PD
                                        ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_PD_NI_plot)

NDM_GDPPC_NI_plot <-  ggplot(data=NDM, aes(x= GDPPC
                                           ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_GDPPC_NI_plot)

NDM_HDI_NI_plot <-  ggplot(data=NDM, aes(x= HDI
                                         ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_HDI_NI_plot)

NDM_NRI_NI_plot <-  ggplot(data=NDM, aes(x= NRI
                                         ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_NRI_NI_plot)

NDM_TPASDW_NI_plot <-  ggplot(data=NDM, aes(x= TPASDW
                                            ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_TPASDW_NI_plot)

NDM_TAPND_NI_plot <-  ggplot(data=NDM, aes(x= TAPND
                                           ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(NDM_PD_NI_plot, NDM_GDPPC_NI_plot, NDM_HDI_NI_plot, NDM_NRI_NI_plot, NDM_TPASDW_NI_plot, NDM_TAPND_NI_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# IMS_BS

NDM_PD_IMS_plot <-  ggplot(data=NDM, aes(x= PD
                                         ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_PD_IMS_plot)

NDM_GDPPC_IMS_plot <-  ggplot(data=NDM, aes(x= GDPPC
                                            ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_GDPPC_IMS_plot)

NDM_HDI_IMS_plot <-  ggplot(data=NDM, aes(x= HDI
                                          ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_HDI_IMS_plot)

NDM_NRI_IMS_plot <-  ggplot(data=NDM, aes(x= NRI
                                          ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_NRI_IMS_plot)

NDM_TPASDW_IMS_plot <-  ggplot(data=NDM, aes(x= TPASDW
                                             ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_TPASDW_IMS_plot)

NDM_TAPND_IMS_plot <-  ggplot(data=NDM, aes(x= TAPND
                                            ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NDM_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(NDM_PD_IMS_plot, NDM_GDPPC_IMS_plot, NDM_HDI_IMS_plot, NDM_NRI_IMS_plot, NDM_TPASDW_IMS_plot, NDM_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# No Stress, Both, Least Developed

NBL <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "2", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NBL) # No data in this case

# No Stress, Origin, Less Developed

NBL1 <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "2", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NBL1) # Only one data available, so it is not possible to plot

# No Stress, Both, More Developed

NBM <- dplyr::select(filter(WMNCA_final, WSL == "0", OD == "2", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(NBM)

# NE

NBM_PD_NE_plot <-  ggplot(data=NBM, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_PD_NE_plot)

NBM_GDPPC_NE_plot <-  ggplot(data=NBM, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_GDPPC_NE_plot)

NBM_HDI_NE_plot <-  ggplot(data=NBM, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_HDI_NE_plot)

NBM_NRI_NE_plot <-  ggplot(data=NBM, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_NRI_NE_plot)

NBM_TPASDW_NE_plot <-  ggplot(data=NBM, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_TPASDW_NE_plot)

NBM_TAPND_NE_plot <-  ggplot(data=NBM, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(NBM_PD_NE_plot, NBM_GDPPC_NE_plot, NBM_HDI_NE_plot, NBM_NRI_NE_plot, NBM_TPASDW_NE_plot, NBM_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# NI

NBM_PD_NI_plot <-  ggplot(data=NBM, aes(x= PD
                                        ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_PD_NI_plot)

NBM_GDPPC_NI_plot <-  ggplot(data=NBM, aes(x= GDPPC
                                           ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_GDPPC_NI_plot)

NBM_HDI_NI_plot <-  ggplot(data=NBM, aes(x= HDI
                                         ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_HDI_NI_plot)

NBM_NRI_NI_plot <-  ggplot(data=NBM, aes(x= NRI
                                         ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_NRI_NI_plot)

NBM_TPASDW_NI_plot <-  ggplot(data=NBM, aes(x= TPASDW
                                            ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_TPASDW_NI_plot)

NBM_TAPND_NI_plot <-  ggplot(data=NBM, aes(x= TAPND
                                           ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    NBM_PD_NI_plot, NBM_GDPPC_NI_plot, NBM_HDI_NI_plot, NBM_NRI_NI_plot, NBM_TPASDW_NI_plot, NBM_TAPND_NI_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

NBM_PD_IMS_plot <-  ggplot(data=NBM, aes(x= PD
                                         ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_PD_IMS_plot)

NBM_GDPPC_IMS_plot <-  ggplot(data=NBM, aes(x= GDPPC
                                            ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_GDPPC_IMS_plot)

NBM_HDI_IMS_plot <-  ggplot(data=NBM, aes(x= HDI
                                          ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_HDI_IMS_plot)

NBM_NRI_IMS_plot <-  ggplot(data=NBM, aes(x= NRI
                                          ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_NRI_IMS_plot)

NBM_TPASDW_IMS_plot <-  ggplot(data=NBM, aes(x= TPASDW
                                             ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_TPASDW_IMS_plot)

NBM_TAPND_IMS_plot <-  ggplot(data=NBM, aes(x= TAPND
                                            ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(NBM_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    NBM_PD_IMS_plot, NBM_GDPPC_IMS_plot, NBM_HDI_IMS_plot, NBM_NRI_IMS_plot, NBM_TPASDW_IMS_plot, NBM_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# Low Stress, Origin, Least Developed

LOL <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "0", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LOL)

LOL_PD_NE_plot <-  ggplot(data=LOL, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL_PD_NE_plot)

LOL_GDPPC_NE_plot <-  ggplot(data=LOL, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL_GDPPC_NE_plot)

LOL_HDI_NE_plot <-  ggplot(data=LOL, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL_HDI_NE_plot)

LOL_NRI_NE_plot <-  ggplot(data=LOL, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL_NRI_NE_plot)

LOL_TPASDW_NE_plot <-  ggplot(data=LOL, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL_TPASDW_NE_plot)

LOL_TAPND_NE_plot <-  ggplot(data=LOL, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(LOL_PD_NE_plot, LOL_GDPPC_NE_plot, LOL_HDI_NE_plot, LOL_NRI_NE_plot, LOL_TPASDW_NE_plot, LOL_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Low Stress, Origin, Less Developed

LOL1 <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "0", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LOL1)

LOL1_PD_NE_plot <-  ggplot(data=LOL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL1_PD_NE_plot)

LOL1_GDPPC_NE_plot <-  ggplot(data=LOL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL1_GDPPC_NE_plot)

LOL1_HDI_NE_plot <-  ggplot(data=LOL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL1_HDI_NE_plot)

LOL1_NRI_NE_plot <-  ggplot(data=LOL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL1_NRI_NE_plot)

LOL1_TPASDW_NE_plot <-  ggplot(data=LOL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL1_TPASDW_NE_plot)

LOL1_TAPND_NE_plot <-  ggplot(data=LOL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(LOL1_PD_NE_plot, LOL1_GDPPC_NE_plot, LOL1_HDI_NE_plot, LOL1_NRI_NE_plot, LOL1_TPASDW_NE_plot, LOL1_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Low Stress, Origin, More Developed

LOM <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "0", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LOM)

LOM_PD_NE_plot <-  ggplot(data=LOM, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOM_PD_NE_plot)

LOM_GDPPC_NE_plot <-  ggplot(data=LOM, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOM_GDPPC_NE_plot)

LOM_HDI_NE_plot <-  ggplot(data=LOM, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOM_HDI_NE_plot)

LOM_NRI_NE_plot <-  ggplot(data=LOM, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOM_NRI_NE_plot)

LOM_TPASDW_NE_plot <-  ggplot(data=LOM, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOM_TPASDW_NE_plot)

LOM_TAPND_NE_plot <-  ggplot(data=LOM, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LOM_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(LOM_PD_NE_plot, LOM_GDPPC_NE_plot, LOM_HDI_NE_plot, LOM_NRI_NE_plot, LOM_TPASDW_NE_plot, LOM_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# Low Stress, Destination, Least Developed

LDL <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "1", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LDL) # Only one value is available in this case, so plotting is not possible


# Low Stress, Destination, Less Developed

LDL1 <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "1", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LDL1)

LDL1_PD_NI_plot <-  ggplot(data=LDL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_PD_NI_plot)

LDL1_GDPPC_NI_plot <-  ggplot(data=LDL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_GDPPC_NI_plot)

LDL1_HDI_NI_plot <-  ggplot(data=LDL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_HDI_NI_plot)

LDL1_NRI_NI_plot <-  ggplot(data=LDL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_NRI_NI_plot)

LDL1_TPASDW_NI_plot <-  ggplot(data=LDL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_TPASDW_NI_plot)

LDL1_TAPND_NI_plot <-  ggplot(data=LDL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(LDL1_PD_NI_plot, LDL1_GDPPC_NI_plot, LDL1_HDI_NI_plot, LDL1_NRI_NI_plot, LDL1_TPASDW_NI_plot, LDL1_TAPND_NI_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

LDL1_PD_IMS_plot <-  ggplot(data=LDL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_PD_IMS_plot)

LDL1_GDPPC_IMS_plot <-  ggplot(data=LDL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_GDPPC_IMS_plot)

LDL1_HDI_IMS_plot <-  ggplot(data=LDL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_HDI_IMS_plot)

LDL1_NRI_IMS_plot <-  ggplot(data=LDL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_NRI_IMS_plot)

LDL1_TPASDW_IMS_plot <-  ggplot(data=LDL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_TPASDW_IMS_plot)

LDL1_TAPND_IMS_plot <-  ggplot(data=LDL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    LDL1_PD_IMS_plot, LDL1_GDPPC_IMS_plot, LDL1_HDI_IMS_plot, LDL1_NRI_IMS_plot, LDL1_TPASDW_IMS_plot, LDL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


## Low Stress, Destination, More Developed

# NI

LDM <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "1", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LDM)

LDM_PD_NI_plot <-  ggplot(data=LDM, aes(x= PD
                                        ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_PD_NI_plot)

LDM_GDPPC_NI_plot <-  ggplot(data=LDM, aes(x= GDPPC
                                           ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_GDPPC_NI_plot)

LDM_HDI_NI_plot <-  ggplot(data=LDM, aes(x= HDI
                                         ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_HDI_NI_plot)

LDM_NRI_NI_plot <-  ggplot(data=LDM, aes(x= NRI
                                         ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_NRI_NI_plot)

LDM_TPASDW_NI_plot <-  ggplot(data=LDM, aes(x= TPASDW
                                            ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_TPASDW_NI_plot)

LDM_TAPND_NI_plot <-  ggplot(data=LDM, aes(x= TAPND
                                           ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(LDM_PD_NI_plot, LDM_GDPPC_NI_plot, LDM_HDI_NI_plot, LDM_NRI_NI_plot, LDM_TPASDW_NI_plot, LDM_TAPND_NI_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))



# IMS_BS

LDM_PD_IMS_plot <-  ggplot(data=LDM, aes(x= PD
                                         ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_PD_IMS_plot)

LDM_GDPPC_IMS_plot <-  ggplot(data=LDM, aes(x= GDPPC
                                            ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_GDPPC_IMS_plot)

LDM_HDI_IMS_plot <-  ggplot(data=LDM, aes(x= HDI
                                          ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_HDI_IMS_plot)

LDM_NRI_IMS_plot <-  ggplot(data=LDM, aes(x= NRI
                                          ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_NRI_IMS_plot)

LDM_TPASDW_IMS_plot <-  ggplot(data=LDM, aes(x= TPASDW
                                             ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_TPASDW_IMS_plot)

LDM_TAPND_IMS_plot <-  ggplot(data=LDM, aes(x= TAPND
                                            ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LDM_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    LDM_PD_IMS_plot, LDM_GDPPC_IMS_plot, LDM_HDI_IMS_plot, LDM_NRI_IMS_plot, LDM_TPASDW_IMS_plot, LDM_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Low Stress, Both, Least Developed

LBL <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "2", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LBL) # No value available

# Low Stress, Both, Less Developed

LBL1 <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "2", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LBL1)

# NE

LBL1_PD_NE_plot <-  ggplot(data=LBL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_PD_NE_plot)

LBL1_GDPPC_NE_plot <-  ggplot(data=LBL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_GDPPC_NE_plot)

LBL1_HDI_NE_plot <-  ggplot(data=LBL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_HDI_NE_plot)

LBL1_NRI_NE_plot <-  ggplot(data=LBL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_NRI_NE_plot)

LBL1_TPASDW_NE_plot <-  ggplot(data=LBL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_TPASDW_NE_plot)

LBL1_TAPND_NE_plot <-  ggplot(data=LBL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(LBL1_PD_NE_plot, LBL1_GDPPC_NE_plot, LBL1_HDI_NE_plot, LBL1_NRI_NE_plot, LBL1_TPASDW_NE_plot, LBL1_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# NI

LBL1_PD_NI_plot <-  ggplot(data=LBL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_PD_NI_plot)

LBL1_GDPPC_NI_plot <-  ggplot(data=LBL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_GDPPC_NI_plot)

LBL1_HDI_NI_plot <-  ggplot(data=LBL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_HDI_NI_plot)

LBL1_NRI_NI_plot <-  ggplot(data=LBL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_NRI_NI_plot)

LBL1_TPASDW_NI_plot <-  ggplot(data=LBL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_TPASDW_NI_plot)

LBL1_TAPND_NI_plot <-  ggplot(data=LBL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    LBL1_PD_NI_plot, LBL1_GDPPC_NI_plot, LBL1_HDI_NI_plot, LBL1_NRI_NI_plot, LBL1_TPASDW_NI_plot, LBL1_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# IMS_BS

LBL1_PD_IMS_plot <-  ggplot(data=LBL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_PD_IMS_plot)

LBL1_GDPPC_IMS_plot <-  ggplot(data=LBL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_GDPPC_IMS_plot)

LBL1_HDI_IMS_plot <-  ggplot(data=LBL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_HDI_IMS_plot)

LBL1_NRI_IMS_plot <-  ggplot(data=LBL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_NRI_IMS_plot)

LBL1_TPASDW_IMS_plot <-  ggplot(data=LBL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_TPASDW_IMS_plot)

LBL1_TAPND_IMS_plot <-  ggplot(data=LBL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    LBL1_PD_IMS_plot, LBL1_GDPPC_IMS_plot, LBL1_HDI_IMS_plot, LBL1_NRI_IMS_plot, LBL1_TPASDW_IMS_plot, LBL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# Low Stress, Both, More Developed

LBM <- dplyr::select(filter(WMNCA_final, WSL == "1", OD == "2", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(LBM)

# NE

LBM_PD_NE_plot <-  ggplot(data=LBM, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_PD_NE_plot)

LBM_GDPPC_NE_plot <-  ggplot(data=LBM, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_GDPPC_NE_plot)

LBM_HDI_NE_plot <-  ggplot(data=LBM, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_HDI_NE_plot)

LBM_NRI_NE_plot <-  ggplot(data=LBM, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_NRI_NE_plot)

LBM_TPASDW_NE_plot <-  ggplot(data=LBM, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_TPASDW_NE_plot)

LBM_TAPND_NE_plot <-  ggplot(data=LBM, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(LBM_PD_NE_plot, LBM_GDPPC_NE_plot, LBM_HDI_NE_plot, LBM_NRI_NE_plot, LBM_TPASDW_NE_plot, LBM_TAPND_NE_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# NI

LBM_PD_NI_plot <-  ggplot(data=LBM, aes(x= PD
                                        ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_PD_NI_plot)

LBM_GDPPC_NI_plot <-  ggplot(data=LBM, aes(x= GDPPC
                                           ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_GDPPC_NI_plot)

LBM_HDI_NI_plot <-  ggplot(data=LBM, aes(x= HDI
                                         ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_HDI_NI_plot)

LBM_NRI_NI_plot <-  ggplot(data=LBM, aes(x= NRI
                                         ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_NRI_NI_plot)

LBM_TPASDW_NI_plot <-  ggplot(data=LBM, aes(x= TPASDW
                                            ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_TPASDW_NI_plot)

LBM_TAPND_NI_plot <-  ggplot(data=LBM, aes(x= TAPND
                                           ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    LBM_PD_NI_plot, LBM_GDPPC_NI_plot, LBM_HDI_NI_plot, LBM_NRI_NI_plot, LBM_TPASDW_NI_plot, LBM_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# IMS_BS

LBM_PD_IMS_plot <-  ggplot(data=LBM, aes(x= PD
                                         ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_PD_IMS_plot)

LBM_GDPPC_IMS_plot <-  ggplot(data=LBM, aes(x= GDPPC
                                            ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_GDPPC_IMS_plot)

LBM_HDI_IMS_plot <-  ggplot(data=LBM, aes(x= HDI
                                          ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_HDI_IMS_plot)

LBM_NRI_IMS_plot <-  ggplot(data=LBM, aes(x= NRI
                                          ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_NRI_IMS_plot)

LBM_TPASDW_IMS_plot <-  ggplot(data=LBM, aes(x= TPASDW
                                             ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_TPASDW_IMS_plot)

LBM_TAPND_IMS_plot <-  ggplot(data=LBM, aes(x= TAPND
                                            ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(LBM_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    LBM_PD_IMS_plot, LBM_GDPPC_IMS_plot, LBM_HDI_IMS_plot, LBM_NRI_IMS_plot, LBM_TPASDW_IMS_plot, LBM_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Medium Stress, Origin, Least Developed

MOL <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "0", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MOL)

MOL_PD_NE_plot <-  ggplot(data=MOL, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL_PD_NE_plot)

MOL_GDPPC_NE_plot <-  ggplot(data=MOL, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL_GDPPC_NE_plot)

MOL_HDI_NE_plot <-  ggplot(data=MOL, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL_HDI_NE_plot)

MOL_NRI_NE_plot <-  ggplot(data=MOL, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL_NRI_NE_plot)

MOL_TPASDW_NE_plot <-  ggplot(data=MOL, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL_TPASDW_NE_plot)

MOL_TAPND_NE_plot <-  ggplot(data=MOL, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(MOL_PD_NE_plot, MOL_GDPPC_NE_plot, MOL_HDI_NE_plot, MOL_NRI_NE_plot, MOL_TPASDW_NE_plot, MOL_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# Medium Stress, Origin, Less Developed

MOL1 <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "0", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MOL1)

MOL1_PD_NE_plot <-  ggplot(data=MOL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL1_PD_NE_plot)

MOL1_GDPPC_NE_plot <-  ggplot(data=MOL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL1_GDPPC_NE_plot)

MOL1_HDI_NE_plot <-  ggplot(data=MOL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL1_HDI_NE_plot)

MOL1_NRI_NE_plot <-  ggplot(data=MOL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL1_NRI_NE_plot)

MOL1_TPASDW_NE_plot <-  ggplot(data=MOL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL1_TPASDW_NE_plot)

MOL1_TAPND_NE_plot <-  ggplot(data=MOL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MOL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(MOL1_PD_NE_plot, MOL1_GDPPC_NE_plot, MOL1_HDI_NE_plot, MOL1_NRI_NE_plot, MOL1_TPASDW_NE_plot, MOL1_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Medium Stress, Origin, More Developed

MOM <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "0", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MOM)  # No data available in this case

# Medium Stress, Destination, Least Developed

MDL <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "1", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MDL) # No data available in this case

# Medium Stress, Destination, Less Developed

MDL1 <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "1", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MDL1)

# NI

MDL1_PD_NI_plot <-  ggplot(data=MDL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_PD_NI_plot)

MDL1_GDPPC_NI_plot <-  ggplot(data=MDL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_GDPPC_NI_plot)

MDL1_HDI_NI_plot <-  ggplot(data=MDL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_HDI_NI_plot)

MDL1_NRI_NI_plot <-  ggplot(data=MDL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_NRI_NI_plot)

MDL1_TPASDW_NI_plot <-  ggplot(data=MDL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_TPASDW_NI_plot)

MDL1_TAPND_NI_plot <-  ggplot(data=MDL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(MDL1_PD_NI_plot, MDL1_GDPPC_NI_plot, MDL1_HDI_NI_plot, MDL1_NRI_NI_plot, MDL1_TPASDW_NI_plot, MDL1_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

MDL1_PD_IMS_plot <-  ggplot(data=MDL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_PD_IMS_plot)

MDL1_GDPPC_IMS_plot <-  ggplot(data=MDL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_GDPPC_IMS_plot)

MDL1_HDI_IMS_plot <-  ggplot(data=MDL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_HDI_IMS_plot)

MDL1_NRI_IMS_plot <-  ggplot(data=MDL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_NRI_IMS_plot)

MDL1_TPASDW_IMS_plot <-  ggplot(data=MDL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_TPASDW_IMS_plot)

MDL1_TAPND_IMS_plot <-  ggplot(data=MDL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MDL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    MDL1_PD_IMS_plot, MDL1_GDPPC_IMS_plot, MDL1_HDI_IMS_plot, MDL1_NRI_IMS_plot, MDL1_TPASDW_IMS_plot, MDL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Medium Stress, Destination, More Developed

MDM <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "1", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MDM) # Only one value available in this case

# Medium Stress, Both, Least Developed

MBL <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "2", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MBL) # No data available in this case

# Medium Stress, Both, Less Developed

MBL1 <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "2", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MBL1)

# NE

MBL1_PD_NE_plot <-  ggplot(data=MBL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_PD_NE_plot)

MBL1_GDPPC_NE_plot <-  ggplot(data=MBL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_GDPPC_NE_plot)

MBL1_HDI_NE_plot <-  ggplot(data=MBL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_HDI_NE_plot)

MBL1_NRI_NE_plot <-  ggplot(data=MBL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_NRI_NE_plot)

MBL1_TPASDW_NE_plot <-  ggplot(data=MBL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_TPASDW_NE_plot)

MBL1_TAPND_NE_plot <-  ggplot(data=MBL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(MBL1_PD_NE_plot, MBL1_GDPPC_NE_plot, MBL1_HDI_NE_plot, MBL1_NRI_NE_plot, MBL1_TPASDW_NE_plot, MBL1_TAPND_NE_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# NI

MBL1_PD_NI_plot <-  ggplot(data=MBL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_PD_NI_plot)

MBL1_GDPPC_NI_plot <-  ggplot(data=MBL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_GDPPC_NI_plot)

MBL1_HDI_NI_plot <-  ggplot(data=MBL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_HDI_NI_plot)

MBL1_NRI_NI_plot <-  ggplot(data=MBL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_NRI_NI_plot)

MBL1_TPASDW_NI_plot <-  ggplot(data=MBL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_TPASDW_NI_plot)

MBL1_TAPND_NI_plot <-  ggplot(data=MBL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    MBL1_PD_NI_plot, MBL1_GDPPC_NI_plot, MBL1_HDI_NI_plot, MBL1_NRI_NI_plot, MBL1_TPASDW_NI_plot, MBL1_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

MBL1_PD_IMS_plot <-  ggplot(data=MBL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_PD_IMS_plot)

MBL1_GDPPC_IMS_plot <-  ggplot(data=MBL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_GDPPC_IMS_plot)

MBL1_HDI_IMS_plot <-  ggplot(data=MBL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_HDI_IMS_plot)

MBL1_NRI_IMS_plot <-  ggplot(data=MBL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_NRI_IMS_plot)

MBL1_TPASDW_IMS_plot <-  ggplot(data=MBL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_TPASDW_IMS_plot)

MBL1_TAPND_IMS_plot <-  ggplot(data=MBL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    MBL1_PD_IMS_plot, MBL1_GDPPC_IMS_plot, MBL1_HDI_IMS_plot, MBL1_NRI_IMS_plot, MBL1_TPASDW_IMS_plot, MBL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# Medium Stress, Both, More Developed

MBM <- dplyr::select(filter(WMNCA_final, WSL == "2", OD == "2", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(MBM)

# NE

MBM_PD_NE_plot <-  ggplot(data=MBM, aes(x= PD
                                        ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_PD_NE_plot)

MBM_GDPPC_NE_plot <-  ggplot(data=MBM, aes(x= GDPPC
                                           ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_GDPPC_NE_plot)

MBM_HDI_NE_plot <-  ggplot(data=MBM, aes(x= HDI
                                         ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_HDI_NE_plot)

MBM_NRI_NE_plot <-  ggplot(data=MBM, aes(x= NRI
                                         ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_NRI_NE_plot)

MBM_TPASDW_NE_plot <-  ggplot(data=MBM, aes(x= TPASDW
                                            ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_TPASDW_NE_plot)

MBM_TAPND_NE_plot <-  ggplot(data=MBM, aes(x= TAPND
                                           ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(MBM_PD_NE_plot, MBM_GDPPC_NE_plot, MBM_HDI_NE_plot, MBM_NRI_NE_plot, MBM_TPASDW_NE_plot, MBM_TAPND_NE_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# NI

MBM_PD_NI_plot <-  ggplot(data=MBM, aes(x= PD
                                        ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_PD_NI_plot)

MBM_GDPPC_NI_plot <-  ggplot(data=MBM, aes(x= GDPPC
                                           ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_GDPPC_NI_plot)

MBM_HDI_NI_plot <-  ggplot(data=MBM, aes(x= HDI
                                         ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_HDI_NI_plot)

MBM_NRI_NI_plot <-  ggplot(data=MBM, aes(x= NRI
                                         ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_NRI_NI_plot)

MBM_TPASDW_NI_plot <-  ggplot(data=MBM, aes(x= TPASDW
                                            ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_TPASDW_NI_plot)

MBM_TAPND_NI_plot <-  ggplot(data=MBM, aes(x= TAPND
                                           ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    MBM_PD_NI_plot, MBM_GDPPC_NI_plot, MBM_HDI_NI_plot, MBM_NRI_NI_plot, MBM_TPASDW_NI_plot, MBM_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

MBM_PD_IMS_plot <-  ggplot(data=MBM, aes(x= PD
                                         ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_PD_IMS_plot)

MBM_GDPPC_IMS_plot <-  ggplot(data=MBM, aes(x= GDPPC
                                            ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_GDPPC_IMS_plot)

MBM_HDI_IMS_plot <-  ggplot(data=MBM, aes(x= HDI
                                          ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_HDI_IMS_plot)

MBM_NRI_IMS_plot <-  ggplot(data=MBM, aes(x= NRI
                                          ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_NRI_IMS_plot)

MBM_TPASDW_IMS_plot <-  ggplot(data=MBM, aes(x= TPASDW
                                             ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_TPASDW_IMS_plot)

MBM_TAPND_IMS_plot <-  ggplot(data=MBM, aes(x= TAPND
                                            ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(MBM_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    MBM_PD_IMS_plot, MBM_GDPPC_IMS_plot, MBM_HDI_IMS_plot, MBM_NRI_IMS_plot, MBM_TPASDW_IMS_plot, MBM_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# High Stress, Origin, Least Developed

HOL <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "0", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HOL) # No data available

# High Stress, Origin, Less Developed

HOL1 <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "0", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HOL1)

HOL1_PD_NE_plot <-  ggplot(data=HOL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HOL1_PD_NE_plot)

HOL1_GDPPC_NE_plot <-  ggplot(data=HOL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HOL1_GDPPC_NE_plot)

HOL1_HDI_NE_plot <-  ggplot(data=HOL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HOL1_HDI_NE_plot)

HOL1_NRI_NE_plot <-  ggplot(data=HOL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HOL1_NRI_NE_plot)

HOL1_TPASDW_NE_plot <-  ggplot(data=HOL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HOL1_TPASDW_NE_plot)

HOL1_TAPND_NE_plot <-  ggplot(data=HOL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HOL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(HOL1_PD_NE_plot, HOL1_GDPPC_NE_plot, HOL1_HDI_NE_plot, HOL1_NRI_NE_plot, HOL1_TPASDW_NE_plot, HOL1_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# High Stress, Origin, More Developed

HOM <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "0", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HOM) # No data available

# High Stress, Destination, Least Developed

HDL <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "1", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HDL) # No data available

# High Stress, Destination, Less Developed

HDL1 <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "1", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HDL1)

# NI

HDL1_PD_NI_plot <-  ggplot(data=HDL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_PD_NI_plot)

HDL1_GDPPC_NI_plot <-  ggplot(data=HDL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_GDPPC_NI_plot)

HDL1_HDI_NI_plot <-  ggplot(data=HDL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_HDI_NI_plot)

HDL1_NRI_NI_plot <-  ggplot(data=HDL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_NRI_NI_plot)

HDL1_TPASDW_NI_plot <-  ggplot(data=HDL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_TPASDW_NI_plot)

HDL1_TAPND_NI_plot <-  ggplot(data=HDL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(HDL1_PD_NI_plot, HDL1_GDPPC_NI_plot, HDL1_HDI_NI_plot, HDL1_NRI_NI_plot, HDL1_TPASDW_NI_plot, HDL1_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

HDL1_PD_IMS_plot <-  ggplot(data=HDL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_PD_IMS_plot)

HDL1_GDPPC_IMS_plot <-  ggplot(data=HDL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_GDPPC_IMS_plot)

HDL1_HDI_IMS_plot <-  ggplot(data=HDL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_HDI_IMS_plot)

HDL1_NRI_IMS_plot <-  ggplot(data=HDL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_NRI_IMS_plot)

HDL1_TPASDW_IMS_plot <-  ggplot(data=HDL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_TPASDW_IMS_plot)

HDL1_TAPND_IMS_plot <-  ggplot(data=HDL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(HDL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    HDL1_PD_IMS_plot, HDL1_GDPPC_IMS_plot, HDL1_HDI_IMS_plot, HDL1_NRI_IMS_plot, HDL1_TPASDW_IMS_plot, HDL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# High Stress, Destination, More Developed

HDM <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "1", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HDM)  # No Data availablein this case


# High Stress, Both, Least Developed

HBL <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "2", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HBL)  # No Data

# High Stress, Both, Less Developed

HBL1 <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "2", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HBL1) # Only one value available

# High Stress, Both, More Developed

HBM <- dplyr::select(filter(WMNCA_final, WSL == "3", OD == "2", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(HBM) # No data

# Critical, Origin, Least Developed

COL <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "0", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(COL) # No data

# Critical, Origin, Less Developed

COL1 <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "0", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(COL1)

COL1_PD_NE_plot <-  ggplot(data=COL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(COL1_PD_NE_plot)

COL1_GDPPC_NE_plot <-  ggplot(data=COL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(COL1_GDPPC_NE_plot)

COL1_HDI_NE_plot <-  ggplot(data=COL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(COL1_HDI_NE_plot)

COL1_NRI_NE_plot <-  ggplot(data=COL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(COL1_NRI_NE_plot)

COL1_TPASDW_NE_plot <-  ggplot(data=COL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(COL1_TPASDW_NE_plot)

COL1_TAPND_NE_plot <-  ggplot(data=COL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(COL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(COL1_PD_NE_plot, COL1_GDPPC_NE_plot, COL1_HDI_NE_plot, COL1_NRI_NE_plot, COL1_TPASDW_NE_plot, COL1_TAPND_NE_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Critical, Origin, More Developed

COM <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "0", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(COM) # No Data

# Critical, Destination, Least Developed

CDL <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "1", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(CDL) # No Data

# Critical, Destination, Less Developed

CDL1 <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "1", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(CDL1)

# NI

CDL1_PD_NI_plot <-  ggplot(data=CDL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_PD_NI_plot)

CDL1_GDPPC_NI_plot <-  ggplot(data=CDL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_GDPPC_NI_plot)

CDL1_HDI_NI_plot <-  ggplot(data=CDL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_HDI_NI_plot)

CDL1_NRI_NI_plot <-  ggplot(data=CDL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_NRI_NI_plot)

CDL1_TPASDW_NI_plot <-  ggplot(data=CDL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_TPASDW_NI_plot)

CDL1_TAPND_NI_plot <-  ggplot(data=CDL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(CDL1_PD_NI_plot, CDL1_GDPPC_NI_plot, CDL1_HDI_NI_plot, CDL1_NRI_NI_plot, CDL1_TPASDW_NI_plot, CDL1_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# IMS_BS

CDL1_PD_IMS_plot <-  ggplot(data=CDL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_PD_IMS_plot)

CDL1_GDPPC_IMS_plot <-  ggplot(data=CDL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_GDPPC_IMS_plot)

CDL1_HDI_IMS_plot <-  ggplot(data=CDL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_HDI_IMS_plot)

CDL1_NRI_IMS_plot <-  ggplot(data=CDL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_NRI_IMS_plot)

CDL1_TPASDW_IMS_plot <-  ggplot(data=CDL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_TPASDW_IMS_plot)

CDL1_TAPND_IMS_plot <-  ggplot(data=CDL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CDL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    CDL1_PD_IMS_plot, CDL1_GDPPC_IMS_plot, CDL1_HDI_IMS_plot, CDL1_NRI_IMS_plot, CDL1_TPASDW_IMS_plot, CDL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# Critical, Destination, More Developed

CDM <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "1", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(CDM)  # No data

# Critical, Both, Least Developed

CBL <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "2", DS == "0"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(CBL)  # No data

# Critical, Both, Less Developed

CBL1 <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "2", DS == "1"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(CBL1)

# NE

CBL1_PD_NE_plot <-  ggplot(data=CBL1, aes(x= PD
                                          ,y= NE)) + labs (x = "Population density (inhab/km2)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_PD_NE_plot)

CBL1_GDPPC_NE_plot <-  ggplot(data=CBL1, aes(x= GDPPC
                                             ,y= NE)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_GDPPC_NE_plot)

CBL1_HDI_NE_plot <-  ggplot(data=CBL1, aes(x= HDI
                                           ,y= NE)) + labs (x = "Human development index (HDI)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_HDI_NE_plot)

CBL1_NRI_NE_plot <-  ggplot(data=CBL1, aes(x= NRI
                                           ,y= NE)) + labs (x = "National rainfall index (mm/yr)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_NRI_NE_plot)

CBL1_TPASDW_NE_plot <-  ggplot(data=CBL1, aes(x= TPASDW
                                              ,y= NE)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_TPASDW_NE_plot)

CBL1_TAPND_NE_plot <-  ggplot(data=CBL1, aes(x= TAPND
                                             ,y= NE)) + labs (x = "Average number of total affected people by natural disaster", y = "Number of emigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_TAPND_NE_plot)

ggstatsplot::combine_plots(
  plotlist = list(CBL1_PD_NE_plot, CBL1_GDPPC_NE_plot, CBL1_HDI_NE_plot, CBL1_NRI_NE_plot, CBL1_TPASDW_NE_plot, CBL1_TAPND_NE_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))

# NI

CBL1_PD_NI_plot <-  ggplot(data=CBL1, aes(x= PD
                                          ,y= NI)) + labs (x = "Population density (inhab/km2)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_PD_NI_plot)

CBL1_GDPPC_NI_plot <-  ggplot(data=CBL1, aes(x= GDPPC
                                             ,y= NI)) + labs (x = "GDP per capita (current US$/inhab)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_GDPPC_NI_plot)

CBL1_HDI_NI_plot <-  ggplot(data=CBL1, aes(x= HDI
                                           ,y= NI)) + labs (x = "Human development index (HDI)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_HDI_NI_plot)

CBL1_NRI_NI_plot <-  ggplot(data=CBL1, aes(x= NRI
                                           ,y= NI)) + labs (x = "National rainfall index (mm/yr)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_NRI_NI_plot)

CBL1_TPASDW_NI_plot <-  ggplot(data=CBL1, aes(x= TPASDW
                                              ,y= NI)) + labs (x = "Total population with access to safe drinking-water (%)", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_TPASDW_NI_plot)

CBL1_TAPND_NI_plot <-  ggplot(data=CBL1, aes(x= TAPND
                                             ,y= NI)) + labs (x = "Average number of total affected People by natural disaster", y = "Number of immigrants") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_TAPND_NI_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    CBL1_PD_NI_plot, CBL1_GDPPC_NI_plot, CBL1_HDI_NI_plot, CBL1_NRI_NI_plot, CBL1_TPASDW_NI_plot, CBL1_TAPND_NI_plot
  ),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))



# IMS_BS

CBL1_PD_IMS_plot <-  ggplot(data=CBL1, aes(x= PD
                                           ,y= IMS_BS)) + labs (x = "Population density (inhab/km2)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_PD_IMS_plot)

CBL1_GDPPC_IMS_plot <-  ggplot(data=CBL1, aes(x= GDPPC
                                              ,y= IMS_BS)) + labs (x = "GDP per capita (current US$/inhab)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_GDPPC_IMS_plot)

CBL1_HDI_IMS_plot <-  ggplot(data=CBL1, aes(x= HDI
                                            ,y= IMS_BS)) + labs (x = "Human development index (HDI)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_HDI_IMS_plot)

CBL1_NRI_IMS_plot <-  ggplot(data=CBL1, aes(x= NRI
                                            ,y= IMS_BS)) + labs (x = "National rainfall index (mm/yr)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_NRI_IMS_plot)

CBL1_TPASDW_IMS_plot <-  ggplot(data=CBL1, aes(x= TPASDW
                                               ,y= IMS_BS)) + labs (x = "Total population with access to safe drinking-water (%)", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_TPASDW_IMS_plot)

CBL1_TAPND_IMS_plot <-  ggplot(data=CBL1, aes(x= TAPND
                                              ,y= IMS_BS)) + labs (x = "Average number of total affected people by natural disaster", y = "IMSBS (%)") +
  geom_smooth(method = lm) +  geom_point() +  stat_cor(label.y.npc = 0.8) + 
  stat_cor(label.y.npc = 1.0,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))
plot(CBL1_TAPND_IMS_plot)

ggstatsplot::combine_plots(
  plotlist = list(
    CBL1_PD_IMS_plot, CBL1_GDPPC_IMS_plot, CBL1_HDI_IMS_plot, CBL1_NRI_IMS_plot, CBL1_TPASDW_IMS_plot, CBL1_TAPND_IMS_plot),
  plotgrid.args = list(nrow = 3),
  annotation.args = list(
    title = ""))


# Critical, Both, More Developed

CBM <- dplyr::select(filter(WMNCA_final, WSL == "4", OD == "2", DS == "2"), c(Countries, Year, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View(CBM) # No data

## Comparing the Dependent variables with respect to the control variables

# Comparing the Origin Countries

org <- dplyr::select(filter(WMNCA_final, OD == "0"), c(Countries, Year, NE, DS, WSL))
View(org)

org_final <- org %>% mutate(DSf = recode_factor(DS, 
                                                `0` = "Least Developed",
                                                `1` = "Less Developed",
                                                `2` = "More Developed"
), WSf = recode_factor(WSL,
                       `0` = "No Stress",
                       `1` = "Low Stress",
                       `2` = "Medium Stress",
                       `3` = "High Stress",
                       `4` = "Critical"))

View(org_final)

org_DS_NE_summary <- org_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_DS = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_DS = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_DS = sd(NE)/sqrt(n())) # calculates the standard error of each group
View(org_DS_NE_summary)

org_DS_NE_plot <-  ggplot(data=org_DS_NE_summary, aes(x= DSf
                                                      ,y= mean_NE_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "Number of emigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NE_DS - sd_NE_DS, ymax = mean_NE_DS + sd_NE_DS), width=0.2)
plot(org_DS_NE_plot)


org_WS_NE_summary <- org_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_WS = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_WS = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_WS = sd(NE)/sqrt(n())) # calculates the standard error of each group

write.csv(org_WS_NE_summary, file="Summary of Water Stress Level_NE.csv")

org_WS_NE_plot <-  ggplot(data=org_WS_NE_summary, aes(x= WSf
                                                      ,y= mean_NE_WS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "Number of emigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NE_WS - sd_NE_WS, ymax = mean_NE_WS + sd_NE_WS), width=0.2)
plot(org_WS_NE_plot)

## Plotting the Number of Emigrants with respect to Development Status and Water Stress Level

# Creating dataset for No, Low, Medium, High, and Critical Stress

# No Stress

org_NE_No <- dplyr::select(filter(org_final, WSL == "0"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_No)

org_NE_No_summary <- org_NE_No %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_No = mean(NE),  # calculates the mean of each group
            sd_NE_No = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_No = sd(NE)/sqrt(n())) # calculates the standard error of each group

View(org_NE_No_summary)

org_NE_No_plot <-  ggplot(data=org_NE_No_summary, aes(x= DSf
                                                      ,y= mean_NE_No)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of Emigrants", title = "No Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+06, 11e+06) +
  geom_errorbar(aes(ymin = mean_NE_No - sd_NE_No, ymax = mean_NE_No + sd_NE_No), width=0.2)
plot(org_NE_No_plot)

# Low Stress

org_NE_Low <- dplyr::select(filter(org_final, WSL == "1"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_Low)

org_NE_Low_summary <- org_NE_Low %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_Low = mean(NE),  # calculates the mean of each group
            sd_NE_Low = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Low = sd(NE)/sqrt(n())) # calculates the standard error of each group

View(org_NE_Low_summary)

org_NE_Low_plot <-  ggplot(data=org_NE_Low_summary, aes(x= DSf
                                                        ,y= mean_NE_Low)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Low Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+06, 11e+06) +
  geom_errorbar(aes(ymin = mean_NE_Low - sd_NE_Low, ymax = mean_NE_Low + sd_NE_Low), width=0.2)
plot(org_NE_Low_plot)

# Medium Stress

org_NE_Med <- dplyr::select(filter(org_final, WSL == "2"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_Med)

org_NE_Med_summary <- org_NE_Med %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_Med = mean(NE),  # calculates the mean of each group
            sd_NE_Med = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Med = sd(NE)/sqrt(n())) # calculates the standard error of each group

View(org_NE_Med_summary)

org_NE_Med_plot <-  ggplot(data=org_NE_Med_summary, aes(x= DSf
                                                        ,y= mean_NE_Med)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development Status", y = "", title = "Medium Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+06, 11e+06) +
  geom_errorbar(aes(ymin = mean_NE_Med - sd_NE_Med, ymax = mean_NE_Med + sd_NE_Med), width=0.2)
plot(org_NE_Med_plot)

# High Stress

org_NE_High <- dplyr::select(filter(org_final, WSL == "3"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_High)

org_NE_High_summary <- org_NE_High %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_High = mean(NE),  # calculates the mean of each group
            sd_NE_High = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_High = sd(NE)/sqrt(n())) # calculates the standard error of each group

View(org_NE_High_summary)

org_NE_High_plot <-  ggplot(data=org_NE_High_summary, aes(x= DSf
                                                          ,y= mean_NE_High)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "High Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+06, 11e+06) +
  geom_errorbar(aes(ymin = mean_NE_High - sd_NE_High, ymax = mean_NE_High + sd_NE_High), width=0.2)
plot(org_NE_High_plot)

# Critical

org_NE_Crit <- dplyr::select(filter(org_final, WSL == "4"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_Crit)

org_NE_Crit_summary <- org_NE_Crit %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_Crit = mean(NE),  # calculates the mean of each group
            sd_NE_Crit = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Crit = sd(NE)/sqrt(n())) # calculates the standard error of each group

View(org_NE_Crit_summary)

org_NE_Crit_plot <-  ggplot(data=org_NE_Crit_summary, aes(x= DSf
                                                          ,y= mean_NE_Crit)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Critical")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+06, 11e+06) +
  geom_errorbar(aes(ymin = mean_NE_Crit - sd_NE_Crit, ymax = mean_NE_Crit + sd_NE_Crit), width=0.2)
plot(org_NE_Crit_plot)

ggstatsplot::combine_plots(
  plotlist = list(org_NE_No_plot, org_NE_Low_plot, org_NE_Med_plot, org_NE_High_plot, org_NE_Crit_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Plotting the origin countries with respect to Development Status

# Creating dataset for Least, Less, and More Developed

# Least Developed

org_NE_Least <- dplyr::select(filter(org_final, DS == "0"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_Least)

org_NE_Least_summary <- org_NE_Least %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_Least = mean(NE) /1000,  # calculates the mean of each group
            sd_NE_Least = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Least = sd(NE)/sqrt(n())) # calculates the standard error of each group



org_NE_Least_plot <-  ggplot(data=org_NE_Least_summary, aes(x= WSf
                                                            ,y= mean_NE_Least)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of emigrants (per thousand)", title = "Least Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+03, 11e+03) +
  geom_errorbar(aes(ymin = mean_NE_Least - sd_NE_Least, ymax = mean_NE_Least + sd_NE_Least), width=0.2)
plot(org_NE_Least_plot)

# Less Developed

org_NE_Less <- dplyr::select(filter(org_final, DS == "1"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_Less)

org_NE_Less_summary <- org_NE_Less %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_Less = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_Less = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Less = sd(NE)/sqrt(n())) # calculates the standard error of each group

org_NE_Less_plot <-  ggplot(data=org_NE_Less_summary, aes(x= WSf
                                                          ,y= mean_NE_Less)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "", title = "Less Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+03, 11e+03) +
  geom_errorbar(aes(ymin = mean_NE_Less - sd_NE_Less, ymax = mean_NE_Less + sd_NE_Less), width=0.2)
plot(org_NE_Less_plot)

# More Developed

org_NE_More <- dplyr::select(filter(org_final, DS == "2"), c(NE, DS, WSL, DSf, WSf))
View(org_NE_More)

org_NE_More_summary <- org_NE_More %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_More = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_More = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_More = sd(NE)/sqrt(n())) # calculates the standard error of each group

org_NE_More_plot <-  ggplot(data=org_NE_More_summary, aes(x= WSf
                                                          ,y= mean_NE_More)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "More Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-0.6e+03, 11e+03) +
  geom_errorbar(aes(ymin = mean_NE_More - sd_NE_More, ymax = mean_NE_More + sd_NE_More), width=0.2)
plot(org_NE_More_plot)


ggstatsplot::combine_plots(
  plotlist = list(org_NE_Least_plot, org_NE_Less_plot, org_NE_More_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Now Plotting Number of Immigrants, and International Migrant Stock for Destination Countries

des <- dplyr::select(filter(WMNCA_final, OD == "1"), c(NI, IMS_BS, DS, WSL))
View(des)

des_final <- des %>% mutate(DSf = recode_factor(DS, 
                                                `0` = "Least Developed",
                                                `1` = "Less Developed",
                                                `2` = "More Developed"
), WSf = recode_factor(WSL,
                       `0` = "No Stress",
                       `1` = "Low Stress",
                       `2` = "Medium Stress",
                       `3` = "High Stress",
                       `4` = "Critical"))

View(des_final)

des_DS_NI_summary <- des_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_DS = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_DS = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_DS = sd(NI)/sqrt(n())) # calculates the standard error of each group

View (des_DS_NI_summary)

des_DS_NI_plot <-  ggplot(data=des_DS_NI_summary, aes(x= DSf
                                                      ,y= mean_NI_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "Number of immigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NI_DS - sd_NI_DS, ymax = mean_NI_DS + sd_NI_DS), width=0.2)
plot(des_DS_NI_plot)

ggpubr::ggboxplot(des_final, x = "DSf", y = "NI",
                  color = "DSf", palette = "jco") +
  ggpubr::stat_compare_means(method = "anova")

des_WS_NI_summary <- des_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_WS = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_WS = sd(NI)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NI_WS = sd(NI)/sqrt(n())) # calculates the standard error of each group


des_WS_NI_plot <-  ggplot(data=des_WS_NI_summary, aes(x= WSf
                                                      ,y= mean_NI_WS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "Number of immigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NI_WS - sd_NI_WS, ymax = mean_NI_WS + sd_NI_WS), width=0.2)
plot(des_WS_NI_plot)

## Plotting the Number of Immigrants with respect to Development Status and Water Stress Level

# Creating dataset for No, Low, Medium, High, and Critical Stress

# No Stress

des_NI_No <- dplyr::select(filter(des_final, WSL == "0"), c(NI,  DS, WSL, DSf, WSf))
View(des_NI_No)

des_NI_No_summary <- des_NI_No %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_No = mean(NI),  # calculates the mean of each group
            sd_NI_No = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_No = sd(NI)/sqrt(n())) # calculates the standard error of each group
View(des_NI_No_summary)

des_NI_No_plot <-  ggplot(data=des_NI_No_summary, aes(x= DSf
                                                      ,y= mean_NI_No)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of Immigrants", title = "No Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+06, 7.5e+06) +
  geom_errorbar(aes(ymin = mean_NI_No - sd_NI_No, ymax = mean_NI_No + sd_NI_No), width=0.2)
plot(des_NI_No_plot)

# Low Stress

des_NI_Low <- dplyr::select(filter(des_final, WSL == "1"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_Low)

des_NI_Low_summary <- des_NI_Low %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_Low = mean(NI),  # calculates the mean of each group
            sd_NI_Low = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Low = sd(NI)/sqrt(n())) # calculates the standard error of each group

des_NI_Low_plot <-  ggplot(data=des_NI_Low_summary, aes(x= DSf
                                                        ,y= mean_NI_Low)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Low Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+06, 7.5e+06) +
  geom_errorbar(aes(ymin = mean_NI_Low - sd_NI_Low, ymax = mean_NI_Low + sd_NI_Low), width=0.2)
plot(des_NI_Low_plot)

# Medium Stress

des_NI_Med <- dplyr::select(filter(des_final, WSL == "2"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_Med)

des_NI_Med_summary <- des_NI_Med %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_Med = mean(NI),  # calculates the mean of each group
            sd_NI_Med = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Med = sd(NI)/sqrt(n())) # calculates the standard error of each group

des_NI_Med_plot <-  ggplot(data=des_NI_Med_summary, aes(x= DSf
                                                        ,y= mean_NI_Med)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development Status", y = "", title = "Medium Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+06, 7.5e+06) +
  geom_errorbar(aes(ymin = mean_NI_Med - sd_NI_Med, ymax = mean_NI_Med + sd_NI_Med), width=0.2)
plot(des_NI_Med_plot)

# High Stress

des_NI_High <- dplyr::select(filter(des_final, WSL == "3"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_High)

des_NI_High_summary <- des_NI_High %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_High = mean(NI),  # calculates the mean of each group
            sd_NI_High = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_High = sd(NI)/sqrt(n())) # calculates the standard error of each group

des_NI_High_plot <-  ggplot(data=des_NI_High_summary, aes(x= DSf
                                                          ,y= mean_NI_High)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "High Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+06, 7.5e+06) +
  geom_errorbar(aes(ymin = mean_NI_High - sd_NI_High, ymax = mean_NI_High + sd_NI_High), width=0.2)
plot(des_NI_High_plot)

# Critical

des_NI_Crit <- dplyr::select(filter(des_final, WSL == "4"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_Crit)

des_NI_Crit_summary <- des_NI_Crit %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_Crit = mean(NI),  # calculates the mean of each group
            sd_NI_Crit = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Crit = sd(NI)/sqrt(n())) # calculates the standard error of each group

des_NI_Crit_plot <-  ggplot(data=des_NI_Crit_summary, aes(x= DSf
                                                          ,y= mean_NI_Crit)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Critical")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+06, 7.5e+06) +
  geom_errorbar(aes(ymin = mean_NI_Crit - sd_NI_Crit, ymax = mean_NI_Crit + sd_NI_Crit), width=0.2)
plot(des_NI_Crit_plot)

ggstatsplot::combine_plots(
  plotlist = list(des_NI_No_plot, des_NI_Low_plot, des_NI_Med_plot, des_NI_High_plot, des_NI_Crit_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Plotting the Destination countries with respect to Development Status

# Creating dataset for Least, Less, and More Developed

# Least Developed

des_NI_Least <- dplyr::select(filter(des_final, DS == "0"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_Least)

des_NI_Least_summary <- des_NI_Least %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_Least = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_Least = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Least = sd(NI)/sqrt(n())) # calculates the standard error of each group


des_NI_Least_plot <-  ggplot(data=des_NI_Least_summary, aes(x= WSf
                                                            ,y= mean_NI_Least)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of immigrants (per thousand)", title = "Least Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+03, 7.5e+03) +
  geom_errorbar(aes(ymin = mean_NI_Least - sd_NI_Least, ymax = mean_NI_Least + sd_NI_Least), width=0.2)
plot(des_NI_Least_plot)

# Less Developed

des_NI_Less <- dplyr::select(filter(des_final, DS == "1"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_Less)

des_NI_Less_summary <- des_NI_Less %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_Less = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_Less = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Less = sd(NI)/sqrt(n())) # calculates the standard error of each group

des_NI_Less_plot <-  ggplot(data=des_NI_Less_summary, aes(x= WSf
                                                          ,y= mean_NI_Less)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "", title = "Less Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+03, 7.5e+03) +
  geom_errorbar(aes(ymin = mean_NI_Less - sd_NI_Less, ymax = mean_NI_Less + sd_NI_Less), width=0.2)
plot(des_NI_Less_plot)

# More Developed

des_NI_More <- dplyr::select(filter(des_final, DS == "2"), c(NI, DS, WSL, DSf, WSf))
View(des_NI_More)

des_NI_More_summary <- des_NI_More %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_More = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_More = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_More = sd(NI)/sqrt(n())) # calculates the standard error of each group

des_NI_More_plot <-  ggplot(data=des_NI_More_summary, aes(x= WSf
                                                          ,y= mean_NI_More)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "More Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+03, 7.5e+03) +
  geom_errorbar(aes(ymin = mean_NI_More - sd_NI_More, ymax = mean_NI_More + sd_NI_More), width=0.2)
plot(des_NI_More_plot)


ggstatsplot::combine_plots(
  plotlist = list(des_NI_Least_plot, des_NI_Less_plot, des_NI_More_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

# Plotting the destination countries for International Migration Stock

des_DS_IMS_summary <- des_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_DS = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_DS = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_DS = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

View (des_DS_IMS_summary)

des_DS_IMS_plot <-  ggplot(data=des_DS_IMS_summary, aes(x= DSf
                                                        ,y= mean_IMS_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "International migrant stock as a percentage of the total population (both sexes)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_IMS_DS - sd_IMS_DS, ymax = mean_IMS_DS + sd_IMS_DS), width=0.2)
plot(des_DS_IMS_plot)


des_WS_IMS_summary <- des_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_WS = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_WS = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_WS = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group


des_WS_IMS_plot <-  ggplot(data=des_WS_IMS_summary, aes(x= WSf
                                                        ,y= mean_IMS_WS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "International migrant stock as a percentage of the total population (both sexes)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_IMS_WS - sd_IMS_WS, ymax = mean_IMS_WS + sd_IMS_WS), width=0.2)
plot(des_WS_IMS_plot)

## Plotting the International Migrant Stock with respect to Development Status and Water Stress Level

# Creating dataset for No, Low, Medium, High, and Critical Stress

# No Stress

des_IMS_No <- dplyr::select(filter(des_final, WSL == "0"), c(IMS_BS,  DS, WSL, DSf, WSf))
View(des_IMS_No)

des_IMS_No_summary <- des_IMS_No %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_No = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_No = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_No = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group


des_IMS_No_plot <-  ggplot(data=des_IMS_No_summary, aes(x= DSf
                                                        ,y= mean_IMS_No)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "International migrant stock as a percentage of the total population (both sexes)", title = "No Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0, 65) +
  geom_errorbar(aes(ymin = mean_IMS_No - sd_IMS_No, ymax = mean_IMS_No + sd_IMS_No), width=0.2)
plot(des_IMS_No_plot)

# Low Stress

des_IMS_Low <- dplyr::select(filter(des_final, WSL == "1"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_Low)

des_IMS_Low_summary <- des_IMS_Low %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_Low = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Low = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Low = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

des_IMS_Low_plot <-  ggplot(data=des_IMS_Low_summary, aes(x= DSf
                                                          ,y= mean_IMS_Low)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Low Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0, 65) +
  geom_errorbar(aes(ymin = mean_IMS_Low - sd_IMS_Low, ymax = mean_IMS_Low + sd_IMS_Low), width=0.2)
plot(des_IMS_Low_plot)

# Medium Stress

des_IMS_Med <- dplyr::select(filter(des_final, WSL == "2"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_Med)

des_IMS_Med_summary <- des_IMS_Med %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_Med = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Med = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Med = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

des_IMS_Med_plot <-  ggplot(data=des_IMS_Med_summary, aes(x= DSf
                                                          ,y= mean_IMS_Med)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development Status", y = "", title = "Medium Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0,65) + 
  geom_errorbar(aes(ymin = mean_IMS_Med - sd_IMS_Med, ymax = mean_IMS_Med + sd_IMS_Med), width=0.2)
plot(des_IMS_Med_plot)

# High Stress

des_IMS_High <- dplyr::select(filter(des_final, WSL == "3"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_High)

des_IMS_High_summary <- des_IMS_High %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_High = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_High = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_High = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

des_IMS_High_plot <-  ggplot(data=des_IMS_High_summary, aes(x= DSf
                                                            ,y= mean_IMS_High)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "High Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0,65) +
  geom_errorbar(aes(ymin = mean_IMS_High - sd_IMS_High, ymax = mean_IMS_High + sd_IMS_High), width=0.2)
plot(des_IMS_High_plot)

# Critical

des_IMS_Crit <- dplyr::select(filter(des_final, WSL == "4"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_Crit)

des_IMS_Crit_summary <- des_IMS_Crit %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_Crit = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Crit = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Crit = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

View(des_IMS_Crit_summary)

des_IMS_Crit_plot <-  ggplot(data=des_IMS_Crit_summary, aes(x= DSf
                                                            ,y= mean_IMS_Crit)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Critical")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0,65) +
  geom_errorbar(aes(ymin = mean_IMS_Crit - sd_IMS_Crit, ymax = mean_IMS_Crit + sd_IMS_Crit), width=0.2)
plot(des_IMS_Crit_plot)

ggstatsplot::combine_plots(
  plotlist = list(des_IMS_No_plot, des_IMS_Low_plot, des_IMS_Med_plot, des_IMS_High_plot, des_IMS_Crit_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Plotting the Destination countries of International Migrant Stock with respect to Development Status

# Creating dataset for Least, Less, and More Developed

# Least Developed

des_IMS_Least <- dplyr::select(filter(des_final, DS == "0"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_Least)

des_IMS_Least_summary <- des_IMS_Least %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_Least = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Least = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Least = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group


des_IMS_Least_plot <-  ggplot(data=des_IMS_Least_summary, aes(x= WSf
                                                              ,y= mean_IMS_Least)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "International migrant stock as a percentage of the total population (both sexes)", title = "Least Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0,65) +
  geom_errorbar(aes(ymin = mean_IMS_Least - sd_IMS_Least, ymax = mean_IMS_Least + sd_IMS_Least), width=0.2)
plot(des_IMS_Least_plot)

# Less Developed

des_IMS_Less <- dplyr::select(filter(des_final, DS == "1"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_Less)

des_IMS_Less_summary <- des_IMS_Less %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_Less = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Less = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Less = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

des_IMS_Less_plot <-  ggplot(data=des_IMS_Less_summary, aes(x= WSf
                                                            ,y= mean_IMS_Less)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "", title = "Less Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0,65) +
  geom_errorbar(aes(ymin = mean_IMS_Less - sd_IMS_Less, ymax = mean_IMS_Less + sd_IMS_Less), width=0.2)
plot(des_IMS_Less_plot)

# More Developed

des_IMS_More <- dplyr::select(filter(des_final, DS == "2"), c(IMS_BS, DS, WSL, DSf, WSf))
View(des_IMS_More)

des_IMS_More_summary <- des_IMS_More %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_More = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_More = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_More = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

des_IMS_More_plot <-  ggplot(data=des_IMS_More_summary, aes(x= WSf
                                                            ,y= mean_IMS_More)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "More Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0, 65) + 
  geom_errorbar(aes(ymin = mean_IMS_More - sd_IMS_More, ymax = mean_IMS_More + sd_IMS_More), width=0.2)
plot(des_IMS_More_plot)


ggstatsplot::combine_plots(
  plotlist = list(des_IMS_Least_plot, des_IMS_Less_plot, des_IMS_More_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))


## Creating Dataset for Both origin and Destination Countries

both <- dplyr::select(filter(WMNCA_final, OD == "2"), c(NE, NI, IMS_BS, DS, WSL))
View(both)

both_final <- both %>% mutate(DSf = recode_factor(DS, 
                                                  `0` = "Least Developed",
                                                  `1` = "Less Developed",
                                                  `2` = "More Developed"
), WSf = recode_factor(WSL,
                       `0` = "No Stress",
                       `1` = "Low Stress",
                       `2` = "Medium Stress",
                       `3` = "High Stress",
                       `4` = "Critical"))

View(both_final)

# Plotting Number of Emigrants

both_DS_NE_summary <- both_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_DS = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_DS = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_DS = sd(NE)/sqrt(n())) # calculates the standard error of each group
View(both_DS_NE_summary)

both_DS_NE_plot <-  ggplot(data=both_DS_NE_summary, aes(x= DSf
                                                        ,y= mean_NE_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "Number of emigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NE_DS - sd_NE_DS, ymax = mean_NE_DS + sd_NE_DS), width=0.2)
plot(both_DS_NE_plot)


both_WS_NE_summary <- both_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_WS = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_WS = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_WS = sd(NE)/sqrt(n())) # calculates the standard error of each group

View(both_WS_NE_summary)

both_WS_NE_plot <-  ggplot(data=both_WS_NE_summary, aes(x= WSf
                                                        ,y= mean_NE_WS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "Number of emigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NE_WS - sd_NE_WS, ymax = mean_NE_WS + sd_NE_WS), width=0.2)
plot(both_WS_NE_plot)

## Plotting the Number of Emigrants with respect to Development Status and Water Stress Level

# Creating dataset for No, Low, Medium, High, and Critical Stress

# No Stress

both_NE_No <- dplyr::select(filter(both_final, WSL == "0"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_No)

both_NE_No_summary <- both_NE_No %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_No = mean(NE),  # calculates the mean of each group
            sd_NE_No = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_No = sd(NE)/sqrt(n())) # calculates the standard error of each group


both_NE_No_plot <-  ggplot(data=both_NE_No_summary, aes(x= DSf
                                                        ,y= mean_NE_No)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of Emigrants", title = "No Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+06) + 
  geom_errorbar(aes(ymin = mean_NE_No - sd_NE_No, ymax = mean_NE_No + sd_NE_No), width=0.2)
plot(both_NE_No_plot)

# Low Stress

both_NE_Low <- dplyr::select(filter(both_final, WSL == "1"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_Low)

both_NE_Low_summary <- both_NE_Low %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_Low = mean(NE),  # calculates the mean of each group
            sd_NE_Low = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Low = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_Low_plot <-  ggplot(data=both_NE_Low_summary, aes(x= DSf
                                                          ,y= mean_NE_Low)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Low Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+06) +
  geom_errorbar(aes(ymin = mean_NE_Low - sd_NE_Low, ymax = mean_NE_Low + sd_NE_Low), width=0.2)
plot(both_NE_Low_plot)

# Medium Stress

both_NE_Med <- dplyr::select(filter(both_final, WSL == "2"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_Med)

both_NE_Med_summary <- both_NE_Med %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_Med = mean(NE),  # calculates the mean of each group
            sd_NE_Med = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Med = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_Med_plot <-  ggplot(data=both_NE_Med_summary, aes(x= DSf
                                                          ,y= mean_NE_Med)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development Status", y = "", title = "Medium Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+06) +
  geom_errorbar(aes(ymin = mean_NE_Med - sd_NE_Med, ymax = mean_NE_Med + sd_NE_Med), width=0.2)
plot(both_NE_Med_plot)

# High Stress

both_NE_High <- dplyr::select(filter(both_final, WSL == "3"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_High)

both_NE_High_summary <- both_NE_High %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_High = mean(NE),  # calculates the mean of each group
            sd_NE_High = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_High = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_High_plot <-  ggplot(data=both_NE_High_summary, aes(x= DSf
                                                            ,y= mean_NE_High)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "High Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+06) +
  geom_errorbar(aes(ymin = mean_NE_High - sd_NE_High, ymax = mean_NE_High + sd_NE_High), width=0.2)
plot(both_NE_High_plot)

# Critical

both_NE_Crit <- dplyr::select(filter(both_final, WSL == "4"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_Crit)

both_NE_Crit_summary <- both_NE_Crit %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NE_Crit = mean(NE),  # calculates the mean of each group
            sd_NE_Crit = sd(NE), # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Crit = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_Crit_plot <-  ggplot(data=both_NE_Crit_summary, aes(x= DSf
                                                            ,y= mean_NE_Crit)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Critical")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+06) +
  geom_errorbar(aes(ymin = mean_NE_Crit - sd_NE_Crit, ymax = mean_NE_Crit + sd_NE_Crit), width=0.2)
plot(both_NE_Crit_plot)

ggstatsplot::combine_plots(
  plotlist = list(both_NE_No_plot, both_NE_Low_plot, both_NE_Med_plot, both_NE_High_plot, both_NE_Crit_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Plotting the both origin and destination countries with respect to Development Status

# Creating dataset for Least, Less, and More Developed

# Least Developed (No data available)

both_NE_Least <- dplyr::select(filter(both_final, DS == "0"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_Least)

both_NE_Least_summary <- both_NE_Least %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_Least = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_Least = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Least = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_Least_plot <-  ggplot(data=both_NE_Least_summary, aes(x= WSf
                                                              ,y= mean_NE_Least)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of emigrants (per thousand)", title = "Least Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NE_Least - sd_NE_Least, ymax = mean_NE_Least + sd_NE_Least), width=0.2)
plot(both_NE_Least_plot)

# Less Developed

both_NE_Less <- dplyr::select(filter(both_final, DS == "1"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_Less)

both_NE_Less_summary <- both_NE_Less %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_Less = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_Less = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_Less = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_Less_plot <-  ggplot(data=both_NE_Less_summary, aes(x= WSf
                                                            ,y= mean_NE_Less)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "Number of emigrants (per thousand)", title = "Less Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1,0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+03) +
  geom_errorbar(aes(ymin = mean_NE_Less - sd_NE_Less, ymax = mean_NE_Less + sd_NE_Less), width=0.2)
plot(both_NE_Less_plot)

# More Developed

both_NE_More <- dplyr::select(filter(both_final, DS == "2"), c(NE, DS, WSL, DSf, WSf))
View(both_NE_More)

both_NE_More_summary <- both_NE_More %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NE_More = mean(NE)/1000,  # calculates the mean of each group
            sd_NE_More = sd(NE)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NE_More = sd(NE)/sqrt(n())) # calculates the standard error of each group

both_NE_More_plot <-  ggplot(data=both_NE_More_summary, aes(x= WSf
                                                            ,y= mean_NE_More)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "", title = "More Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,15e+03) +
  geom_errorbar(aes(ymin = mean_NE_More - sd_NE_More, ymax = mean_NE_More + sd_NE_More), width=0.2)
plot(both_NE_More_plot)


ggstatsplot::combine_plots(
  plotlist = list(both_NE_Less_plot, both_NE_More_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))


# Plotting of Both Origin and Destination Countries for Number of Immigrants

both_DS_NI_summary <- both_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_DS = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_DS = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_DS = sd(NI)/sqrt(n())) # calculates the standard error of each group

View (both_DS_NI_summary)

both_DS_NI_plot <-  ggplot(data=both_DS_NI_summary, aes(x= DSf
                                                        ,y= mean_NI_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "Number of immigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NI_DS - sd_NI_DS, ymax = mean_NI_DS + sd_NI_DS), width=0.2)
plot(both_DS_NI_plot)


both_WS_NI_summary <- both_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_WS = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_WS = sd(NI)/1000, # calculates the standard deviation of each group
            n_NE = n(),  # calculates the sample size per group
            SE_NI_WS = sd(NI)/sqrt(n())) # calculates the standard error of each group
View(both_WS_NI_summary)

both_WS_NI_plot <-  ggplot(data=both_WS_NI_summary, aes(x= WSf
                                                        ,y= mean_NI_WS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "Number of immigrants (per thousand)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_NI_WS - sd_NI_WS, ymax = mean_NI_WS + sd_NI_WS), width=0.2)
plot(both_WS_NI_plot)

## Plotting the Number of Immigrants with respect to Development Status and Water Stress Level

# Creating dataset for No, Low, Medium, High, and Critical Stress

# No Stress

both_NI_No <- dplyr::select(filter(both_final, WSL == "0"), c(NI,  DS, WSL, DSf, WSf))
View(both_NI_No)

both_NI_No_summary <- both_NI_No %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_No = mean(NI),  # calculates the mean of each group
            sd_NI_No = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_No = sd(NI)/sqrt(n())) # calculates the standard error of each group

View(both_NI_No_summary)

both_NI_No_plot <-  ggplot(data=both_NI_No_summary, aes(x= DSf
                                                        ,y= mean_NI_No)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of Immigrants", title = "No Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00, 35e+06) + 
  geom_errorbar(aes(ymin = mean_NI_No - sd_NI_No, ymax = mean_NI_No + sd_NI_No), width=0.2)
plot(both_NI_No_plot)

# Low Stress

both_NI_Low <- dplyr::select(filter(both_final, WSL == "1"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_Low)

both_NI_Low_summary <- both_NI_Low %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_Low = mean(NI),  # calculates the mean of each group
            sd_NI_Low = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Low = sd(NI)/sqrt(n())) # calculates the standard error of each group

View(both_NI_Low_summary)

both_NI_Low_plot <-  ggplot(data=both_NI_Low_summary, aes(x= DSf
                                                          ,y= mean_NI_Low)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Low Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+06) +
  geom_errorbar(aes(ymin = mean_NI_Low - sd_NI_Low, ymax = mean_NI_Low + sd_NI_Low), width=0.2)
plot(both_NI_Low_plot)

# Medium Stress

both_NI_Med <- dplyr::select(filter(both_final, WSL == "2"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_Med)

both_NI_Med_summary <- both_NI_Med %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_Med = mean(NI),  # calculates the mean of each group
            sd_NI_Med = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Med = sd(NI)/sqrt(n())) # calculates the standard error of each group

both_NI_Med_plot <-  ggplot(data=both_NI_Med_summary, aes(x= DSf
                                                          ,y= mean_NI_Med)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development Status", y = "", title = "Medium Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+06) +
  geom_errorbar(aes(ymin = mean_NI_Med - sd_NI_Med, ymax = mean_NI_Med + sd_NI_Med), width=0.2)
plot(both_NI_Med_plot)

# High Stress

both_NI_High <- dplyr::select(filter(both_final, WSL == "3"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_High)

both_NI_High_summary <- both_NI_High %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_High = mean(NI),  # calculates the mean of each group
            sd_NI_High = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_High = sd(NI)/sqrt(n())) # calculates the standard error of each group

both_NI_High_plot <-  ggplot(data=both_NI_High_summary, aes(x= DSf
                                                            ,y= mean_NI_High)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "High Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+06) +
  geom_errorbar(aes(ymin = mean_NI_High - sd_NI_High, ymax = mean_NI_High + sd_NI_High), width=0.2)
plot(both_NI_High_plot)

# Critical

both_NI_Crit <- dplyr::select(filter(both_final, WSL == "4"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_Crit)

both_NI_Crit_summary <- both_NI_Crit %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_NI_Crit = mean(NI),  # calculates the mean of each group
            sd_NI_Crit = sd(NI), # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Crit = sd(NI)/sqrt(n())) # calculates the standard error of each group

both_NI_Crit_plot <-  ggplot(data=both_NI_Crit_summary, aes(x= DSf
                                                            ,y= mean_NI_Crit)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Critical")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+06) +
  geom_errorbar(aes(ymin = mean_NI_Crit - sd_NI_Crit, ymax = mean_NI_Crit + sd_NI_Crit), width=0.2)
plot(both_NI_Crit_plot)

ggstatsplot::combine_plots(
  plotlist = list(both_NI_No_plot, both_NI_Low_plot, both_NI_Med_plot, both_NI_High_plot, both_NI_Crit_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Plotting the Destination countries with respect to Development Status

# Creating dataset for Least, Less, and More Developed

# Least Developed (No data)

both_NI_Least <- dplyr::select(filter(both_final, DS == "0"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_Least)

both_NI_Least_summary <- both_NI_Least %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_Least = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_Least = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Least = sd(NI)/sqrt(n())) # calculates the standard error of each group


both_NI_Least_plot <-  ggplot(data=both_NI_Least_summary, aes(x= WSf
                                                              ,y= mean_NI_Least)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "Number of immigrants (per thousand)", title = "Least Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+03) +
  geom_errorbar(aes(ymin = mean_NI_Least - sd_NI_Least, ymax = mean_NI_Least + sd_NI_Least), width=0.2)
plot(both_NI_Least_plot)

# Less Developed

both_NI_Less <- dplyr::select(filter(both_final, DS == "1"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_Less)

both_NI_Less_summary <- both_NI_Less %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_Less = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_Less = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_Less = sd(NI)/sqrt(n())) # calculates the standard error of each group

both_NI_Less_plot <-  ggplot(data=both_NI_Less_summary, aes(x= WSf
                                                            ,y= mean_NI_Less)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "Number of immigrants (per thousand)", title = "Less Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+03) +
  geom_errorbar(aes(ymin = mean_NI_Less - sd_NI_Less, ymax = mean_NI_Less + sd_NI_Less), width=0.2)
plot(both_NI_Less_plot)

# More Developed

both_NI_More <- dplyr::select(filter(both_final, DS == "2"), c(NI, DS, WSL, DSf, WSf))
View(both_NI_More)

both_NI_More_summary <- both_NI_More %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_NI_More = mean(NI)/1000,  # calculates the mean of each group
            sd_NI_More = sd(NI)/1000, # calculates the standard deviation of each group
            n_NI = n(),  # calculates the sample size per group
            SE_NI_More = sd(NI)/sqrt(n())) # calculates the standard error of each group

both_NI_More_plot <-  ggplot(data=both_NI_More_summary, aes(x= WSf
                                                            ,y= mean_NI_More)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "", title = "More Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(0e+00,35e+03) +
  geom_errorbar(aes(ymin = mean_NI_More - sd_NI_More, ymax = mean_NI_More + sd_NI_More), width=0.2)
plot(both_NI_More_plot)


ggstatsplot::combine_plots(
  plotlist = list(both_NI_Less_plot, both_NI_More_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))


# Plotting Both Origin and Destination countries for International Migration Stock

both_DS_IMS_summary <- both_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_DS = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_DS = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_DS = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

View (both_DS_IMS_summary)

both_DS_IMS_plot <-  ggplot(data=both_DS_IMS_summary, aes(x= DSf
                                                          ,y= mean_IMS_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "International migrant stock as a percentage of the total population (both sexes)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_IMS_DS - sd_IMS_DS, ymax = mean_IMS_DS + sd_IMS_DS), width=0.2)
plot(both_DS_IMS_plot)


both_WS_IMS_summary <- both_final %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_WS = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_WS = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_WS = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group


both_WS_IMS_plot <-  ggplot(data=both_WS_IMS_summary, aes(x= WSf
                                                          ,y= mean_IMS_WS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "International migrant stock as a percentage of the total population (both sexes)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_IMS_WS - sd_IMS_WS, ymax = mean_IMS_WS + sd_IMS_WS), width=0.2)
plot(both_WS_IMS_plot)

## Plotting the International Migrant Stock with respect to Development Status and Water Stress Level

# Creating dataset for No, Low, Medium, High, and Critical Stress

# No Stress

both_IMS_No <- dplyr::select(filter(both_final, WSL == "0"), c(IMS_BS,  DS, WSL, DSf, WSf))
View(both_IMS_No)

both_IMS_No_summary <- both_IMS_No %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_No = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_No = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_No = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group


both_IMS_No_plot <-  ggplot(data=both_IMS_No_summary, aes(x= DSf
                                                          ,y= mean_IMS_No)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "International migrant stock as a percentage of the total population (both sexes)", title = "No Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) + 
  geom_errorbar(aes(ymin = mean_IMS_No - sd_IMS_No, ymax = mean_IMS_No + sd_IMS_No), width=0.2)
plot(both_IMS_No_plot)

# Low Stress

both_IMS_Low <- dplyr::select(filter(both_final, WSL == "1"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_Low)

both_IMS_Low_summary <- both_IMS_Low %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_Low = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Low = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Low = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

View(both_IMS_Low_summary)
both_IMS_Low_plot <-  ggplot(data=both_IMS_Low_summary, aes(x= DSf
                                                            ,y= mean_IMS_Low)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Low Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) + 
  geom_errorbar(aes(ymin = mean_IMS_Low - sd_IMS_Low, ymax = mean_IMS_Low + sd_IMS_Low), width=0.2)
plot(both_IMS_Low_plot)

# Medium Stress

both_IMS_Med <- dplyr::select(filter(both_final, WSL == "2"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_Med)

both_IMS_Med_summary <- both_IMS_Med %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_Med = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Med = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Med = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

View(both_IMS_Med_summary)
both_IMS_Med_plot <-  ggplot(data=both_IMS_Med_summary, aes(x= DSf
                                                            ,y= mean_IMS_Med)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development Status", y = "", title = "Medium Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) + 
  geom_errorbar(aes(ymin = mean_IMS_Med - sd_IMS_Med, ymax = mean_IMS_Med + sd_IMS_Med), width=0.2)
plot(both_IMS_Med_plot)

# High Stress

both_IMS_High <- dplyr::select(filter(both_final, WSL == "3"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_High)

both_IMS_High_summary <- both_IMS_High %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_High = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_High = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_High = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

both_IMS_High_plot <-  ggplot(data=both_IMS_High_summary, aes(x= DSf
                                                              ,y= mean_IMS_High)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "High Stress")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) + 
  geom_errorbar(aes(ymin = mean_IMS_High - sd_IMS_High, ymax = mean_IMS_High + sd_IMS_High), width=0.2)
plot(both_IMS_High_plot)

# Critical

both_IMS_Crit <- dplyr::select(filter(both_final, WSL == "4"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_Crit)

both_IMS_Crit_summary <- both_IMS_Crit %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_IMS_Crit = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Crit = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Crit = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

both_IMS_Crit_plot <-  ggplot(data=both_IMS_Crit_summary, aes(x= DSf
                                                              ,y= mean_IMS_Crit)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "", title = "Critical")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) + 
  geom_errorbar(aes(ymin = mean_IMS_Crit - sd_IMS_Crit, ymax = mean_IMS_Crit + sd_IMS_Crit), width=0.2)
plot(both_IMS_Crit_plot)

ggstatsplot::combine_plots(
  plotlist = list(both_IMS_No_plot, both_IMS_Low_plot, both_IMS_Med_plot, both_IMS_High_plot, both_IMS_Crit_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Plotting the Destination countries of International Migrant Stock with respect to Development Status

# Creating dataset for Least, Less, and More Developed

# Least Developed (No Data)

both_IMS_Least <- dplyr::select(filter(both_final, DS == "0"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_Least)

des_IMS_Least_summary <- des_IMS_Least %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_Least = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Least = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Least = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group


des_IMS_Least_plot <-  ggplot(data=des_IMS_Least_summary, aes(x= WSf
                                                              ,y= mean_IMS_Least)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "", y = "International migrant stock as a percentage of the total population (both sexes)", title = "Least Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) +
  geom_errorbar(aes(ymin = mean_IMS_Least - sd_IMS_Least, ymax = mean_IMS_Least + sd_IMS_Least), width=0.2)
plot(des_IMS_Least_plot)

# Less Developed

both_IMS_Less <- dplyr::select(filter(both_final, DS == "1"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_Less)

both_IMS_Less_summary <- both_IMS_Less %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_Less = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_Less = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_Less = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

both_IMS_Less_plot <-  ggplot(data=both_IMS_Less_summary, aes(x= WSf
                                                              ,y= mean_IMS_Less)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "olivedrab", "cyan4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "International migrant stock as a percentage of the total population (both sexes)", title = "Less Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) +
  geom_errorbar(aes(ymin = mean_IMS_Less - sd_IMS_Less, ymax = mean_IMS_Less + sd_IMS_Less), width=0.2)
plot(both_IMS_Less_plot)

# More Developed

both_IMS_More <- dplyr::select(filter(both_final, DS == "2"), c(IMS_BS, DS, WSL, DSf, WSf))
View(both_IMS_More)

both_IMS_More_summary <- both_IMS_More %>% # the names of the new data frame and the data frame to be summarised
  group_by(WSf) %>%   # the grouping variable
  summarise(mean_IMS_More = mean(IMS_BS),  # calculates the mean of each group
            sd_IMS_More = sd(IMS_BS), # calculates the standard deviation of each group
            n_IMS = n(),  # calculates the sample size per group
            SE_IMS_More = sd(IMS_BS)/sqrt(n())) # calculates the standard error of each group

both_IMS_More_plot <-  ggplot(data=both_IMS_More_summary, aes(x= WSf
                                                              ,y= mean_IMS_More)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4","red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Level of water stress", y = "", title = "More Developed")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + ylim(-5,20) + 
  geom_errorbar(aes(ymin = mean_IMS_More - sd_IMS_More, ymax = mean_IMS_More + sd_IMS_More), width=0.2)
plot(both_IMS_More_plot)


ggstatsplot::combine_plots(
  plotlist = list(both_IMS_Less_plot, both_IMS_More_plot),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = ""))

## Two-way Anova Test to study the effect of water stress is different depending on the development status or in the other way around

tuk_plot <- function (x, xlab, ylab, ylabels = NULL, ...) {
  for (i in seq_along(x)) {
    xi <- x[[i]][, -4L, drop = FALSE]
    yvals <- nrow(xi):1L
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2L), 
         type = "n", axes = FALSE, xlab = "", ylab = "", main = NULL, 
         ...)
    axis(1, ...)
    # change for custom axis labels
    if (is.null(ylabels)) ylabels <- dimnames(xi)[[1L]]
    
    axis(2, at = nrow(xi):1, labels = ylabels, 
         srt = 0, ...)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0.5, ...)
    segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, ...)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3L), as.vector(xi), 
             rep.int(yvals + 0.1, 3L), ...)
    title(main = paste0(format(100 * attr(x, "conf.level"), 
                               digits = 2L), "% family-wise confidence level\n"), 
          # change for custom axis titles
          xlab = xlab, ylab = ylab)
    
    box()
    dev.flush()
    on.exit()
  }
}


# Origin Countries

origin_anova_test_DS_WS_NE <- aov(NE ~ DSf + WSf + DSf*WSf, data = org_final)
summary(origin_anova_test_DS_WS_NE)


a <- tukey_hsd(origin_anova_test_DS_WS_NE)

write.csv(a, file = "TukeyHSD_origin_DS_WS_NE.csv")



# Destination Countries

# NI
destination_anova_test_DS_WS_NI <- aov(NI ~ DSf + WSf + DSf*WSf, data = des_final)
summary(destination_anova_test_DS_WS_NI)

b <- tukey_hsd(destination_anova_test_DS_WS_NI)
b

TukeyHSD(destination_anova_test_DS_WS_NI)

write.csv(b, file = "TukeyHSD_destination_DS_WS_NI.csv")


# IMS_BS

destination_anova_test_DS_WS_IMS <- aov(IMS_BS ~ DSf + WSf + DSf*WSf, data = des_final)
summary(destination_anova_test_DS_WS_IMS)

c <- tukey_hsd(destination_anova_test_DS_WS_IMS)

write.csv(c, "TukeyHSD_Destination_DS_WS_IMS.csv")

# Both Origin and Destination Countries

# NE

both_anova_test_DS_WS_NE <- aov(NE ~ DSf + WSf + DSf*WSf, data = both_final)
summary(both_anova_test_DS_WS_NE)

d <- tukey_hsd(both_anova_test_DS_WS_NE)
write.csv(d, "TukeyHSD_Both_DS_WS_NE.csv")


# NI

both_anova_test_DS_WS_NI <- aov(NI ~ DSf + WSf + DSf*WSf, data = both_final)
summary(both_anova_test_DS_WS_NI)

e <- tukey_hsd(both_anova_test_DS_WS_NI)
write.csv(e, "TukeyHSD_Both_DS_WS_NI.csv")

# IMS_BS

both_anova_test_DS_WS_IMS <- aov(IMS_BS ~ DSf + WSf + DSf*WSf, data = both_final)
summary(both_anova_test_DS_WS_IMS)

f <- tukey_hsd(both_anova_test_DS_WS_IMS)
write.csv(f, "TukeyHSD_Both_DS_WS_IMS.csv")


## Creating descriptive statistics

## Creating a descriptive Statistic keeping year-wise distribution

mean <- aggregate(WMNCA_final[,6:17], by= list (Year=WMNCA_final$Year), mean, na.rm=TRUE)
max <- aggregate(WMNCA_final[,6:17], by= list (Year=WMNCA_final$Year), max, na.rm=TRUE)
min <- aggregate(WMNCA_final[,6:17], by= list (Year=WMNCA_final$Year), min, na.rm=TRUE)
sd <- aggregate(WMNCA_final[,6:17], by= list (Year=WMNCA_final$Year), sd, na.rm=TRUE)

ds <- cbind (mean, max, min, sd)

view(ds)

write.csv(ds, file = "Descriptive Statistics.csv")

# Descriptive statistics of origin, Destination, and Both Origin and Destination Countries

mean_od <- aggregate(WMNCA_final[,6:17], by= list (OriginorDestination=WMNCA_final$OD), mean, na.rm=TRUE)
max_od <- aggregate(WMNCA_final[,6:17], by= list (OriginorDestination=WMNCA_final$OD), max, na.rm=TRUE)
min_od <- aggregate(WMNCA_final[,6:17], by= list (OriginorDestination=WMNCA_final$OD), min, na.rm=TRUE)
sd_od <- aggregate(WMNCA_final[,6:17], by= list (OriginorDestination=WMNCA_final$OD), sd, na.rm=TRUE)

ds_od <- cbind (mean_od, max_od, min_od, sd_od)

view(ds_od)

write.csv(ds_od, file = "Descriptive Statistics_origin or destination.csv")


# Descriptive Stattistics of origin countires

mean_org <- aggregate(data_org[,6:16], by= list (Year=data_org$Year), mean, na.rm=TRUE)
max_org <- aggregate(data_org[,6:16], by= list (Year=data_org$Year), max, na.rm=TRUE)
min_org <- aggregate(data_org[,6:16], by= list (Year=data_org$Year), min, na.rm=TRUE)
sd_org <- aggregate(data_org[,6:16], by= list (Year=data_org$Year), sd, na.rm=TRUE)

ds_org <- cbind (mean_org, max_org, min_org, sd_org)

view(ds_org)

write.csv(ds_org, file = "Descriptive Statistics of origin countries.csv")

# Descriptive statistics of Destination Countries

mean_des <- aggregate(data_des[,6:16], by= list (Year=data_des$Year), mean, na.rm=TRUE)
max_des <- aggregate(data_des[,6:16], by= list (Year=data_des$Year), max, na.rm=TRUE)
min_des <- aggregate(data_des[,6:16], by= list (Year=data_des$Year), min, na.rm=TRUE)
sd_des <- aggregate(data_des[,6:16], by= list (Year=data_des$Year), sd, na.rm=TRUE)

ds_des <- cbind (mean_des, max_des, min_des, sd_des)

view(ds_des)

write.csv(ds_des, file = "Descriptive Statistics of destination countries.csv")

# Descriptive Statistics of Both Origin and Destination Countries

mean_both <- aggregate(data_both[,6:16], by= list (Year=data_both$Year), mean, na.rm=TRUE)
max_both <- aggregate(data_both[,6:16], by= list (Year=data_both$Year), max, na.rm=TRUE)
min_both <- aggregate(data_both[,6:16], by= list (Year=data_both$Year), min, na.rm=TRUE)
sd_both <- aggregate(data_both[,6:16], by= list (Year=data_both$Year), sd, na.rm=TRUE)

ds_both <- cbind (mean_both, max_both, min_both, sd_both)

view(ds_both)

write.csv(ds_both, file = "Descriptive Statistics of both origin and destination countries.csv")


### Panel Data Analysis

## Creating Dataset for top origin countries

data_org <- dplyr::select(filter(WMNCA_final, OD == "0"), c(Countries, Year, DS, OD, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View (data_org)

## Creating Dataset for top Destination Countries

data_des <- dplyr::select(filter(WMNCA_final, OD == "1"), c(Countries, Year, DS, OD, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View (data_des)

## Creating Dataset for both top Origin and Destination Countries

data_both <- dplyr::select(filter(WMNCA_final, OD == "2"), c(Countries, Year, DS, OD, NI, NE, IMS_BS, PD, GDPPC, HDI, WS, WSL, WUE, NRI, TPASDW, TAPND))
View (data_both)


attach(WMNCA_final)
attach(data_org)
attach(data_des)
attach(data_both)

Y1 <- cbind(NE)
Y2 <- cbind(NI)
Y3 <- cbind(IMS_BS)

X <- cbind(PD, GDPPC, HDI, NRI, TPASDW, TAPND)

C <- cbind(Region)
Countries <- Countries

# Setting the data as panel data
pdata <- pdata.frame(WMNCA_final, index= c("Countries", "Year"))

pdata_org <- pdata.frame(data_org, index= c("Countries", "Year"))

pdata_des <- pdata.frame(data_des, index= c("Countries", "Year"))

pdata_both <- pdata.frame(data_both, index= c("Countries", "Year"))

## Plotting the Changes of the Dependent Variables over the time period


scatterplot(Y1~Year|Countries, data=pdata, ylab = "Number of Emigrants")
scatterplot(Y2~Year|Countries, data=pdata, ylab = "Number of Immigrants")
scatterplot(Y3~Year|Countries, data=pdata, ylab = "International migrant stock as a percentage of the total population (Both sexes)(%)")

## Plotting the Changes of the Dependent Variables according to Development Status/Region

DSFac <- factor(pdata$DS, levels=0:2, labels=c("Least Developed", "Less Developed", "More Developed"))
View(DSFac)

RegionFac <- factor(pdata$Region, levels=0:21, labels=c("EAf", "MAf", "SAf", "WAf",
                                                        "NAf", "WAs", "CAs", "SAs",
                                                        "EAs", "SEAs", "Car", "CA",
                                                        "SAm", "Aus/NZ", "Mel", "Mic",
                                                        "Pol", "EE", "NE", "SE",
                                                        "WE", "NA"))
RegionFac

scatterplot(Y1~DSFac|Year, data=pdata, ylab = "Number of Emigrants", xlab = "Development Status")
scatterplot(Y2~DSFac|Year, data=pdata, ylab = "Number of Immigrants", xlab = "Development Status")
scatterplot(Y3~DSFac|Year, data=pdata, ylab = "International migrant stock as a percentage of the total population (Both sexes)(%)", xlab = "Development Status")


scatterplot(Y1~RegionFac|Year, data=pdata, ylab = "Number of Emigrants", xlab = "Region")
scatterplot(Y2~RegionFac|Year, data=pdata, ylab = "Number of Immigrants", xlab = "Region")
scatterplot(Y3~RegionFac|Year, data=pdata, ylab = "International migrant stock as a percentage of the total population (Both sexes)(%)", xlab = "Region")

WSFac <- factor(pdata$WSL, levels=0:4, labels=c("No Stress", "Low Stress", "Medium Stress", "High Stress", "Critical"))
View(WSFac)

scatterplot(Y1~WSFac|Year, data=pdata, ylab = "Number of Emigrants", xlab = "Water Stress Level")
scatterplot(Y2~WSFac|Year, data=pdata, ylab = "Number of Immigrants", xlab = "Water Stress Level")
scatterplot(Y3~WSFac|Year, data=pdata, ylab = "International migrant stock as a percentage of the total population (Both sexes)(%)", xlab = "Water Stress Level")


# VIF Test

pooling_Y1_8_vif <- car::vif(lm(NE~ PD + GDPPC + HDI + NRI + TPASDW + WSL + DS, data = WMNCA_final))
## Plotting the VIF Test Result
barplot(pooling_Y1_8_vif, col = "steelblue")

# add vertical line at 5
abline(h = 5, lwd = 2, lty = 3)

write.csv(regmodel_1_vif, file = "VIF Test.csv")

## After Performing VIF Test, it has been found that the score of HDI is above 5. So, HDI should be omitted from the regression models

#### The final regression models

### Origin Countries
## Dependent Variable : NE

# Pooling Models

pooling_final_NE_org_1 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL), data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_1)

pooling_final_NE_org_2 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + NRI:TPASDW, data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_2)

pooling_final_NE_org_3 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):factor(pdata_org$DS), data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_3)

pooling_final_NE_org_4 <- plm(NE~ PD + GDPPC + NRI + TPASDW +  factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):GDPPC, data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_4)

pooling_final_NE_org_5 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):TPASDW, data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_5)

pooling_final_NE_org_6 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):PD, data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_6)

pooling_final_NE_org_7 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + NRI:factor(pdata_org$WSL), data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_7)

pooling_final_NE_org_8 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + TPASDW:factor(pdata_org$DS), data = pdata_org, model = "pooling")
summary(pooling_final_NE_org_8)


# Fixed Models

fixed_final_NE_org_1 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL), data = pdata_org, model = "within")
summary(fixed_final_NE_org_1)

fixed_final_NE_org_2 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + NRI:TPASDW, data = pdata_org, model = "within")
summary(fixed_final_NE_org_2)

fixed_final_NE_org_3 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):factor(pdata_org$DS), data = pdata_org, model = "within")
summary(fixed_final_NE_org_3)

fixed_final_NE_org_4 <- plm(NE~ PD + GDPPC + NRI + TPASDW +  factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):GDPPC, data = pdata_org, model = "within")
summary(fixed_final_NE_org_4)

fixed_final_NE_org_5 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):TPASDW, data = pdata_org, model = "within")
summary(fixed_final_NE_org_5)

fixed_final_NE_org_6 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):PD, data = pdata_org, model = "within")
summary(fixed_final_NE_org_6)

fixed_final_NE_org_7 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + NRI:factor(pdata_org$WSL), data = pdata_org, model = "within")
summary(fixed_final_NE_org_7)

fixed_final_NE_org_8 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + TPASDW:factor(pdata_org$DS), data = pdata_org, model = "within")
summary(fixed_final_NE_org_8)

# Random Models

random_final_NE_org_1 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL), data = pdata_org, model = "random")
summary(random_final_NE_org_1)

random_final_NE_org_2 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + NRI:TPASDW, data = pdata_org, model = "random")
summary(random_final_NE_org_2)

random_final_NE_org_3 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):factor(pdata_org$DS), data = pdata_org, model = "random", random.method = "amemiya")
summary(random_final_NE_org_3)

random_final_NE_org_4 <- plm(NE~ PD + GDPPC + NRI + TPASDW +  factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):GDPPC, data = pdata_org, model = "random")
summary(random_final_NE_org_4)

random_final_NE_org_5 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):TPASDW, data = pdata_org, model = "random")
summary(random_final_NE_org_5)

random_final_NE_org_6 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + factor(pdata_org$WSL):PD, data = pdata_org, model = "random")
summary(random_final_NE_org_6)

random_final_NE_org_7 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + NRI:factor(pdata_org$WSL), data = pdata_org, model = "random")
summary(random_final_NE_org_7)

random_final_NE_org_8 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_org$DS) + factor(pdata_org$WSL) + TPASDW:factor(pdata_org$DS), data = pdata_org, model = "random")
summary(random_final_NE_org_8)


## Choosing the most suitable model by performing suitability tests

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_Y1)

plmtest(pooling_final_NE_org_1, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_2, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_3, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_4, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_5, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_6, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_7, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_org_8, type = "bp", effect = "twoways")

# LM Test for Fixed Effects Model vs OLS Model
pFtest(fixed_Y1, pooling_Y1)

pFtest(fixed_final_NE_org_1, pooling_final_NE_org_1)
pFtest(fixed_final_NE_org_2, pooling_final_NE_org_2)
pFtest(fixed_final_NE_org_3, pooling_final_NE_org_3)
pFtest(fixed_final_NE_org_4, pooling_final_NE_org_4)
pFtest(fixed_final_NE_org_5, pooling_final_NE_org_5)
pFtest(fixed_final_NE_org_6, pooling_final_NE_org_6)
pFtest(fixed_final_NE_org_7, pooling_final_NE_org_7)
pFtest(fixed_final_NE_org_8, pooling_final_NE_org_8)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_Y1, fixed_Y1)

phtest(random_final_NE_org_1, fixed_final_NE_org_1)
phtest(random_final_NE_org_2, fixed_final_NE_org_2)
phtest(random_final_NE_org_3, fixed_final_NE_org_3)
phtest(random_final_NE_org_4, fixed_final_NE_org_4)
phtest(random_final_NE_org_5, fixed_final_NE_org_5)
phtest(random_final_NE_org_6, fixed_final_NE_org_6)
phtest(random_final_NE_org_7, fixed_final_NE_org_7)
phtest(random_final_NE_org_8, fixed_final_NE_org_8)

### Destination Countries

## Depedent Variable:NI

# Pooling Models

pooling_final_NI_des_1 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL), data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_1)

pooling_final_NI_des_2 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:TPASDW, data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_2)

pooling_final_NI_des_3 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):factor(pdata_des$DS), data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_3)

pooling_final_NI_des_4 <- plm(NI~ PD + GDPPC + NRI + TPASDW +  factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):GDPPC, data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_4)

pooling_final_NI_des_5 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):TPASDW, data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_5)

pooling_final_NI_des_6 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):PD, data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_6)

pooling_final_NI_des_7 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:factor(pdata_des$WSL), data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_7)

pooling_final_NI_des_8 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + TPASDW:factor(pdata_des$DS), data = pdata_des, model = "pooling")
summary(pooling_final_NI_des_8)

# Fixed Models

fixed_final_NI_des_1 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL), data = pdata_des, model = "within")
summary(fixed_final_NI_des_1)

fixed_final_NI_des_2 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:TPASDW, data = pdata_des, model = "within")
summary(fixed_final_NI_des_2)

fixed_final_NI_des_3 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):factor(pdata_des$DS), data = pdata_des, model = "within")
summary(fixed_final_NI_des_3)

fixed_final_NI_des_4 <- plm(NI~ PD + GDPPC + NRI + TPASDW +  factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):GDPPC, data = pdata_des, model = "within")
summary(fixed_final_NI_des_4)

fixed_final_NI_des_5 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):TPASDW, data = pdata_des, model = "within")
summary(fixed_final_NI_des_5)

fixed_final_NI_des_6 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):PD, data = pdata_des, model = "within")
summary(fixed_final_NI_des_6)

fixed_final_NI_des_7 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:factor(pdata_des$WSL), data = pdata_des, model = "within")
summary(fixed_final_NI_des_7)

fixed_final_NI_des_8 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + TPASDW:factor(pdata_des$DS), data = pdata_des, model = "within")
summary(fixed_final_NI_des_8)

# Random Models

random_final_NI_des_1 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL), data = pdata_des, model = "random")
summary(random_final_NI_des_1)

random_final_NI_des_2 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:TPASDW, data = pdata_des, model = "random")
summary(random_final_NI_des_2)

random_final_NI_des_3 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):factor(pdata_des$DS), data = pdata_des, model = "random", random.method = "amemiya")
summary(random_final_NI_des_3)

random_final_NI_des_4 <- plm(NI~ PD + GDPPC + NRI + TPASDW +  factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):GDPPC, data = pdata_des, model = "random")
summary(random_final_NI_des_4)

random_final_NI_des_5 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):TPASDW, data = pdata_des, model = "random")
summary(random_final_NI_des_5)

random_final_NI_des_6 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):PD, data = pdata_des, model = "random")
summary(random_final_NI_des_6)

random_final_NI_des_7 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:factor(pdata_des$WSL), data = pdata_des, model = "random")
summary(random_final_NI_des_7)

random_final_NI_des_8 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + TPASDW:factor(pdata_des$DS), data = pdata_des, model = "random")
summary(random_final_NI_des_8)

## Choosing the most suitable model by performing suitability tests

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_Y1)

plmtest(pooling_final_NI_des_1, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_2, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_3, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_4, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_5, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_6, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_7, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_des_8, type = "bp", effect = "twoways")

# F Test for Fixed Effects Model vs OLS Model
pFtest(fixed_Y1, pooling_Y1)

pFtest(fixed_final_NI_des_1, pooling_final_NI_des_1)
pFtest(fixed_final_NI_des_2, pooling_final_NI_des_2)
pFtest(fixed_final_NI_des_3, pooling_final_NI_des_3)
pFtest(fixed_final_NI_des_4, pooling_final_NI_des_4)
pFtest(fixed_final_NI_des_5, pooling_final_NI_des_5)
pFtest(fixed_final_NI_des_6, pooling_final_NI_des_6)
pFtest(fixed_final_NI_des_7, pooling_final_NI_des_7)
pFtest(fixed_final_NI_des_8, pooling_final_NI_des_8)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_Y1, fixed_Y1)

phtest(random_final_NI_des_1, fixed_final_NI_des_1)
phtest(random_final_NI_des_2, fixed_final_NI_des_2)
phtest(random_final_NI_des_3, fixed_final_NI_des_3)
phtest(random_final_NI_des_4, fixed_final_NI_des_4)
phtest(random_final_NI_des_5, fixed_final_NI_des_5)
phtest(random_final_NI_des_6, fixed_final_NI_des_6)
phtest(random_final_NI_des_7, fixed_final_NI_des_7)
phtest(random_final_NI_des_8, fixed_final_NI_des_8)

## Dependent Variable: IMS_BS

# Pooling Models

pooling_final_IMS_des_1 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL), data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_1)

pooling_final_IMS_des_2 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:TPASDW, data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_2)

pooling_final_IMS_des_3 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):factor(pdata_des$DS), data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_3)

pooling_final_IMS_des_4 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW +  factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):GDPPC, data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_4)

pooling_final_IMS_des_5 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):TPASDW, data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_5)

pooling_final_IMS_des_6 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):PD, data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_6)

pooling_final_IMS_des_7 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:factor(pdata_des$WSL), data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_7)

pooling_final_IMS_des_8 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + TPASDW:factor(pdata_des$DS), data = pdata_des, model = "pooling")
summary(pooling_final_IMS_des_8)

# Fixed Models

fixed_final_IMS_des_1 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL), data = pdata_des, model = "within")
summary(fixed_final_IMS_des_1)

fixed_final_IMS_des_2 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:TPASDW, data = pdata_des, model = "within")
summary(fixed_final_IMS_des_2)

fixed_final_IMS_des_3 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):factor(pdata_des$DS), data = pdata_des, model = "within")
summary(fixed_final_IMS_des_3)

fixed_final_IMS_des_4 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW +  factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):GDPPC, data = pdata_des, model = "within")
summary(fixed_final_IMS_des_4)

fixed_final_IMS_des_5 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):TPASDW, data = pdata_des, model = "within")
summary(fixed_final_IMS_des_5)

fixed_final_IMS_des_6 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):PD, data = pdata_des, model = "within")
summary(fixed_final_IMS_des_6)

fixed_final_IMS_des_7 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:factor(pdata_des$WSL), data = pdata_des, model = "within")
summary(fixed_final_IMS_des_7)

fixed_final_IMS_des_8 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + TPASDW:factor(pdata_des$DS), data = pdata_des, model = "within")
summary(fixed_final_IMS_des_8)

# Random Models

random_final_IMS_des_1 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL), data = pdata_des, model = "random")
summary(random_final_IMS_des_1)

random_final_IMS_des_2 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:TPASDW, data = pdata_des, model = "random")
summary(random_final_IMS_des_2)

random_final_IMS_des_3 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):factor(pdata_des$DS), data = pdata_des, model = "random", random.method = "amemiya")
summary(random_final_IMS_des_3)

random_final_IMS_des_4 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW +  factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):GDPPC, data = pdata_des, model = "random")
summary(random_final_IMS_des_4)

random_final_IMS_des_5 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):TPASDW, data = pdata_des, model = "random")
summary(random_final_IMS_des_5)

random_final_IMS_des_6 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + factor(pdata_des$WSL):PD, data = pdata_des, model = "random")
summary(random_final_IMS_des_6)

random_final_IMS_des_7 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + NRI:factor(pdata_des$WSL), data = pdata_des, model = "random")
summary(random_final_IMS_des_7)

random_final_IMS_des_8 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_des$DS) + factor(pdata_des$WSL) + TPASDW:factor(pdata_des$DS), data = pdata_des, model = "random")
summary(random_final_IMS_des_8)

## Choosing the most suitable model by performing suitability tests

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_Y1)

plmtest(pooling_final_IMS_des_1, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_2, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_3, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_4, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_5, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_6, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_7, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_des_8, type = "bp", effect = "twoways")

# LM Test for Fixed Effects Model vs OLS Model
pFtest(fixed_Y1, pooling_Y1)

pFtest(fixed_final_IMS_des_1, pooling_final_IMS_des_1)
pFtest(fixed_final_IMS_des_2, pooling_final_IMS_des_2)
pFtest(fixed_final_IMS_des_3, pooling_final_IMS_des_3)
pFtest(fixed_final_IMS_des_4, pooling_final_IMS_des_4)
pFtest(fixed_final_IMS_des_5, pooling_final_IMS_des_5)
pFtest(fixed_final_IMS_des_6, pooling_final_IMS_des_6)
pFtest(fixed_final_IMS_des_7, pooling_final_IMS_des_7)
pFtest(fixed_final_IMS_des_8, pooling_final_IMS_des_8)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_Y1, fixed_Y1)

phtest(random_final_IMS_des_1, fixed_final_IMS_des_1)
phtest(random_final_IMS_des_2, fixed_final_IMS_des_2)
phtest(random_final_IMS_des_3, fixed_final_IMS_des_3)
phtest(random_final_IMS_des_4, fixed_final_IMS_des_4)
phtest(random_final_IMS_des_5, fixed_final_IMS_des_5)
phtest(random_final_IMS_des_6, fixed_final_IMS_des_6)
phtest(random_final_IMS_des_7, fixed_final_IMS_des_7)
phtest(random_final_IMS_des_8, fixed_final_IMS_des_8)

#### Both Origin and Destination Countries

## Dependent Variable: NE

# Pooling Models

pooling_final_NE_both_1 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_1)

pooling_final_NE_both_2 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_2)

pooling_final_NE_both_3 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_3)

pooling_final_NE_both_4 <- plm(NE~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_4)

pooling_final_NE_both_5 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_5)

pooling_final_NE_both_6 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_6)

pooling_final_NE_both_7 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_7)

pooling_final_NE_both_8 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "pooling")
summary(pooling_final_NE_both_8)

# Fixed Models

fixed_final_NE_both_1 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "within")
summary(fixed_final_NE_both_1)

fixed_final_NE_both_2 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "within")
summary(fixed_final_NE_both_2)

fixed_final_NE_both_3 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "within")
summary(fixed_final_NE_both_3)

fixed_final_NE_both_4 <- plm(NE~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "within")
summary(fixed_final_NE_both_4)

fixed_final_NE_both_5 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "within")
summary(fixed_final_NE_both_5)

fixed_final_NE_both_6 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "within")
summary(fixed_final_NE_both_6)

fixed_final_NE_both_7 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "within")
summary(fixed_final_NE_both_7)

fixed_final_NE_both_8 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "within")
summary(fixed_final_NE_both_8)

# Random Models

random_final_NE_both_1 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "random")
summary(random_final_NE_both_1)

random_final_NE_both_2 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_2)

random_final_NE_both_3 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_3)

random_final_NE_both_4 <- plm(NE~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_4)

random_final_NE_both_5 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_5)

random_final_NE_both_6 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_6)

random_final_NE_both_7 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_7)

random_final_NE_both_8 <- plm(NE~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NE_both_8)

## Choosing the most suitable model by performing suitability tests

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_Y1)

plmtest(pooling_final_NE_both_1, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_2, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_3, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_4, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_5, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_6, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_7, type = "bp", effect = "twoways")
plmtest(pooling_final_NE_both_8, type = "bp", effect = "twoways")

# LM Test for fixed Effects Model vs OLS Model
pFtest(fixed_Y1, pooling_Y1)

pFtest(fixed_final_NE_both_1, pooling_final_NE_both_1)
pFtest(fixed_final_NE_both_2, pooling_final_NE_both_2)
pFtest(fixed_final_NE_both_3, pooling_final_NE_both_3)
pFtest(fixed_final_NE_both_4, pooling_final_NE_both_4)
pFtest(fixed_final_NE_both_5, pooling_final_NE_both_5)
pFtest(fixed_final_NE_both_6, pooling_final_NE_both_6)
pFtest(fixed_final_NE_both_7, pooling_final_NE_both_7)
pFtest(fixed_final_NE_both_8, pooling_final_NE_both_8)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_Y1, fixed_Y1)

phtest(random_final_NE_both_1, fixed_final_NE_both_1)
phtest(random_final_NE_both_2, fixed_final_NE_both_2)
phtest(random_final_NE_both_3, fixed_final_NE_both_3)
phtest(random_final_NE_both_4, fixed_final_NE_both_4)
phtest(random_final_NE_both_5, fixed_final_NE_both_5)
phtest(random_final_NE_both_6, fixed_final_NE_both_6)
phtest(random_final_NE_both_7, fixed_final_NE_both_7)
phtest(random_final_NE_both_8, fixed_final_NE_both_8)

## Dependent Variable: NI

# Pooling Models

pooling_final_NI_both_1 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_1)

pooling_final_NI_both_2 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_2)

pooling_final_NI_both_3 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_3)

pooling_final_NI_both_4 <- plm(NI~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_4)

pooling_final_NI_both_5 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_5)

pooling_final_NI_both_6 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_6)

pooling_final_NI_both_7 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_7)

pooling_final_NI_both_8 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "pooling")
summary(pooling_final_NI_both_8)

# Fixed Models

fixed_final_NI_both_1 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "within")
summary(fixed_final_NI_both_1)

fixed_final_NI_both_2 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "within")
summary(fixed_final_NI_both_2)

fixed_final_NI_both_3 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "within")
summary(fixed_final_NI_both_3)

fixed_final_NI_both_4 <- plm(NI~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "within")
summary(fixed_final_NI_both_4)

fixed_final_NI_both_5 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "within")
summary(fixed_final_NI_both_5)

fixed_final_NI_both_6 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "within")
summary(fixed_final_NI_both_6)

fixed_final_NI_both_7 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "within")
summary(fixed_final_NI_both_7)

fixed_final_NI_both_8 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "within")
summary(fixed_final_NI_both_8)

# Random Models

random_final_NI_both_1 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "random")
summary(random_final_NI_both_1)

random_final_NI_both_2 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_2)

random_final_NI_both_3 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_3)

random_final_NI_both_4 <- plm(NI~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_4)

random_final_NI_both_5 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_5)

random_final_NI_both_6 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_6)

random_final_NI_both_7 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_7)

random_final_NI_both_8 <- plm(NI~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_NI_both_8)


## Choosing the most suitable model by performing suitability tests

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_Y1)

plmtest(pooling_final_NI_both_1, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_2, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_3, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_4, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_5, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_6, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_7, type = "bp", effect = "twoways")
plmtest(pooling_final_NI_both_8, type = "bp", effect = "twoways")

# LM Test for Fixed Effects Model vs OLS Model
pFtest(fixed_Y1, pooling_Y1)

pFtest(fixed_final_NI_both_1, pooling_final_NI_both_1)
pFtest(fixed_final_NI_both_2, pooling_final_NI_both_2)
pFtest(fixed_final_NI_both_3, pooling_final_NI_both_3)
pFtest(fixed_final_NI_both_4, pooling_final_NI_both_4)
pFtest(fixed_final_NI_both_5, pooling_final_NI_both_5)
pFtest(fixed_final_NI_both_6, pooling_final_NI_both_6)
pFtest(fixed_final_NI_both_7, pooling_final_NI_both_7)
pFtest(fixed_final_NI_both_8, pooling_final_NI_both_8)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_Y1, fixed_Y1)

phtest(random_final_NI_both_1, fixed_final_NI_both_1)
phtest(random_final_NI_both_2, fixed_final_NI_both_2)
phtest(random_final_NI_both_3, fixed_final_NI_both_3)
phtest(random_final_NI_both_4, fixed_final_NI_both_4)
phtest(random_final_NI_both_5, fixed_final_NI_both_5)
phtest(random_final_NI_both_6, fixed_final_NI_both_6)
phtest(random_final_NI_both_7, fixed_final_NI_both_7)
phtest(random_final_NI_both_8, fixed_final_NI_both_8)

## Depedent Variable: IMS_BS

# Pooling Models

pooling_final_IMS_both_1 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_1)

pooling_final_IMS_both_2 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_2)

pooling_final_IMS_both_3 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_3)

pooling_final_IMS_both_4 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_4)

pooling_final_IMS_both_5 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_5)

pooling_final_IMS_both_6 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_6)

pooling_final_IMS_both_7 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_7)

pooling_final_IMS_both_8 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "pooling")
summary(pooling_final_IMS_both_8)

# Fixed Models

fixed_final_IMS_both_1 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "within")
summary(fixed_final_IMS_both_1)

fixed_final_IMS_both_2 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "within")
summary(fixed_final_IMS_both_2)

fixed_final_IMS_both_3 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "within")
summary(fixed_final_IMS_both_3)

fixed_final_IMS_both_4 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "within")
summary(fixed_final_IMS_both_4)

fixed_final_IMS_both_5 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "within")
summary(fixed_final_IMS_both_5)

fixed_final_IMS_both_6 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "within")
summary(fixed_final_IMS_both_6)

fixed_final_IMS_both_7 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "within")
summary(fixed_final_IMS_both_7)

fixed_final_IMS_both_8 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "within")
summary(fixed_final_IMS_both_8)

# Random Models

random_final_IMS_both_1 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL), data = pdata_both, model = "random")
summary(random_final_IMS_both_1)

random_final_IMS_both_2 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:TPASDW, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_2)

random_final_IMS_both_3 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):factor(pdata_both$DS), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_3)

random_final_IMS_both_4 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW +  factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):GDPPC, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_4)

random_final_IMS_both_5 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):TPASDW, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_5)

random_final_IMS_both_6 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + factor(pdata_both$WSL):PD, data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_6)

random_final_IMS_both_7 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + NRI:factor(pdata_both$WSL), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_7)

random_final_IMS_both_8 <- plm(IMS_BS~ PD + GDPPC + NRI + TPASDW + factor(pdata_both$DS) + factor(pdata_both$WSL) + TPASDW:factor(pdata_both$DS), data = pdata_both, model = "random", random.method = "amemiya")
summary(random_final_IMS_both_8)


## Choosing the most suitable model by performing suitability tests

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_Y1)

plmtest(pooling_final_IMS_both_1, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_2, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_3, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_4, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_5, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_6, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_7, type = "bp", effect = "twoways")
plmtest(pooling_final_IMS_both_8, type = "bp", effect = "twoways")

# F Test for Fixed Effects Model vs OLS Model
pFtest(fixed_Y1, pooling_Y1)

pFtest(fixed_final_IMS_both_1, pooling_final_IMS_both_1)
pFtest(fixed_final_IMS_both_2, pooling_final_IMS_both_2)
pFtest(fixed_final_IMS_both_3, pooling_final_IMS_both_3)
pFtest(fixed_final_IMS_both_4, pooling_final_IMS_both_4)
pFtest(fixed_final_IMS_both_5, pooling_final_IMS_both_5)
pFtest(fixed_final_IMS_both_6, pooling_final_IMS_both_6)
pFtest(fixed_final_IMS_both_7, pooling_final_IMS_both_7)
pFtest(fixed_final_IMS_both_8, pooling_final_IMS_both_8)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_Y1, fixed_Y1)

phtest(random_final_IMS_both_1, fixed_final_IMS_both_1)
phtest(random_final_IMS_both_2, fixed_final_IMS_both_2)
phtest(random_final_IMS_both_3, fixed_final_IMS_both_3)
phtest(random_final_IMS_both_4, fixed_final_IMS_both_4)
phtest(random_final_IMS_both_5, fixed_final_IMS_both_5)
phtest(random_final_IMS_both_6, fixed_final_IMS_both_6)
phtest(random_final_IMS_both_7, fixed_final_IMS_both_7)
phtest(random_final_IMS_both_8, fixed_final_IMS_both_8)

