
################################################
## ---------- Plot the coefficients --------- ##
################################################

library(gridExtra)
library(Rglpk)
library(ggplot2)

#load("~/output/v9/m0.RData")
#load("~/output/v9/m1.RData")
#load("~/output/v9/m2.RData")
#load("~/output/v9/m3.RData")
#load("~/output/v9/m4.RData")

## Extract coefficients
makeCoVector <- function(model_list, coef_name){
  # Coefficients
  coef_list <- list()
  for(model_i in c(1:length(model_list))){
    model <- model_list[[model_i]]
    matrix_coef <- summary(model_list[[model_i]])$coefficients[coef_name, "Estimate"]
    coef_list[[(model_i)]] <- matrix_coef
  }
  coef_list
  coef <- as.numeric(paste0(coef_list))
  coef
}

# Coefficients for m1
m1_co2 <- makeCoVector(m1, "nodeicov.inf")
m1_co3 <- makeCoVector(m1, "nodeicov.pol")
m1_co11 <- makeCoVector(m1, "gwideg.fixed.0")
m1_co12 <- makeCoVector(m1, "nodeicov.all_deg")
# Coefficients for model 2
m2_co13 <- makeCoVector(m2, "nodeicov.inf_deg")
# Indegree terms for model 3
m3_co14 <- makeCoVector(m3, "nodeicov.pol_deg")

# P-values for anova-test
makePvAnova <- function(model_list_1, model_list_2){
  pvalue_list <- list()
  for(m_i in c(1:length(model_list_1))){
    anova_list <- Map(anova, model_list_1, model_list_2) 
    pvalue <- anova_list[[m_i]]$`Pr(>|Chisq|)`[3]
    pvalue_list[[(m_i)]] <- pvalue
    pvalue <- as.numeric(paste0(pvalue_list))
    options(scipen=999)
    pvalue[pvalue>0.05] <- 2
    pvalue[pvalue< 0.05 ] <- 1
  }
  pvalue
}
## List p-values ANOVA
pvA_m1 <- makePvAnova(m0, m1) # change m0 -> m1
pvA_m2 <- makePvAnova(m1, m2) # change m1 -> m2
pvA_m3 <- makePvAnova(m1, m3) # change m1 -> m3

# List p-values for the coefficients
makePvCo <- function(model_list, coef_name){
  pv_list <- list()
  for(model_i in c(1:length(model_list))){
    model <- model_list[[model_i]]
    matrix_coef <- summary(model_list[[model_i]])$coefficients[coef_name, 5]
    pv_list[[(model_i)]] <- matrix_coef
  }
  pv <- as.numeric(paste0(pv_list))
  options(scipen=999)
  pv[pv>0.05] <- 2
  pv[pv< 0.05 ] <- 1
  pv[is.na(pv)] <- 3
  pv
}
# P-values for m1
m1_pco2 <- makePvCo(m1, "nodeicov.inf")
m1_pco3 <- makePvCo(m1, "nodeicov.pol")
m1_pco11 <- makePvCo(m1, "gwideg.fixed.0")
m1_pco12 <- makePvCo(m1, "nodeicov.all_deg")
# P-values for m2
m2_pco13 <- makePvCo(m2, "nodeicov.inf_deg")
# P-values for m3
m3_pco14 <- makePvCo(m3, "nodeicov.pol_deg")

###### Plotting ##### 
# Function to plot coefficients and p-values without ANOVA test
plotCoPv_m1 <- function(coefficient, pv_plot, coefname_modelnr){
  df <- data.frame(c(2:48), coefficient, pv_plot)
  colnames(df) <- c("Month", "Coefficient", "Pvalue")
  plot_plot_name <- ggplot(df, aes(x=Month, y=Coefficient, group=1)) + ggtitle(coefname_modelnr)+
    geom_line()+
    theme_bw() +
    geom_hline(yintercept = 0, color = 'black', size = 1)+
    geom_point(aes(fill=factor(pv_plot), color=factor(pv_plot), shape=factor(pv_plot), size=4),size=4)+ 
    scale_fill_manual(values=c('1'='#0072B2', '2'='#F05039'))+
    scale_color_manual(values=c('1'='black', '2'='black'))+
    scale_shape_manual(values=c('1' = 21, '2'=4))+
    theme(legend.position = "none",
          plot.title=element_text(size=20
                                  #   ,family="Times New Roman"
          ),
          axis.title.x=element_text(vjust=0,  
                                    size=18, 
          ),  
          axis.title.y=element_text(size=18),
          axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14, colour = 'black'
                                   #,angle = 30,vjust=.5
          ))
  plot_plot_name
}

# Function to plot coefficients and p-values with ANOVA test
plotCoPv <- function(coefficient, pvalue_anova, pvalue_co, coefname_modelnr){
  pv_df <- data.frame(pvalue_anova, pvalue_co)
  pv_df$pv <- NA
  pv_df$pv[pv_df$pvalue_anova==1 & pv_df$pvalue_co==1] <- "1"
  pv_df$pv[pv_df$pvalue_anova==1 & pv_df$pvalue_co==2] <- "2"
  pv_df$pv[pv_df$pvalue_anova==2 & pv_df$pvalue_co==1] <- "3"
  pv_df$pv[pv_df$pvalue_anova==2 & pv_df$pvalue_co==2] <- "4"
  pv_plot <-pv_df$pv
  df <- data.frame(c(2:48), coefficient, pv_plot)
  colnames(df) <- c("Month", "Coefficient", "Pvalue")
  plot_plot_name <- ggplot(df, aes(x=Month, y=Coefficient, group=1)) + ggtitle(coefname_modelnr)+
    geom_line()+
    theme_bw() +
    geom_hline(yintercept = 0, color = 'black', size = 1)+
    geom_point(aes(fill=factor(pv_plot), color=factor(pv_plot), shape=factor(pv_plot), size=4),size=4)+ 
    scale_fill_manual(values=c('1'='#0072B2', '2'='#0072B2', '3'='#F05039', '4'='#F05039'))+
    scale_color_manual(values=c('1'='black', '2'='black', '3'='black', '4'='black'))+
    scale_shape_manual(values=c('1' = 21, '2'=24, '3'=4, '4'=4)  )+
    theme(legend.position = "none",
          plot.title=element_text(size=20
                                  #   ,family="Times New Roman"
          ),
          axis.title.x=element_text(vjust=0,  
                                    size=18
                                    #,family="Times New Roman"
          ),  
          axis.title.y=element_text(size=18
                                    #,family="Times New Roman"
          ),
          axis.text.y=element_text(size=14, colour = 'black'
                                   # ,family="Times New Roman"
          ),
          axis.text.x=element_text(size=14, colour = 'black'
                                   #    ,family="Times New Roman"
                                   #,angle = 30,vjust=.5
          ))
  plot_plot_name
}

# M0
m0_p2 <- plotCoPv_m1(m0_co2, m0_pco2, "Reputational Influence")
m0_p3 <- plotCoPv_m1(m0_co3, m0_pco3, "Government Influence")
# M1 without indicating the anova test
m1_p2 <- plotCoPv_m1(m1_co2, m1_pco2, "Reputational Influence")
m1_p3 <- plotCoPv_m1(m1_co3, m1_pco3, "Government Influence")
# Model 2 
m2_p13 <- plotCoPv(m2_co13, pvA_m2, m2_pco13, "Reputational Boosting Influence")
# Model 3 
m3_p14 <- plotCoPv(m3_co14, pvA_m3, m3_pco14, "Government Boosting Influence")

#Fix labels
labelsm <- c("Jan-18", "July-18",
             "Jan-19", "July-19",
             "Jan-20", "July-20",
             "Jan-21", "July-21")

# Save Influence -plot
pdf("output/pdf/m1_influence.pdf", width = 7, height =7)
m1_p2 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
dev.off()
# Save government -plot
pdf("output/pdf/m1_government.pdf", width = 7, height =7)
m1_p3 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
dev.off()
# Save influence + government in the same figure
pdf("output/pdf/m1_inf_gov_2605.pdf", width = 14, height = 7)
p2 <- m1_p2 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p3 <- m1_p3 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
plots_ALL <- grid.arrange(p2, p3,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()
# Save popularity + inf indeg + gov indeg -plot
pdf("output/pdf/inf.indeg_gov.indeg_2605.pdf", width = 14, height = 7)
#plot.new()
#p12 <- m2_p12 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p13 <- m2_p13 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p14 <- m3_p14 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
plots_ALL <- grid.arrange(p13, p14,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
#plots_ALL <- grid.arrange(p12, p13, p14,
#                          ncol=2,
#                          layout_matrix = rbind(c(1, 2), 
 #                                               c(NA, 3)));
#legend('bottomleft', legend = c('Anova-test and coefficient', 'Anova-test', 'Coefficient', 'None'), 
#       border= c('black', 'black', 'black', 'black'),
#       pch= c(21, 24, 13, 4), #'1' = 21, '2'=24, '3'=13, '4'=4)
#       pt.bg= 'black', 
#       bty = 'o', cex = 2, title = "Significant p-value")
dev.off()

