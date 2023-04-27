
################################################
## ---------- Plot the coefficients --------- ##
################################################

library(gridExtra)
library(Rglpk)
library(ggplot2)

load("~/output/v9/m0.RData")
load("~/output/v9/m1.RData")
load("~/output/v9/m2.RData")
load("~/output/v9/m3.RData")
load("~/output/v9/m4.RData")

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
# Coefficients for model 0
m0_co2 <- makeCoVector(m0, "nodeicov.inf")
m0_co3 <- makeCoVector(m0, "nodeicov.pol")
# Coefficients for m1
m1_co2 <- makeCoVector(m1, "nodeicov.inf")
m1_co3 <- makeCoVector(m1, "nodeicov.pol")
m1_co9 <- makeCoVector(m1, "mutual")
m1_co10 <- makeCoVector(m1, "gwesp.fixed.0")
m1_co11 <- makeCoVector(m1, "gwideg.fixed.0")
# Coefficients for model 2
m2_co1 <- makeCoVector(m2, "edges")
m2_co2 <- makeCoVector(m2, "nodeicov.inf")
m2_co3 <- makeCoVector(m2, "nodeicov.pol")
m2_co9 <- makeCoVector(m2, "mutual")
m2_co10 <- makeCoVector(m2, "gwesp.fixed.0")
m2_co11 <- makeCoVector(m2, "gwideg.fixed.0")
m2_co12 <- makeCoVector(m2, "nodeicov.all_deg")
# Indegree terms for model 3
m3_co12 <- makeCoVector(m3, "nodeicov.all_deg")
m3_co13 <- makeCoVector(m3, "nodeicov.inf_deg")
# Indegree terms for model 4
m4_co12 <- makeCoVector(m4, "nodeicov.all_deg")
m4_co14 <- makeCoVector(m4, "nodeicov.pol_deg")

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
pvA_m3 <- makePvAnova(m2, m3) # change m2 -> m3
pvA_m4 <- makePvAnova(m2, m4) # change m2  -> m4


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
# P-values for m0
m0_pco2 <- makePvCo(m0, "nodeicov.inf")
m0_pco3 <- makePvCo(m0, "nodeicov.pol")
# P-values for m1
m1_pco2 <- makePvCo(m1, "nodeicov.inf")
m1_pco3 <- makePvCo(m1, "nodeicov.pol")
m1_pco9 <- makePvCo(m1, "mutual")
m1_pco10 <- makePvCo(m1, "gwesp.fixed.0")
m1_pco11 <- makePvCo(m1, "gwideg.fixed.0")
# P-values for m2
m2_pco2 <- makePvCo(m2, "nodeicov.inf")
m2_pco3 <- makePvCo(m2, "nodeicov.pol")
m2_pco9 <- makePvCo(m2, "mutual")
m2_pco10 <- makePvCo(m2, "gwesp.fixed.0")
m2_pco11 <- makePvCo(m2, "gwideg.fixed.0")
m2_pco12 <- makePvCo(m2, "nodeicov.all_deg")
# P-values for m3
m3_pco12 <- makePvCo(m3, "nodeicov.all_deg")
m3_pco13 <- makePvCo(m3, "nodeicov.inf_deg")
# P-values for m4
m4_pco12 <- makePvCo(m4, "nodeicov.all_deg")
m4_pco14 <- makePvCo(m4, "nodeicov.pol_deg")

# Black and white plot function
# Plotting without anova test
plotCoPv_m1 <- function(coefficient, pv_plot, coefname_modelnr){
  df <- data.frame(c(2:48), coefficient, pv_plot)
  colnames(df) <- c("Month", "Coefficient", "Pvalue")
  plot_plot_name <- ggplot(df, aes(x=Month, y=Coefficient, group=1)) + ggtitle(coefname_modelnr)+
    geom_line()+
    theme_bw() +
    geom_hline(yintercept = 0, color = 'black', size = 1)+
    geom_point(aes(fill=factor(pv_plot), color=factor(pv_plot), shape=factor(pv_plot), size=4),size=3)+ 
    scale_fill_manual(values=c('1'='black', '2'='black'))+
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
    geom_point(aes(fill=factor(pv_plot), color=factor(pv_plot), shape=factor(pv_plot), size=4),size=3)+ 
    scale_fill_manual(values=c('1'='black', '2'='black', '3'='black', '4'='black'))+
    scale_color_manual(values=c('1'='black', '2'='black', '3'='black', '4'='black'))+
    scale_shape_manual(values=c('1' = 21, '2'=24, '3'=13, '4'=4)  )+
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
m0_p2 <- plotCoPv_m1(m0_co2, m0_pco2, "Influence Effect")
m0_p3 <- plotCoPv_m1(m0_co3, m0_pco3, "Government Effect")
# M1 without indicating the anova test
m1_p2 <- plotCoPv_m1(m1_co2, m1_pco2, "Influence Effect")
m1_p3 <- plotCoPv_m1(m1_co3, m1_pco3, "Government Effect")
# M1 with anova test
#m1_p2_anova <- plotCoPv(m1_co2, pvA_m1, m1_pco2, "Influence Effect")
#m1_p3_anova <- plotCoPv(m1_co3, pvA_m1, m1_pco3, "Government Effect")
m1_p9 <- plotCoPv(m1_co9, pvA_m1, m1_pco9, "Reciprocity") 
m1_p10 <- plotCoPv(m1_co10, pvA_m1, m1_pco10, "GWESP")
m1_p11 <- plotCoPv(m1_co11, pvA_m1, m1_pco11, "GWI Degree")
# Model 2 
m2_p2 <- plotCoPv(m2_co2, pvA_m2, m2_pco2, "Influence Effect")
m2_p3 <- plotCoPv(m2_co3, pvA_m2, m2_pco3, "Governmental Effect")
m2_p9 <- plotCoPv(m2_co9, pvA_m2, m2_pco9, "Reciprocity") 
m2_p10 <- plotCoPv(m2_co10, pvA_m2, m2_pco10, "GWESP")
m2_p11 <- plotCoPv(m2_co11, pvA_m2, m2_pco11, "GWI Degree")
m2_p12 <- plotCoPv(m2_co12, pvA_m2, m2_pco12, "Popularity Effect") 
# Model 3 
m3_p13 <- plotCoPv(m3_co13, pvA_m3, m3_pco13, "Influence In-degree")
# Model 4 
m4_p14 <- plotCoPv(m4_co14, pvA_m4, m4_pco14, "Government In-degree")

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
pdf("output/pdf/m1_inf_gov.pdf", width = 14, height = 7)
p2 <- m1_p2 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p3 <- m1_p3 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
plots_ALL <- grid.arrange(p2, p3,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()
# Save popularity + inf indeg + gov indeg -plot
pdf("output/pdf/popularity_inf.indeg_gov.indeg.pdf", width = 14, height = 12)
plot.new()
p12 <- m2_p12 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p13 <- m3_p13 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p14 <- m4_p14 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
plots_ALL <- grid.arrange(p12, p13, p14,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2), 
                                                c(NA, 3)));
legend('bottomleft', legend = c('Anova-test and coefficient', 'Anova-test', 'Coefficient', 'None'), 
       border= c('black', 'black', 'black', 'black'),
       pch= c(21, 24, 13, 4), #'1' = 21, '2'=24, '3'=13, '4'=4)
       pt.bg= 'black', 
       bty = 'o', cex = 2, title = "Significant p-value")
dev.off()
