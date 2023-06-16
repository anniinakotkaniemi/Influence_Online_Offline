
################################################
## ---------- Plot the ERGM results --------- ##
################################################

library(gridExtra)
library(Rglpk)
library(ggplot2)
load("~/output/final/m0.RData")
load("~/output/final/m1.RData")
load("~/output/final/m2.RData")
load("~/output/final/m3.RData")

## Extract coefficients
makeCo <- function(model_list, coef_name){
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
# Extract p-values for the coefficients
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
# P-values for deviance test
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

# Coefficients for m1
co1 <- makeCo(m1, "nodeicov.inf")
co2 <- makeCo(m1, "nodeicov.pol")
co3 <- makeCo(m1, "gwideg.fixed.0")
co4 <- makeCo(m1, "nodeicov.all_deg")
# Coefficients for model 2
co5 <- makeCo(m2, "nodeicov.inf_deg")
# Coefficients for model 3
co6 <- makeCo(m3, "nodeicov.pol_deg")
# Coefficient P-values for m1
cop1 <- makePvCo(m1, "nodeicov.inf")
cop2 <- makePvCo(m1, "nodeicov.pol")
cop3 <- makePvCo(m1, "gwideg.fixed.0")
cop4 <- makePvCo(m1, "nodeicov.all_deg")
# Coefficient P-values for m2
cop5 <- makePvCo(m2, "nodeicov.inf_deg")
# Coefficient P-values for m3
cop6 <- makePvCo(m3, "nodeicov.pol_deg")
## List p-values ANOVA
p_m1 <- makePvAnova(m0, m1) # change m0 -> m1
p_m2 <- makePvAnova(m1, m2) # change m1 -> m2
p_m3 <- makePvAnova(m1, m3) # change m1 -> m3


###### Plotting ##### 
# Fix labels
labelsm <- c("Jan '18", "Jul '18",
             "Jan '19", "Jul '19",
             "Jan '20", "Jul '20",
             "Jan '21", "Jul '21")
# Function to plot coefficients and p-values without ANOVA test
plotCoPv_m1 <- function(coefficient, pv_plot, coefname_modelnr){
  df <- data.frame(c(2:48), coefficient, pv_plot)
  colnames(df) <- c("Month", "Coefficient", "Pvalue")
  px <- ggplot(df, aes(x=Month, y=Coefficient, group=1)) + ggtitle(coefname_modelnr)+
    geom_line()+
    theme_bw() +
    scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm) +
    geom_hline(yintercept = 0, color = 'black', linewidth = 1)+
    geom_point(aes(fill=factor(pv_plot), color=factor(pv_plot), shape=factor(pv_plot), size=4),size=4)+ 
    scale_fill_manual(values=c('1'='#0072B2', '2'='#F05039'))+
    scale_color_manual(values=c('1'='black', '2'='black'))+
    scale_shape_manual(values=c('1' = 21, '2'=4))+
    labs(x = "") +
    theme(legend.position = "none",
          plot.title=element_text(size=20),
          axis.title.x=element_text(vjust=0,  
                                    size=18),  
          axis.title.y=element_text(size=18),
          axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14, colour = 'black'))
  px
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
  px <- ggplot(df, aes(x=Month, y=Coefficient, group=1)) + ggtitle(coefname_modelnr)+
    geom_line()+
    theme_bw() +
    scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm) +
    geom_hline(yintercept = 0, color = 'black', linewidth = 1)+
    geom_point(aes(fill=factor(pv_plot), color=factor(pv_plot), shape=factor(pv_plot), size=4),size=4)+ 
    scale_fill_manual(values=c('1'='#0072B2', '2'='#0072B2', '3'='#F05039', '4'='#F05039'))+
    scale_color_manual(values=c('1'='black', '2'='black', '3'='black', '4'='black'))+
    scale_shape_manual(values=c('1' = 21, '2'=24, '3'=4, '4'=4)  )+
    labs(x = "") +
    theme(legend.position = "none",
          plot.title=element_text(size=20),
          axis.title.x=element_text(vjust=0,  
                                    size=18),  
          axis.title.y=element_text(size=18),
          axis.text.y=element_text(size=14, colour = 'black'),
          axis.text.x=element_text(size=14, colour = 'black'))
  px
}

# M1 without the deviance test p-value
p1 <- plotCoPv_m1(co1, cop1, "Reputational Influence")
p2 <- plotCoPv_m1(co2, cop2, "Formal-institutional Influence")
p3 <- plotCoPv_m1(co3, cop3, "gwidegree")
p4 <- plotCoPv_m1(co4, cop4, "Popularity Effect")
# Model 2 
p5 <- plotCoPv(co5, p_m2, cop5, "Reputational Boosting Influence")
# Model 3 
p6 <- plotCoPv(co6, p_m3, cop6, "Formal-institutional Boosting Influence")

######
## Save plots as pdf
# Save Influence -plot
pdf("output/pdf/inf.pdf", width = 7, height =7)
p1
dev.off()
# Save government -plot
pdf("output/pdf/gov.pdf", width = 7, height =7)
p2 
dev.off()
# Save influence + government in the same figure
pdf("output/pdf/inf_gov.pdf", width = 14, height = 7)
plots_ALL <- grid.arrange(p1, p2,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()
# Save inf indeg + gov indeg -plot
pdf("output/pdf/inf_gov_indeg.pdf", width = 14, height = 7)
plots_ALL <- grid.arrange(p5, p6,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()
# Save GWI degree and popularity plots
pdf("output/pdf/gwi_popularity.pdf", width = 14, height = 7)
plots_ALL <- grid.arrange(p4, p3,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()

rm(co1, co2, co3, co4, co5, co6, cop1, cop2, cop3, cop4, cop5, cop6, 
   p_m1, p_m2, p_m3, p1, p2, p3, p4, p5, p6)
#save.image("~/FI Workspace Objects 0706.RData")
#                                               ;
#legend('bottomleft', legend = c('Anova-test and coefficient', 'Anova-test', 'Coefficient', 'None'), 
#       border= c('black', 'black', 'black', 'black'),
#       pch= c(21, 24, 13, 4), #'1' = 21, '2'=24, '3'=13, '4'=4)
#       pt.bg= 'black', 
#       bty = 'o', cex = 2, title = "Significant p-value")
