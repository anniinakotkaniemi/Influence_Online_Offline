# Influence score & Twitter ranking figure

# Highest indegrees in the Twitter data by organization
twitter.in <- table(nt.climate$org_R2_receiver)
twitter.in <- data.frame(twitter.in)
twitter.in <- twitter.in[order(twitter.in$Freq, decreasing=T),]
twitter.in$rank <- c(1:100)
colnames(twitter.in) <- c("org_R2", "tw.indeg", "tw.rank")
twitter.in

# Make a dataframe with influence score and Twitter ranking
inf.tw <- merge(df, twitter.in, by.x = "org_R2", by.y = "org_R2", all.x = FALSE,
               all.y = FALSE)
inf.tw <- select(inf.tw, org_R2, ID, OT_BIG, n1, tw.rank)
inf.tw <- inf.tw[order(inf.tw$n1, decreasing=T),]
inf.tw$inf.rank <- c(1:100)
inf25.tw <- inf.tw[c(1:25),]
# Re-do Twitter ranking to only include top 25 influential organizations
inf25.tw <- inf25.tw[order(inf25.tw$tw.rank, decreasing=F),]
inf25.tw$tw.rank <- c(1:25)
inf25.tw <- inf25.tw[order(inf25.tw$inf.rank, decreasing=F),]
mat <- select(inf25.tw, org_R2, OT_BIG, inf.rank, tw.rank)
# English labels
org_en <- read.csv2("~/Documents/org_en.csv", stringsAsFactors = F)
org_en <- org_en[,1]
org_en <- rev(org_en)
# Color organizations
ot.col  <- c("#4477AA","#CCBB44", "#AA3377", "#228833")
ot.col <- ot.col[as.numeric(inf25.tw$OT_BIG)]

# Make figure
plot.new()
par(mar = c(2, 20, 1, 8), xpd=TRUE) 
plot.window(xlim = c(0, 1), ylim = c(25, 1))
axis(1, at = c(0,1), labels = c("Reputational Influence", "Retweet Ranking"), line = NA, lty=0)
axis(2, at = 25:1, labels = org_en, las = 2, lwd=0)
points(x = rep(0,25), y = mat[, 3], pch = 21, col = ot.col, bg = ot.col, cex=1.8)
points(x = rep(1,25), y = mat[, 4], pch = 21, col = ot.col, bg = ot.col, cex=1.8)
segments(x0 = 0, x1 = 1, y0 = mat[,3], y1 = mat[,4], lty = 1, adjustcolor(col = ot.col, alpha.f = 0.5), lwd = 1.5)
legend('topright', inset=c(-0.55,0), legend = c('Government', 'Science', 'Business', 'Civil Society'), 
        col= c("#4477AA","#CCBB44", "#AA3377", "#228833"),
       border= F,
       pch= c(21, 21, 21, 21),
       pt.bg= c("#4477AA","#CCBB44", "#AA3377", "#228833"),
       bg='white',
       bty = 'n', cex = 1, title = "Organisation Type")
mtext("Top 25 Most Reputationally Influential Organisations", 
      side = 2, line = -2, at = -0.5, adj = 1.05, col = "black", font = 2)
#box()

