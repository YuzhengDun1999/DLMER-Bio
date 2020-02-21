home = 'G:/source_tracking/evaluation/data1/full_feature'
setwd(home)
library('ggplot2')
ROC = function(TPR, FPR){
  data1 = data.frame(FPR,TPR)
  data2 = data.frame(FPR1 = c(FPR,0,1),TPR1 = c(TPR,0,1))
  data3 = data2[order(data2$FPR1), ]
  area = 0
  FPR_or = data3$FPR1
  TPR_or = data3$TPR1
  for (i in 2:length(FPR_or)) {
    area = area + (TPR_or[i-1] + TPR_or[i])*(FPR_or[i]-FPR_or[i-1])/2
  }
  area = as.character(round(area,5))
  area = paste('AUC =', area)
  p = ggplot(data1, aes(x = FPR, y = TPR)) + geom_point() + geom_area(fill = 'blue', alpha = 0.1) 
  p +  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold')) + ggtitle("ROC") + annotate("text", x = 0.006, y = 0.25, label = area, size = 8)
}
result = read.csv('output.csv')
Pr = result$Pr
Rc = result$Rc
AvgPr = result$AvgPr
AvgRc = result$AvgRc
em = result$em
f1 = result$f1
s1 = result$s1
TPR = result$TPR
FPR = result$FPR
# TPR = c(TPR)
# FPR = c(FPR)
ROC(TPR,FPR)