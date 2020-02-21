setwd('G:/source_tracking/evaluation')
#timestart = Sys.time()
data2 = file('result.txt',"r")
data_proc = alnres(data2)
Pr_all = c()
Rc_all = c()
AvgPr_all = c()
AvgRc_all = c()
em_all = c()
f1_all = c()
s1_all = c()
FPR_all = c()
TPR_all = c()
for(i in 1:100){
  cutoff = i/100
  result = efs(cutoff,data_proc)
  Pr = result$Pr
  Rc = result$Rc
  AvgPr = result$AvgPr
  AvgRc = result$AvgRc
  em = result$em
  f1 = result$f1
  s1 = result$s1
  TPR = result$TPR
  FPR = result$FPR
  Pr_all = c(Pr_all, Pr)
  Rc_all = c(Rc_all, Rc)
  AvgPr_all = c(AvgPr_all, AvgPr)
  AvgRc_all = c(AvgRc_all, AvgRc)
  em_all = c(em_all, em)
  f1_all = c(f1_all, f1)
  s1_all = c(s1_all, s1)
  TPR_all = c(TPR_all, TPR)
  FPR_all = c(FPR_all, FPR)
}
output = list(Pr_all, Rc_all, AvgPr_all, AvgRc_all, em_all, f1_all, s1_all, TPR_all, FPR_all)
write.csv(output, file = "output.csv")
close(data2)
# cutoff = 0.5
# result = efs(cutoff,data_proc)
# timeend = Sys.time()
# running = timeend - timestart
# Pr = result$Pr
# Rc = result$Rc 
# AvgPr = result$AvgPr
# AvgRc = result$AvgRc
# em = result$em
# f1 = result$f1
# s1 = result$s1
# TPR = result$TPR
# FPR = result$FPR
# cat("Average Precision: ", AvgPr, "\n")
# cat("Average Recall: ", AvgRc, "\n")
# cat("exact match: ", em, "\n")
# cat("F1 measure: ", f1, "\n")
# cat("S-measure: ", s1, "\n")
# cat("TPR: ", TPR, "\n")
# cat("FPR: ", FPR, "\n")
# cat("time:",running)
