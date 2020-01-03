setwd('G:/source_tracking/evaluation')
cutoff = 0.0
result = efs(cutoff)
Pr = result$Pr
Rc = result$Rc 
AvgPr = result$AvgPr
AvgRc = result$AvgRc
em = result$em
f1 = result$f1
s1 = result$s1
cat("Average Precision: ", AvgPr, "\n")
cat("Average Recall: ", AvgRc, "\n")
cat("exact match: ", em, "\n")
cat("F1 measure: ", f1, "\n")
cat("S-measure: ", s1, "\n")