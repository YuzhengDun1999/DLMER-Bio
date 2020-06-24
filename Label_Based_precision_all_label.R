##evaluation on server
setwd('G:/source_tracking/evaluation/Label_Based/test')
#setwd('G:/source_tracking/evaluation/Label_Based/test_JSD/')
data2 = file('test.txt',"r")

alnres = function(filein){
  f = readLines(filein)
  close(filein)
  all_predicted = c()
  all_true = c()
  all_prob = c()
  all_unique = c()
  all_true_label = c()
  all_true_ID = c()
  all_predicted_ID = c()
  j=0
  for (line in f) {
    j=j+1
    line = trimws(line, which = c("both", "left", "right"))
    temp1 = unlist(strsplit(line, split = "\\s+"))
    temp2 = unlist(strsplit(temp1[1], split = "\\|"))
    # qid = c(qid, temp2[1])
    temp3 = unlist(strsplit(temp1[2], split = "\\|"))
    # tid = c(tid, temp3[1])
    # seqid = append(seqid, temp1[3])
    # qlabel = c(qlabel, temp2[2])
    # tlabel = c(tlabel, temp3[2])
    predicted = unlist(temp2[2])
    predicted = unlist(strsplit(as.character(predicted), split = ","))##splited predicted label
    tlabel = unlist(temp3[2])
    tlabel = unlist(strsplit(as.character(tlabel), split = ","))##splited true label
    true = rep(0,length(predicted))
    unique_label = predicted[!duplicated(predicted)]
    for (i in 1:length(tlabel)) {
      true[which(predicted == tlabel[i])] = 1
    }
    prob = as.numeric(unlist(strsplit(temp1[3], split = ",")))
    all_predicted = c(all_predicted, predicted)
    all_true = c(all_true, true)
    all_prob = c(all_prob, prob)
    all_unique = c(all_unique, unique_label)
    all_true_label = c(all_true_label, tlabel)
    all_predicted_ID = c(all_predicted_ID, rep(j,length(prob)))
    all_true_ID = c(all_true_ID, rep(j,length(tlabel)))
  }
  result = list(true = all_true, predicted = all_predicted, prob = all_prob, unique = all_unique, true_label = all_true_label, true_ID = all_true_ID, pre_ID = all_predicted_ID)
  return(result)
}

evaluate = function(cutoff, all_true, all_predicted, all_prob, all_label, num_label){
  TPR_all = c()
  FPR_all = c()
  TP_all = c()
  FN_all = c()
  FP_all = c()
  TN_all = c()
  accuracy_all = c()
  precision_all = c()
  recall_all = c()
  all_true = data_proc$true
  all_predicted = data_proc$predicted
  all_prob = data_proc$prob
  temp = rep(1, length(all_label))#whether the label is all 1 or all 0
  
  for (i in 1:length(all_label)) {
  
    label = all_label[i]
    true = all_true[which(all_predicted == label)] ###label in true
    prob = all_prob[which(all_predicted == label)] ###label probability
    predicted = prob
    predicted[predicted >= cutoff] = 1
    predicted[predicted < cutoff] = 0
    
    TP = sum(predicted[which(true == 1)]) ##in T and P
    FN = sum(true) - TP ## true-TP
    FP = sum(predicted[which(true == 0)]) ##in P not in T
    TN = length(which(true == 0)) - FP ## false sample - FP
    
    if((TP + FN) == 0){
      temp[i] = 0
      next
    }
    else{
      TPR = TP / (TP + FN)
    }
    
    if((FP + TN) == 0){
      temp[i] = 0
      next
    }
    else{
      FPR = FP / (FP + TN)
    }
    accuracy = (TP+TN)/(TP+FP+TN+FN)
    if((FP+TP)==0){
      precision = 1
    }
    else{
      precision = TP/(TP+FP)
    }
    recall = TP/(TP+FN)
    
    TP_all = c(TP_all, TP)
    FN_all = c(FN_all, FN)
    FP_all = c(FP_all, FP)
    TN_all = c(TN_all, TN)
    TPR_all = c(TPR_all, TPR)
    FPR_all = c(FPR_all, FPR)
    accuracy_all = c(accuracy_all, accuracy)
    precision_all = c(precision_all, precision)
    recall_all = c(recall_all, recall)
  }
  
  if (sum(temp)==0){
    return(list(true="No value"))
  }
  else{
    TP_all = TP_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    FN_all = FN_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    FP_all = FP_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    TN_all = TN_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    TPR_all = TPR_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    FPR_all = FPR_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    accuracy_all = accuracy_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    precision_all = precision_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
    recall_all = recall_all %*% num_label[temp == 1] / sum(num_label[temp == 1])
  
    return(list(true="right",TP = TP_all, FN = FN_all, FP = FP_all, TN = TN_all, TPR = TPR_all, FPR = FPR_all, accuracy = accuracy_all, precision = precision_all, recall = recall_all))
  }
}

AUC = function(FPR, TPR){
  area = 0
  for (i in 2:length(FPR)) {
    area = area + (TPR[i-1] + TPR[i])*(FPR[i-1]-FPR[i])/2
  }
  return(sprintf("%0.4f",area))
}

data_proc = alnres(data2)
#all_label = data_proc$unique[!duplicated(data_proc$unique)]##labels needed to be evluated
all_label = data_proc$true_label[!duplicated(data_proc$true_label)]
num_label = c()
num_true_label = c()#number of true labels
layer_label = c()
output_name = c("first.csv","second.csv","third.csv","fourth.csv","fifth.csv","sixth.csv","seventh.csv","eighth.csv")
for (i in 1:length(all_label)) {
  num_true_label[i] = length(data_proc$true_label[data_proc$true_label==all_label[i]])
}

delete_label = all_label[num_true_label<20]
delete_position = c()
for (i in 1:length(delete_label)) {
  delete_ID = data_proc$true_ID[data_proc$true_label==delete_label[i]]
  for (j in 1:length(delete_ID)) {
    delete_position = c(delete_position, which(data_proc$pre_ID==delete_ID[j]))
  }
  all_label = all_label[-which(all_label==delete_label[i])]
}
for (i in 1:length(all_label)) {
  num_label[i] = length(which(data_proc$unique == all_label[i]))
  layer_label[i]  = length(unlist(strsplit(all_label[i],split = "-")))
}
delete_position = unique(delete_position)
input_true = data_proc$true[-c(delete_position)]
input_predicted = data_proc$predicted[-c(delete_position)]
input_prob = data_proc$prob[-c(delete_position)]
TP_out = c()
FN_out = c()
FP_out = c()
TN_out = c()
FPR_out = c()
TPR_out = c()
accuracy_out = c()
precision_out = c()
recall_out = c()
AUC_all = c()
labels = c()

for(j in 1:length(all_label)){
    input_label = all_label[j]
    input_num = 1
    TP_all = c()
    FN_all = c()
    FP_all = c()
    TN_all = c()
    FPR_all = c()
    TPR_all = c()
    accuracy_all = c()
    precision_all = c()
    recall_all = c()
    for(i in 0:101){
    #for(i in 100:100){
      cutoff = i/100
      result = evaluate(cutoff, input_true, input_predicted, input_prob, input_label, input_num)
      if(result$true == "No value"){
        break
      }
      TP = result$TP
      FN = result$FN
      FP = result$FP
      TN = result$TN
      TPR = result$TPR
      FPR = result$FPR
      accuracy = result$accuracy
      precision = result$precision
      recall = result$recall
      TP_all = c(TP_all, TP)
      FN_all = c(FN_all, FN)
      FP_all = c(FP_all, FP)
      TN_all = c(TN_all, TN)
      TPR_all = c(TPR_all, TPR)
      FPR_all = c(FPR_all, FPR)
      accuracy_all = c(accuracy_all, accuracy)
      precision_all = c(precision_all, precision)
      recall_all = c(recall_all, recall)
    }
    
    if(result$true == "No value"){
      next
    }
    AUC_all = c(AUC_all, AUC(FPR_all, TPR_all))
    TP_out = c(TP_out, TP_all[51])
    FN_out = c(FN_out, FN_all[51])
    FP_out = c(FP_out, FP_all[51])
    TN_out = c(TN_out, TN_all[51])
    TPR_out = c(TPR_out, TPR_all[51])
    FPR_out = c(FPR_out, FPR_all[51])
    accuracy_out = c(accuracy_out, accuracy_all[51])
    precision_out = c(precision_out, precision_all[51])
    recall_out = c(recall_out, recall_all[51])
    labels = c(labels,all_label[j])
}
output = list(label = labels, AUC = AUC_all, precision = precision_out, recall = recall_out, accuracy = accuracy_out, TP = TP_out, FP = FP_out, FN = FN_out, TN = TN_out, TPR=TPR_out,FPR=FPR_out)
write.csv(output, file = "label_preformance.csv")