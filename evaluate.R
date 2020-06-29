alnres = function(filein){
  qid = list()
  tid = list()
  seqid = list()
  qlabel = list()#predicted label
  tlabel = list()#true label
  f = readLines(filein)
  close(filein)
  for (line in f) {
    line = trimws(line, which = c("both", "left", "right"))
    temp1 = unlist(strsplit(line, split = "\\s+"))
    temp2 = unlist(strsplit(temp1[1], split = "\\|"))
    qid = c(qid, temp2[1])
    temp3 = unlist(strsplit(temp1[2], split = "\\|"))
    tid = c(tid, temp3[1])
    seqid = append(seqid, temp1[3])
    #alnlen = c(alnlen, as.integer((temp1[4])))
    #nmiss = c(nmiss, as.integer((temp1[5])))
    #ngap = c(ngap, as.integer((temp1[6])))
    #qbegin = c(qbegin, as.integer((temp1[7])))
    #qend = c(qend, as.integer((temp1[8])))
    #tbegin = c(tbegin, as.integer((temp1[9])))
    #tend = c(tend, as.integer((temp1[10])))
    #evalue = c(evalue, as.numeric(temp1[11]))
    #bscore = c(bscore, as.numeric(temp1[12]))
    qlabel = c(qlabel, temp2[2])
    tlabel = c(tlabel, temp3[2])
  }
  #feasco = append(feasco, list(seqid, alnlen, nmiss, ngap, evalue, bscore))
  #feature = feasco
  result = list(qid = qid, tid = tid, qlabel = qlabel, tlabel = tlabel, seqid = seqid)
  return(result)
}

comparison = function(y_tru, y_pre, seqid, cutoff){
  qamount = 0
  exactmatch = 0
  ru = list()
  mi =list()
  cr = list()
  tmp = list()
  delete = c()
  YT = unlist(strsplit(as.character(y_tru), split = ","))
  #YP = unlist(strsplit(as.character(y_pre), split = ","))
  YP = y_pre
  Prob = as.numeric(unlist(strsplit(seqid, split = ",")))
  for (i in 1:length(Prob)) {
    if(Prob[i] < cutoff){
      delete = c(delete,i)
    }
  }
  if(is.null(delete) == FALSE){
    YP = YP[-delete]
  }
  if (identical(YT, YP)){
    exactmatch = 1 + exactmatch
    tmp = YT
    return('exact match')
  }
  else{
    ru = append(ru, YT[!YT%in%YP]) #in T not in P,false negative
    mi = append(mi, YP[!YP%in%YT]) #in P not in T,false positive
    cr = append(cr, YP[YP%in%YT]) #in P and T, true positive
    tmp = append(tmp,list(ru = ru, mi = mi, cr = cr))
    return(tmp)
  }
}

readIC = function(label){
  list_label = c()
  list_IC = c()
  #argv = commandArgs(trailingOnly = TRUE)
  #data1 = read.table(argv[2], sep = " ")
  #data1 = read.table('argminer_IC_new.txt', sep = " ")
  data1 = file(paste0('/mnt/c/Users/ch379/Documents/project/DLMER-Bio/','IC.txt'),"r")
  for (line in readLines(data1)) {
   line = trimws(line, which = c("both", "left", "right")) #eliminate space
   line_temp_1 = unlist(strsplit(line, split = ","))
   list_label = c(list_label, line_temp_1[1])
   list_IC = c(list_IC, as.numeric(line_temp_1[2]))
  }
  close(data1)
  if (isTRUE(label%in%list_label)){
    for (i in 1:length(list_label)) {
      if(identical(label, list_label[i])){
        index_label = i
        break
      }
    }
    IC = list_IC[index_label]
    return(IC)
  }
  return(0)
}

efs = function(cutoff,data_proc){
  em = 0
  f1 = 0
  s1 = 0
  Pr = 0 #sum of precision
  Rc = 0 #sum of recall
  m = 0
  n = 0
  avgpr = 0 #avg precision
  avgrc = 0 #avg recall
  ru = 0
  mi = 0
  FPR = 0
  #argf = commandArgs(trailingOnly = TRUE) ## it may be wrong here, check later
  #data2 = read.table(argf[1], sep = " ")
  #data2 = read.table('argminer_res0.uniq', sep = " ")
  #data2 = file('result.txt',"r")
  result = data_proc
  qid = result$qid
  tid = result$tid
  qlabel = result$qlabel#predicted label
  tlabel = result$tlabel#true label
  #feature = result$feature
  PRPC = result$seqid#the probability of predicted class
  all_label = unlist(strsplit(as.character(qlabel[[1]]), split = ","))
  all_length = length(all_label)
  
  for (i in 1:length(qid)){
  #for (i in 1:1){
    pr = 0
    rc = 0
    seqid = PRPC[[i]]
    compres = comparison(tlabel[[i]],all_label,seqid,cutoff)
    
    if(identical(compres, 'exact match')){
      em = em + 1
      pr = 1
      rc = 1
      fpr = 0
      ru = ru + 0
      mi = mi + 0
    }
    else{
      pr = length(compres$cr)/(length(compres$cr) + length(compres$mi))#precision for each sample
      if(length(compres$cr) + length(compres$mi) == 0){
        pr = 0
      }
      rc = length(compres$cr)/(length(compres$cr) + length(compres$ru))
      FP_TN = all_length - length(compres$cr) - length(compres$ru)
      
      if(FP_TN == 0){
        fpr = 0
      }
      else{
        fpr = length(compres$mi)/FP_TN
      }
      
      for ( j in 1:length(compres$ru)) {
        if (length(compres$ru) == 0){
          break
        }
        ru = ru + readIC(compres$ru[[j]])
      }
      
      for (j in 1:length(compres$mi)) {
        if (length(compres$mi) == 0){
          break
        }
        mi =mi + readIC(compres$mi[[j]])
      }
    }
    Pr = pr + Pr
    Rc = rc + Rc
    FPR = FPR + fpr
    m = 1 + m
    n = 1 + n
  }
  
  avgpr = Pr/m
  avgrc = Rc/n
  TPR = avgrc
  FPR = FPR/n
  emv = em/n
  f1 = 2*avgpr*avgrc/(avgpr+avgrc)
  ru = ru/n
  mi = mi/n
  s1 = sqrt(ru ^ 2 + mi ^ 2)
  result_all = list(Pr = Pr, Rc = Rc, AvgPr = avgpr, AvgRc = avgrc, emv = emv, f1 =  f1, s1 = s1, TPR = TPR, FPR = FPR)
  return(result_all)
}

ParCal = function(thhold){
  cutoff = thhold/100
  result = efs(cutoff, data_proc)
  return(result)
}
