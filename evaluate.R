library("stringr")

comparison = function(y_tru, y_pre){
  qamount = 0
  exactmatch = 0
  ru = list()
  mi =list()
  cr = list()
  tmp = list()
  YT = unlist(strsplit(as.character(y_tru), split = ","))
  YP = unlist(strsplit(as.character(y_pre), split = ","))
  if (identical(YT, YP)){
    exactmatch = 1 + exactmatch
    tmp = YT
    return('exact match')
  }
  else{
    ru = append(ru, YT[!YT%in%YP]) #in T not in P
    mi = append(mi, YP[!YP%in%YT]) #in P not in T
    cr = append(cr, YP[YP%in%YT]) #in P and T
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
  data1 = file('argminer_IC_new.txt',"r")
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

efs = function(cutoff){
  em = 0
  f1 = 0
  s1 = 0
  Pr = 0 #precision
  Rc = 0 #recall
  m = 0
  n = 0
  avgpr = 0 #avg precision
  avgrc = 0 #avg recall
  ru = 0
  mi = 0
  #argf = commandArgs(trailingOnly = TRUE) ## it may be wrong here, check later
  #data2 = read.table(argf[1], sep = " ")
  #data2 = read.table('argminer_res0.uniq', sep = " ")
  data2 = file('argminer_res0.uniq',"r")
  result = alnres(data2)
  #result = alnres('argminer_res0.uniq')
  qid = result$qid
  tid = result$tid
  qlabel = result$qlabel
  tlabel = result$tlabel
  feature = result$feature
  
  for (i in 1:length(qid)){
    pr = 0
    rc = 0
    compres = comparison(qlabel[[i]],tlabel[[i]])
    seqid = feature[[1]][[i]]
    if(seqid >= cutoff){
      if(identical(compres, 'exact match')){
        em = em + 1
        pr = 1
        rc = 1
        ru = ru + 0
        mi = mi + 0
      }
      else{
        pr = length(compres$cr)/(length(compres$cr) + length(compres$mi))
        rc = length(compres$cr)/(length(compres$cr) + length(compres$ru))
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
      m = 1 + m
      n = 1 + n
    }
    
    if(seqid < cutoff){
      rc = 0
      n = n + 1
      Rc = Rc + rc
      for (j in 1:length(compres$ru)){
        if (length(compres$ru) == 0){
          break
        }
        ru = ru + readIC(compres$ru[[j]])
      }
      for (j in 1:length(compres$cr)){
        if (length(compres$cr) == 0){
          break
        }
        ru = ru + readIC(compres$cr[[j]])
      }
    }
  }
  
  avgpr = Pr/m
  avgrc = Rc/n
  emv = em/n
  f1 = 2*avgpr*avgrc/(avgpr+avgrc)
  ru = ru/n
  mi = mi/n
  s1 = sqrt(ru ^ 2 + mi ^ 2)
  result = list(emv = emv, f1 =  f1, s1 = s1)
  return(result)
}