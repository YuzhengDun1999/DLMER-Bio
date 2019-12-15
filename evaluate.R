library("stringr")

comparison = function(y_tru, y_pre){
  qamount = 0
  exactmatch = 0
  ru = c()
  mi = c()
  cr = c()
  tmp = c()
  YT = unlist(strsplit(y_tru, split = ","))
  YP = unlist(strsplit(y_pre, split = ","))
  if (identical(YT, YP)){
    exactmatch = 1 + exactmatch
    tmp = YT
    return('exact match')
  }
  else{
    ru = c(ru, YT[!YT%in%YP])
    mi = c(mi, YP[!YP%in%YT])
    cr = c(cr, YP[YP%in%YT])
    tmp = c(tmp,ru, mi, cr)
    return(tmp)
  }
}

readIC = function(label){
  list_label = c()
  list_IC = c()
  argf = commandArgs(trailingOnly = TRUE)
  for (line in readLines(argf)) {
   line = trimws(line, which = c("both", "left", "right"))
   line_temp_1 = unlist(strsplit(line, split = ","))
   list_label = c(list_label, list_temp_1[0])
   line_temp_2 = unlist(strsplit(line, split = ","))
   list_IC = c(list_IC, as.numeric(line_temp_2[1]))
  }
  if (isTRUE(label%in%argv)){
    index_label = grep(label, list_label)
    IC = list_IC[index_label]
    return(IC)
  }
  return(0)
}

efs = function(cutoff){
  em = 0
  f1 = 0
  s1 = 0
  Pr = 0
  Rc = 0
  m = 0
  n = 0
  avgpr = 0
  avgrc = 0
  ru = 0
  mi = 0
  #qid,tid,qlabel,tlabel,feature = alnres(sys.argv[1])
  
  for (i in 0:lenth(qid)){
    pr = 0
    rc = 0
    compres = comparison(qlabel[i],tlabel[i])
    seqid = feature[i][0]
    if(seqid >= cutoff){
      if(compres == 'exact match'){
        em = em + 1
        pr = 1
        rc = 1
        ru = ru + 0
        mi = mi + 0
      }
      else{
        pr = lenth(compres[2])/(lenth(compres[2]) + lenth(compres[1]))
        rc = lenth(compres[2])/(lenth(compres[2]) + lenth(compres[0]))
        for ( j in 0:lenth(compres[0])) {
          ru = ru + readIC(compres[0][j])
        }
        for (j in 0:lenth(compres[1])) {
          mi =mi + readIC(compres[1][j])
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
      for (j in 0:lenth(compres[0])){
        ru = ru + readIC(compres[0][j])
      }
      for (j in 0:lenth(compres[2])){
        ru = ru + readIC(compres[2][j])
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
  result = c(emc, f1, s1)
  return(result)
}


draw = function(a, b, name){
  
}
