#!/usr/bin/env python3

alnres = function(filein){
  qid = c()
  tid = c()
  seqid = c()
  alnlen = c()
  nmiss = c()
  ngap = c()
  qbegin = c()
  qend = c()
  tbegin = c()
  tend = c()
  evalue = c()
  bscore = c()
  feasco = c()
  qlabel = c()
  tlabel = c()
  f = readLines(filein)
  for (line in f) {
    line = trimws(line, which = c("both", "left", "right"))
    temp1 = unlist(strsplit(line, split = "\\s+"))
    temp2 = unlist(strsplit(temp1[0], split = "\\|"))
    qid = c(qid, temp2[0])
    temp3 = unlist(strsplit(temp1[1], split = "\\|"))
    tid = c(tid, temp3[0])
    seqid = c(seqid, as.numeric(temp1[2]))
    alnlen = c(alnlen, as.integer((temp1[3])))
    nmiss = c(nmiss, as.integer((temp1[4])))
    ngap = c(ngap, as.integer((temp1[5])))
    qbegin = c(qbegin, as.integer((temp1[6])))
    qend = c(qend, as.integer((temp1[7])))
    tbegin = c(tbegin, as.integer((temp1[8])))
    tend = c(tend, as.integer((temp1[9])))
    evalue = c(evalue, as.numeric(temp1[10]))
    bscore = c(bscore, as.numeric(temp1[11]))
    qlabel = c(qlabel, temp2[1])
    tlabel = c(tlabel, temp3[1])
    
  }
feasco.append(seqid)
feasco.append(alnlen)
feasco.append(nmiss)
feasco.append(ngap)
feasco.append(evalue)
feasco.append(bscore)
feature = np.array(feasco)
feature = feature.T
return(qid,tid,qlabel,tlabel,feature)
}

#fileX = 'E:/ARG0823/tmp/argminer_res1.uniq'
def main():
  qid,tid,qlabel,tlabel,feature = alnres(sys.argv[1])

if(__name__ == '__main__'):
  main()
