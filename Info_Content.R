setwd('G:/source_tracking/evaluation')
data = file('sample_count.result',"r")
label = c('root')
number = c(0)
father = list(0)#the father nodes of the label
father_num = c(0)#the total number of father nodes
IC = c(0)
for (line in readLines(data)){
  temp1 = unlist(strsplit(line, split = ":"))#1rt is label, 2nd is number
  temp2 = unlist(strsplit(temp1[1], split = "-"))#ith labels
  label = c(label, temp2[!temp2%in%label]) #all labels
  for (i in 1:length(label)) {
    for (j in 1:length(temp2)) {
      if(identical(label[i], temp2[j])){#the position in label is i
        if (is.na(number[i])){ #not be visited
          number[i] = as.numeric(temp1[2])
          father[[i]] = list(temp2[j-1])
        }
        else{#visited
          number[i] = as.numeric(number[i]) + as.numeric(temp1[2])
          temp3 = unlist(father[[i]])
          if(isTRUE(temp2[j-1]%in%temp3)){
            #continue
          }
          else{
            father[[i]] = append(father[[i]],temp2[j-1])
          }
        }
      }
      else{}
    }
  }
}

for (j in 2: length(father)){
  for (i in 1:length(label)) {
    temp = unlist(father[[j]])
    father_num[j] = sum(as.numeric(number[match(temp,label)]))
    if(identical(label[i],father[j])){
      father[j] = i
      break
    }
  }
}
  
for (i in 2:length(label)){
  IC[i] = -log(number[i]/father_num[i])
}
result = list()
for (i in 1:length(IC)) {
  result[i] = paste(label[i],IC[i],sep=',')
}
close(data)
write.table(result,file="IC.txt" , sep ="\n", row.names =FALSE,col.names =FALSE, quote =FALSE)

result1 = list()
for (i in 1:length(IC)) {
  result1[i] = paste(label[i],father[[i]],sep=',')
}
write.table(result1,file="father.txt" , sep ="\n", row.names =FALSE,col.names =FALSE, quote =FALSE)
