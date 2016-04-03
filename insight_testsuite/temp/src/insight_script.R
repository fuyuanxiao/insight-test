library(rjson); #should installed this package firstly

#############################################################
#loading data
args = commandArgs(trailingOnly=TRUE);
input.file<-args[1];
dataset.org<-readLines(input.file);

############################################################
#Parse Jason script and extract time and hash tag informaton
data.jason.org<-sapply(dataset.org,fromJSON);
len<-sapply(data.jason.org,length);
ind<-len>3; #remove the rate limits messages according to the length of records
data.jason<-data.jason.org[ind];
n<-length(data.jason);
names(data.jason)<-NULL;
time<-sapply(data.jason,function(x) x$created_at);
htag<-sapply(data.jason,
             function(x) unique(sapply(x$entities$hashtags,
                                function(y) y$text)));
htag.num<-sapply(htag,length);

#############################################################
#Self-defined functions
time.diff<-function(t1,t2){#calculating the time difference from t1 to t2(t2>t1)
  sub1<-gsub("[^0-9]","",t1);
  sub2<-gsub("[^0-9]","",t2);
  sec1<-as.numeric(substr(sub1,7,8));
  sec2<-as.numeric(substr(sub2,7,8));
  if(substr(t1,1,16)==substr(t2,1,16)) return(sec2-sec1);
  mon1<-substr(t1,5,7);
  mon2<-substr(t2,5,7);
  year1<-substr(sub1,16,16);
  year2<-substr(sub2,16,16);
  day1<-as.numeric(substr(sub1,1,2));
  day2<-as.numeric(substr(sub2,1,2));
  hour1<-as.numeric(substr(sub1,3,4));
  hour2<-as.numeric(substr(sub2,3,4));
  min1<-as.numeric(substr(sub1,5,6));
  min2<-as.numeric(substr(sub2,5,6));
  if((mon2!=mon1)|(year2!=year1)) day2<-day1+1;
  return(86400*(day2-day1)+
           3600*(hour2-hour1)+
           60*(min2-min1)+
           (sec2-sec1));
}

ave.deg<-function(){#calculating the average degree of the adjacency matrix
  if(ncol(adj.mat)==0) return(0);
  degs<-colSums(adj.mat>0);
  ind<-degs>0;#ind stores the indices of the nodes with positive degree
  adj.mat<<-adj.mat[ind,ind];#maintain the adjacency matrix by removing the isolated nodes
  if(ncol(adj.mat)==0) return(0);
  return(sum(degs)/sum(ind));
}

proc<-function(id,act){#add or remove edges from the idth tweet to the graph, act=1 means add, act=-1 means remove
  tag.bag<-colnames(adj.mat); #nodes set of the graph
  j<-length(tag.bag);
  pos<-rep(0,htag.num[id]);#stores the indices of hashtags
  for(i in (1:(htag.num[id]))){
    temp<-which(htag[[id]][i]==tag.bag);
    if(length(temp)>0) pos[i]<-temp else{#
      j<-j+1;
      pos[i]<-j;
      adj.mat<<-cbind(adj.mat,0);
      adj.mat<<-rbind(adj.mat,0);
      colnames(adj.mat)[j]<<-htag[[id]][[i]];
      rownames(adj.mat)[j]<<-htag[[id]][[i]];
    }
  }
  for(i in (1:(htag.num[id]-1)))
    for(j in ((i+1):htag.num[id])){
      adj.mat[pos[i],pos[j]]<<-adj.mat[pos[i],pos[j]]+act;
      adj.mat[pos[j],pos[i]]<<-adj.mat[pos[j],pos[i]]+act;
    }
}

#############################################################
#Initial values
time.reform<-sapply(time,function(x) time.diff(time[1],x)); #reform created time into numerical format
adj.mat<-matrix(0,0,0);
timestamp<--Inf;
stack<-c();#stores the set of tweets which contribute to the current graph
ans<-rep(NA,n);#stores the average degrees in 7 digits of decimal
#############################################################
#Iterations
for (i in (1:n)){
  if(time.reform[i]>(timestamp-59)){
    inorder<-(time.reform[i]>timestamp); #whether the ith tweet is in time order
    if(inorder) timestamp<-time.reform[i];
    if(htag.num[i]>=2){
      stack<-c(stack,i); #push the ith tweet into stack
      proc(i,1);
      }
    if(inorder){ #if the ith tweet is in time order, we should pop the stack and deleting edges
      ind<-which(time.reform[stack]<(timestamp-59));
      len<-length(ind);
      if(len>0){ 
        for(j in (1:len)) proc(stack[ind[j]],-1);
        stack<-stack[-ind]; #pop the out of window tweets from stack
      }
    }
  }
  ans[i]<-ave.deg();
}
#############################################################
#Format the answer and output
ans.trunc<-as.character(trunc(100*ans)/100);
deci<-grep("\\.",ans.trunc);
ans.trunc[-deci]<-paste(ans.trunc[-deci],"00",sep=".");
two.d<-grep("\\.\\d\\d",ans.trunc[deci]);
ans.trunc[deci][-two.d]<-paste(ans.trunc[deci][-two.d],"0",sep="");
write.table(ans.trunc,file=args[2],row.names=F,col.names=F,quote=F);
