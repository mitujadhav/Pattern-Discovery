#patterns1<-read.table("D:/My Stuff/coursera/Pattern Discovery in Data Mining/Assignment 1 - bae90_categories.txt",sep="\n")

patterns<-read.csv("D:/My Stuff/coursera/Pattern Discovery in Data Mining/Assignment 1.csv")

install.packages("plyr")
install.packages("arules", dependencies=TRUE)
library(arules)

txn<-read.transactions(file="D:/My Stuff/coursera/Pattern Discovery in Data Mining/Assignment 1.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1,quote = "");

#basket_rules <- apriori(txn,parameter = list(support(type="absolute"), conf = 0.1,target="rules"));
basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.5,target="rules"))
inspect(txn)
itemFrequencyPlot(txn)

support(txn,type="absolute")

support(as.matrix(patterns),type="absolute")
as.matrix.data.frame(patterns)
library(tm)
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}


df_basket <- as(basket_rules,"data.frame")
View(df_basket)

#inspect(basket_rules)
#install.packages("arulesViz")
library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
# plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
# plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)

image(txn)
itemFrequencyPlot(txn, support = 0.1)
itemFrequencyPlot(txn, support = 0.01)

#--------------------------------------------------------------------
  
temple.text<-scan(choose.files(), what="char", sep="\n")
#temple.text<-tolower(temple.text)
temple.words.list<-strsplit(temple.text, "\\;", perl=TRUE)

temple.words.vector<-unlist(temple.words.list)

temple.freq.list<-table(temple.words.vector)
a<-as.data.frame(temple.freq.list)
a<-paste0(a$Freq,':',a$temple.words.vector)

write.csv(a,file="D:/My Stuff/coursera/Pattern Discovery in Data Mining/out1.csv" )

output2<-read.csv("D:/My Stuff/coursera/Pattern Discovery in Data Mining/output2.csv")
a<-paste0(output2$support,':',output2$rules)

df_basket$support<-round(df_basket$support*77185)
b<-paste0(df_basket$support,':',df_basket$rules)


write.csv(b,file="D:/My Stuff/coursera/Pattern Discovery in Data Mining/out2.csv" )
##------------------------------------------------------------------------------
## Assignment2
suppressMessages(require('arules', quietly = TRUE))
suppressMessages(require('BBmisc', quietly = TRUE))
install.packages("BBmisc")

lnk <- 'https://raw.githubusercontent.com/englianhu/Coursera-Data-Mining/master/4%20Pattern%20Discovery%20in%20Data%20Mining/data/categories.txt'

transDat <- suppressAll(read.transactions(lnk, format = 'single', cols = c(1, 2), sep = ';'))
transDat2 <- suppressAll(read.transactions(lnk, format = 'basket', sep = ';'))
library(rvest)

txt <- lnk %>% read_html %>% html_text
txt %<>% str_replace_all('\n', ';') %>% str_split(';') %>% .[[1]] %>% na.omit

uniqueItem <- sort(unique(txt))

####################################
list1<-c("Active Life","American (New)","American (Traditional)","Arts & Entertainment","Auto Repair","Automotive","Bakeries","Bars","Beauty & Spas","Breakfast & Brunch","Burgers","Cafes","Chinese","Coffee & Tea","Dentists","Doctors","Event Planning & Services","Fashion","Fast Food","Financial Services","Fitness & Instruction","Food","General Dentistry","Grocery","Hair Salons","Health & Medical","Home & Garden","Home Services","Hotels","Hotels & Travel","Ice Cream & Frozen Yogurt","Italian","Japanese","Local Services","Mexican","Nail Salons","Nightlife","Pet Services","Pets","Pizza","Professional Services","Pubs","Real Estate","Restaurants","Sandwiches","Shopping","Specialty Food","Sports Bars","Sushi Bars","Women's Clothing")


for(j in seq(length(list1)))
{
  s<-as.character(list1[j])
  print(paste0("------",s,"-----"))
  demo1<-patterns[which(patterns$A==s | patterns$B==s |patterns$C==s |patterns$D==s | patterns$E==s | patterns$F==s | patterns$G==s | patterns$X==s | patterns$X.1==s | patterns$X.2==s),]
  a<-as.data.frame(unique(demo1$A))
  b<-as.data.frame(unique(demo1$B))
  c<-as.data.frame(unique(demo1$C))
  d<-as.data.frame(unique(demo1$D))
  e<-as.data.frame(unique(demo1$E))
  f<-as.data.frame(unique(demo1$F))
  g<-as.data.frame(unique(demo1$G))
  h<-as.data.frame(unique(demo1$X))
  i<-as.data.frame(unique(demo1$X.1))
  j<-as.data.frame(unique(demo1$X.2))
  colnames(a)[1]<-"Term"
  colnames(b)[1]<-"Term"
  colnames(c)[1]<-"Term"
  colnames(d)[1]<-"Term"
  colnames(e)[1]<-"Term"
  colnames(f)[1]<-"Term"
  colnames(g)[1]<-"Term"
  colnames(h)[1]<-"Term"
  colnames(i)[1]<-"Term"
  colnames(j)[1]<-"Term"
  a<-rbind(a,b)
  a<-rbind(a,c)
  a<-rbind(a,d)
  a<-rbind(a,e)
  a<-rbind(a,f)
  a<-rbind(a,g)
  a<-rbind(a,h)
  a<-rbind(a,i)
  a<-rbind(a,j)
  unq<-unique(a)
  
  for(i in seq(1:dim(unq)[1]))
  {
    freq<-dim(demo1[which(demo1$A==as.character(unq$Term[i]) | demo1$B==as.character(unq$Term[i]) |demo1$C==as.character(unq$Term[i]) |demo1$D==as.character(unq$Term[i]) | demo1$E==as.character(unq$Term[i]) | demo1$F==as.character(unq$Term[i]) | demo1$G==as.character(unq$Term[i]) | demo1$X==as.character(unq$Term[i]) | demo1$X.1==as.character(unq$Term[i]) | demo1$X.2==as.character(unq$Term[i])),])[1]
    if(freq>771)
    {
      print(unq$Term[i])
      print(freq)
    }
  }
}

###########################


