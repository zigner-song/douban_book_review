library(XML)
library(rvest)
library(stringr)
library(dplyr)





#douban  short comment

url_douban<-"https://book.douban.com/subject/26834861/comments/new?p="  #26834861Ϊ���ڶ����ID


comment0<-NULL
my.comment<-NULL
for(i in 1:2){ #������2ҳ
  print(paste(">>>> ",trimws(i/2*100),"% >>>>",sep="")) #������
  
  url<-paste(url_douban,str_trim(as.character(i)),sep ="")
  web<-read_html(url)
  comment0<-web %>% html_nodes(".comment") %>% html_nodes("p") %>% html_text()
  my.comment<-c(my.comment,comment0)
}

my.comment<-str_replace_all(my.comment,"[:space:]","")   #ɾ������Ŀո�ͻ���



library(jiebaR)


seg<-worker(user='DTE.utf8') #���ӱ��شʿ⣬û���͡��뵱�� һ����
tagger <- worker("tag")      #����

raw.comment<-seg[comment]   #�ִ�
raw.comment<-(tagger <= raw.comment)  #���Ӵ���

ins<-data.frame(char=raw.comment,tag=names(raw.comment)) %>%distinct()

freq.comment<-freq(raw.comment) #�����Ƶ
freq.comment<-merge(freq.comment,ins,by="char")
head(freq.comment)

freq.comment<-subset(freq.comment,freq>2 & str_length(freq.comment$char)>1 & 
                       !(freq.comment$char %in% c("����")))


library(wordcloud2)


#��jsp����html�д��Ƶ���ɫ
js_color_fun <- "function (word, weight) {      
return (weight > 10) ? '#f02222' : '#c09292';
}"

wordcloud2(freq.comment, color = htmlwidgets::JS(js_color_fun),
           backgroundColor = 'black',
           shape="cardioid",size=1)




# douban long comment




#####################


url_douban.long_comment<-"https://book.douban.com/subject/26834861/reviews?start="
url_long.comment<-NULL

#��ȡ����URL
for(i in seq(0,80,20)){
  
  print(paste(">>>> ",trimws(i),"% >>>>",sep="")) #������
  i<-0
  url_douban.long_comment_sub<-paste(url_douban.long_comment,i,sep="")
  web0<-read_Url(url_douban.long_comment_sub)
  url0<-((web0 %>% html_nodes(".main-bd")) %>% html_nodes("h2") %>% html_nodes("a")) %>% html_attr("href")
  url_long.comment<-c(url_long.comment,url0)
}

comment_long<-NULL
for(i in 1:length(url_long.comment)){
  Sys.sleep(2)#����������쵼�±���IP
  print(paste(">>>> ",trimws(i/length(url_long.comment)*100),"% >>>>",sep="")) #������
  
  
  #i<-1
  web_long.comment<-read_Url(url_long.comment[i])
  if(is.na(web_long.comment)==F){
    comment0<-(web_long.comment %>% html_nodes(".clearfix"))[2] %>% 
      as.character() %>% str_replace_all(.,pattern="(<[^>]+>)|[:space:]","")%>%
      str_trim()
  } else{
    comment0<-NA
    
  }
  comment_long<-c(comment_long,comment0)
}


library(jiebaR)


seg<-worker(user='DTE.utf8') #���ӱ��شʿ⣬û���͡��뵱�� һ����
tagger <- worker("tag")      #����

raw.comment_long<-seg[comment_long]   #�ִ�
raw.comment_long<-(tagger <= raw.comment)  #���Ӵ���

#ins<-data.frame(char=raw.comment,tag=names(raw.comment)) %>%distinct()

freq.comment_long<-freq(raw.comment_long) #�����Ƶ
freq.comment_long<-merge(freq.comment_long,
                         data.frame(char=raw.comment_long,tag=names(raw.comment_long)) %>%distinct(),
                         by="char")
head(freq.comment_long)

freq.comment_long<-subset(freq.comment_long,freq>5 & str_length(freq.comment_long$char)>1 & 
                       (!(freq.comment_long$char %in% c("����","��Ϊ")) &
                           freq.comment_long$tag %in% c("n","ns","vn","x","nr","a")))

freq.comment_long<-freq.comment_long[order(-freq.comment_long$freq),]
library(wordcloud2)


#��jsp����html�д��Ƶ���ɫ
js_color_fun <- "function (word, weight) {      
return (weight > 500) ? '#f02222' : '#c09292';
}"

lishu<- system.file("examples/lishu.jpg",package = "wordcloud2")
wordcloud2(freq.comment_long,backgroundColor = "black",
           #figPath=lishu,
           shape="cardioid",
           size=1,color = htmlwidgets::JS(js_color_fun))