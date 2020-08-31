#The Challenge is a show that has run on MTV for 35 seasons. Orginally they took cast members from the Real World
#and Road Rules, but over the years it has opened up to many other shows they pull contestants from.
#This visualization shows the progression of which cast members appeared on which seasons, in addition to
#which show they originally appeared on (Real World, Road Rules, etc.), and what their result was on that particular 
#season of the challenge (winner, finalist, etc.). The data originally came from https://en.wikipedia.org/wiki/List_of_The_Challenge_(TV_series)_contestants

library(readxl)
library(RColorBrewer)
library(beepr)

ch = as.data.frame(read_excel("challenge.xlsx",col_names = F))
ch = cbind(ch,NA,NA)
colnames(ch) = c("name","origin","season","result","season.number","season.name")
for(i in 1:nrow(ch)){
  ch[i,5] = as.numeric(strsplit(ch[i,3],".",fixed = T)[[1]][1])
  ch[i,6] = strsplit(ch[i,3],".",fixed = T)[[1]][2]
}
ch$season.number[901:906] = 35
ch = ch[,-3]
unique(ch$season.name)
for(i in 1:nrow(ch)){
  if(grepl("(",ch[i,"season.name"],fixed=T)){
    ch[i,"season.name"] = "Battle of the Seasons II"
  }else{
    ch[i,"season.name"] = substr(ch[i,"season.name"],2,nchar(ch[i,"season.name"]))
  }
}
ch$name[grepl("Devenanzio",ch$name)] = "Johnny \"Bananas\" Devenanzio"
ch$name[grepl("Eric Banks",ch$name)] = "Eric \"Big Easy\" Banks"
ch$name[grepl("Nany",ch$name)] = "Nany Gonzalez"

chtemp = ch
ch = data.frame()
for(i in 1:max(chtemp$season.number)){
  ch = rbind(ch, subset(chtemp,season.number==i)[order(subset(chtemp,season.number==i)$name),])
}

resultcol = c("white", brewer.pal(6, "RdYlGn"))[-c(3,5)]
showcol = c("black", brewer.pal(11,"Spectral")[-c(2,5:8)])
            
png(filename = "thechallenge.png",width = 1300, height = 4800)
par(mar=c(6.5, 15, 6.5, 13.2))
plot(0,0,col="white",xlim=c(1,max(ch$season.number)),ylim=c(0,length(unique(ch$name))),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")

xtick = 1:max(ch$season.number)
lab = paste(unique(ch$season.number),". ",unique(ch$season.name),sep="")
text(xtick-.1,  par("usr")[3]+12.5, labels = lab, srt = 90, pos = 2, xpd = T,cex=1.5)
text(xtick-.8,  330, labels = lab, srt = 90, pos = 4, xpd = T,cex=1.5)

ytick = 1:length(unique(ch$name))
lab = unique(ch$name)
show = numeric()
for(i in 1:length(lab)){
  if(grepl("Real World",subset(ch,name==lab[i])$origin[1])){
    show[i] = 2
  }else if(grepl("Road Rules",subset(ch,name==lab[i])$origin[1])){
    show[i] = 3
  }else if(grepl("Challenge",subset(ch,name==lab[i])$origin[1])){
    show[i] = 1
  }else if(grepl("Fresh Meat",subset(ch,name==lab[i])$origin[1])){
    show[i] = 1
  }else if(grepl("Are You",subset(ch,name==lab[i])$origin[1])){
    show[i] = 4
  }else if(grepl("Bloodlines",subset(ch,name==lab[i])$origin[1])){
    show[i] = 1
  }else if(grepl("UK",subset(ch,name==lab[i])$origin[1])){
    show[i] = 5
  }else if(grepl("Geordie",subset(ch,name==lab[i])$origin[1])){
    show[i] = 5
  }else if(grepl("Big Brother",subset(ch,name==lab[i])$origin[1])){
    show[i] = 6
  }else{
    show[i] = 7
  }
}
text(par("usr")[1]+.5,  ytick-.5, labels = lab, srt = 0, pos = 2, xpd = T,cex=1.1,col=showcol[show])
text(35,  ytick-.5, labels = lab, srt = 0, pos = 4, xpd = T,cex=1.1,col=showcol[show])

result = matrix(nrow = length(unique(ch$name)), ncol = max(ch$season.number))
for(i in 1:length(unique(ch$name))){
  for(j in 1:max(ch$season.number)){
    data = subset(ch, season.number == j)
    finish = data[which(data$name==unique(ch$name)[i]),3]
    if(length(finish)>0){
      if(grepl("Win",finish)){
        result[i,j] = 5
      }else if (grepl("Final",finish) | grepl("place",finish) | grepl("Runner",finish)){
        result[i,j] = 4
      }else if (grepl("DQ",finish) | grepl("Left",finish) | grepl("Quit",finish) | grepl("Disq",finish) | grepl("Injur",finish)){
        result[i,j] = 2
      }else{
        result[i,j] = 3
      }
    }else{
      result[i,j] = 1
    }
    rect(j-1,i-1,j,i,border="lightgrey",col=resultcol[result[i,j]])
  }
}

par(xpd=TRUE)
legend(36,343,legend = c("DQed/quit/injured","Eliminated","Finalist","Winner"),
       col = resultcol[-1], pch = 15,title = expression(bold("Result")), cex = 1.6)
legend(-9,345,legend = c("The Challenge","Real World","Road Rules","Are You The One?","MTV UK","Big Brother","Other"),
       col = showcol, pch = 15,title = expression(bold("First appeared on")), cex = 1.3)

dev.off();beep()
