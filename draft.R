lottery<-read.table("draftLottery1970.txt",header=T)

lottery = rbind( cbind(subset(lottery,month %in% 1:3),quarter=1) ,
                 cbind(subset(lottery,month %in% 4:6),quarter=2) ,
                 cbind(subset(lottery,month %in% 7:9),quarter=3) ,
                 cbind(subset(lottery,month %in% 10:12),quarter=4)  )
lottery$birth.order = 1:366


co = brewer.pal(4,"Set1")
plot(lottery$draft.order, cex = .8, pch = 16, 
     col = co[lottery$quarter],
     xaxt = "n", xlab = "Birth Day", ylab = "Draft Order",
     main = "Birth Order vs. Draft Order\n1969 Vietnam War Draft Lottery")
days=c(0,31,29,31,30,31,30,31,31,30,31,30,31) 
axis(1,at=cumsum(days)[-13],labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
centers = matrix(nrow=4,ncol=2)
for(i in 1:4){
  data = subset(lottery, quarter == i)
  y = mean(data$draft.order)
  x = mean(data$birth.order)
  centers[i,] = c(x,y)
}
points(centers, col = co, pch = 4, lwd=3, cex = 1.5)
line = matrix(nrow = 7, ncol = 2)
line[(1:4)*2-1,] = centers
for(i in (1:3)*2){
  line[i,1] = mean(c(line[i-1,1],line[i+1,1]))
  line[i,2] = mean(c(line[i-1,2],line[i+1,2]))
}
co2 = c(co[1],rep(co[2:3],each = 2),co[4])
for(i in 1:6){
  lines(line[(i:(i+1)),], col = co2[i], lwd = 1.8)
}