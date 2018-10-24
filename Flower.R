Seeds <- "~/Flower_Seed"
Flowers <- "~/Flowers"
library('png')
setwd(Seeds)
x <- readPNG(source = "FourthSeed.png")
x <- x[,,1:3]
max <- 3000000

for(each in 1:max){
  ggx <- sum(sample(x = 0:300, replace = T, size = 1))+sample(x = 1:80, size = 1)+sample(x = 2:18, size = 1)
  ggy <- sum(sample(x = 0:300, replace = T, size = 1))+sample(x = 1:80, size = 1)+sample(x = 2:18, size = 1)
  pos <- c(ggx, ggy)
  if(x[pos[1], pos[2],1]!=1 && x[pos[1], pos[2],2]!=1 && x[pos[1], pos[2],3]!=1){
    direction <- sample(x = 1:4, replace = F, size = 4)
    Placed <- F
    for(d in direction){
      if(d == 1 && !Placed){
        if(x[pos[1]-1, pos[2],1] == 1 && x[pos[1]-1, pos[2],2] == 1 && x[pos[1]-1, pos[2],3] == 1){
          x[pos[1]-1, pos[2],] <- x[pos[1], pos[2],]
          Placed <- T
        }
      } 
      else if(d == 2 && !Placed){
        if(x[pos[1]+1, pos[2],1] == 1 && x[pos[1]+1, pos[2],2] == 1 && x[pos[1]+1, pos[2],3] == 1){
          x[pos[1]+1, pos[2],] <- x[pos[1], pos[2],]
          Placed <- T
        }
      } 
      else if(d == 3 && !Placed){
        if(x[pos[1], pos[2]-1,1] == 1 && x[pos[1], pos[2]-1,2] == 1 && x[pos[1], pos[2]-1,3] == 1){
          x[pos[1],pos[2]-1,] <- x[pos[1],pos[2],]
          Placed <- T
        }
      }
      else if(d == 4 && !Placed){
        if(x[pos[1], pos[2]+1,1] == 1 && x[pos[1], pos[2]+1,2] == 1 && x[pos[1], pos[2]+1,3] == 1){
          x[pos[1],pos[2]+1,] <- x[pos[1],pos[2],]
          Placed <- T
        }
      }
    }
  }
  if(each%%5000==0){print(paste(round(each/max*10000)/100,"%"))}
}
setwd(Flowers)
writePNG(image = x, target = "FourthFlower.png")




