#creating qtable
Qtable <- data.frame(x=1,y=1, up=0, down=0 , left=0, right=0)

for(x in 1:15){
  for(y in 1:14){
    state <- c(x,y,0,0,0,0)
    if(x==1){
      state<-c(x,y,0,0,-10000000,0)
      }
    if(y==1){
    state<-c(x,y,-10000000,0, 0,0)
      }
    if(x==15){
      state<-c(x,y,0,0,0,-10000000)
     }
    if(y==14){
      state<-c(x,y,0,-10000000,0,0)
    }
    if(x==1 & y==1){
      state<-c(x,y,-10000000,0,-10000000,0)
    } 
    if(x==1 & y==14){
      state<-c(x,y,0,-10000000,-10000000,0)
    } 
    if(x==15 & y==1){
      state<-c(x,y,-10000000,0,0,-10000000)
    }
    if(x==15 & y==14){
      state<-c(x,y,0,-10000000,0,-10000000)
    }
    Qtable <- rbind(Qtable,state)
  }
}
#removing 0,0
Qtable<- Qtable[-1,]

#Defining reward function

Reward <- function(x,y){
  if(x==6 & y==12){
    Reward <-1
  }
  #working with forbidden states
  #else if (x==3 & y==2){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==4 & y==2){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==5 & y==2){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==10 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==12 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==13 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==2 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==3 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==4& y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==5 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==6 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==7 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==8 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==9 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==1 & y==7){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==1 & y==8){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==7){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==8){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==9){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==10){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==7){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==2 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==3 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==4 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==5 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==6 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==7 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==10){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==12){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==13){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==14){
  #  Reward<-- 1
  #}
  else{
    Reward <- -0.05
  }
  return(Reward)
}

#defining the environment characteristics
Environment <- function(x,y,action){
  reward = 0
  update= TRUE
  if(x == 1 & action == "left"){
    x = x
    y = y
    update = FALSE
  }
  else if(y == 1 & action == "up"){
    x = x
    y = y
    update = FALSE 
  }
  else if(x == 15 & action == "right"){
    x = x
    y = y
    update = FALSE 
  }
  else if(y == 14 & action == "down"){
    x = x
    y = y
    update = FALSE 
  }
  else{
    if(action == "up"){
      xnew = x
      ynew = y-1
    }
    else if(action == "down"){
      xnew = x
      ynew = y+1
    }
    else if(action == "left"){
      xnew = x-1
      ynew = y
    }
    else if(action == "right"){
      xnew = x+1
      ynew = y
    }
    #working with forbidden states
    if(xnew ==3  & ynew ==2 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 4 & ynew == 2 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 5 & ynew == 2 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 10 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 12 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 13 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 2 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 3 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 4 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 5 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 6 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 7 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 8 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 9 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 1 & ynew == 7 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 1 & ynew == 8 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 7 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 8 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 9 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 10 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 7 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 10 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 12 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 13 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 14 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 2 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 3 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 4 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 5 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 6 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 7 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    x=xnew
    y=ynew
    reward = Reward(x,y)
  }
  return(list(x,y,reward,update))
}

#Q-learning
episodes = 1
Maxsteps = 10000
results = rep(0,Maxsteps)
#learning part boi
while(episodes <= Maxsteps){
  x=1
  y=1
  epsilon = 0.1
  alpha = 0.1
  gamma=0.95
  steps =0
  terminate = FALSE
  path = c(x,y)
  while(terminate == FALSE){
    #Define the policy
    up = Qtable[((x-1)*14+y),"up"]
    down = Qtable[((x-1)*14+y),"down"]
    left = Qtable[((x-1)*14+y),"left"]
    right = Qtable[((x-1)*14+y),"right"]
    actions <- c(up,down,left,right)
    TheAction <- c("up","down","left","right")
    if(runif(1) < epsilon){
      Random = sample.int(4 , size = 1)
      action = TheAction[Random]
    }else{
      action = TheAction[which.max(actions)]
    }
    NewState = Environment(x,y,action)
    reward = Reward(NewState[[1]],NewState[[2]])
    if(NewState[[4]] == TRUE){
      xnew = NewState[[1]]
      ynew = NewState[[2]]
      up = Qtable[((xnew-1)*14+ynew),"up"]
      down = Qtable[((xnew-1)*14+ynew),"down"]
      left = Qtable[((xnew-1)*14+ynew),"left"]
      right = Qtable[((xnew-1)*14+ynew),"right"]
      actions <- c(up,down,left,right)
      MaxAction <- TheAction[which.max(actions)]
      #print("section 3.2")
      Qtable[((x-1)*14 + y),action] = Qtable[((x-1)*14 + y),action] + alpha*(reward + gamma*Qtable[((xnew-1)*14 + ynew),MaxAction]- Qtable[((x-1)*14 + y),action])
      #print("section 3.3")
      x = xnew
      y = ynew
      path = rbind(path, c(x,y))
      steps = steps+1
      if(reward == 1){
        terminate = TRUE
      }else if (reward == -1){
        terminate = TRUE
      }
      
    }
  }
  print(episodes)
  results[episodes] = reward*steps
  episodes = episodes+1
}
library(scales)
plot(results,type = 'o', pch = 16, col= alpha("black",0.3), xlim = c(0,10000), main= "Q-learning over all episodes", xlab="Episodes" ,ylab = "Number of steps")
plot(results,type = 'o', pch = 16, col= alpha("black",0.3), xlim = c(0,100), main= "Q-learning over first 100 episodes", xlab="Episodes" ,ylab = "Number of steps")
plot(path, pch = 16, col= "black", main= "Q-learning path",xlab="Column number" ,ylab = "Row number")


#creating qtable
Qtable <- data.frame(x=1,y=1, up=0, down=0 , left=0, right=0)

for(x in 1:15){
  for(y in 1:14){
    state <- c(x,y,0,0,0,0)
    if(x==1){
      state<-c(x,y,0,0,-10000000,0)
    }
    if(y==1){
      state<-c(x,y,-10000000,0, 0,0)
    }
    if(x==15){
      state<-c(x,y,0,0,0,-10000000)
    }
    if(y==14){
      state<-c(x,y,0,-10000000,0,0)
    }
    if(x==1 & y==1){
      state<-c(x,y,-10000000,0,-10000000,0)
    } 
    if(x==1 & y==14){
      state<-c(x,y,0,-10000000,-10000000,0)
    } 
    if(x==15 & y==1){
      state<-c(x,y,-10000000,0,0,-10000000)
    }
    if(x==15 & y==14){
      state<-c(x,y,0,-10000000,0,-10000000)
    }
    Qtable <- rbind(Qtable,state)
  }
}
#removing 0,0
Qtable<- Qtable[-1,]

#Defining reward function

Reward <- function(x,y){
  if(x==6 & y==12){
    Reward <-1
  }
  #working with forbidden states
  #else if (x==3 & y==2){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==4 & y==2){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==5 & y==2){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==10 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==12 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==13 & y==3){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==2 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==3 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==4& y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==5 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==6 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==7 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==8 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==9 & y==5){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==1 & y==7){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==1 & y==8){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==7){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==8){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==9){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==10){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==11 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==7){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==2 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==3 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==4 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==5 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==6 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==7 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==10){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==11){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==12){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==13){
  #  Reward<-- 1
  #}
  #working with forbidden states
  #else if (x==14 & y==14){
  #  Reward<-- 1
  #}
  else{
    Reward <- 0
  }
  return(Reward)
}

#defining the environment characteristics
Environment <- function(x,y,action){
  reward = 0
  update= TRUE
  if(x == 1 & action == "left"){
    x = x
    y = y
    update = FALSE
  }
  else if(y == 1 & action == "up"){
    x = x
    y = y
    update = FALSE 
  }
  else if(x == 15 & action == "right"){
    x = x
    y = y
    update = FALSE 
  }
  else if(y == 14 & action == "down"){
    x = x
    y = y
    update = FALSE 
  }
  else{
    if(action == "up"){
      xnew = x
      ynew = y-1
    }
    else if(action == "down"){
      xnew = x
      ynew = y+1
    }
    else if(action == "left"){
      xnew = x-1
      ynew = y
    }
    else if(action == "right"){
      xnew = x+1
      ynew = y
    }
    #working with forbidden states
    if(xnew ==3  & ynew ==2 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 4 & ynew == 2 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 5 & ynew == 2 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 10 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 12 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 13 & ynew == 3 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 2 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 3 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 4 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 5 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 6 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 7 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 8 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 9 & ynew == 5 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 1 & ynew == 7 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 1 & ynew == 8 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 7 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 8 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 9 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 10 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 11 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 7 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 10 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 12 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 13 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 14 & ynew == 14 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 2 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 3 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 4 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 5 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 6 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    else if(xnew == 7 & ynew == 11 ){
      xnew = x
      ynew = y
      update = FALSE
    }
    x=xnew
    y=ynew
    reward = Reward(x,y)
  }
  return(list(x,y,reward,update))
}

#Q-learning
episodes = 1
Maxsteps = 10000
results = rep(0,Maxsteps)
#learning part boi
while(episodes <= Maxsteps){
  x=1
  y=1
  epsilon = 0.1
  alpha = 0.1
  gamma=0.95
  steps =0
  terminate = FALSE
  path = c(x,y)

  while(terminate == FALSE){
    #Define the policy
    up = Qtable[((x-1)*14+y),"up"]
    down = Qtable[((x-1)*14+y),"down"]
    left = Qtable[((x-1)*14+y),"left"]
    right = Qtable[((x-1)*14+y),"right"]
    actions <- c(up,down,left,right)
    TheAction <- c("up","down","left","right")
    if(runif(1) < epsilon){
      Random = sample.int(4 , size = 1)
      action = TheAction[Random]
    }
    else{
      action = TheAction[which.max(actions)]
    }
    NewState = Environment(x,y,action)
    reward = Reward(NewState[[1]],NewState[[2]])
    if(NewState[[4]] == TRUE){
      xnew = NewState[[1]]
      ynew = NewState[[2]]
      up = Qtable[((xnew-1)*14+ynew),"up"]
      down = Qtable[((xnew-1)*14+ynew),"down"]
      left = Qtable[((xnew-1)*14+ynew),"left"]
      right = Qtable[((xnew-1)*14+ynew),"right"]
      actions <- c(up,down,left,right)
      TheAction <- c("up","down","left","right")
      if(runif(1) < epsilon){
        Random = sample.int(4 , size = 1)
        action_1 = TheAction[Random]
      }
      else{
        action_1 = TheAction[which.max(actions)]
      }
      #MaxAction <- TheAction[which.max(actions)]
      #print("section 3.2")
      Qtable[((x-1)*14 + y),action] = Qtable[((x-1)*14 + y),action] + alpha*(reward + gamma*Qtable[((xnew-1)*14 + ynew),action_1]- Qtable[((x-1)*14 + y),action])
      #print("section 3.3")
      x = xnew
      y = ynew
      path = rbind(path, c(x,y))
      steps = steps+1
      if(reward == 1){
        terminate = TRUE
      }else if (reward == -1){
        terminate = TRUE
      }
      
    }
  }
  print(episodes)
  results[episodes] = reward*steps
  episodes = episodes+1
}

library(scales)
plot(results,type = 'o', pch = 16, col= alpha("black",0.3), xlim = c(0,10000), main= "Sarsa over all episodes", xlab="Episodes" ,ylab = "Number of steps")
plot(results,type = 'o', pch = 16, col= alpha("black",0.3), xlim = c(0,100), main= "Sarsa over first 100 episodes", xlab="Episodes" ,ylab = "Number of steps")
plot(path, pch = 16, col= "black", main= "Sarsa path",xlab="Column number" ,ylab = "Row number")



