?seq.along
# ----- dumbDM -----
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}

# ----- basicDM -----
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load  
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}

# ----- manualDM -----
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

# ----- averageTest -----
averageTest <- function(tests){
  sum = 0
  for (i in 1:tests) {
    sum=sum+runDeliveryMan(carReady = astarDM, dim = 10, turns = 2000, doPlot = F, pause = 0, del = 5)
    if(i%%10==0){
      print(i)
      print(sum/i)
    }
  }
  print(sum/i)
  return(0)
}
#averageTest(500)

# ----- Helper function -----
# Removes an item from the frontier by replacing all crucial slots with 1001
removeFromFrontier=function(theFrontier,x,y,cost) {
  for(i in 1:(length(theFrontier)/6)){
    if(theFrontier[1,i]==x && theFrontier[2,i]==y && theFrontier[3,i]==cost){
      theFrontier[,i] <- 1001
    } 
  }
  return (theFrontier)
}     

# ----- astarDM -----
# Uses a* algorithm to find the best paths
astarDM=function(roads,car,packages) {
  
  # Init
  nextMove=0
  toGo=0
  offset=0
 
  # If we currently do not carry a package, 
  # we choose the next one with the smallest Manhattan distances to the package and its goal
  if (car$load==0) {
    min_dist = 100
    for (i in 1:5) {
      if (packages[i,5] == 0) {
        dist_to_package = abs(car$x - packages[i,1]) + abs(car$y - packages[i,2])
        dist_to_goal = abs(packages[i,1] - packages[i,3]) + abs(packages[i,2] - packages[i,4])
        if ((dist_to_package+dist_to_goal) < min_dist) {
          min_dist = (dist_to_package+dist_to_goal)
          goal = i  
        }
      }  
    }
    toGo = goal
    
  } else {
    toGo=car$load  
    offset=2
  }
  
  # Init matrix of heuristics with Manhattan distances
  heuristics = matrix(, nrow = 10, ncol = 10)
  for(y in 1:nrow(heuristics)) {
    for(x in 1:ncol(heuristics)) {
      # y specifies rows, x columns
      heuristics[y,x] = abs((packages[toGo,1+offset])-x) + abs((packages[toGo,2+offset])-11+y)
    }
  }
  
  # Each item on frontier has 5 slots: x and y position, cost, and x and y position of predecessor
  frontier = array(1000, dim=c(6,10000))
  # First thing on frontier: current position
  frontier[,1] <- c(car$x, car$y, 0, -1, -1, 0)
  f_len = 2
  
  # Visited set for all nodes we have already seen
  visited = array(1000, dim=c(6,10000))
  v_len = 1
  
  # While we did not reach goal
  foundTheGoal = -1
  goal_x = packages[toGo,1+offset]
  goal_y = packages[toGo,2+offset]
  while (foundTheGoal < 0) {
    to_expand = vector()
    min_cost = 1000
    # Find node on frontier with lowest cost
    for(i in 1:f_len) {
      if (frontier[3,i] < min_cost){
        min_cost = frontier[3,i]
        to_expand <- frontier[,i]
      }
    }
  
    current_x = to_expand[1]
    current_y = to_expand[2]
    current_cost = to_expand[3]
    current_prev_roadcost = to_expand[6]
    
    # Add the current node to the visited set
    visited[,v_len] <- c(current_x, current_y, current_cost, to_expand[4], to_expand[5], current_prev_roadcost)
    v_len = v_len +1
    # Remove the one we visited from the frontier
    frontier = removeFromFrontier(frontier, current_x, current_y, current_cost)
    
    # Expand the node to the right (if possible)
    if (current_x < 10) {
      right_h = heuristics[11-current_y, current_x+1]
      right_roadcost = roads$hroads[11-current_y, current_x+1-1]
      right_totalcost = right_h + right_roadcost
      frontier[,f_len] <- c(current_x+1, current_y, current_prev_roadcost+right_totalcost, current_x, current_y, current_prev_roadcost+right_roadcost)
      f_len = f_len + 1
    }
    
    # Expand to the left
    if (current_x > 1) {
      frontier[,f_len] <- c((current_x-1), current_y,  current_prev_roadcost+(roads$hroads[(11-current_y), current_x-1] + heuristics[(11-current_y), current_x]), current_x, current_y, (current_prev_roadcost+(roads$hroads[(11-current_y), current_x-1])))
      f_len = f_len + 1
    }
    
    # Expand upwards
    if (current_y < 10) {
      frontier[,f_len] <- c(current_x, current_y+1,  current_prev_roadcost+(roads$vroads[(10-current_y), current_x] + heuristics[(11-current_y), current_x]),current_x, current_y, current_prev_roadcost+(roads$vroads[(10-current_y), current_x]))
      f_len = f_len + 1
    }
    
    # Expand downwards
    if (current_y > 1) {
      frontier[,f_len] <- c(current_x, (current_y-1),  current_prev_roadcost+(roads$vroads[(10-current_y+1), current_x] + heuristics[(11-current_y+1), current_x]), current_x, current_y, current_prev_roadcost+(roads$vroads[(10-current_y+1), current_x]))
      f_len = f_len + 1
    }
    
  
    # If the node with lowest cost (the current node) is also the goal node, we are done
    if(current_x == goal_x && current_y == goal_y) {
      foundTheGoal = 1
    }
    
  } # while did not find goal
  
  # Here the visited set is completed
  # We start looking at the goal node, find the goal node in visited set with lowest cost,
  # and look what point led there. Then we repeat with that node until the start node
  predecessor_x = goal_x
  predecessor_y = goal_y
  foundWhereToGo = -1
  whatsTheMove = 2
  
  while(foundWhereToGo < 0) {
    previous_predecessor_x = -1
    previous_predecessor_y = -1
    # min_cost is used to look for all the visited nodes with correct x,y and to only pick the one with lowest cost
    min_cost = 1000
    max_loops = 0
    
    for(i in 1:v_len) {
      if (visited[3,i] < min_cost && min_cost > 0 && visited[1,i] == predecessor_x && visited[2,i] == predecessor_y) {
        min_cost = visited[3,i]
        # When we find a match in x,y node with lower cost we update 
        previous_predecessor_x = visited[4,i]
        previous_predecessor_y = visited[5,i]
        }
    }
    max_loops = max_loops + 1
    
    # If this new predecessor is the same as the car pos, we are done 
    if ((previous_predecessor_x == car$x && previous_predecessor_y == car$y) || max_loops == 10) {
      foundWhereToGo = 1
      
      # If the car's position has high x, then the node we should go to the left
      if (predecessor_x < previous_predecessor_x) {
        whatsTheMove = 4
      }
      # Go right
      if (predecessor_x > previous_predecessor_x) {
        whatsTheMove = 6
      }
      # Go up
      if (predecessor_y > previous_predecessor_y) {
        whatsTheMove = 8
      }
      # Go down
      if (predecessor_y < previous_predecessor_y) {
        whatsTheMove = 2
      }
    }
    
    # The last value these have will be the best route to the node
    # We update and look at this node 
    predecessor_x = previous_predecessor_x
    predecessor_y = previous_predecessor_y
    
  }
  
  nextMove = whatsTheMove
  car$nextMove=nextMove
  car$mem=list()
  return (car)
  
} # astarDM

#' Run Delivery Man
#' 
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are 
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be 
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i) 
      plotRoads(roads$hroads,roads$vroads) 
      points(car$x,car$y,pch=16,col="blue",cex=3)  
      plotPackages(packages)      
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }      
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  } 
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")    
  }
  car$nextMove=NA
  return (car)
} 

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0) 
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {
  
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  x <- 1:n
  #plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n), ylim=rev(range(x)),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }    
  }
  list (hroads=hroads,vroads=vroads)
}

