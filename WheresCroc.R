#' @export
getStatistics=function(rounds){
  sum = 0
  for(i in 1:rounds){
    result = runWheresCroc(ourWC, T,0)
    sum = sum + result
  }
  average = sum/rounds
  print(average)
  
}
# --- Calculates 'how probable is each state given the current emissions?' ---
# in order to choose the next edge to move to
ourWC=function(moveInfo,readings,positions,edges,probs) {
  
  # Build HMM:
  # Start with 38 nodes with equal probs and two nodes with prob 0 because croc won't start where the backpackers start
  # We should only do this at the very first move (after the first move, we will have stored 2 things in moveInfo$mem)
  if (length(moveInfo$mem) < 2) {
    init_vec <- rep((1/38), 40)
    init_vec[positions[1]] <- 0
    init_vec[positions[2]] <- 0
    # Store it in moveInfo$mem list for next turn
    moveInfo$mem$init_vec <- init_vec
  }
  else {
    init_vec = moveInfo$mem$init_vec
  }
  
  # Calculate emission matrix (the probabilities that a state outputs an emission)
  # for all waterholes w and our three current observations: 
  # P(O1, O2, O3 | w) = P(O1|w) * P(O2|w) * P(O3|w)
  emissionMatrix = matrix(0, nrow=40, ncol=1)
  for(i in 1:length(probs$salinity[,1])){
    emissionMatrix[i] = (dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])
                         *dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2])
                         *dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2]))
  }
  
  # Calculate transition matrix 
  # (e.g. if a node has two neighbours, each possible transition has prob. 1/3 (including staying at same state))
  move_to = 0
  if (length(moveInfo$mem) < 2) {
    trans_matrix = createTransitionMatrix(edges)
    # Store it in moveInfo$mem list for next turn
    moveInfo$mem$trans_matrix <- trans_matrix
    # On the first round we use only the emission matrix to calculate our next move, this gave better result after testing.
    highestValueInEmssionMatrix = -1
    for(i in 1:40){
      if(emissionMatrix[i]>highestValueInEmssionMatrix){
        highestValueInEmssionMatrix = emissionMatrix[i]
        move_to = i
      }
    }
  }
  else {
    trans_matrix = moveInfo$mem$trans_matrix
  }
  
  # Checking whether a hiker died and moving there
  hikerDied = -1
  positions[is.na(positions)] <- 0
  if((positions[1])<0){
    move_to = -1*positions[1]
    hikerDied = 1
  }
  if((positions[2])<0){
    move_to = -1*positions[2]
    hikerDied = 1
  }
  
  # If move_to is more than zero, a hiker has died and we move towards it
  # And we also need to reset the matrixes
  if(move_to>0){
    if(hikerDied>0){
      # Reset emission matrix because a hiker died
      for(i in 1:40){
        emissionMatrix[i] = 0
        init_vec[i] = 0
        if(i == move_to){
          emissionMatrix[i] = 1
          init_vec[i] = 1
        }
      }
    }
  }
  
  # Now we need to use the history(init_vec), the transistion and the emission matrix to create a picture of the situation
  sum_of_probs_for_nodes =  matrix(0, nrow=40, ncol=1)
  for(i in 1:40){
    trans_matrix[,i] = trans_matrix[,i]*emissionMatrix[i]*init_vec[i]
  }
  sum_of_probs_for_nodes = rowSums(trans_matrix)
  moveInfo$mem$init_vec <- sum_of_probs_for_nodes
  for(i in 1:40){
    
  }
  second_guess = 0
  if( move_to<1){
    highestValueInProbMatrix= -1
    for(i in 1:40){
      if(sum_of_probs_for_nodes[i]>highestValueInProbMatrix){
        highestValueInProbMatrix = sum_of_probs_for_nodes[i]
        move_to = i
      }
    }
    highestValueInProbMatrix = -1
    for(i in 1:40){
      if(sum_of_probs_for_nodes[i]>highestValueInProbMatrix){
        if( move_to != i){
          highestValueInProbMatrix = sum_of_probs_for_nodes[i]
          second_guess = i
        }
      }
    }
  }
  
  
  # Moving towards the middle of two nodes when our guesses are far apart was not worth it.
  # This might have to do with the way the grid is connected. 
  # Ex: node 1 and 21, 10 is much closer to 1, while 11 is much closer to 21.
  # if(abs(move_to-second_guess)>20){
  # Our best guesses are very far apart. maybe its better to go towards the middle. 
  # move_to = floor(move_to+second_guess/2)
  # }
  
  # Use the move_to which is the goal where the croc prob is, to choose which the next step should be
  next_move_based_on_goal = findShoterstPathThroughEdges(edges, positions[3], move_to)
  
  if(next_move_based_on_goal== move_to){
    # If the croc was one step away last turn, we might want to look first and then walk?
    ourMoves = c(0, next_move_based_on_goal)  
  }else{
    ourMoves = c(next_move_based_on_goal,0)  
  }
  
  
  moveInfo$moves=ourMoves
  return(moveInfo)
  
}

randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)  
  return(moveInfo)
}

# --- Create the transition matrix from the edges vectors ---
createTransitionMatrix=function(edges) {
  trans_matrix = matrix(0, nrow=40, ncol=40)
  current = 1
  neighbours = c(0, 0, 0, 0, 0, 0)
  num_neighbours = 0
  for (checking_i in 1:40) {
    # Add the node itself 
    num_neighbours = 1
    neighbours = c(0, 0, 0, 0, 0, 0)
    neighbours[num_neighbours] = checking_i
    # Go through the entire edge matrix
    for (compare_i in 1:length(edges[,1])) {
      # Look at both sides of the edge matrix to find all neighbours
      if(checking_i == edges[compare_i,1]){
        num_neighbours = num_neighbours + 1
        neighbours[num_neighbours] = edges[compare_i,2]
      }
      if(checking_i == edges[compare_i,2]){
        num_neighbours = num_neighbours + 1
        neighbours[num_neighbours] = edges[compare_i,1]
      }
    }
    # Here, all neighbours of the node where are checking checking_i have been added
    # Add them to transition matrix
    for (j in 1:length(neighbours)) {
      if (neighbours[j] > 0) {
        trans_matrix[checking_i, neighbours[j]] = (1/num_neighbours)
      }
    }
  }
  
  return (trans_matrix)
}


# --- Find the shortest path when we have found our goal ---
findShoterstPathThroughEdges=function(edges,start,goal) {
  foundPath = -3
  steps = 0
  path = array(-1, dim=c(3,40))
  prevNodes = c(start)
  path_len = 0
  visitedNodes = c(start)
  while(foundPath<0){
    
    steps = steps +1
    newNodes = c()
    for(i in 1:length(edges[,1])){
      nodeToAdd = 0
      origin = 0
      if(any(prevNodes==edges[i,1])){
        nodeToAdd = edges[i,2]
        origin = edges[i,1]
      }
      if(any(prevNodes==edges[i,2])){
        nodeToAdd = edges[i,1]
        origin = edges[i,2]
      }       
      if(nodeToAdd!=0){
        if(any(visitedNodes==nodeToAdd)){
          
        }else{
          newNodes = c(newNodes,nodeToAdd)
          visitedNodes = c(visitedNodes,nodeToAdd)
          path[,path_len] <- c(nodeToAdd, origin, steps)
          path_len = path_len+1
        }
        
      }
    }
    if(any(visitedNodes==goal)){
      foundPath = 1
    }
    prevNodes = newNodes
  }
  lookingFor = goal
  lookingForNext=goal
  while(steps>0){
    lookingFor = lookingForNext
    lookingForNext = 0
    for(i in 1:length(path[1,])){
      if(path[1,i]==lookingFor && path[3,i] == steps){
        lookingForNext = path[2,i]
      }
    }
    
    steps = steps-1
  }
  return(lookingFor)
}



#' @export
manualWC=function(moveInfo,readings,positions,edges,probs) {
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2=readline("Move 2: ")    
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
}

#' Run Where's Croc
#' 
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park. 
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record 
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also 
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in 
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game. 
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fiels. The first is a vector of numbers called 'moves', where you will enter 
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the 
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A 
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current 
#' location. (3) A vector giving the positions of the two tourists and yourself. If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist 
#' was eaten by Croc in a previous turn, then the position will be NA. (4) a matrix giving the 
#' edges paths between waterholes (edges) present. (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector 
#' and any changes to the 'mem' field you wish to access later on.
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Ignore this.
#' @return A string describing the outcome of the game.
#' @export
runWheresCroc=function(makeMoves,showCroc=F,pause=1) {
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list())
  while (!is.na(positions[1])) {
    move=move+1
    positions[1]=sample(getOptions(positions[1],edges),1)
    if (!is.na(positions[2])&&positions[2]>0) {
      positions[2]=sample(getOptions(positions[2],edges),1)
    } else if (!is.na(positions[2]) && positions[2]<0) {
      positions[2]=NA
    }
    if (!is.na(positions[3])&&positions[3]>0) {
      positions[3]=sample(getOptions(positions[3],edges),1)
    } else if (!is.na(positions[3]) && positions[3]<0) {
      positions[3]=NA
    }
    if (!is.na(positions[2]) && positions[2]==positions[1]) {
      positions[2]=-positions[2]
    }
    if (!is.na(positions[3]) && positions[3]==positions[1]) {
      positions[3]=-positions[3]
    }
    plotGameboard(points,edges,move,positions,showCroc)
    
    Sys.sleep(pause)
    
    readings=getReadings(positions[1],probs)
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m==0) {
        if (positions[1]==positions[4]) {
          #print(paste("Congratualations! You got croc at move ",move,".",sep=""))
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }      
    }
  }
}
#' @export
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @export
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))
  
  return (edges)
}

#' @export
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @export
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @export
plotGameboard=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)      
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}

#' @export
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}