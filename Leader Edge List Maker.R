setwd("E:\\Documents\\School\\PHD\\GAShip\\Network Analysis Evan Playground")

library(tidyverse)
library(stringr)
library(stringdist)
library(igraph)

leaders <- read_csv('leader network t3.csv',
                    col_names = TRUE,
                    col_types = cols(.default=col_character())
)

leaders <- leaders[c(18:129)]
leaders <- leaders[-c(1:2),]

# Define lookup table vectors.

categoryVector <- c('education','politics','industry','healthcare','engineering','family','information technology')

names(categoryVector) <- c(1:length(categoryVector))

resourceVector <- c('Feedback','Resources','Ideas')

names(resourceVector) <- c(1:length(resourceVector))

# Role Vectors

educationVector <- c('Education.Teacher','Education.Campus Administrator','Education.Campus Staff',
                     'Education.District Administrator','Education.District Staff','none')

names(educationVector) <- c(1:length(educationVector))

politicalVector <- c('Politics.Elected Official','Politics.Board Member','Politics.Staffer','none')

names(politicalVector) <- c(1:length(politicalVector))

businessVector <- c('Business.Entrepeneur','Business.Executive manager','Business.Non-Profit','none')

names(businessVector) <- c(1:length(businessVector))

healthcareVector <- c('Health.Doctor','Health.Nurse','Health.Pharmacist','none')

names(healthcareVector) <- c(1:length(healthcareVector))

engineeringVector <- c('Engineering.Civil/Electical','Engineering.Mechanical/Industrial','Engineering.Chemical','none')

names(engineeringVector) <- c(1:length(engineeringVector))

familyVector <- c('Family.Parents/Grandparents','Family.Siblings','Family.Spouse/Partner','none')

names(familyVector) <- c(1:length(familyVector))

ITVector <- c('IT.Software Developer/Support','IT.IT Architect/Data Scientist','IT.Cyber Security')

names(ITVector) <- c(1:length(ITVector))

# Define lookup table function.

getValue <- function(key, lookupvector) {
  
    value <- lookupvector[key]
    
    value <- unname(value)
    
    return(value)
    
}

# Define edge list creation function.

edgeList <- function(somerow) {
  
  # Determine problems and lookup vector.
  
  print(somerow[1])
  
  problems <- as.vector(somerow[7:8])
  
  problems <- paste0('Problem.',problems)
  
  names(problems) <- c(1:length(problems))
  
  # Identify names.
  
  caseNames <- as.vector(somerow[9:18])
  
  caseNames <- caseNames[!is.na(caseNames)]
  
  # Define role lookup function.
  
  roleDefine <- function(rowVector,roleVector){
    
    catNum <- rowVector
    
    roles <- c()
    
    for (i in catNum){
      
      roles <- append(roles,getValue(i, roleVector))
    
    }
    
    return(roles)
    
  }
  
  # Mapply to get dataframe of role values.
  
  roleList <- list(as.vector(somerow[20:29]),
                   as.vector(somerow[30:39]),
                   as.vector(somerow[40:49]),
                   as.vector(somerow[50:59]),
                   as.vector(somerow[60:69]),
                   as.vector(somerow[70:79]),
                   as.vector(somerow[80:89])
  )
  
  roleVectors <- list(educationVector,
                      politicalVector,
                      businessVector,
                      healthcareVector,
                      engineeringVector,
                      familyVector,
                      ITVector)
  
  catStage <- mapply(roleDefine,roleList,roleVectors)
  
  print(catStage)
  
  color <- rep(c('gold'),length(caseNames))
  
  # Create categories dataframe.
  
  catFrame <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('caseNames','values', 'color'))
  
  for (i in c(1:ncol(catStage))){
    
    values <- catStage[,i]
    
    values <- values[!is.na(values)]
    
    if (!is_empty(values)){
      
      catFrameStage <- data.frame(caseNames,
                                  values,
                                  color)
      
      catFrame <- rbind(catFrame,
                        catFrameStage)
      
    }
    
  }
  
  # Make sure case names vector and problem vector will be the same length.
  
  # Create a vector that indicates how many problems each name has.
  
  problemsNum <- as.vector(somerow[91:100])
  
  problemsNum <- problemsNum[!is.na(problemsNum)]
  
  probInd <- c()
  
  for (i in problemsNum){
    probInd <- append(probInd,str_count(i,','))
  }
  
  # Add one to this vector to make sure all names get assigned.
  
  probInd <- probInd + 1
  
  # Repeat each name a number of times equal to the equivalent integer in the 
  # probInd vector.
  
  caseNames <- rep(caseNames,probInd)
    
  # Assign problems to names.

  values <- c()
  for (i in problemsNum) {
    if (str_detect(i,',')){
      stage <- str_split(i,',')
      for (n in stage){
        v <- getValue(n,problems)
        values <- append(values,v)
      }
    }
    else {
      values <- append(values,getValue(i, problems))
    }
  }
  
  color <- rep(c('tomato'),length(caseNames))
  
  # Create problems dataframe.
  
  probFrame <- data.frame(caseNames,
                          values,
                          color)
  
  # Do the same thing for the resources vector.
  
  caseNames <- as.vector(somerow[9:18])
  
  caseNames <- caseNames[!is.na(caseNames)]
  
  resourceNum <- as.vector(somerow[101:110])
  
  resourceNum <- resourceNum[!is.na(resourceNum)]
  
  resourceInd <- c()
  
  for (i in resourceNum){
    resourceInd <- append(resourceInd,str_count(i,','))
  }
  
  resourceInd <- resourceInd + 1
  
  print(resourceInd)
  
  caseNames <- rep(caseNames,resourceInd)
  
  values <- c()
  for (i in resourceNum) {
    if (str_detect(i,',')){
      stage <- str_split(i,',')
      for (n in stage){
        v <- getValue(n,resourceVector)
        values <- append(values,v)
      }
    }
    else {
      values <- append(values,getValue(i, resourceVector))
    }
  }
  
  color <- rep(c('darkolivegreen'),length(caseNames))

  # Create resources dataframe.
  
  resourceFrame <- data.frame(caseNames,
                              values,
                              color)
  
  # Create final edge list dataframe.
  
  edgeDF <- rbind(catFrame,
                  probFrame,
                  resourceFrame)
  
  colnames(edgeDF)[1] <- somerow[1]
  
  edgeDF <- subset(edgeDF, values!='none')
  
  return(edgeDF)
}

# Iterate through cases, creating edge lists for each.

edges <- apply(leaders,1,edgeList)

edges3 <- edges[which(lapply(edges,nrow)!=0)]

# Plot.

for (i in edges3) {
  
  el <-  graph_from_data_frame(i, directed = T)
  
  tg <- as_adjacency_matrix(el, sparse = F)
  
  tg <- graph_from_adjacency_matrix(tg, mode = c('directed'))
  
  windowsFonts("Helvetica" = windowsFont("Helvetica"))
  
  l <- layout_with_fr(tg)
  
  V(tg)$color = "azure3"
    
  V(tg)[i$values]$color = i$color
  
  plot(tg,
       layout=l,
       remove.multiple = F, 
       remove.loops = T,
       vertex.size = 10,
       edge.arrow.size = .2,
       vertex.label.family = ('Helvetica'),
       vertex.label.dist = 1.5,
       vertex.label.degree = -pi/2,
       main = colnames(i[1]),
       vertex.label.color = "black"
       )
  
  clrs <- c('azure3','gold','tomato','darkolivegreen')
  
  legend(x=-1.5, y=-0.5, c('Collaborator','Area and Role','Problem','Value from Collaboration'), pch=21,
         
         col="#777777", pt.bg=clrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  
}

# COMMENTS

# Improve labeling.
# ADD A LEGEND FOR COLOR CODING
# Weight nodes by degree.
# Differentiate between in/out-org questions for education/other industries.
