
#
# the real data about voters (needed for filtering)
# and about each vote they gave
#
votes <- read.csv2("D:\\!!!!!!DATASETS\\elections\\results.csv", stringsAsFactors = FALSE)
voters <- read.csv2("D:\\!!!!!!DATASETS\\elections\\voters.csv", stringsAsFactors = FALSE)
voters[,5] <- as.logical(voters[,5])

#
# now the names of the candidates and their blocks\curias
#
candidates <- read.csv2("D:\\!!!!!!DATASETS\\elections\\candidates_grouped.csv",
 	header = FALSE, stringsAsFactors = FALSE)



#registration times
reg.times <- strptime(voters[,2], format = "%m/%d/%Y %H:%M:%S")
late.registration <- reg.times >= strptime("10/16/2012 12:00:00", format = "%m/%d/%Y %H:%M:%S")
suspect.ids <- which(votes[,1] %in% intersect(votes[,1], voters[late.registration,1]))



# mavrodi list
mavrodi.list <- c(7,8,14,19,22,36,42,45,47,51,58,63,70,72,73,103,112,113,116,
	117,121,165,172,187,194,198,201,203,215,222,10,57,148,156,178,16,32,105)




#
# dummy entries, we just need to leave them away
#


location <- c(39, 40, 41, 85)
sex <- c(83, 139)
status <- c(25, 80, 145, 159, 169, 183, 202, 211)


#
#
# let's construct the edge matrices
#
#

raw.edges.matr <- matrix(0, nrow(candidates), nrow(candidates))
colnames(raw.edges.matr) <- candidates[,1]
rownames(raw.edges.matr) <- candidates[,1]

#
# we'd need to access the specific columns
# edges.matr[as.character(sex),] <- 1
#


duplicates.detected <- 0
unique.inds <- unique(votes[,1]) #it's the same length as the votes,, so there's no demand in this procedure
bad.detected <- 0

intersect <- numeric(nrow(votes))

for(i in 1:nrow(votes)){
	#the data comes with a 1-shift in the voters
	raw.choices <- as.numeric(strsplit(votes[i,3], ",")[[1]]) + 1
	
	mark.of.mavr1 <- intersect(raw.choices, mavrodi.list)
	intersect[i] <- length(mark.of.mavr1) 
	
	#max(length(mark.of.mavr1), length(mark.of.mavr2), length(mark.of.mavr3))
	#which.mavr <- which.max(c(length(mark.of.mavr1), length(mark.of.mavr2), length(mark.of.mavr3)))
	
	if(i %in% suspect.ids & intersect[i] >= 38){ #option for "compromise" filtering
	#if(intersect[i] >= 35){ #option for no-compromise filtering
		bad.detected <- bad.detected +1
		print(paste("mavrodi ", bad.detected))
	}else{
	
		choices <- unique(raw.choices) #in order to remove the voice duplicates
		if(length(choices) != length(raw.choices))	duplicates.detected <- duplicates.detected +1
		
		
		for(j in 1:length(choices)){
			k <- choices[j]
			raw.edges.matr[as.character(k), as.character(choices)] <- raw.edges.matr[as.character(k), as.character(choices)] + 1		
		}
		

	}

	print(i)
}

print(bad.detected)


#
# now let's process the matrix that we just constructed
#

creepy.stuff <- c(location, sex, status)
left.inds <- which(candidates[,1] %in% creepy.stuff)
edges.correct <- raw.edges.matr[-left.inds, -left.inds]

#
# reserve a filed for mavrodi
#

node.field.2 <- candidates[,4]*0
node.field.2[candidates[,1] %in% mavrodi.list] <- 1


total.votes <- diag(edges.correct)


node.names <- candidates[-left.inds,2]
node.ids <- 1:length(node.names)
node.field.1 <- candidates[-left.inds,3] #ideology-column
node.field.2 <- node.field.2[-left.inds] #block
node.field.3 <- total.votes
node.field.4 <- candidates[-left.inds,5] #collapsed blocks


#
# collapse blocks
#

collapse.groups <- node.field.4
collapse.groups[node.field.4 == 1] <- 1
collapse.groups[node.field.4 == 5] <- 2
collapse.groups[node.field.4 == 13] <- 3
collapse.groups[node.field.4 == 15] <- 4
collapse.groups[node.field.4 == 2 | node.field.4 == 14 ] <- 5
collapse.groups[node.field.4 == 4 | node.field.4 == 16 ] <- 6
collapse.groups[node.field.4 == 3 | node.field.4 == 8 | node.field.4 == 9 | node.field.4 == 12 ] <- 7
collapse.groups[node.field.4 == 6 | node.field.4 == 7 | node.field.4 == 10 | node.field.4 == 11 ] <- 8
node.field.4 <- collapse.groups


#
#
# process the edge matrix
# there are different options for edge-weights
#
#

edges.matr <- edges.correct

for(i in 1:(ncol(edges.matr) - 1)){
	for(j in (i+1):ncol(edges.matr)){
		if(min(edges.matr[i,i], edges.matr[j,j]) >  0) {
			#shared votes metric:
			edges.matr[i,j] <- edges.matr[i,j]/(edges.matr[i,i] + edges.matr[j,j])
			
			#devotion metric:
			#edges.matr[i,j] <- 	edges.matr[i,j]/min(edges.matr[i,i], edges.matr[j,j])
			
			#geometric mean metric:
			#edges.matr[i,j] <- 	edges.matr[i,j]/sqrt(edges.matr[i,i] * edges.matr[j,j])
			
			#skepsis measure
			#edges.matr[i,j] <-  edges.matr[i,j]/max(edges.matr[i,i], edges.matr[j,j])
			
			edges.matr[j,i] <- 0
		}
	}
}

edges.matr <- edges.matr - diag(diag(edges.matr))
edges.matr[edges.matr < 0] <- 0

#
# manual correction of one of the candidate blocks
#
node.field.4[c(0,3,57,60,100,123,132,146,203)+1] <- c(4,5,7,4,4,4,4,4,5)





my.file <- "D:\\opposition_corrupted_shared.gdf"

#
#
# everything below this comment is dealing with writing only. all you'd want to change is the file-destination
#
#

all.nodes <- character(0)
for(i in node.ids){
	id = i - 1
	name = node.names[i]
	field.1 = node.field.1[i] #ideology-column
	field.2 = node.field.2[i] #block
	field.3 = node.field.3[i] #vote-count	
	field.4 = node.field.4[i] #collapsed blocks
	
	nodes.text <- paste(id,",\"", name,"\",", 
		field.1, ",", field.2,",", field.4,",", i,",", i,",", field.3, ",", field.3/nrow(votes),sep="")
	
	all.nodes <- c(all.nodes, nodes.text)				
}




#note that the matrix doesn't necessary has to be transitive

all.edges <- character(0)
for(i in 1:nrow(edges.matr)){
	for(j in 1:ncol(edges.matr)){
		if(edges.matr[i,j] > 0){
			id = length(all.edges) 	
			source = i - 1
			target = j - 1
			value = edges.matr[i,j]				
			edges.text <- paste(source,",", target,",", value,sep="")
	
			
			all.edges <- c(all.edges, edges.text)

		}
	}
}



#
# now we're ready to compile all the stuff in gdf style
#

header.string <- "nodedef>name,label varchar(50),group_1 DOUBLE,group_2 DOUBLE,group_3 DOUBLE,y DOUBLE,x DOUBLE,votes DOUBLE,norm_votes DOUBLE"
ending.string <- "edgedef>node1,node2,weight DOUBLE"
whole.story <- c(header.string, all.nodes, ending.string, all.edges)


#
# just write the whole story. note that we owe a %->" replacement in npp afterwards
#

connect <- file(my.file)
cat(whole.story , file = connect, sep = "\n")
close(connect)

