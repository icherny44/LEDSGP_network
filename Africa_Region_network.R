# Africa LEDS Network

# Africa Region LEDS GP Network Visualization
library(data.table)
library(RColorBrewer)

# Update contents of Google sheet
source("get.gsheet.R")

# Extract Africa Region activities and REAL
gsheet_la <- gsheet[Region == "Africa",]

gsheet_real_la <- gsheet_real[Region == "AfLP",]

#gsheet_la <- gsheet_la[1:10,]
#-------------------------------------------------------------------------------
# Create nodes

activities <- gsheet_la[,.(Activity,`Type of activity`,Country,Status)]
activities[,Type := "Activity"]
activities <- activities[!Status == "Complete",]
activities[,id := paste0("a",1:nrow(activities))]
setnames(activities,"Activity","Name")
setnames(activities,"Type of activity","Group")

# add leads and collaborators as nodes
collabs <- strsplit(gsheet_la[,Collaborators],split = ",")

wgs.rps <- c(gsheet_la[,Lead],unlist(collabs))
wgs.rps <- unlist(lapply(wgs.rps,str_trim))
wgs.rps <- unlist(lapply(wgs.rps, 
                         function(x) str_replace_all(x,"[[:punct:]]","")))
wgs.rps <- wgs.rps[!grepl("All",wgs.rps)]
wgs.rps <- data.table(unique(wgs.rps))

gp.nodes <- merge(Names,wgs.rps,by="V1")

nodes <- rbind(activities,gp.nodes,
               fill=TRUE)

# add REAL assistance as nodes
real <- gsheet_real_la[,.(Name,Country,Status)]
real <- real[!is.na(Name)]
real[,Group := "REAL"]
real[,Type := "REAL"]
# removed archived and provided REAL
real <- real[!(Status == "Archived" | Status == "Provided")]

# add real ID
real[,id := paste0("real",1:nrow(real))]

# add REAL wgs as nodes
real.wgs <- strsplit(gsheet_real_la[,`WG involved`],split = ",")
real.wgs <- unlist(lapply(real.wgs,str_trim))
real.wgs <- unlist(lapply(real.wgs, 
                          function(x) str_replace_all(x,"[[:punct:]]","")))

real.wgs <- data.table(unique(real.wgs))

real.wgs <- merge(Names,real.wgs,by="V1",all.y = T)
real.wgs[is.na(Name),Name := V1]

real.nodes <- rbind(real.wgs,real,fill = TRUE)

# create ids for nodes not defined in Names.csv
real.nodes[is.na(id),id := paste0("o",.I)]

nodes <- rbind(nodes,real.nodes,
               fill=TRUE)

# remove nodes with misssing name
nodes <- nodes[!is.na(Name),]

# remove Africa LEDS Platform node
nodes <- nodes[!(Name == "Africa LEDS Platform"),]

# add "other" type to other nodes
nodes[is.na(Type),Type := "Other"]

# make sure nodes are unique
setkey(nodes,Name)

nodes <- unique(nodes)

# identify nodes by type
nodes[,typeID := .GRP,by=Type]

node.types <- unique(nodes[,Type])
num.types <- length(node.types)

#-------------------------------------------------------------------------------

# Create edges

edges <- melt(gsheet_la[,.(Activity,Lead)],id.vars=1)

collabs <- melt(strsplit(gsheet_la[,Collaborators],split = ","))
collabs[1] <- unlist(lapply(collabs[1],str_trim))
collabs$Activity <- gsheet_la[,Activity][collabs$L1]
collabs$variable <- "Collaborator"
collabs <- collabs[!is.na(collabs[1]),]

edges <- rbind(edges,collabs[,-2],fill=TRUE)

edges <- merge(edges,nodes[,.(id,Name)],by.x="Activity",by.y="Name",
               all.x=T,all.y=F)
edges <- merge(edges,nodes[!is.na(V1),.(id,V1)],by.x="value",by.y="V1",
               all.x=T,all.y=F)

edges <- edges[!(is.na(id.x) | is.na(id.y))]

setnames(edges,"Activity","Name")

# Create edges from REAL

edges.real <- melt(gsheet_real_la[,.(Name,`WG involved`)],id.vars=1)

edges.real <- merge(edges.real,nodes[,.(id,Name)],by="Name",
                    all.x=T,all.y=F)

edges.real$value <- str_replace_all(edges.real$value,"[[:punct:]]","")

edges.real <- edges.real[!(is.na(Name) | is.na(id))]

edges.real <- merge(edges.real,nodes[,.(id,V1)],by.x="value",by.y="V1",
                    all.x=T,all.y=F)

# switch order so that edges are always from WGs to REAL
edges.real <- data.table(Name = edges.real$Name,
                         id.x = edges.real$id.y, id.y = edges.real$id.x)

# combine to get all the edges
edges <- rbind(edges,edges.real,fill=TRUE)
edges <- edges[!is.na(Name)]

#-------------------------------------------------------------------------------
# order nodes by type
setkey(nodes,typeID)

# set up the nodes data.frame with node attributes
visnodes <- data.frame(id = nodes$id,
                       title = ifelse(!is.na(nodes$Status),
                                      paste0("Country: ",nodes$Country,
                                             "<br>",
                                             "Status: ",nodes$Status), ""),
                       label = nodes$Name,
                       font = list(size = 14,
                                   color = "black"),
                       `Type of Activity` = nodes$Group,                                      
                       size = ifelse(nodes$Type == "Activity" | nodes$Type == "REAL",15,10),
                       shape = ifelse(nodes$Type == "Activity" | nodes$Type == "REAL","dot","square"),
                       color = brewer.pal(num.types, "Accent")[nodes$typeID],
                       hidden = ifelse(nodes$Name == "TBD",TRUE,FALSE))

# order by type of node to get correct hierarchy
visnodes$idType <- strtrim(visnodes$id,1)
visnodes$idType <- factor(visnodes$idType, levels = c("o","w","s","a","r"))
visnodes <- visnodes[order(visnodes$idType),]

# create hidden nodes 
hidden <- list(id = "h", hidden = TRUE)
visnodes <- rbind(as.data.table(visnodes),hidden,fill= TRUE)

# set up the edges data.frame with edge attributes
TBD <- unique(nodes[Name == "TBD",id])
visedges <- data.frame(from = edges$id.x, to = edges$id.y,
                       width = 2,
                       smooth = FALSE,
                       color = ifelse(edges$id.x == TBD,"white",NA))

visedges$fromID <- strtrim(visedges$from,1)
visedges$fromID <- factor(visedges$fromID, levels = c("o","w","s","a","r"))
visedges <- visedges[order(visedges$fromID),]

# add hidden edges
mid.nodes <- nodes[!is.na(V1),id]
hidden.edge <- list(from = rep("h",length(mid.nodes)), to = mid.nodes,
                    color = rep("white",length(mid.nodes)))
visedges <- rbind(as.data.table(visedges),hidden.edge,fill= TRUE)

# nodes for legend
lnodes <- data.frame(id = 1:num.types,
                     label = node.types,
                     shape = ifelse(node.types == "Activity" | node.types == "REAL"
                                    ,"dot","square"), 
                     color = brewer.pal(num.types,"Accent"),
                     size = 10,
                     stringsAsFactors = F)

# plot the thing
Africa_Region_network <- visNetwork(visnodes,visedges,width = "100%",heigh="600px",
                                  main = "LEDS GP Activities - Africa Region") %>%
  visInteraction(tooltipDelay = 0) %>%
  visLegend(addNodes = lnodes, 
            useGroups = F,
            width = .1) %>%
  visHierarchicalLayout(direction = "LR", treeSpacing = 100,
                        levelSeparation = 400, sortMethod = 'directed')

visSave(Africa_Region_network,"Africa_Region_network.html")