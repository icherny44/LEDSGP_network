# LEDS GP Activities - Network visualization

# Update contents of Google sheet
source("get.gsheet.R")

#-------------------------------------------------------------------------------
# Create nodes

activities <- gsheet[,.(Activity,Country,Begin.Date,End.Date,Status)]
activities[,Type := "Activity"]
activities[,id := paste0("a",1:nrow(activities))]
setnames(activities,"Activity","Name")


# add leads and collaborators as nodes
collabs <- strsplit(gsheet[,Collaborators],split = ",")

wgs.rps <- c(gsheet[,Lead],unlist(collabs))
wgs.rps <- unlist(lapply(wgs.rps,str_trim))
wgs.rps <- unlist(lapply(wgs.rps, 
                         function(x) str_replace_all(x,"[[:punct:]]","")))
wgs.rps <- wgs.rps[!grepl("All",wgs.rps)]
wgs.rps <- data.table(unique(wgs.rps))

gp.nodes <- merge(Names,wgs.rps,by="V1")

nodes <- rbind(activities,gp.nodes,
               fill=TRUE)
setkey(nodes,id)

nodes <- unique(nodes)

nodes[,typeID := .GRP,by=Type]

node.types <- unique(nodes[,Type])
num.types <- length(node.types)

#-------------------------------------------------------------------------------
# Create edges

edges <- melt(gsheet[,.(Activity,Lead)],id.vars=1)

collabs <- melt(strsplit(gsheet[,Collaborators],split = ","))
collabs[1] <- unlist(lapply(collabs[1],str_trim))
collabs$Activity <- gsheet[,Activity][collabs$L1]
collabs$variable <- "Collaborator"
collabs <- collabs[!is.na(collabs[1]),]

edges <- rbind(edges,collabs[,-2],fill=TRUE)

edges <- merge(edges,nodes[,.(id,Name)],by.x="Activity",by.y="Name",
               all.x=T,all.y=F)
edges <- merge(edges,nodes[,.(id,V1)],by.x="value",by.y="V1",
               all.x=T,all.y=F)

#-------------------------------------------------------------------------------
# set up the nodes data.frame with node attributes
visnodes <- data.frame(id = nodes$id,
                       title = nodes$Name,
                       label = ifelse(nodes$Type == "Activity","",nodes$Name),
                       font = list(size = 14,
                                   color = "black"),
                       group = nodes$Type,                                      
                       size = ifelse(nodes$Type == "Activity",10,20),
                       shape = "dot",
                       color = c("green","tomato","purple","blue")[nodes$typeID])

# set up the edges data.frame with edge attributes
visedges <- data.frame(from = edges$id.x, to = edges$id.y,
                       width = 2,
                       smooth = TRUE)

visedges <- visedges[!is.na(visedges$to),]

# nodes for legend
lnodes <- data.frame(id = 1:num.types,
                     label = node.types,
                     shape = c( "dot"), 
                     color = c("green","tomato","purple","blue")[1:num.types],
                     size = 10,
                     stringsAsFactors = F)

# plot the thing
LEDS_GP_network <- visNetwork(visnodes,visedges,width = "100%",heigh="1000px",
                              main = "LEDS GP Activities") %>%
  visPhysics(barnesHut = list(gravitationalConstant = "-8000")) %>%
  visInteraction(tooltipDelay = 0) %>%
  visOptions(selectedBy = list(variable = "group",
                               #  selected = "Country",
                               style = 'width: 200px; height: 26px;
                               background: #f8f8f8;
                               color: black;
                               border: none;
                               outline: none;'),
             highlightNearest = TRUE) %>%
  visLegend(addNodes = lnodes, 
            useGroups = F,
            width = .21) 
 
