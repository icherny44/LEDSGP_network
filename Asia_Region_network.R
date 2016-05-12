# Asia Region LEDS GP Network Visualization

# Update contents of Google sheet
source("get.gsheet.R")

# Extract Asia Region activities
gsheet_ar <- gsheet[Region == "Asia",]

#gsheet_ar <- gsheet_ar[1:10,]
#-------------------------------------------------------------------------------
# Create nodes

activities <- gsheet_ar[,.(Activity,Country,Begin.Date,End.Date,Status)]
activities[,Type := "Activity"]
activities[,id := paste0("a",1:nrow(activities))]
setnames(activities,"Activity","Name")

# add leads and collaborators as nodes
collabs <- strsplit(gsheet_ar[,Collaborators],split = ",")

wgs.rps <- c(gsheet_ar[,Lead],unlist(collabs))
wgs.rps <- unlist(lapply(wgs.rps,str_trim))
wgs.rps <- unlist(lapply(wgs.rps, 
                         function(x) str_replace_all(x,"[[:punct:]]","")))
wgs.rps <- wgs.rps[!grepl("All",wgs.rps)]
wgs.rps <- data.table(unique(wgs.rps))

Names <- fread("Names.csv")

gp.nodes <- merge(Names,wgs.rps,by="V1")

nodes <- rbind(activities,gp.nodes,
               fill=TRUE)
setkey(nodes,id)

nodes <- unique(nodes)

nodes[,typeID := .GRP,by=Type]

# remove Asia LEDS Platform node
nodes <- nodes[!(Name == "Asia LEDS Platform"),]

node.types <- unique(nodes[,Type])
num.types <- length(node.types)


#-------------------------------------------------------------------------------

# Create edges

edges <- melt(gsheet_ar[,.(Activity,Lead)],id.vars=1)

collabs <- melt(strsplit(gsheet_ar[,Collaborators],split = ","))
collabs[1] <- unlist(lapply(collabs[1],str_trim))
collabs$Activity <- gsheet_ar[,Activity][collabs$L1]
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
                       title = ifelse(nodes$Type == "Activity",nodes$Status,""),
                       label = nodes$Name,
                       font = list(size = 14,
                                   color = "black"),
                       group = nodes$Type,                                      
                       size = ifelse(nodes$Type == "Activity",20,10),
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
Asia_Region_network <- visNetwork(visnodes,visedges,width = "100%",heigh="800px",
                              main = "LEDS GP Activities - Asia Region") %>%
  visPhysics(barnesHut = list(gravitationalConstant = "-3000")) %>%
  visInteraction(tooltipDelay = 0) %>%
  visOptions(selectedBy = list(variable = "title",
                               #  selected = "Country",
                               style = 'width: 200px; height: 26px;
                               background: #f8f8f8;
                               color: black;
                               border: none;
                               outline: none;'),
             highlightNearest = TRUE) %>%
  visLegend(addNodes = lnodes, 
            useGroups = F,
            width = .1) 

visSave(Asia_Region_network,"Asia_Region_network.html")

