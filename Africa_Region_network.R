# Africa Regional Network

library(data.table)
library(RColorBrewer)

# Update contents of Google sheet
source("get.gsheet.R")

# Extract Africa Region activities
gsheet_af <- gsheet[Region == "Africa",]

# Extract Africa Region REAL
gsheet_real_af <- gsheet_real[Region == "Africa",]

#gsheet_ar <- gsheet_ar[1:10,]
#-------------------------------------------------------------------------------
# Create nodes

# activities as nodes
activities <- gsheet_af[,.(Activity,`Type of activity`,Country,Status)]
activities[,Type := "Activity"]
activities[,id := paste0("activity",1:nrow(activities))]
setnames(activities,"Activity","Name")
setnames(activities,"Type of activity","Group")

# add leads and collaborators as nodes
collabs <- strsplit(gsheet_af[,Collaborators],split = ",")

wgs.rps <- c(gsheet_af[,Lead],unlist(collabs))
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

# add REAL assistance as nodes
real <- gsheet_real_af[,.(Name,Country,Status)]
real[,Group := "REAL"]
real[,Type := "REAL"]
real[,id := paste0("real",1:nrow(real))]

# add REAL wgs as nodes
real.wgs <- strsplit(gsheet_real_af[,`WG involved`],split = ",")
real.wgs <- unlist(lapply(real.wgs,str_trim))
real.wgs <- unlist(lapply(real.wgs, 
                         function(x) str_replace_all(x,"[[:punct:]]","")))

real.wgs <- data.table(unique(real.wgs))

real.nodes <- merge(Names,real.wgs,by="V1")

real.nodes <- rbind(real.nodes,real,fill = TRUE)

nodes <- rbind(nodes,real.nodes,
               fill=TRUE)

nodes <- nodes[!is.na(Name),]
# make sure nodes are unique
setkey(nodes,Name)

nodes <- unique(nodes)

# remove Africa LEDS Platform node
nodes <- nodes[!(Name == "Africa LEDS Platform"),]

# identify nodes by type
nodes[,typeID := .GRP,by=Type]

node.types <- unique(nodes[,Type])
num.types <- length(node.types)


#-------------------------------------------------------------------------------

# Create edges from activities

edges <- melt(gsheet_af[,.(Activity,Lead)],id.vars=1)

collabs <- melt(strsplit(gsheet_af[,Collaborators],split = ","))
collabs[1] <- unlist(lapply(collabs[1],str_trim))
collabs$Activity <- gsheet_af[,Activity][collabs$L1]
collabs$variable <- "Collaborator"
collabs <- collabs[!is.na(collabs[1]),]

edges <- rbind(edges,collabs[,-2],fill=TRUE)

edges <- merge(edges,nodes[,.(id,Name)],by.x="Activity",by.y="Name",
               all.x=T,all.y=F)
edges <- merge(edges,nodes[,.(id,V1)],by.x="value",by.y="V1",
               all.x=T,all.y=F)

setnames(edges,"Activity","Name")

# Create edges from REAL

edges.real <- melt(gsheet_real_af[,.(Name,`WG involved`)],id.vars=1)

edges.real <- merge(edges.real,nodes[,.(id,Name)],by="Name",
               all.x=T,all.y=F)

edges.real$value <- str_replace_all(edges.real$value,"[[:punct:]]","")

edges.real <- edges.real[!is.na(value)]

edges.real <- merge(edges.real,nodes[,.(id,V1)],by.x="value",by.y="V1",
               all.x=T,all.y=F)

# combine to get all the edges
edges <- rbind(edges,edges.real,fill=TRUE)

#-------------------------------------------------------------------------------
# order nodes by type
setkey(nodes,typeID)

# set up the nodes data.frame with node attributes
visnodes <- data.frame(id = nodes$id,
                       title = nodes$Status,
                       label = nodes$Name,
                       font = list(size = 14,
                                   color = "black"),
                       `Type of Activity` = nodes$Group,                                      
                       size = ifelse(nodes$Type == "Activity" | nodes$Type == "REAL",15,10),
                       shape = ifelse(nodes$Type == "Activity" | nodes$Type == "REAL","dot","square"),
                       color = brewer.pal(num.types, "Accent")[nodes$typeID])

# set up the edges data.frame with edge attributes
visedges <- data.frame(from = edges$id.x, to = edges$id.y,
                       width = 2,
                       smooth = TRUE)

visedges <- visedges[!is.na(visedges$to),]

# nodes for legend
lnodes <- data.frame(id = 1:num.types,
                     label = node.types,
                     shape = ifelse(node.types == "Activity" | node.types == "REAL"
                                    ,"circle","square"), 
                     color = brewer.pal(num.types,"Accent"),
                     size = 10,
                     stringsAsFactors = F)

# plot the thing
Africa_Region_network <- visNetwork(visnodes,visedges,width = "100%",heigh="600px",
                                  main = "LEDS GP Activities - Africa Region") %>%
  visPhysics(barnesHut = list(gravitationalConstant = "-1800")) %>%
  visInteraction(tooltipDelay = 0) %>%
  visOptions(selectedBy = list(variable = "Type.of.Activity",
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

visSave(Africa_Region_network,"Africa_Region_network.html")

