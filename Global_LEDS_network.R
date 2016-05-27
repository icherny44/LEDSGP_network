# Global Activities network
library(data.table)
library(RColorBrewer)

# Update contents of Google sheet
source("get.gsheet.R")

# Extract Global activities and REAL
gsheet_gl <- gsheet[Region == "Global",]

#-------------------------------------------------------------------------------
# Create nodes

activities <- gsheet_gl[,.(Activity,`Type of activity`,Country,Status)]
activities[,Type := "Activity"]
activities[,id := paste0("a",1:nrow(activities))]
setnames(activities,"Activity","Name")
setnames(activities,"Type of activity","Group")

# add leads and collaborators as nodes
collabs <- strsplit(gsheet_gl[,Collaborators],split = ",")

wgs.rps <- c(gsheet_gl[,Lead],unlist(collabs))
wgs.rps <- unlist(lapply(wgs.rps,str_trim))
wgs.rps <- unlist(lapply(wgs.rps, 
                         function(x) str_replace_all(x,"[[:punct:]]","")))
wgs.rps <- wgs.rps[!grepl("All",wgs.rps)]
wgs.rps <- data.table(unique(wgs.rps))

gp.nodes <- merge(Names,wgs.rps,by="V1")

nodes <- rbind(activities,gp.nodes,
               fill=TRUE)

# remove nodes with misssing name
nodes <- nodes[!is.na(Name),]

# make sure nodes are unique
setkey(nodes,Name)

nodes <- unique(nodes)

# identify nodes by type
nodes[,typeID := .GRP,by=Type]

node.types <- unique(nodes[,Type])
num.types <- length(node.types)

#-------------------------------------------------------------------------------

# Create edges

edges <- melt(gsheet_gl[,.(Activity,Lead)],id.vars=1)

collabs <- melt(strsplit(gsheet_gl[,Collaborators],split = ","))
collabs[1] <- unlist(lapply(collabs[1],str_trim))
collabs$Activity <- gsheet_gl[,Activity][collabs$L1]
collabs$variable <- "Collaborator"
collabs <- collabs[!is.na(collabs[1]),]

edges <- rbind(edges,collabs[,-2],fill=TRUE)

edges <- merge(edges,nodes[,.(id,Name)],by.x="Activity",by.y="Name",
               all.x=T,all.y=F)
edges <- merge(edges,nodes[,.(id,V1)],by.x="value",by.y="V1",
               all.x=T,all.y=F)

setnames(edges,"Activity","Name")

#-------------------------------------------------------------------------------
# order nodes by type
setkey(nodes,typeID)

# set up the nodes data.frame with node attributes
visnodes <- data.frame(id = nodes$id,
                       title = ifelse(!is.na(nodes$Status),
                                      paste(nodes$Country,nodes$Status,sep = ", "), ""),
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
Global_LEDS_network <- visNetwork(visnodes,visedges,width = "100%",heigh="600px",
                                 main = "LEDS GP Activities - Global") %>%
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

visSave(Global_LEDS_network,"Global_LEDS_network.html")



