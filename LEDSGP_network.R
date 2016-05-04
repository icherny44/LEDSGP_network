# LEDS GP organization - Network visualization

library(visNetwork)

edges <- read.csv("edges.csv", stringsAsFactors = F)
nodes <- read.csv("nodes.csv", stringsAsFactors = F)

# remove nodes with no connections
nodes <- nodes[nodes$id %in% edges$Var1 | nodes$id %in% edges$Var2, ]

# set up the nodes data.frame with node attributes
visnodes <- data.frame(id = nodes$id,
                        Name = nodes$Name,
                        label = nodes$Name,
                        font = list(size = 18,
                                    color = "black"),
                        group = nodes$Type,                                      
                        size = c(15,45,30)[as.factor(nodes$Type)],
                        shape = "dot",
                        color = c("green","purple","tomato")[as.factor(nodes$Type)],
                        title = paste0(nodes$Description)) 

# set up the edges data.frame with edge attributes
names(edges) <- c("from","to","Value","Description","Status")

visedges <- data.frame(from = edges$from, to = edges$to,
                       title = paste0(edges$Description),
                       width = 2,
                       smooth = TRUE)

# nodes for legend
lnodes <- data.frame(id = 1:3,
                     label = c("Country", "Working Group","Regional Platform"),
                     shape = c( "dot"), color = c("green","tomato","purple"),
                     size = 10,
                     stringsAsFactors = F)

# plot the thing
LEDS_GP_network <- visNetwork(visnodes,visedges,width = "1200px", height = "600px",
                      main = "LEDS GP Activities") %>%
                      visPhysics(enabled = TRUE,
                                 barnesHut = list(avoidOverlap = .2)) %>%
                      visInteraction(keyboard = TRUE, tooltipDelay = 0) %>%
                      visOptions(selectedBy = list(variable = "group",
                                                 #  selected = "Country",
                                                   style = 'width: 200px; height: 26px;
                                                   background: #f8f8f8;
                                                   color: black;
                                                   border: none;
                                                   outline: none;')) %>%
                      visLegend(addNodes = lnodes, 
                                useGroups = F,
                                width = .2) 

visSave(LEDS_GP_network, file = "LEDS_GP_network.html", selfcontained = FALSE)
 
