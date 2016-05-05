# LEDS GP organization - Network visualization

library(visNetwork)
library(shiny)

server <- function(input, output) {
  output$network <- renderVisNetwork({

edges <- read.csv("edges.csv", stringsAsFactors = F)
nodes <- read.csv("nodes.csv", stringsAsFactors = F)

# set up the nodes data.frame with node attributes
visnodes <- data.frame(id = nodes$id,
                        Name = nodes$Name,
                        label = nodes$Name,
                        font = list(size = 18,
                                    color = "black"),
                        group = nodes$Type,                                      
                        size = c(15,30,45)[nodes$TypeID],
                        shape = "dot",
                        color = c("green","tomato","purple")[nodes$TypeID],
                        title = paste0(nodes$Description)) 

# set up the edges data.frame with edge attributes
names(edges) <- c("from","to","Value","Description","Status")

visedges <- data.frame(from = edges$from, to = edges$to,
                       title = paste0(edges$Description),
                       width = 2,
                       smooth = TRUE)

# remove nodes with no connections
visnodes <- visnodes[nodes$id %in% edges$from | nodes$id %in% edges$to, ]

# nodes for legend
lnodes <- data.frame(id = 1:3,
                     label = c("Country", "Working Group","Regional Platform"),
                     shape = c( "dot"), color = c("green","tomato","purple"),
                     size = c(10,15,20),
                     stringsAsFactors = F)

# plot the thing
visNetwork(visnodes,visedges,width = "100%", height = "100%") %>% 
  visPhysics(enabled = TRUE,
             barnesHut = list(avoidOverlap = .19)) %>%
  visInteraction(keyboard = TRUE, tooltipDelay = 0) %>%
  visOptions(highlightNearest = TRUE,
             selectedBy = list(variable = "group", style = 'width: 200px; height: 26px;
                                                   background: #f8f8f8;
                                                   color: black;
                                                   border: none;
                                                   outline: none;')) %>%
  visLegend(addNodes = lnodes, 
            useGroups = F,
            width = .21) 
})
  
  output$shiny_return <- renderPrint({
    input$current_node_id
  })
}

ui <- fluidPage(
  titlePanel("LEDS GP Network Visualization"),
  visNetworkOutput("network",width="100%",height="100%")
#  verbatimTextOutput("shiny_return")
)

shinyApp(ui = ui, server = server)
  
