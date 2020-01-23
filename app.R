library(shiny)
library(ggplot2)
library(zoo)
library(shinyjs)
selmin <- 0 
selmax <- 0
ui <- shinyUI(fluidPage(
    useShinyjs(),
    titlePanel("DroughtBox Analysis v 1.2"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"'),
                         textInput(inputId = "drymass",
                                   label = "Dry Mass, (g)",
                                   value = ""),
                         textInput(inputId = "leafarea",
                                   label = paste("Leaf Area (m","\U00B2",")", sep = ""),
                                   value = "")
                         
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Interactive Plot",
                 pageWithSidebar(
                     headerPanel('DroughtBox Plot'),
                     sidebarPanel(
                         textOutput("my_csv_name"),
                         textOutput("text1"),
                         textOutput("text2"),
                         textOutput("text3"),
                         textOutput("text4"),
                         textOutput("text5"),
                       # textOutput("text6"),
                         textOutput("text7"),
                         textOutput("text8"),
                         br(),
                         actionButton("button", "Show k-means fit")
                         
                         # "Empty inputs" - they will be updated after the data is uploaded

                         
                     ),
                     mainPanel(
                         uiOutput("plotui"),
                         plotOutput("plot1", height = 100),
                         hidden(plotOutput("plot2")),
                         hidden(plotOutput("plot3"))
                     )
                 )
        )
        
    )
)
)

server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
    
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric
        # variables you could set "choices = sapply(df, is.numeric)"
        # It depends on what do you want to do later on.
       
        return(df)
    })

    add_to_df <- reactive({
        alldata <- as.data.frame(data())
        alldata$water.mass <- as.numeric(alldata$mass) - as.numeric(input$drymass)
        alldata$mmol.h2o <- alldata$water.mass / 0.01802 #convert g to mmol
        alldata
        
    })
    
    output$contents <- renderTable({
         req(input$drymass)
#        cbind(data(), input$drymass)
        add_to_df()
    })


    # reactive expressions to get the values from the brushed area
    selmin <- reactive(round(ifna(input$plot_brush$xmin, 0, 0)))
    selmax <- reactive(round(ifna(input$plot_brush$xmax, 0, 0)))
    
    # include the brush option: direction = "x" says that y values are fixed (min and max)
    output$plotui <- renderUI({
        plotOutput("plot", height = 300, 
                   brush = brushOpts(id = "plot_brush", direction = "x",
                                     fill = "blue", opacity = 0.5)
        )
    })
    output$my_csv_name <- renderText({
        # Test if file is selected
        if (!is.null(input$file1$datapath)) {
            # Extract file name (additionally remove file extension using sub)
            return(sub(".csv$", "", basename(input$file1$name)))
        } else {
            return(NULL)
        }
    })
    output$text1 <- renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        paste(expression(R^2),"=",signif(summary(fit)$adj.r.squared,5))
    })
    output$text2 <- renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        paste("\nTemp:", round(mean(dat.brushed$temp),2), "(C)")
    })
    output$text3 <- renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        paste("\nRH:", round(mean(dat.brushed$rh),2), "%")
    })
    output$text4 <- renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        (paste("\nVPD:", round(mean(dat.brushed$vpd),2), "(kPa)"))
    })
    output$text5 <- renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        paste("\nSlope =",format(signif(fit$coef[[2]], 5),scientific=FALSE), "(mmol/sec)")
    })
    # output$text6 <-renderText({
    #     req(input$plot_brush)
    #     dat <- as.data.frame(add_to_df())
    #     dat$Time <- as.POSIXct(dat$Time)
    #     dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
    #     fit <- lm(mmol.h2o~Time, data=dat.brushed)
    #     paste("\nP =",signif(summary(fit)$coef[2,4], 5))
    #     
    # })
    output$text7 <-renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        paste("\n(E) =",(as.numeric(format(signif(fit$coef[[2]], 5),scientific=FALSE))/as.numeric(input$leafarea)*-1), " (mmol/s/m","\U00B2",")", sep = "")
        
    })
    output$text8 <-renderText({
        req(input$plot_brush)
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        paste("\ngmin =",format(signif((as.numeric(format(signif(fit$coef[[2]], 5),scientific=FALSE))/
                                            as.numeric(input$leafarea))/as.numeric(mean(dat.brushed$vpd))*
                                           101.325*-1, 5), scientific=FALSE), " (mmol/s/m","\U00B2",")", sep = "") #101.325 = atm pressure, kpa
        
    })
    # render the first plot including brush
    output$plot <- renderPlot({
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        ggplot() + geom_point(data = dat, aes(x = Time, y = mmol.h2o)) +
            
            #  geom_rect(data=dat,aes(xmin = min(Time), xmax = max(Time), 
            #                ymin = -Inf, ymax = Inf), fill = "NA") +
            ylab("mmol.h2o ") +
            theme_minimal()+
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  plot.margin = margin(1,1,0,1)) +
            ggtitle(" ")
    })
    output$plot1 <-renderPlot({
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        ggplot() + geom_line(data = dat, aes(x = Time, y = temp), color = "red") +
            ylab("Temp, C") + xlab("time") +
            theme_minimal()
    })
    
    # render the second plot reactive to the brushed area
    output$info <- renderPrint({
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        # data.writer<-c(signif(summary(fit)$adj.r.squared),
        #                round(mean(dat.brushed$temp),2),
        #                round(mean(dat.brushed$rh),2),
        #                round(mean(dat.brushed$vpd),2),
        #                format(signif(fit$coef[[2]], 5),scientific=FALSE),
        #                signif(summary(fit)$coef[2,4], 5))
        # write.csv(as.data.frame(data.writer), file = "data.csv", append=T)
        
    })
    output$plot2 <- renderPlot({
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        # prepare the data
        #  pdat <- subset(dat, Time < selmax & Time > selmin)
        dat.brushed<-subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax)
        fit <- lm(mmol.h2o~Time, data=dat.brushed)
        # data.writer<-c(signif(summary(fit)$adj.r.squared),
        #                round(mean(dat.brushed$temp),2),
        #                round(mean(dat.brushed$rh),2),
        #                round(mean(dat.brushed$vpd),2),
        #                format(signif(fit$coef[[2]], 5),scientific=FALSE),
        #                signif(summary(fit)$coef[2,4], 5))
        # write.csv(as.data.frame(data.writer, file = "data.csv", append=T))
        ggplot(data=dat, aes(x = Time, y = mmol.h2o)) + 
            geom_smooth(data=subset(dat, Time > input$plot_brush$xmin & Time < input$plot_brush$xmax),
                        aes(x=Time, y=mmol.h2o), method = "lm", color = "red", size = 5) + 
            geom_point() +
            # labs(aes(x=min(x), y=min(y)), title = paste("Selected linear fit",
            #                                             "\nAdj R2 = ",signif(summary(fit)$adj.r.squared, 5),
            #                                             "\nTemp:", round(mean(dat.brushed$temp),2), "(C)",
            #                                             "\nRH:", round(mean(dat.brushed$rh),2), "%",
            #                                             "\nVPD:", round(mean(dat.brushed$vpd),2), "(kPa)",
            #                                             "\nSlope =",format(signif(fit$coef[[2]], 5),scientific=FALSE), "(g/sec)",
            #                                             "\nP =",signif(summary(fit)$coef[2,4], 5))) +
            theme_minimal()
        
    })
    output$plot3 <-renderPlot({
        dat <- as.data.frame(add_to_df())
        dat$Time <- as.POSIXct(dat$Time)
        # prepare the data
        #  pdat <- subset(dat, Time < selmax & Time > selmin)
        a <- data.frame(x=as.POSIXct(dat$Time), 
                           y=dat$mass)
        a$x<-as.numeric(a$x)  
        #a<-subset(a, x > as.POSIXct("2019-12-23 20:00:00") & x < as.POSIXct("2019-12-24 12:00:00"))
        f <- function (d) {
            m <- lm(y~as.numeric(x), as.data.frame(d))
            return(coef(m)[2])
        }
        co <- rollapply(a, 10, f, by.column=F)
        co.cl <- kmeans(co, 5, nstart=25)
        b.points <- which(co.cl$cluster == match(max(co.cl$centers), co.cl$centers))+1
        RES <- a[b.points,]
        # plot(a, xlim=c(100,1200))
        # points(RES,pch=15,col="red")
        # abline(lm(y~x,RES),col="blue")
        # summary(lm(y~x,RES))
        # coef(lm(y~x,RES))
        
        fit<-lm(y~x,RES)
        ggplot(a, aes(y=y, x=x)) + geom_point() +
            geom_point(aes(x=RES$x, y=RES$y),data=RES, col="red") +
            geom_smooth(data=RES, method = "lm", se=TRUE) +
            labs(aes(x=min(x), y=min(y)), title = paste("K-means Clustering [AUTOREGRESSION]",
                                                        "\nAdj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                                                        "     Intercept =",signif(fit$coef[[1]],5 ),
                                                        "\nSlope =",signif(fit$coef[[2]], 5),
                                                        "     P =",signif(summary(fit)$coef[2,4], 5)))
    })
    
    observeEvent(input$plot_brush, {
        # show plot
        show("plot2")
    })
    observeEvent(input$button, {
        # every time the button is pressed, alternate between hiding and showing the plot
        show("plot3")
    })
})

shinyApp(ui, server)