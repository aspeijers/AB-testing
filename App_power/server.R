# load packages
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

shinyServer(function(input, output, session) {
    
    # calc sample size
    rel <- reactive({
        
        1 - pnorm( (input$p1/100 - input$p2/100 + qnorm(1-input$signif/100)*sqrt(input$p1/100*(1-input$p1/100)/input$n)) 
               * sqrt(input$n/(input$p2/100*(1-input$p2/100))) )
    })
    
    # generate data for Control
    x1_dens <- reactive({
        density( rnorm(n=10^6, mean=input$p1/100, sd=sqrt(input$n*input$p1/100*(1-input$p1/100) )/input$n) - input$p1/100)
    })
    
    # generate data for Treatment
    x2_dens <- reactive({
        density( rnorm(n=10^6, mean=input$p2/100, sd=sqrt(input$n*input$p2/100*(1-input$p2/100) )/input$n) - input$p1/100)
    })

    # combine data into dataframe
    dist <- reactive({
        as.data.frame( cbind(x1_dens()$x, x1_dens()$y/input$n, x2_dens()$x, x2_dens()$y/input$n) )
    })
    
    # calc intersections for alpha and beta
    int_x <- reactive({ qnorm(p=(1-input$signif/100), mean=input$p1/100, sd=sqrt(input$n*input$p1/100*(1-input$p1/100))/input$n) - input$p1/100})
    int_y <- reactive({ dnorm(x=int_x(), mean=input$p1/100, sd=sqrt(input$n*input$p1/100*(1-input$p1/100) )/input$n) /input$n })
    int_y_B <- reactive({ dnorm(x=int_x(), mean=input$p2/100, sd=sqrt(input$n*input$p2/100*(1-input$p2/100) )/input$n) /input$n })
    
    # define shaded areas
    shaded <- reactive({
        shade_A <- rbind( c(int_x(), 0), subset(dist()[,1:2], V1 > int_x()), c(dist()[nrow(dist()),1],0) )
        shade_B <- rbind( c(dist()[1,3],0), subset(dist()[,3:4], V3 < int_x()), c(int_x(), 0) ) 
        names(shade_A) <- c("V1", "V2")
        names(shade_B) <- c("V3", "V4")
        return(list(shade_A, shade_B))
    })
    
    # plot
    output$plot1 <- renderPlot({
        
        ggplot(dist()) +
            geom_line(aes(x=V1, y=V2, col="version_a")) +
            geom_line(aes(x=V3, y=V4, col='version_b')) +
            geom_vline(xintercept=int_x()) +
            #geom_segment(aes(x = int_x, y = 0, xend = int_x, yend = int_y), col="black") +
            geom_polygon(data=shaded()[[1]], aes(x=V1, y=V2, fill="alpha"), alpha=0.3) +
            geom_polygon(data=shaded()[[2]], aes(x=V3, y=V4, fill="beta"), alpha=0.3) +
            labs(x="Conversion Rate (CR)", y="Density") +
            annotate("text", x=(input$p2 - input$p1)/100, y=max(int_y(), int_y_B())/3, label = paste0("significance = ", round(input$signif/100,2)), col="red") +
            annotate("text", x=0, y=max(int_y(), int_y_B())/3, label = paste0("beta = ", round(1-rel(),2)), col="blue") +
            annotate("text", x=Inf, y=Inf, hjust=1.2, vjust=1.2, label=paste0("reliability = ", round(rel(),2)), size=8) +
            scale_colour_manual(name = "Distributions",
                                breaks = c("version_a", "version_b"),
                                values = c('red', 'blue'),
                                labels = c("Control (A)", "Treatment (B)")) +
            scale_fill_manual(name = "",
                              breaks = c("alpha", "beta"),
                              values = c('red', 'blue'),
                              labels = c("alpha", "beta"))
    }) 

})
