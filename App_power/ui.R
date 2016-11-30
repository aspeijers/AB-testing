shinyUI(pageWithSidebar(

    headerPanel('A/B Testing - Calculate Sample Size'),

    sidebarPanel(
        sliderInput('signif', 'Significance (%)', 
                    min=1, max=10, value=5, step=1),
        sliderInput('n', 'Sample size',
                    min=0, max=10000, value=5000, step=1),
        numericInput('p1', 'Conversion Rate of A (%)', value=4,
                     min = 0, max = 100, step=0.1),
        numericInput('p2', 'Required or Expected Conversion Rate of B (%)', value=4.5,
                     min = 0, max = 100, step=0.1)
    ),
    
    mainPanel(
        plotOutput('plot1')
    )
))
