shinyUI(pageWithSidebar(

    headerPanel('A/B Testing - Calculate Sample Size'),

    sidebarPanel(
        sliderInput('signif', 'Significance (%)', 
                    min=1, max=10, value=5, step=1),
        sliderInput('rel', 'Reliability (%)',
                    min=60, max=99, value=80, step=1),
        numericInput('p1', 'Conversion Rate of A (%)', value=4,
                     min = 0, max = 100, step=0.5),
        numericInput('p2', 'Required Conversion Rate of B (%)', value=4.5,
                     min = 0, max = 100, step=0.5)
    ),
    
    mainPanel(
        plotOutput('plot1')
    )
))
