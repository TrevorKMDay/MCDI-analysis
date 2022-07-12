#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/")

library(shiny)
library(tidyverse)
library(viridis)
library(umx)
library(plotly)

source("mcdi-setup.R")

ws_n <- read_data("Wordbank/WS-scored.rds")$n %>%
    select(data_id, age, LEXICAL, SYNTAX)

college_ed <- c("College", "Some Graduate", "Graduate")

ws_demo <- read_data("Wordbank/Ws-demographics.rds") %>%
    mutate(
        male        = if_else(!is.na(sex),
                              if_else(sex == "Male", 1, -1),
                              NA_real_),
        mom_college = if_else(!is.na(mom_ed),
                              if_else(mom_ed %in% college_ed, 1, -1),
                              NA_real_),
        first_born  = if_else(!is.na(birth_order),
                              if_else(birth_order == "False", 1, -1),
                              NA_real_)
    ) %>%
    select(data_id, age, male, mom_college, first_born)

ws_2f <- read_data("Wordbank/WS-FA_scores.rds")$scores %>%
    as_tibble() %>%
    rename(LEXICAL = MR1, SYNTAX = MR2)

WS_2fd <- bind_cols(ws_demo, ws_2f) %>%
    mutate(
        model = "fa"
    )

WS_nd <- left_join(ws_demo, ws_n) %>%
    mutate(
        model = "raw"
    )

WS_all <- bind_rows(WS_nd, WS_2fd) %>%
    mutate(
        age2 = age * age
    )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wordbank data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("model", "Model:",
                         c("Raw" = "raw", "FA" = "fa"),
                         inline = TRUE),

            checkboxGroupInput("covariates", "Covariates:",
                               c("Age" = "age", "Age*age" = "age2",
                                 "Sex" = "male",
                                 "Mom college grad" = "mom_college",
                                 "First-born" = "first_born"),
                               selected = c("age", "age2")),

            sliderInput("age_cuts", "Age cuts",
                        2, 7, 4)

        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
            #     column(width = 6, plotOutput("rawPlot")),
                column(width = 8, plotlyOutput("plotly"))
            ),
            # fluidRow(
            #     column(width = 6, plotOutput("rPlot")),
            #     column(width = 6, plotOutput("sPlot"))
            # )
        )
    )
)

colors = c("yellow2", "grey75", "darkblue")

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Residualize covariates
    covariates <- reactive({input$covariates})

    # Residualize the set of scores against the given covariates
    current_resid <- reactive({

        WS_all %>%
            umx_residualize(c("SYNTAX", "LEXICAL"),
                            covs = c(covariates()),
                            data = .) %>%
            mutate(
                age_bin = cut(age, input$age_cuts)
            )

        })

    # Residualize syn against lex only
    syn_over_lex <- reactive({

        current_resid() %>%
            group_by(model) %>%
            umx_residualize("SYNTAX", "LEXICAL", data = .) %>%
            mutate(
                syn_over_lex = scale(SYNTAX),
                frame = "syn_over_lex"
            )

    })

    WS_all_reactive <- reactive({

        WS_all %>%
            mutate(
                age_bin = cut(age, input$age_cuts),
                frame = "raw"
            ) %>%
            left_join(select(syn_over_lex(), data_id, age, syn_over_lex)) %>%
            filter(
                !is.na(syn_over_lex)
            )

    })

    current_resid2 <- reactive({

        current_resid() %>%
            left_join(select(syn_over_lex(), data_id, age, syn_over_lex)) %>%
            filter(
                !is.na(syn_over_lex)
            ) %>%
            mutate(
                frame = "residualized"
            )

    })

    three_frames <- reactive({
        bind_rows(WS_all_reactive(), current_resid2(), syn_over_lex()) %>%
            group_by(model, frame) %>%
            mutate(
                across(c(LEXICAL, SYNTAX), scale)
            )
    })


    p <- reactive({

        ggplot(three_frames(),
               aes(x = LEXICAL, y = SYNTAX, color = syn_over_lex)) +
            geom_point(aes(frame = frame), alpha = 0.2) +
            geom_smooth(method = "lm", aes(frame = frame, linetype = age_bin),
                        se = FALSE) +
            scale_color_viridis() +
            facet_wrap(vars(model)) +
            theme_minimal()

    })

    output$plotly <- renderPlotly({

        ggplotly(p())

    })

    # output$rawPlot <- renderPlot({
    #
    #     ggplot(filter(WS_all_reactive(), model == input$model),
    #            aes(x = LEXICAL, y = SYNTAX, color = syn_over_lex)) +
    #         geom_point(alpha = 0.25) +
    #         geom_smooth(method = "lm", aes(linetype = age_bin)) +
    #         scale_color_gradient2(high = colors[1], mid = colors[2],
    #                               low = colors[3]) +
    #         theme_minimal() +
    #         labs(title = "Raw data")
    #
    # })
    #
    # output$rPlot <- renderPlot({
    #
    #     ggplot(current_resid2(),
    #            aes(x = LEXICAL, y = SYNTAX, color = syn_over_lex)) +
    #         geom_point(alpha = 0.25) +
    #         geom_smooth(method = "lm", aes(linetype = age_bin)) +
    #         scale_color_gradient2(high = colors[1], mid = colors[2],
    #                               low = colors[3]) +
    #         theme_minimal() +
    #         labs(title = "Residualized scores")
    #
    # })
    #
    # output$sPlot <- renderPlot({
    #
    #     ggplot(syn_over_lex(),
    #            aes(x = LEXICAL, y = syn_over_lex, color = syn_over_lex)) +
    #         geom_point(alpha = 0.25) +
    #         geom_smooth(method = "lm", aes(linetype = age_bin)) +
    #         scale_color_gradient2(high = colors[1], mid = colors[2],
    #                               low = colors[3]) +
    #         theme_minimal() +
    #         labs(title = "Residualized syn. against lex.")
    #
    # })

}

# Run the application
shinyApp(ui = ui, server = server)
