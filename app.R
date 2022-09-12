# shiny-valkompass

source("functions.R")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  fixedRow(
    column(12,
      style = "margin:15px;",
      fluidRow(
        style = "margin-bottom:10px;",
        span(tags$h4(strong("Valkompass PCA"), style = "margin:0px;"), style = "vertical-align:middle;display:inline-block;")
      ),
      fixedRow(
        column(5,
          class = "box-left",
          rc(div(class="scroll-panel",
            helpText(class="help-text","1=Disagree, 4=Agree. All questions are mandatory. Click 'Draw' to generate plot."),
            hr(),
            sliderInput("q1",min=1,max=4,step=1,value=3,label=a("Profit distribution for independent schools must be prohibited",href="https://valkompass.svt.se/2022/riksdag/fraga/vinstutdelning-for-friskolor-ska-forbjudas",target="_blank")),
            sliderInput("q2",min=1,max=4,step=1,value=3,label=a("Nuclear power must be expanded.",href="https://valkompass.svt.se/2022/riksdag/fraga/karnkraften-ska-byggas-ut",target="_blank")),
            sliderInput("q3",min=1,max=4,step=1,value=3,label=a("The police must be able to establish visitation zones in crime-prone areas.",href="https://valkompass.svt.se/2022/riksdag/fraga/polisen-ska-kunna-uppratta-visitationszoner-i-brottsutsatta-omraden",target="_blank")),
            sliderInput("q4",min=1,max=4,step=1,value=3,label=a("The RUT deduction must be abolished.",href="https://valkompass.svt.se/2022/riksdag/fraga/rut-avdraget-ska-avskaffas",target="_blank")),
            sliderInput("q5",min=1,max=4,step=1,value=3,label=a("Sweden must reduce international aid.",href="https://valkompass.svt.se/2022/riksdag/fraga/sverige-ska-minska-det-internationella-bistandet",target="_blank")),
            sliderInput("q6",min=1,max=4,step=1,value=3,label=a("The royal family should be allocated less money.",href="https://valkompass.svt.se/2022/riksdag/fraga/kungahuset-ska-tilldelas-mindre-pengar",target="_blank")),
            sliderInput("q7",min=1,max=4,step=1,value=3,label=a("The tax on petrol and diesel must be reduced.",href="https://valkompass.svt.se/2022/riksdag/fraga/skatten-pa-bensin-och-diesel-ska-sankas",target="_blank")),
            sliderInput("q8",min=1,max=4,step=1,value=3,label=a("The tax on properties must be increased.",href="https://valkompass.svt.se/2022/riksdag/fraga/skatten-pa-fastigheter-ska-hojas",target="_blank")),
            sliderInput("q9",min=1,max=4,step=1,value=3,label=a("Culture must be largely financed privately.",href="https://valkompass.svt.se/2022/riksdag/fraga/kulturen-ska-till-storre-del-finansieras-av-privata-aktorer",target="_blank")),
            sliderInput("q10",min=1,max=4,step=1,value=3,label=a("It will become more difficult for municipalities to stop wind power construction.",href="https://valkompass.svt.se/2022/riksdag/fraga/det-ska-bli-svarare-for-kommuner-att-stoppa-vindkraftsbyggen",target="_blank")),
            sliderInput("q11",min=1,max=4,step=1,value=3,label=a("It should be forbidden to beg in Sweden.",href="https://valkompass.svt.se/2022/riksdag/fraga/det-ska-vara-forbjudet-att-tigga-i-sverige",target="_blank")),
            sliderInput("q12",min=1,max=4,step=1,value=3,label=a("A third legal gender must be introduced for those who identify as neither female nor male.",href="https://valkompass.svt.se/2022/riksdag/fraga/ett-tredje-juridiskt-kon-ska-inforas-for-de-som-varken-identifierar-sig-som-kvinna-eller-man",target="_blank")),
            sliderInput("q13",min=1,max=4,step=1,value=3,label=a("Parents must decide for themselves how they distribute the parental leave.",href="https://valkompass.svt.se/2022/riksdag/fraga/foraldrar-ska-sjalva-fa-bestamma-hur-de-fordelar-foraldraledigheten",target="_blank")),
            sliderInput("q14",min=1,max=4,step=1,value=3,label=a("Private online doctors will no longer be financed by tax funds.",href="https://valkompass.svt.se/2022/riksdag/fraga/privata-natlakare-ska-inte-langre-finansieras-av-skattemedel",target="_blank")),
            sliderInput("q15",min=1,max=4,step=1,value=3,label=a("Permanent residence permits must be the main rule in migration policy.",href="https://valkompass.svt.se/2022/riksdag/fraga/permanenta-uppehallstillstand-ska-vara-huvudregel-i-migrationspolitiken",target="_blank")),
            sliderInput("q16",min=1,max=4,step=1,value=3,label=a("Sweden must become a member of NATO.",href="https://valkompass.svt.se/2022/riksdag/fraga/sverige-ska-vara-medlem-i-nato",target="_blank")),
            sliderInput("q17",min=1,max=4,step=1,value=3,label=a("The temporary increase in the A-kassa must be made permanent.",href="https://valkompass.svt.se/2022/riksdag/fraga/den-tillfalliga-hojningen-i-a-kassan-ska-permanentas",target="_blank")),
            sliderInput("q18",min=1,max=4,step=1,value=3,label=a("Beach protection in rural municipalities must be abolished.",href="https://valkompass.svt.se/2022/riksdag/fraga/strandskyddet-i-landsbygdskommuner-ska-avskaffas",target="_blank")),
            sliderInput("q19",min=1,max=4,step=1,value=3,label=a("The rearmament of the Swedish defense should be financed with higher taxes.",href="https://valkompass.svt.se/2022/riksdag/fraga/upprustningen-av-svenska-forsvaret-ska-finansieras-med-hojd-skatt",target="_blank")),
            sliderInput("q20",min=1,max=4,step=1,value=3,label=a("Public service - TV and radio - must have a narrower mission.",href="https://valkompass.svt.se/2022/riksdag/fraga/public-service-tv-och-radio-ska-ha-ett-smalare-uppdrag",target="_blank")),
            sliderInput("q21",min=1,max=4,step=1,value=3,label=a("New sales of petrol and diesel cars should be banned from 2024 onwards.",href="https://valkompass.svt.se/2022/riksdag/fraga/nyforsaljning-av-bensin-och-dieselbilar-ska-forbjudas-fran-och-med-ar-2025",target="_blank")),
            sliderInput("q22",min=1,max=4,step=1,value=3,label=a("Grades must be compulsory from year 4.",href="https://valkompass.svt.se/2022/riksdag/fraga/betyg-ska-vara-obligatoriskt-fran-arskurs-4",target="_blank")),
            sliderInput("q23",min=1,max=4,step=1,value=3,label=a("More tax income must be redistributed from rich to poor municipalities.",href="https://valkompass.svt.se/2022/riksdag/fraga/mer-skatteinkomster-ska-omfordelas-fran-rika-till-fattiga-kommuner",target="_blank")),
            sliderInput("q24",min=1,max=4,step=1,value=3,label=a("The purchase of sexual services shall be punished with at least imprisonment.",href="https://valkompass.svt.se/2022/riksdag/fraga/kop-av-sexuella-tjanster-ska-straffas-med-minst-fangelse",target="_blank")),
            sliderInput("q25",min=1,max=4,step=1,value=3,label=a("Market rents must be introduced on new tenancies.",href="https://valkompass.svt.se/2022/riksdag/fraga/marknadshyror-ska-inforas-pa-nya-hyresratter",target="_blank")),
            sliderInput("q26",min=1,max=4,step=1,value=3,label=a("Sweden must dismantle the plans for high-speed trains.",href="https://valkompass.svt.se/2022/riksdag/fraga/sverige-ska-avveckla-planerna-pa-hoghastighetstag",target="_blank")),
            sliderInput("q27",min=1,max=4,step=1,value=3,label=a("The state must take over responsibility for healthcare.",href="https://valkompass.svt.se/2022/riksdag/fraga/staten-ska-ta-over-ansvaret-for-sjukvarden",target="_blank")),
            sliderInput("q28",min=1,max=4,step=1,value=3,label=a("The grace day must be abolished.",href="https://valkompass.svt.se/2022/riksdag/fraga/karensdagen-ska-avskaffas",target="_blank")),
            sliderInput("q29",min=1,max=4,step=1,value=3,label=a("It should be easier for employers to dismiss employees.",href="https://valkompass.svt.se/2022/riksdag/fraga/det-ska-bli-enklare-for-arbetsgivare-att-saga-upp-anstallda",target="_blank")),
            sliderInput("q30",min=1,max=4,step=1,value=3,label=a("It should be easier to get permission to open mines in Sweden.",href="https://valkompass.svt.se/2022/riksdag/fraga/det-ska-vara-enklare-att-fa-tillstand-att-oppna-gruvor-i-sverige",target="_blank"))
          )),
          hr(),
          fluidRow(
            column(6,
              style = "margin:10px 0;min-width:fit-content;",
              actionButton("btn_update", "Draw"),
              downloadButton("btn_download", "Download")
            ),
            column(6,
              style = "margin:10px 0;",
              actionButton("btn_reset", "Reset", class = "btn-sm btn-warning")
            )
          ),
          rc(div(
            class = "help-note",
            paste0(format(Sys.time(), "%Y"), " Roy Francis • Version: ", fn_version()),
            HTML("• <a class='help-note' href='https://github.com/royfrancis/shiny-valkompass\' target='_blank'><i class='fab fa-github'></i></a>")
          ))
        ),
        column(
          7,
          div(
            class = "img-output",
            imageOutput("out_plot", width = "auto", height = "auto")
          )
        )
      )
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## get temporary directory
  store <- reactiveValues(epath = fn_dir(session), sign = NULL)
  
  ## FN: fn_data -------------------------------------------------------------
  ## function to get user data
  
  fn_data <- eventReactive(input$btn_update, {
    x <- sapply(paste0("input$q",1:30),function(x ) eval(parse(text=x)))
    x <- data.frame(U=unlist(x))
    rownames(x) <- paste0("q",1:30)
    return(as.data.frame(t(x)))
  })
  
  ## FN: fn_data -------------------------------------------------------------
  ## function to get user data
  
  fn_pca <- reactive({
    
    pca_df1 <- as.data.frame(p2018$x)
    pca_df1$id <- rownames(pca_df1)
    pca_df1$year <- 2018
    
    pca_df2 <- rbind(as.data.frame(p2022$x),
                     scale(fn_data(), p2022$center, p2022$scale) %*% p2022$rotation)
    pca_df2$id <- rownames(pca_df2)
    pca_df2$year <- 2022
    
    return(rbind(pca_df1,pca_df2) %>% mutate(year=as.factor(year)))
  })
  
  ## OUT: out_plot -------------------------------------------------------------
  ## plots figure
  
  output$out_plot <- renderImage(
    {
      pca_df <- fn_pca()
      progress_plot <- shiny::Progress$new()
      progress_plot$set(message = "Generating figure...", value = 45)
      
      plot_pca(pca_df,store$epath)
      
      progress_plot$set(message = "Completed.", value = 100)
      progress_plot$close()
      
      scaling <- 2.2
      return(list(
        src = file.path(store$epath, "valkompass-pca.png"), contentType = "image/png",
        width = round(5.5 * 96 * scaling, 0),
        height = round(4 * 96 * scaling, 0),
        alt = "image"
      ))
    },
    deleteFile = TRUE
  )
  
  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images
  
  fn_download <- function() {
    pca_df <- fn_pca()
    plot_pca(pca_df,store$epath)
  }
  
  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file
  
  output$btn_download <- downloadHandler(
    filename = "valkompass-pca.png",
    content = function(file) {
      progress <- shiny::Progress$new()
      progress$set(message = "Generating figure...", value = 45)
      fn_download()
      
      progress$set(message = "Downloading file...", value = 90)
      file.copy(file.path(store$epath, "valkompass-pca.png"), file, overwrite = T)
      
      progress$set(message = "Completed.", value = 100)
      progress$close()
    }
  )
  
  # OBS: btn_reset -------------------------------------------------------------
  # observer for reset
  
  observeEvent(input$btn_reset, {
    updateSliderInput(session,inputId="q1",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q2",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q3",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q4",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q5",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q6",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q7",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q8",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q9",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q10",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q11",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q12",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q13",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q14",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q15",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q16",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q17",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q18",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q19",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q20",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q21",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q22",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q23",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q24",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q25",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q26",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q27",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q28",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q29",min=1,max=4,step=1,value=3)
    updateSliderInput(session,"q30",min=1,max=4,step=1,value=3)
  })
  
  ## OSE -----------------------------------------------------------------------
  ## delete user directory when session ends
  
  session$onSessionEnded(function() {
    cat(paste0("Removing user directory ", isolate(store$epath), " ...\n"))
    if (dir.exists(isolate(store$epath))) {
      unlink(isolate(store$epath), recursive = TRUE)
    }
  })
  
}

shinyApp(ui = ui, server = server)
