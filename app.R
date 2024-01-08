library(shiny)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(DT)
library(dplyr)
library(shinydashboard)

# Data CTRs
CTR_data <- read.csv(".../CTR.csv", sep = ";")

# Melt the data for visualization
CTR_data_long <- reshape2::melt(CTR_data, id.vars = "Day", variable.name = "Ad_Placement", value.name = "CTR")

# Summary of ANOVA results
hasil_anova <- CTR_data_long %>%
  group_by(Ad_Placement) %>%
  summarise(rata_rata_CTR = mean(CTR))

# Message for ANOVA results
pesan_anova <- "Hasil ANOVA menunjukkan bahwa rata-rata CTR pada 'Center Page' lebih tinggi dibandingkan dengan 'Left Sidebar' dan 'Right Sidebar'. Hal ini dapat menunjukkan bahwa iklan yang ditempatkan di 'Center Page' cenderung lebih efektif dalam menarik perhatian pengguna untuk melakukan klik."
pesan_analisis <- "Hasil ANOVA yang memiliki rata-rata CTR tertinggi dibandingkan dengan rata-rata lainnya menunjukkan bahwa iklan yang ditempatkan pada bagian itu lebih efektif dalam menarik perhatian pengguna untuk melakukan klik."
pesan_input <- "Pastikan data yang Anda masukkan telah memenuhi asumsi dasar statistik seperti Normalitas, Homogenitas, dan Independensi. Perhatikan bahwa semakin banyak data yang dimasukkan, performa aplikasi bisa terpengaruh."

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard CTR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "home"),
      menuItem("COBA", tabName = "coba")
    )
  ),
  dashboardBody(
    # Custom CSS for data analysis section
    tags$style(HTML("
      /* Change font size for data analysis section */
      .analisis-data {
        font-size: 18px;
        text-align: justify;
        margin-top: 20px; 
      }
     .input-data {
    font-size: 24px;
    text-align: center;
    border: 1px solid #ccc; /* Menggunakan border dengan ketebalan 1px dan warna abu-abu */
    padding: 10px; /* Padding agar konten tidak terlalu dekat dengan border */
    font-weight: bold;
  }
    ")),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          column(12, DTOutput("data_table"))
        ),
        fluidRow(
          column(6, plotOutput("barPlot")),
          column(
            6,
            h3("Analisis Data dengan ANOVA:"),
            tableOutput("anova_table"),
            div(
              style = "margin-top: 10px; margin-bottom: 10px; text-align: center;",
              tags$hr(style = "width:50%;")
            ),
            p(class = "analisis-data", pesan_anova)
          )
        )
      ),
      tabItem(
        tabName = "coba",
        p(class = "input-data", pesan_input),
        fluidRow(
          column(
            12,
            h3("Input Data CTR")),
          column(
            2,
            numericInput("left_sidebar_ctr", "Left Sidebar CTR", 0, min = 0, max = 10, step = 0.1)
            ),
          column(
            2,
            numericInput("center_page_ctr", "Center Page CTR", 0, min = 0, max = 10, step = 0.1)
            ),
          column(
            2,
            numericInput("right_sidebar_ctr", "Right Sidebar CTR", 0, min = 0, max = 10, step = 0.1),
          )
        ),
        fluidRow(
          column(
            1,
            actionButton("add_data", "Input Data")
          ),
          column(
            1,
            uiOutput("reset_button")  # Display reset button
          ),
          column(
            4,
            uiOutput("analyze_button")  # Display analyze button
          )
        ),
        fluidRow(
        column(
          6,
          p(class = "analisis-data", pesan_analisis),
          h3("Hasil Analisis Data dengan ANOVA:"),
          tableOutput("hasil_analisis"),
          div(
            style = "margin-top: 10px; margin-bottom: 10px; text-align: center;",
            tags$hr(style = "width:50%;")
          )
        ),
        column(
          6,
          DTOutput("added_data_table")
        )
      ),
        fluidRow(
          column(
            12,
            plotOutput("analysis_plot") # Display analysis plot
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  added_data <- reactiveVal(data.frame()) # Initializing empty data frame to store added data
  
  observeEvent(input$add_data, {
    reference_length <- length(input$left_sidebar_ctr) # Gunakan panjang dari left_sidebar_ctr sebagai referensi
    
    new_data <- data.frame(
      Hari = seq(nrow(added_data()) + 1, nrow(added_data()) + reference_length),
      Kiri = input$left_sidebar_ctr,
      Tengah = input$center_page_ctr,
      Kanan = input$right_sidebar_ctr
    )
    
    # Melt the new data for visualization
    added_data(rbind(added_data(), new_data))
  })
  
  output$added_data_table <- renderDT({
    added_data()
  })
  
  # Update and render analysis when "Analisis Data" button is clicked
  observeEvent(input$analyze_data, {
    if (nrow(added_data()) > 0) {
      new_data_long <- reshape2::melt(added_data(), id.vars = "Hari", variable.name = "Tempat", value.name = "Nilai")
      
      # Summary of ANOVA results
      hasil_anova <- new_data_long %>%
        group_by(Tempat) %>%
        summarise(rata_rata_CTR = mean(Nilai))
      
      output$hasil_analisis <- renderTable({
        hasil_anova
      })
      
      output$pesan_analisis <- renderText({
        pesan_analisis 
      })
        
      output$analysis_plot <- renderPlot({
        gg <- ggplot(new_data_long, aes(x = factor(Hari), y = Nilai, fill = Tempat)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.7) +
          labs(title = "Perbandingan CTR Berdasarkan Lokasi Penempatan Iklan",
               x = "Hari", y = "CTR") +
          theme_economist() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(face = "bold", size = 16),
                legend.title = element_text(face = "italic", size = 12))
        gg
      })
      
    }
  })
  
  output$added_data_table <- renderDT({
    datatable(
      added_data(),
      options = list(pageLength = 5)
    )
  })
  
  output$barPlot <- renderPlot({
    ggplot(CTR_data_long, aes(x = factor(Day), y = CTR, fill = Ad_Placement)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = "Perbandingan CTR Berdasarkan Lokasi Penempatan Iklan",
           x = "Hari", y = "CTR") +
      theme_economist() +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16),
            legend.title = element_text(face = "italic", size = 12))
  })
  
  output$data_table <- renderDT({
    datatable(CTR_data, options = list(pageLength = 5))
  })
  
  output$anova_table <- renderTable({
    hasil_anova
  })
  
  observeEvent(input$reset_data, {
    added_data(data.frame()) # Reset added data
  })
  
  output$reset_button <- renderUI({
    actionButton("reset_data", "Reset Data")
  })
  
  output$analyze_button <- renderUI({
    actionButton("analyze_data", "Analisis Data")
  })
}

shinyApp(ui = ui, server = server)
