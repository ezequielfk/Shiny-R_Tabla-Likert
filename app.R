# app.R
library(shiny)
library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)
library(shinyWidgets)   # pickerInput
library(htmltools)

ui <- fluidPage(
  titlePanel("Generador de Tablas: Escalas Likert"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        "file", 
        "Subir archivo (CSV o XLSX)", 
        accept = c(
          ".csv", 
          ".xlsx"
          )
        ),
      
      radioButtons(
        "csv_delim", 
        "Delimitador CSV (si aplica):",
        choices = c(
          "Auto" = "auto", 
          "Coma" = ",", 
          "Punto y coma" = ";", 
          "Tab" = "\t"),
        selected = "auto"),
      
      uiOutput("var_select_ui"),
      
      helpText("Selecciona las variables tipo Likert (orden en que aparecerán)."),
      
      textAreaInput(
        "labels_vars", 
        "Etiquetas para cada variable",
        placeholder = "Me gustaría usar...\nFue fácil...",
        rows = 5
        ),
      
      helpText("Escribir una etiqueta por línea, en el mismo orden en que se las seleccionó anteriormente."),
      
      uiOutput("rename_cats_ui"), 
      
      textInput(
        "titulo", 
        "Título de la Tabla", 
        value = NULL),
      
      textInput(
        "subtitulo", 
        "Subtítulo de la Tabla", 
        value = NULL),
      
      hr(),
      
      h4("Definición de categorías"),
      
      uiOutput("disagree_ui"),
      
      uiOutput("agree_ui"),
      
      hr(),
      
      textAreaInput(
        "nota_extra", 
        "Nota al pie", 
        "Nota: ", 
        rows = 3),
      
      helpText("En caso de no querer ingresar nota, dejar en blanco el campo de Nota al pie."),
      
      checkboxInput(
        "manualN", 
        "Ingresar manualmente el n", 
        value = FALSE),
      
      conditionalPanel(
        condition = "input.manualN == true",
        numericInput(
          "N_manual",
          "N (Total respondieron)", 
          value = NA, 
          min = 1)
        ),
      
      hr(),
      
      numericInput(
        "png_width", 
        "PNG width (px)", 
        value = 1600, 
        min = 400),
      
      numericInput(
        "png_height", 
        "PNG height (px)", 
        value = 900, 
        min = 200),
      
      actionButton(
        "run", 
        "Generar tabla"),
      
      br(), 
      
      br(),
      
      downloadButton(
        "download_png", 
        "Descargar PNG"),
      
      downloadButton(
        "download_html", 
        "Descargar HTML")
      ),
    
    mainPanel(
      gt_output("tabla_gt"),
      br(),
      verbatimTextOutput("debug")
    )
  )
)

server <- function(input, output, session) {
  
    safe_read <- function(
    path, 
    delim_choice = "auto"
    ) {
      
    ext <- tools::file_ext(path) |> 
      tolower()
    
    if (ext %in% c("xlsx", "xls")) {
      df <- tryCatch(read_excel(
        path, 
        .name_repair = "minimal"),
        error = function(e) NULL)
      
      return(as.data.frame(df))
    }
    
    try_csv <- function(d) {
      tryCatch(readr::read_delim(
        path, 
        delim = d, 
        guess_max = 20000, 
        show_col_types = FALSE),
        error = function(e) NULL)
    }
    
    if (delim_choice == "auto") {
      for (d in c(";", ",", "\t")) {
        df <- try_csv(d)
        if (!is.null(df)) return(as.data.frame(df))
      }
      return(NULL)
    } else {
      df <- try_csv(delim_choice)
      return(as.data.frame(df))
    }
  }
  
  # selector de variables
  output$var_select_ui <- renderUI({
    
    req(input$file)
    
    df <- safe_read(
      input$file$datapath, 
      input$csv_delim)
    
    validate(need(
      !is.null(df), 
      "No se pudo leer el archivo.")
      )
    
    selectizeInput(
      "vars", 
      "Variables Likert:", 
      choices = names(df), 
      multiple = TRUE)
  })
  
  # reactive que detecta categorias (disponible en todo el server)
  cats_reactive <- reactive({
    
    req(
      input$file, 
      input$vars)
    
    df <- safe_read(input$file$datapath, 
                    input$csv_delim)
    
    req(df)
    
    cats <- df %>%
      select(all_of(input$vars)) %>%
      unlist() %>%
      unique() %>%
      sort()
    
    cats <- cats[!is.na(cats)]
    
    # garantizar que sean character (para usar en input ids)
    
    as.character(cats)
    
  })
  
  # generar UI dinámico para renombrar categorias para acuerdo/desacuerdo
  observeEvent(cats_reactive(), {
    
    cats <- cats_reactive()
    
    output$rename_cats_ui <- renderUI({
      if (length(cats) == 0) return(NULL)
      tagList(
        h4("Renombrar categorías detectadas"),
        lapply(
          cats, 
          function(cat) {
          textInput(
            inputId = paste0(
              "rename_cat_",
              make.names(cat)
              ), # make.names para seguridad
            label   = paste0(
              "Etiqueta para categoría '", 
              cat, 
              "'"),
            value   = as.character(cat)
          )
        })
      )
    })
    

    output$disagree_ui <- renderUI({
      pickerInput(
        "disagree_cat", 
        "Categorías que se suman como DESACUERDO:",
        choices = cats, 
        multiple = TRUE, 
        selected = NULL,
        options = list(`actions-box` = TRUE))
    })
    
    output$agree_ui <- renderUI({
      pickerInput(
        "agree_cat", 
        "Categorías que se suman como ACUERDO:",
        choices = cats, multiple = TRUE, selected = NULL,
        options = list(`actions-box` = TRUE))
    })
    
  })
  
  
  # =============================
  # TABLA GT
  # =============================
  tabla_reactiva <- eventReactive(input$run, {
    
    req(
      input$file, 
      input$vars)
    
    df <- safe_read(
      input$file$datapath, 
      input$csv_delim)
    
    validate(
      need(!is.null(df), 
           "Archivo no válido.")
      )
    
    etiquetas <- strsplit(
      input$labels_vars, 
      "\n")[[1]] |> 
      trimws()
    
    validate(
      need(length(etiquetas) == length(input$vars),
           "Número de etiquetas ≠ número de variables")
      )
    
    names(etiquetas) <- input$vars
    
    ds_sub <- df |> 
      select(all_of(input$vars)
             )
    
    # volver a obtener cats dentro del reactive
    cats <- cats_reactive()
    
    # ---- nombres personalizados de categorías (recolectar desde inputs dinámicos) ----
    rename_cats <- c()
    if (length(cats) > 0) {
      for (cat in cats) {
        
        input_id <- paste0(
          "rename_cat_", 
          make.names(cat))
        
        entry <- input[[input_id]]
        
        if (!is.null(entry) && entry != "") {
          rename_cats[as.character(cat)] <- entry
        } else {
          rename_cats[as.character(cat)] <- as.character(cat)
        }
      }
    }

    # total personas respondieron (al menos una respuesta válida)
    N_auto <- ds_sub |> 
      mutate(row_valid = if_any(everything(), ~ !is.na(.))) |>
      filter(row_valid) |> 
      nrow()
    
    N_use <- if (isTRUE(input$manualN) && !is.na(input$N_manual)) input$N_manual else N_auto
    
    tbl_longer <- ds_sub |> 
      pivot_longer(cols = everything(),
                   names_to = "variable",
                   values_to = "categoria") |>
      filter(!is.na(categoria)) |>
      group_by(variable, categoria) |>
      summarise(
        n = n(), 
        .groups = "drop") |>
      group_by(variable) |>
      mutate(pct = n / sum(n) * 100) |>
      ungroup() |>
      mutate(categoria = as.character(categoria))
    
    # pivot_wider usando los valores originales como nombres de columna
    tbl1 <- tbl_longer |> 
      select(
        variable, 
        categoria, 
        pct) |> 
      pivot_wider(
        names_from = categoria, 
        values_from = pct) |> 
      mutate(variable = recode(variable, !!!etiquetas))
    
    # Guardar los nombres originales de columns (categorías) existentes en tbl1
    orig_cat_cols <- setdiff(
      names(tbl1), 
      "variable") # ej c("1","2","3","4","5")
   
    # Aplicar renombrado dinámico: cambiar solo las columnas que correspondan a categories detectadas
    new_colnames <- names(tbl1)
    for (i in seq_along(new_colnames)) {
      coln <- new_colnames[i]
      if (coln %in% names(rename_cats)) {
        new_colnames[i] <- rename_cats[[coln]]
      }
    }
    names(tbl1) <- new_colnames
    
    # ahora, calcular los nombres de columnas que corresponden a agree/disagree según selección del usuario
    # input$agree_cat and input$disagree_cat contienen los valores originales (ej "1","2")

    disagree_sel <- if (!is.null(input$disagree_cat)) as.character(input$disagree_cat) else character(0)
    
    agree_sel <- if (!is.null(input$agree_cat)) as.character(input$agree_cat) else character(0)
    
    disagree_cols <- intersect(
      names(tbl1), 
      as.character(rename_cats[disagree_sel]))
    
    agree_cols <- intersect(
      names(tbl1), 
      as.character(rename_cats[agree_sel]))
    
    # SUMAS usando las columnas renombradas (si no existen, sum será 0)
    sum_cols_safe <- function(tbl, cols) {
      if (length(cols) == 0) return(rep(0, nrow(tbl)))
      # c_across needs tidyselect; we'll use rowSums on selection
      mat <- tbl %>% 
        select(all_of(cols))
      # if any NA, replace with 0
      mat[is.na(mat)] <- 0
      rowSums(as.matrix(mat))
    }
    
    tbl1 <- tbl1 %>%
      mutate(
        `% SD` = sum_cols_safe(., disagree_cols),
        `% SA` = sum_cols_safe(., agree_cols)
      )
    
    # columnas a mostrar: usar las columnas renombradas correspondientes
    keep_cols <- c(
      "variable",
      disagree_cols,
      "% SD",
      agree_cols,
      "% SA"
    )
    keep_cols <- keep_cols[keep_cols %in% names(tbl1)]
    
    cols_to_hide <- setdiff(
      names(tbl1), 
      c(keep_cols, "plot")
      )
    
    # determinar columnas numéricas para formateo porcentual
    numeric_cols <- names(tbl1)[vapply(
      tbl1,
      is.numeric, 
      logical(1))]
    
    # excluimos posibles columnas no deseadas (si hubiera)
    numeric_cols <- setdiff(
      numeric_cols,
      0) 
    
    # generar gt
    gt_tab <- tbl1 |> 
      gt(rowname_col = "variable") |> 
      tab_header(
        title = md(
          paste0("**", 
                 input$titulo, 
                 "**")),
        subtitle = md(
          paste0("*",
                 input$subtitulo, 
                 "*"))
      )
    
    # Añadir nanoplot si existen columnas para ello
    plot_cols <- c(disagree_cols, agree_cols)
    plot_cols <- plot_cols[plot_cols %in% names(tbl1)]
    if (length(plot_cols) > 0) {
      gt_tab <- gt_tab |> 
        cols_nanoplot(
          columns = all_of(plot_cols),
          new_col_name = " ",
          plot_type = "bar",
          plot_height = "4em"
        )
    }
    
    # aplicar formato de porcentajes solo a columnas numéricas
    if (length(numeric_cols) > 0) {
      gt_tab <- gt_tab |> 
        fmt_percent(
          columns = all_of(numeric_cols),
          decimals = 1,
          scale_values = FALSE
        )
    }
    
    # spanners (usar cols que existan)
    sp_dis_cols <- c(disagree_cols, "% SD")
    sp_dis_cols <- sp_dis_cols[sp_dis_cols %in% names(tbl1)]
    sp_ag_cols <- c(agree_cols, "% SA")
    sp_ag_cols <- sp_ag_cols[sp_ag_cols %in% names(tbl1)]
    
    if (length(sp_dis_cols) > 0) {
      gt_tab <- gt_tab |> 
        tab_spanner(label = "En Desacuerdo", 
                    columns = all_of(sp_dis_cols)
                    )
    }
    if (length(sp_ag_cols) > 0) {
      gt_tab <- gt_tab |> 
        tab_spanner(label = "De Acuerdo", 
                    columns = all_of(sp_ag_cols)
                    )
    }
    
    gt_tab <- gt_tab |>
      cols_align(
        columns = everything(), 
        align = "center")
    
    # ocultar columnas no deseadas (las intermedias)
    if (length(cols_to_hide) > 0) {
      gt_tab <- gt_tab |> 
        cols_hide(columns = all_of(cols_to_hide))
    }
    
    gt_tab <- gt_tab |>
      tab_style(
        style = cell_fill(color = "#f0f0f0"),
        locations = cells_body(
          columns = c("% SD", "% SA")[c("% SD", "% SA") %in% names(tbl1)])
        ) |> 
      tab_footnote(
        footnote = paste0(
          "Total de estudiantes que respondieron = ", 
          N_use),
        locations = cells_title(groups = "title")
        ) |> 
      tab_footnote(
        footnote = "% SD = Porcentaje Total de Desacuerdo",
        locations = cells_column_labels ("% SD")
        ) |> 
      tab_footnote(
        footnote = "% SA = Porcentaje Total de Acuerdo",
        locations = cells_column_labels ("% SA")
        )
    
    # nota extra opcional
    if (!is.null(input$nota_extra) && nzchar(input$nota_extra)) {
      gt_tab <- gt_tab |>
        tab_source_note(
          source_note = input$nota_extra
        )
    }
    
    list(
      gt = gt_tab, 
      tbl = tbl1, 
      N = N_use)
  })
  
  
  output$tabla_gt <- render_gt({
    tabla_reactiva()$gt
  })
  
  output$download_png <- downloadHandler(
    filename = function() paste0("tabla_", Sys.Date(), ".png"),
    content = function(file) {
      gtsave(
        tabla_reactiva()$gt, 
        file = file,
        vwidth = input$png_width, 
        vheight = input$png_height)
    }
  )
  
  output$download_html <- downloadHandler(
    filename = function() paste0("tabla_", Sys.Date(), ".html"),
    content = function(file) {
      gtsave(
        tabla_reactiva()$gt, 
        file = file)
    }
  )
}

shinyApp(ui, server)
