library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(DT)
library(plotly)
library(readr)
library(stringr)
library(rsconnect)

num_locale <- locale(
  decimal_mark  = ",",
  grouping_mark = ".",
  encoding      = "UTF-8"
)

caminho_tempo     <- "dados/2024TemposAtracacao.txt"
caminho_atracacao <- "dados/2024Atracacao.txt"
caminho_carga_rio <- "dados/2024Carga_Rio.txt"

# ---- Tempos de atracação ----
tempo_raw <- read_delim(
  caminho_tempo,
  delim = ";",
  locale = num_locale,
  show_col_types = FALSE,
  trim_ws = TRUE
)

tempo <- tempo_raw %>%
  rename(
    id_atracacao         = IDAtracacao,
    t_espera_atracacao   = TEsperaAtracacao,
    t_espera_inicio      = TEsperaInicioOp,
    t_operacao           = TOperacao,
    t_espera_desatrac    = TEsperaDesatracacao,
    t_atracado           = TAtracado,
    t_estadia            = TEstadia
  ) %>%
  mutate(
    across(
      c(t_espera_atracacao,
        t_espera_inicio,
        t_operacao,
        t_espera_desatrac,
        t_atracado,
        t_estadia),
      as.numeric
    )
  )
atr_raw <- read_delim(
  caminho_atracacao,
  delim = ";",
  locale = num_locale,
  show_col_types = FALSE,
  trim_ws = TRUE
)

atracacao <- atr_raw %>%
  rename(
    id_atracacao        = IDAtracacao,
    cdtup               = CDTUP,
    id_berco            = IDBerco,
    berco               = `Berço`,
    porto               = `Porto Atracação`,
    coordenadas         = Coordenadas,
    apelido_inst        = `Apelido Instalação Portuária`,
    complexo_portuario  = `Complexo Portuário`,
    tipo_autoridade     = `Tipo da Autoridade Portuária`,
    data_atracacao      = `Data Atracação`,
    data_chegada        = `Data Chegada`,
    data_desatracacao   = `Data Desatracação`,
    data_inicio_op      = `Data Início Operação`,
    data_termino_op     = `Data Término Operação`,
    ano                 = Ano,
    mes                 = Mes,
    tipo_operacao       = `Tipo de Operação`,
    tipo_navegacao      = `Tipo de Navegação da Atracação`,
    nacionalidade_arm   = `Nacionalidade do Armador`,
    flag_mc_operacao    = FlagMCOperacaoAtracacao,
    terminal            = Terminal,
    municipio           = Município,
    uf                  = UF,
    sg_uf               = SGUF,
    regiao_geo          = `Região Geográfica`,
    regiao_hidro        = `Região Hidrográfica`,
    instalacao_rio      = `Instalação Portuária em Rio`,
    n_capitania         = `Nº da Capitania`,
    n_imo               = `Nº do IMO`
  ) %>%
  mutate(
    ano = as.integer(ano)
  ) %>%
  separate(
    col    = coordenadas,
    into   = c("lon", "lat"),
    sep    = ",",
    remove = FALSE,
    fill   = "right",
    extra  = "drop"
  ) %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )

carga_rio_raw <- read_delim(
  caminho_carga_rio,
  delim = ";",
  locale = num_locale,
  show_col_types = FALSE,
  trim_ws = TRUE
)

carga_rio <- carga_rio_raw %>%
  rename(
    id_carga          = IDCarga,
    rio               = Rio,
    valor_movimentado = ValorMovimentado
  ) %>%
  mutate(
    valor_movimentado = as.numeric(valor_movimentado)
  )

dados_ops <- tempo %>%
  inner_join(atracacao, by = "id_atracacao")

ordena_mes <- function(x) {
  meses <- c("jan","fev","mar","abr","mai","jun",
             "jul","ago","set","out","nov","dez")
  if (is.numeric(x)) {
    factor(x, levels = 1:12, ordered = TRUE)
  } else {
    x2 <- str_sub(str_to_lower(x), 1, 3)
    factor(x2, levels = meses, ordered = TRUE)
  }
}

dados_ops <- dados_ops %>%
  mutate(
    mes_ord = ordena_mes(mes)
  )

anos_disp  <- sort(unique(dados_ops$ano))
ufs_disp   <- sort(na.omit(unique(dados_ops$uf)))
tipos_disp <- sort(na.omit(unique(dados_ops$tipo_navegacao)))


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Operações Portuárias ANTAQ – 2024"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão geral",         tabName = "visao",   icon = icon("tachometer-alt")),
      menuItem("Operações x Estadia", tabName = "ops",     icon = icon("project-diagram")),
      menuItem("Mapa",                tabName = "mapa",    icon = icon("globe-americas")),
      menuItem("Carga por rio",       tabName = "carga",   icon = icon("water")),
      menuItem("Tabela",              tabName = "tabela",  icon = icon("table"))
    ),
    br(),
    tags$h4("Filtros"),
    selectInput(
      "filtro_ano", "Ano:",
      choices  = anos_disp,
      selected = max(anos_disp)
    ),
    selectInput(
      "filtro_uf", "UF do porto:",
      choices  = c("Todas" = "Todas", ufs_disp),
      selected = "Todas"
    ),
    selectInput(
      "filtro_tipo_nav", "Tipo de navegação:",
      choices  = c("Todos" = "Todos", tipos_disp),
      selected = "Todos"
    ),
    helpText("Filtros afetam todos os gráficos, mapa e tabela.")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "visao",
        fluidRow(
          box(
            title = "Resumo do recorte selecionado",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            p("Indicadores calculados a partir dos tempos de operação e estadia das atracações.")
          )
        ),
        fluidRow(
          valueBoxOutput("box_atracacoes", width = 4),
          valueBoxOutput("box_tempo_op",   width = 4),
          valueBoxOutput("box_tempo_est",  width = 4)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Evolução mensal do tempo médio de operação",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("grafico_mensal", height = "350px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Como interpretar este dashboard e principais conclusões",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("texto_conclusoes")
          )
        )
      ),
      tabItem(
        tabName = "ops",
        fluidRow(
          box(
            width = 12,
            title = "Relação entre tempo médio de operação e estadia por porto",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("grafico_dispersao", height = "450px")
          )
        )
      ),
      tabItem(
        tabName = "mapa",
        fluidRow(
          box(
            width = 12,
            title = "Mapa das instalações portuárias",
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("mapa_portos", height = "500px")
          )
        )
      ),
      tabItem(
        tabName = "carga",
        fluidRow(
          box(
            width = 12,
            title = "Carga movimentada por rio (sem filtro)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("grafico_carga_rio", height = "450px")
          )
        )
      ),
      # ------------------------------------------------------
      # TABELA
      # ------------------------------------------------------
      tabItem(
        tabName = "tabela",
        fluidRow(
          box(
            width = 12,
            title = "Tabela dinâmica – resumo por porto / mês / tipo de navegação",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("tabela_resumo")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ----------------------------
  # Reativos filtrados
  # ----------------------------
  dados_filtrados <- reactive({
    df <- dados_ops
    
    if (!is.null(input$filtro_ano)) {
      df <- df %>% filter(ano == input$filtro_ano)
    }
    if (!is.null(input$filtro_uf) && input$filtro_uf != "Todas") {
      df <- df %>% filter(uf == input$filtro_uf)
    }
    if (!is.null(input$filtro_tipo_nav) && input$filtro_tipo_nav != "Todos") {
      df <- df %>% filter(tipo_navegacao == input$filtro_tipo_nav)
    }
    
    df
  })
  
  output$box_atracacoes <- renderValueBox({
    df <- dados_filtrados()
    if (nrow(df) == 0) {
      return(valueBox("0", "Atracações no recorte", icon = icon("exclamation-triangle"), color = "yellow"))
    }
    total_atrac <- n_distinct(df$id_atracacao)
    valueBox(
      format(total_atrac, big.mark = ".", decimal.mark = ","),
      "Atracações no recorte selecionado",
      icon  = icon("ship"),
      color = "blue"
    )
  })
  
  output$box_tempo_op <- renderValueBox({
    df <- dados_filtrados()
    if (nrow(df) == 0) {
      return(valueBox("-", "Tempo médio de operação (dias)", icon = icon("clock"), color = "yellow"))
    }
    tempo_medio_op <- mean(df$t_operacao, na.rm = TRUE)
    valueBox(
      round(tempo_medio_op, 2),
      "Tempo médio de operação (dias)",
      icon  = icon("clock"),
      color = "green"
    )
  })
  
  output$box_tempo_est <- renderValueBox({
    df <- dados_filtrados()
    if (nrow(df) == 0) {
      return(valueBox("-", "Tempo médio de estadia (dias)", icon = icon("calendar"), color = "yellow"))
    }
    tempo_medio_est <- mean(df$t_estadia, na.rm = TRUE)
    valueBox(
      round(tempo_medio_est, 2),
      "Tempo médio de estadia (dias)",
      icon  = icon("calendar"),
      color = "purple"
    )
  })
  output$texto_conclusoes <- renderUI({
    HTML("
      <h4>Introdução</h4>
      <p>
      Este dashboard utiliza dados da ANTAQ para o ano de 2024, integrando informações de tempos de atracação,
      registros de atracações (porto, UF, tipo de navegação) e carga movimentada por rio. 
      O objetivo é oferecer uma visão analítica do desempenho do sistema portuário brasileiro e da navegação interior.
      </p>

      <h4>Indicadores principais</h4>
      <p>
      Considerando o recorte completo de 2024 (todas as UFs e tipos de navegação), o painel apresenta aproximadamente:
      </p>
      <ul>
        <li>106.839 atracações registradas, indicando elevado volume de operações portuárias no período.</li>
        <li>34,1 dias de tempo médio de operação, isto é, o tempo médio em que os navios permanecem 
        efetivamente carregando ou descarregando.</li>
        <li>60,67 dias de tempo médio de estadia, correspondendo ao tempo total entre atracação e desatracação.</li>
      </ul>
      <p>
      O fato de a estadia média (60,67 dias) ser bem maior que o tempo médio de operação (34,1 dias) sugere 
      que uma parcela relevante do tempo dos navios é gasta em espera ou em períodos sem operação efetiva, 
      o que pode indicar gargalos ou ineficiências em alguns portos.
      </p>
      <p>
      Ao alterar os filtros de UF e tipo de navegação, esses indicadores são recalculados, 
      permitindo comparar o desempenho entre regiões ou tipos de tráfego (por exemplo, cabotagem x longo curso).
      </p>

      <h4>Evolução mensal do tempo médio de operação</h4>
      <p>
      O gráfico de linha mostra a variação do tempo médio de operação ao longo dos meses.
      Observa-se que os valores oscilam em torno da média de 34 dias, com meses mais rápidos e outros mais lentos:
      há mínimos próximos de 29 dias (por exemplo, em abril) e picos próximos de 39 dias (como em agosto/setembro).
      </p>
      <p>
      Essas oscilações podem refletir a sazonalidade de cargas (como safras agrícolas), condições climáticas,
      filas de navios e o perfil de utilização de cada porto. Ao aplicar filtros de UF e tipo de navegação,
      o usuário pode verificar se determinados estados ou tipos de tráfego apresentam padrões sazonais 
      mais pronunciados ou maior estabilidade ao longo do ano.
      </p>

      <h4>Relação entre operação e estadia por porto</h4>
      <p>
      O gráfico de dispersão compara, para cada porto, o tempo médio de operação com o 
      tempo médio de estadia. O tamanho do ponto representa o número de atracações e a cor identifica a UF.
      </p>
      <ul>
        <li>A maior parte dos portos concentra-se em faixas de operação abaixo de cerca de 80 dias e estadia abaixo de 150 dias,
        indicando desempenho relativamente ágil.</li>
        <li>Alguns portos se destacam como outliers, com tempos médios de operação acima de 150 dias 
        e estadias superiores a 300 dias, sugerindo maior complexidade de operação ou possíveis gargalos.</li>
        <li>Há uma tendência geral de que portos com operações mais longas também apresentem estadias totais maiores,
        o que é esperado, já que a operação compõe parte importante do tempo em porto.</li>
      </ul>
      <p>
      Esse gráfico permite identificar portos mais eficientes (pontos próximos da origem, com tempos reduzidos)
      e portos críticos (pontos mais distantes), além de comparar o comportamento entre diferentes UFs.
      </p>

      <h4>Mapa das instalações portuárias</h4>
      <p>
      O mapa interativo apresenta a distribuição espacial dos portos brasileiros. Cada círculo representa uma instalação,
      com tamanho proporcional ao tempo médio de estadia e popup contendo porto, município, UF, número de atracações 
      e tempo médio observado.
      </p>
      <ul>
        <li>A forte concentração de pontos ao longo da costa evidencia a importância da navegação marítima
        (longo curso e cabotagem) para o comércio exterior e o abastecimento interno.</li>
        <li>Os portos localizados em rios, especialmente na região Norte, mostram a relevância da navegação interior
        para o escoamento de cargas na Amazônia e em bacias hidrográficas estratégicas.</li>
        <li>Círculos maiores chamam atenção por apresentarem estadias médias mais elevadas,
        indicando casos que merecem análise mais detalhada.</li>
      </ul>

      <h4>Carga movimentada por rio</h4>
      <p>
      O gráfico de barras horizontais resume a carga total movimentada por rio. 
      Observa-se que alguns rios se destacam claramente, como o Amazonas e o Pará, 
      que concentram grande parte do volume transportado,
      enquanto outros rios aparecem com participação menor. Isso reforça a existência de eixos principais 
      da navegação interior no país.
      </p>

      <h4>Tabela dinâmica e aprofundamento</h4>
      <p>
      A tabela dinâmica apresenta, para cada combinação de ano, mês, porto, UF e tipo de navegação,
      o número de atracações, o tempo médio de operação e o tempo médio de estadia.
      Ela permite identificar quais portos concentram mais movimento, comparar tempos médios entre regiões
      e exportar os dados para análises adicionais (por exemplo, em planilhas ou modelos estatísticos).
      </p>

    ")
  })
  
  output$grafico_mensal <- renderPlotly({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Sem dados para o recorte selecionado."))
    
    df_mes <- df %>%
      group_by(ano, mes_ord) %>%
      summarise(
        tempo_medio_op = mean(t_operacao, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(mes_ord)
    
    plot_ly(
      df_mes,
      x = ~mes_ord,
      y = ~tempo_medio_op,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = "Mês: %{x}<br>Tempo médio: %{y:.2f} dias<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Mês"),
        yaxis = list(title = "Tempo médio de operação (dias)")
      )
  })
  
  output$grafico_dispersao <- renderPlotly({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Sem dados para o recorte selecionado."))
    
    df_portos <- df %>%
      group_by(porto, uf) %>%
      summarise(
        tempo_medio_op  = mean(t_operacao, na.rm = TRUE),
        tempo_medio_est = mean(t_estadia,  na.rm = TRUE),
        n_atracacoes    = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(tempo_medio_op), !is.na(tempo_medio_est))
    
    validate(need(nrow(df_portos) > 0, "Sem dados agregados para este recorte."))
    
    plot_ly(
      df_portos,
      x = ~tempo_medio_op,
      y = ~tempo_medio_est,
      size = ~n_atracacoes,
      color = ~uf,
      type = "scatter",
      mode = "markers",
      hovertemplate = paste(
        "Porto: %{text}<br>",
        "Tempo médio operação: %{x:.2f} dias<br>",
        "Tempo médio estadia: %{y:.2f} dias<br>",
        "Atracações: %{marker.size}<extra></extra>"
      ),
      text = ~paste0(porto, " (", uf, ")")
    ) %>%
      layout(
        xaxis = list(title = "Tempo médio de operação (dias)"),
        yaxis = list(title = "Tempo médio de estadia (dias)")
      )
  })
  
  output$mapa_portos <- renderLeaflet({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Sem dados para o recorte selecionado."))
    
    df_map <- df %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      group_by(porto, municipio, uf, lon, lat) %>%
      summarise(
        tempo_medio_est = mean(t_estadia, na.rm = TRUE),
        n_atracacoes    = n(),
        .groups = "drop"
      )
    
    validate(need(nrow(df_map) > 0, "Não há coordenadas cadastradas para este recorte."))
    
    max_est <- max(df_map$tempo_medio_est, na.rm = TRUE)
    df_map <- df_map %>%
      mutate(raio = 5 + 15 * (tempo_medio_est / max_est))
    
    leaflet(df_map) %>%
      addTiles() %>%
      setView(
        lng  = mean(df_map$lon, na.rm = TRUE),
        lat  = mean(df_map$lat, na.rm = TRUE),
        zoom = 4
      ) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~raio,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0(
          "", porto, " (", uf, ")<br>",
          "Município: ", municipio, "<br>",
          "Atracações: ", n_atracacoes, "<br>",
          "Tempo médio de estadia: ", round(tempo_medio_est, 2), " dias"
        )
      )
  })
  
  output$grafico_carga_rio <- renderPlotly({
    df <- carga_rio
    validate(need(nrow(df) > 0, "Sem dados de carga."))
    
    df_rio <- df %>%
      group_by(rio) %>%
      summarise(
        total_carga = sum(valor_movimentado, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_carga))
    
    plot_ly(
      df_rio,
      x = ~total_carga,
      y = ~rio,
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "Rio: %{y}<br>",
        "Carga movimentada: %{x:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Carga movimentada (unidade da base ANTAQ)"),
        yaxis = list(title = "Rio")
      )
  })
  output$tabela_resumo <- renderDT({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Sem dados para o recorte selecionado."))
    
    df_tab <- df %>%
      group_by(ano, mes, porto, uf, tipo_navegacao) %>%
      summarise(
        atracacoes      = n(),
        tempo_medio_op  = round(mean(t_operacao, na.rm = TRUE), 2),
        tempo_medio_est = round(mean(t_estadia,  na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(atracacoes))
    
    datatable(
      df_tab,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("copy", "csv", "excel")
      )
    )
  })
}
shinyApp(ui, server)
