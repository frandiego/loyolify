tab_plot <- argonTabItem(
  tabName = "plot",
  argonRow(argonColumn(uiOutput("main_title"), width = 12, center = T)),
  argonRow(
    argonColumn(uiOutput("title"), width = 6),
    argonColumn(uiOutput("title_comp"), width = 6)
  ),
  shiny::br(),
  argonRow(
    argonColumn(uiOutput("subtitle_left"), width = 6, center = T),
    argonColumn(uiOutput("subtitle_right"), width = 6, center = T)
  ),
  htmlOutput("plot"),
  argonRow(
    argonColumn(
      with = 6,
      pickerInput(
        inputId = "compare",
        label = "Comparar",
        choices = list(
          " " = " ",
          Centro = "school",
          Curso = "course",
          Grupo = "group",
          Género = "gender",
          Repetidor = "is_repeater",
          Popular = "is_popular"
        ),
        multiple = TRUE,
        selected = " ",
        options = pickerOptions(maxOptions = 1, noneSelectedText = " "),
        choicesOpt = list(
          style = "color: steelblue"
        )
      )
    ),
    argonColumn(
      with = 6,
      pickerInput(
        inputId = "facet",
        label = "Dividir",
        choices = list(
          " " = " ",
          Género = "gender",
          Repetidor = "is_repeater",
          Popular = "is_popular"
        ),
        selected = " ",
        multiple = T,
        options = pickerOptions(maxOptions = 1, noneSelectedText = " "),
        choicesOpt = list(
          style = "color: steelblue"
        )
      )
    )
  )
)


tab_card <- argonTabItem(
  tabName = "card",

  # classic cards
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = "Shiny Inputs",
      argonRow(
        argonColumn(
          width = 6,
          sliderInput(
            "obs",
            "Number of observations:",
            min = 0,
            max = 1000,
            value = 500
          )
        ),
        argonColumn(width = 6, plotOutput("distPlot"))
      )
    ),
    br(), br(),
    argonCard(
      width = 12,
      title = "Argon Card",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE,
      floating = FALSE,
      argonRow(
        argonColumn(
          width = 6,
          radioButtons(
            "dist",
            "Distribution type:",
            c(
              "Normal" = "norm",
              "Uniform" = "unif",
              "Log-normal" = "lnorm",
              "Exponential" = "exp"
            )
          )
        ),
        argonColumn(width = 6, plotOutput("plot"))
      )
    )
  ),
  br(),

  # info cards
  argonH1("Info Cards", display = 4),
  argonRow(
    argonInfoCard(
      value = "350,897",
      title = "TRAFFIC",
      stat = 3.48,
      stat_icon = icon("arrow-up"),
      description = "Since last month",
      icon = argonIcon("planet"),
      icon_background = "danger",
      hover_lift = TRUE
    ),
    argonInfoCard(
      value = "2,356",
      title = "NEW USERS",
      stat = -3.48,
      stat_icon = icon("arrow-down"),
      description = "Since last week",
      icon = icon("chart-pie"),
      icon_background = "warning",
      shadow = TRUE
    ),
    argonInfoCard(
      value = "924",
      title = "SALES",
      stat = -1.10,
      stat_icon = icon("arrow-down"),
      description = "Since yesterday",
      icon = icon("users"),
      icon_background = "yellow",
      background_color = "default"
    ),
    argonInfoCard(
      value = "49,65%",
      title = "PERFORMANCE",
      stat = 12,
      stat_icon = icon("arrow-up"),
      description = "Since last month",
      icon = icon("percent"),
      icon_background = "info",
      gradient = TRUE,
      background_color = "orange",
      hover_lift = TRUE
    )
  ),

  # profile cards
  argonH1("User Cards", display = 4),
  argonRow(
    argonColumn(
      width = 3,
      argonUser(
        title = "Ryan Tompson",
        subtitle = "Web Developer",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-1-800x800.jpg"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Romina Hadid",
        subtitle = "Marketing Strategist",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-2-800x800.jpg"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "Alexander Smith",
        subtitle = "UI/UX Designer",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-3-800x800.jpg"
      )
    ),
    argonColumn(
      width = 3,
      argonUser(
        title = "John Doe",
        subtitle = "Founder and CEO",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-4-800x800.jpg"
      )
    )
  ),
  br(), br(),
  argonH1("Profile Card", display = 4),
  argonRow(
    argonColumn(
      width = 12,
      argonProfile(
        title = "John",
        subtitle = "Japan, Kagoshima",
        src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-1-800x800.jpg",
        url = "https://www.google.com",
        url_1 = "https://www.google.com",
        url_2 = "https://www.google.com",
        stats = argonProfileStats(
          argonProfileStat(
            value = 22,
            description = "Friends"
          ),
          argonProfileStat(
            value = 10,
            description = "Photos"
          ),
          argonProfileStat(
            value = 89,
            description = "Comments"
          )
        ),
        "An artist of considerable range, Ryan —
                  the name taken by Melbourne-raised,
                  Brooklyn-based Nick Murphy — writes,
                  performs and records all of his own music,
                  giving it a warm, intimate feel with a solid
                  groove structure. An artist of considerable
                  range."
      )
    )
  )
)


tables_tab <- argonTabItem(
  tabName = "tables",
  radioButtons(
    inputId = "cardWrap",
    inline = TRUE,
    label = "Enable card wrap?",
    choices = c("Enable", "Disable"),
    selected = "Enable"
  ),
  uiOutput("argonTable")
)
