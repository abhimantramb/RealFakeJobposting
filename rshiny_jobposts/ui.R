
# Header ------------------------------------------------------------------
header <- 
argonDashHeader(
  
  gradient = TRUE,
  background_img = "https://revivme.com/wp-content/uploads/2019/05/newYork-1.jpeg",
  separator = TRUE,
  separator_color = "primary",
  height = 250
  
#End  
)

# Navbar -----------------------------------------------------------------


# Sidebar --------------------------------------------------------------------
sidebar <- 
argonDashSidebar(
  
  vertical = TRUE,
  skin = "light",
  background = "ghostwhite",
  size = "md",
  side = "left",
  id = "my_sidebar",
  brand_logo = "https://w0.pngwave.com/png/42/542/magnifying-glass-transparency-and-translucency-computer-icons-magnifying-glass-png-clip-art.png",
  
  argonSidebarHeader(title = "Menu"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "home_tab",
      style="text-align:center",
      "Home"
    ),
    argonSidebarItem(
      tabName = "show_tab",
      style="text-align:center",
      "EDA"
    ),
    argonSidebarItem(
      tabName = "ml_tab",
      style="text-align:center",
      "Machine Learning"
    ),
    argonSidebarItem(
      tabName = "ref_tab",
      style="text-align:center",
      "About"
    )
  )
  
#End  
)


# Body --------------------------------------------------------------------
body <- 
argonDashBody(
  argonTabItems(
    
    # Home Tab ----------------------------------------------------------------
    argonTabItem(
      tabName = "home_tab",
      
      #Welcome Box
      fluidRow(
        column(width = 2),
        column(
          width = 8,
          
          argonCard(
            width = 12,
            title = h1("Welcome!"),
            hover_lift = FALSE,
            shadow = FALSE,
            floating = FALSE,
            h2("Background"),
            h3(
               "This project aims in classifying and detecting Online Recruitment Frauds (ORF) by 
               analysing the text attribute and any attribute following it.
               "
               ),
            br(), br(),
            h2("Goal"),
            h3("1. Exploring and examining the attribute characteristics of ORF"),
            h3("2. Detecting Online Recruitment Fraud (ORF) attempt")
          )
          
        ),
        column(width = 2)
        
      )
      
    ),
    
    # EDA Tab ----------------------------------------------------------------
    argonTabItem(
      tabName = "show_tab",
      
      # Opt Tab for EDA
      fluidRow(
        argonTabSet(
          id = "x-tab",
          width = 12,
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "md",
          
          # Text EDA
          argonTab(
            tabName = "Jobpost Text Analysis",
            active = TRUE,
            fluidRow(
              
              # Graph
              column(
                width = 9,
                argonCard(
                  width = 12,
                  plotOutput("txt_aly")
                )
              ),
              
              # Explaination
              column(
                width = 3,
                argonCard(
                  width = 12,
                  hover_lift = FALSE,
                  shadow = TRUE,
                  floating = FALSE,
                  h3("Since every submitted jobposts have their own naration text, it is a key point procedure in determining frauds by
                     analysing what comes within the text by using text analysis. Here in the project, we gather several key elements 
                     and we can how the data spread in every features corresponding to the fraudulency attempt.")
                )
              )
              
            )
          ),
          
          # Attribute EDA
          argonTab(
            tabName = "Attribute Analysis",
            active = FALSE,
            fluidRow(
              
              # Graph
              column(
                width = 9,
                argonCard(
                  width = 12,
                  plotOutput("att_aly")
                )
              ),
              
              # Explaination
              column(
                width = 3,
                argonCard(
                  width = 12,
                  hover_lift = FALSE,
                  shadow = TRUE,
                  floating = FALSE,
                  h3("Every jobposts came with their own attributes. Usually it revolves around non-naration related fields, for example:
                     company's logos, education requirements, and so on. Here, we can see the data spread of every attributes
                     that corespond to any fraudulency attempt")
                )
              )
              
            )
          )
        )
      )
    # End  
    ),
    
    # ML Tab ------------------------------------------------------------------
    argonTabItem(
      tabName = "ml_tab",
      
      # Demo Classification
      fluidRow(
        # User Input
        argonCard(
          width = 12,
          title = h1("Please Input User Data"),
          hover_lift = FALSE,
          shadow = TRUE,
          floating = FALSE,
          
          div(
            id = "fillform",
            
            # User Input Text
            textInput("bar_title", h3("Position Title"), width = "50%"),
            textInput("bar_loc", h3("Location"), width = "50%"),
            selectInput("bar_edu",
                        h3("Required Education"),
                        c("","Bachelor's Degree", "Master's Degree", "High School or equivalent", "Unspecified",
                          "Some College Coursework Completed", "Vocational", "Certification", "Associate Degree",
                          "Professional", "Doctorate", "Some High School Coursework", "Vocational - Degree",
                          "Vocational - HS Diploma"), 
                        width = "50%"
            ),
            textAreaInput("bar_compprof", h3("Company Profile"), width = "200%", resize = "vertical"),
            textAreaInput("bar_jobdesc", h3("Job Description"), width = "200%", resize = "vertical"),
            textAreaInput("bar_jobreq", h3("Job Requirements"), width = "200%", resize = "vertical"),
            textAreaInput("bar_jobbenf", h3("Job Benefits"), width = "200%", resize = "vertical"),
            br(),
            fluidRow(
              column(width = 4, checkboxInput("ch_tele", h3("Telecommuting"), value = FALSE)),
              column(width = 4, checkboxInput("ch_logo", h3("Company's Logo"), value = FALSE)),
              column(width = 4, checkboxInput("ch_qtns", h3("Screening Questions"), value = FALSE)) 
            ),
            br(), br(),
            actionButton("btn_proc", "Process", class = "btn-primary")
            
          )
          
        )
      ),
      
      # Show Result
      fluidRow(
        hidden(
          div(
            id = "showrslt",
            
            # Result ALert
            argonCard(
              width = 12,
              title = h1("RESULT"),
              hover_lift = FALSE,
              shadow = TRUE,
              floating = TRUE,
              
              # Notification of result
              h2(textOutput("notif")), br(),
              
              # To submit again
              actionButton("btn_again", "Submit Another", class = "btn_primary")
            )
            
          )
        )
      )
      
    # End  
    ),
    
    # About ---------------------------------------------------------------
    argonTabItem(
      tabName = "ref_tab",
      
      # Model Credit Box
      fluidRow(
        argonCard(
          width = 12,
          hover_lift = FALSE,
          shadow = FALSE,
          floating = FALSE,
          
          h2("Dataset Reference:"),
          h3(
            tags$a(href="https://www.kaggle.com/shivamb/real-or-fake-fake-jobposting-prediction", 
                   "[Real or Fake] : Fake Job Description Prediction"
            )
          ),
          br(), 
          h2("Model Reference:"),
          h3("[1]"),
          h3("(Sokratis Vidros, Constantinos Kolias, Georgios Kambourakis, Leman Akoglu)"),
          h3(
            tags$a(href="https://www.researchgate.net/publication/314225032_Automatic_Detection_of_Online_Recruitment_Frauds_Characteristics_Methods_and_a_Public_Dataset", 
                   "Automatic Detection of Online Recruitment Frauds: Characteristics, Methods, and a Public Dataset"
                   )
          )
          
        )
      ),
      
      # Personal Credit Box
      fluidRow(
        argonCard(
          width = 12,
          hover_lift = FALSE,
          shadow = FALSE,
          floating = FALSE,
          h2("Author:"),
          h3("Abhimantra Manggala Bhawikastawa"),
          h3(
            tags$a(href="https://www.linkedin.com/in/abhimantra-bhawikastawa-1948b41a3/", 
                   "LinkedIn Account"
            )
          )
        )
      )
      
    )
    
  )
#End  
)


# Footer ------------------------------------------------------------------


# Dashboard Page ----------------------------------------------------------
argonDashPage(
  # For Mandatory Fill-Ins
  useShinyjs(),
  inlineCSS(appCSS),
  
  # The Content
  header = header,
  sidebar = sidebar,
  body = body 
)