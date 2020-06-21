function(input, output, session){

# Input Reactive ----------------------------------------------------------
  ## nova_df is the user's input prefered values
  nova <- 
  reactive({
    
    # The Inputs
    title <- as.character(input$bar_title)
    location <- as.character(input$bar_loc)
    required_education <- as.character(input$bar_edu)
    company_profile <- as.character(input$bar_compprof)
    description <- as.character(input$bar_jobdesc)
    requirements <- as.character(input$bar_jobreq)
    benefits <- as.character(input$bar_jobbenf)
    telecommuting <- as.character(input$ch_tele)
    has_company_logo <- as.character(input$ch_logo)
    has_questions <- as.character(input$ch_qtns)
    
    # The Making of New DF
    pre_df <- cbind(title, location, company_profile, description, requirements,
                    benefits, required_education, telecommuting, has_company_logo, 
                    has_questions)
    
    x_df<- as.data.frame(pre_df) %>% 
              mutate(
                job_id = row_number(),
                telecommuting = (case_when(telecommuting == TRUE ~ 1, telecommuting == FALSE ~ 0)) %>% as.factor,
                has_company_logo = (case_when(has_company_logo == TRUE ~ 1, has_company_logo == FALSE ~ 0)) %>% as.factor,
                has_questions = (case_when(has_questions == TRUE ~ 1, has_questions == FALSE ~ 0)) %>% as.factor
              )
    
    post_df <- x_df[, c("job_id","title", "location", "company_profile", "description", "requirements",
                        "benefits", "required_education", "telecommuting", "has_company_logo", "has_questions")]
    return(post_df)
    
  })

# Output Reactives
## EDA Tab ----------------------------------------------------------------
  
  # Text Analysis
  output$txt_aly <-
    renderPlot({

      gr.dlr_title <- tb.dlr_title %>% 
                      ggplot(aes(x = dollar_title, 
                                 y = Freq,
                                 fill = fraudulent)) + 
                      geom_col(position = "stack") + 
                      labs(x = "Money Sign in Job Title", y = "Count", fill = "Label") +
                      theme(plot.title = element_text(hjust = 0.5))
      
      gr.dlr_desc <- tb.dlr_desc %>% 
                     ggplot(aes(x = dollar_desc, 
                                y = Freq,
                                fill = fraudulent)) + 
                     geom_col(position = "stack") + 
                     labs(x = "Money Sign in Job Description", y = "Count", fill = "Label") +
                     theme(plot.title = element_text(hjust = 0.5))
      
      gr.html_desc <- tb.html_desc %>% 
                      ggplot(aes(x = html_desc, 
                                 y = Freq,
                                 fill = fraudulent)) + 
                      geom_col(position = "stack") + 
                      labs(x = "External Link in Job Description", y = "Count", fill = "Label") +
                      theme(plot.title = element_text(hjust = 0.5))
      
      gr.html_req <- tb.html_req %>% 
                      ggplot(aes(x = html_req, 
                                 y = Freq,
                                 fill = fraudulent)) + 
                      geom_col(position = "stack") + 
                      labs(x = "External Link in Job Requiremnets", y = "Count", fill = "Label") +
                      theme(plot.title = element_text(hjust = 0.5))
      
      gr.html_benf <- tb.html_benf %>% 
                      ggplot(aes(x = html_benf, 
                                 y = Freq,
                                 fill = fraudulent)) + 
                      geom_col(position = "stack") + 
                      labs(x = "External Link in Job Benefits", y = "Count", fill = "Label") +
                      theme(plot.title = element_text(hjust = 0.5))
      
      gr.email <- tb.email %>% 
                  ggplot(aes(x = email_link, 
                             y = Freq,
                             fill = fraudulent)) + 
                  geom_col(position = "stack") + 
                  labs(x = "E-mail Link in Jobposts", y = "Count", fill = "Label") +
                  theme(plot.title = element_text(hjust = 0.5))
      
      gr.sh_prof <- tb.sh_prof %>% 
                    ggplot(aes(x = short_prof, 
                               y = Freq,
                               fill = fraudulent)) + 
                    geom_col(position = "stack") + 
                    labs(x = "<10 Word Count in Company Profile", y = "Count", fill = "Label") +
                    theme(plot.title = element_text(hjust = 0.5))
      
      gr.lg_prof <- tb.lg_prof %>% 
                    ggplot(aes(x = not_long_prof, 
                               y = Freq,
                               fill = fraudulent)) + 
                    geom_col(position = "stack") + 
                    labs(x = "10~100 Word Count in Company Profile", y = "Count", fill = "Label") +
                    theme(plot.title = element_text(hjust = 0.5))
      
      gr.sh_desc <- tb.sh_desc %>% 
                    ggplot(aes(x = short_desc, 
                               y = Freq,
                               fill = fraudulent)) + 
                    geom_col(position = "stack") + 
                    labs(x = "<10 Word Count in Job Description", y = "Count", fill = "Label") +
                    theme(plot.title = element_text(hjust = 0.5))
      
      gr.sh_req <- tb.sh_req %>% 
                    ggplot(aes(x = short_req, 
                               y = Freq,
                               fill = fraudulent)) + 
                    geom_col(position = "stack") + 
                    labs(x = "<10 Word Count in Job Requirement", y = "Count", fill = "Label") +
                    theme(plot.title = element_text(hjust = 0.5))
      
      gr.spm <- tb.spm %>% 
                ggplot(aes(x = spam_wrd, 
                           y = Freq,
                           fill = fraudulent)) + 
                geom_col(position = "stack") + 
                labs(x = "Spam Words in Jobposts", y = "Count", fill = "Label") +
                theme(plot.title = element_text(hjust = 0.5))
      
      gr.punct <- tb.punct %>% 
                  ggplot(aes(x = cons_punct, 
                             y = Freq,
                             fill = fraudulent)) + 
                  geom_col(position = "stack") + 
                  labs(x = "Consecutive Punctuation in Jobpost", y = "Count", fill = "Label") +
                  theme(plot.title = element_text(hjust = 0.5))
      
      # Graph Location
      grid.arrange(gr.dlr_title, gr.dlr_desc, gr.html_desc, gr.html_req, gr.html_benf, gr.email,
                   gr.sh_prof, gr.lg_prof, gr.sh_desc, gr.sh_req, gr.spm, gr.punct,
                   ncol = 4)
    })
  
  # Attribute Analysis
  output$att_aly<-
    renderPlot({
      
      gr.tele <- tb.tele %>% 
                  ggplot(aes(x = telecommuting, 
                             y = Freq,
                             fill = fraudulent)) + 
                  geom_col(position = "stack") + 
                  labs(x = "Is Telecommuting", y = "Count", fill = "Label") +
                  theme(plot.title = element_text(hjust = 0.5))
      
      gr.logo <- tb.logo %>% 
                  ggplot(aes(x = has_company_logo, 
                             y = Freq,
                             fill = fraudulent)) + 
                  geom_col(position = "stack") + 
                  labs(x = "Have Company Logo", y = "Count", fill = "Label") +
                  theme(plot.title = element_text(hjust = 0.5))
      
      gr.qs <- tb.qs %>% 
                ggplot(aes(x = has_questions, 
                           y = Freq,
                           fill = fraudulent)) + 
                geom_col(position = "stack") + 
                labs(x = "Have Screening Questions", y = "Count", fill = "Label") +
                theme(plot.title = element_text(hjust = 0.5))
      
      gr.edu <- tb.edu %>% 
                ggplot(aes(x = addr_low_edu, 
                           y = Freq,
                           fill = fraudulent)) + 
                geom_col(position = "stack") + 
                labs(x = "Addressing Low Education", y = "Count", fill = "Label") +
                theme(plot.title = element_text(hjust = 0.5))
      
      gr.us <- tb.us %>% 
                ggplot(aes(x = us_based, 
                           y = Freq,
                           fill = fraudulent)
                ) + 
                geom_col(position = "stack") + 
                labs(x = "Placement in USA", y = "Count", fill = "Label") +
                theme(plot.title = element_text(hjust = 0.5))
      
      # Graph Location
      grid.arrange(gr.tele, gr.logo, gr.qs, gr.edu, gr.us,
                   ncol = 2)
      
    })

## ML Tab ------------------------------------------------------------------
  # Reactive Values
  v1 <- reactiveValues(vd = NULL)
  
  #  Process Button
  observeEvent(
    input$btn_proc,
    {
      v1$vd <- txtProcs(nova())
      
      # Model Prediciton
      r_pred <- predict(model, v1$vd[,c("telecommuting", "has_company_logo", "has_questions", "addr_low_edu", "us_based",
                                        "dollar_title", "dollar_desc", "html_desc", "email_link", "html_req", "html_benf", 
                                        "short_prof", "not_long_prof", "short_desc", "short_req", "spam_wrd", "cons_punct")]
                        )
      
      show("showrslt")
      
      # Conditional Alert Output
      if(r_pred == 1){output$notif <- renderText("Be Careful There...")} else
        if(r_pred == 0){output$notif <- renderText("All Clear There...")} else{return()}
      
      
    })
  
  # Submit again button
  observeEvent(
    input$btn_again,
    {
      reset("fillform")
      hide("showrslt")
    })
  
}