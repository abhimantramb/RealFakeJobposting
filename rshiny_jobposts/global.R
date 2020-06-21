
# Libraries ---------------------------------------------------------------

# Shinyapp Dashboard Libs
suppressPackageStartupMessages({library(shiny);
                                library(shinydashboard);
                                library(shinyWidgets);
                                library(argonR);
                                library(argonDash);
                              })

# Utility Libs
suppressPackageStartupMessages({library(dplyr);
                                library(tm);
                                library(SnowballC);
                                library(caret);
                                library(stringr);
                                library(tidytext);
                                library(randomForest);
                                library(e1071);
                                library(caTools);
                                library(rsample);
                                library(ggplot2);
                                library(plotly);
                                library(glue);
                                library(gridExtra);
                                library(digest);
                                library(DT);
                                library(shinyjs)
                              })

# Inputs ------------------------------------------------------------------

# Downsampled model random forest
model <- readRDS("data/mod_rfdown.RDS")

# Presentation dataset !!! (TEST 1ST)
gold_dt <- read.csv("data/DONOTUSE_ATM.csv") %>% 
           mutate(job_id = job_id %>% as.numeric(),
                  title = title %>% as.character(),
                  location = location %>% as.character(),
                  department = department %>% as.character(),
                  company_profile = company_profile %>% as.character(),
                  description = description %>% as.character(),
                  requirements = requirements %>% as.character(),
                  benefits = benefits %>% as.character(),
                  department = department %>% as.character(),
                  count_prof = count_prof %>% as.numeric(), count_desc = count_desc %>% as.numeric(),
                  count_req = count_req %>% as.numeric(), count_benf = count_benf %>% as.numeric()
           ) %>% 
           mutate_if(is.integer, as.factor)

# Xtabs Dataset
c_df <- read.csv("data/downsamp_cdf.csv") %>% 
        mutate(job_id = job_id %>% as.numeric(),
               title = title %>% as.character(),
               location = location %>% as.character(),
               department = department %>% as.character(),
               company_profile = company_profile %>% as.character(),
               description = description %>% as.character(),
               requirements = requirements %>% as.character(),
               benefits = benefits %>% as.character(),
               department = department %>% as.character(),
               count_prof = count_prof %>% as.numeric(), count_desc = count_desc %>% as.numeric(),
               count_req = count_req %>% as.numeric(), count_benf = count_benf %>% as.numeric(),
               telecommuting = telecommuting %>% as.factor(),
               has_company_logo = has_company_logo %>% as.factor(),
               has_questions = has_questions %>% as.factor(),
               addr_low_edu = addr_low_edu %>% as.factor(),
               us_based = us_based %>% as.factor(),
               dollar_title = dollar_title %>% as.factor(),
               dollar_desc = dollar_desc %>% as.factor(),
               html_desc = html_desc %>% as.factor(),
               email_link = email_link %>% as.factor(),
               html_req = html_req %>% as.factor(),
               html_benf = html_benf %>% as.factor(),
               short_prof = short_prof %>% as.factor(),
               not_long_prof = not_long_prof %>% as.factor(),
               short_desc = short_desc %>% as.factor(),
               short_req = short_req %>% as.factor(),
               spam_wrd = spam_wrd %>% as.factor(),
               cons_punct = cons_punct %>% as.factor(),
               fraudulent = fraudulent %>% as.factor()
        )

# Global Conditionals -----------------------------------------------------

## Xtabs - Other Attribute -------------------------------------------------
tb.tele <- as.data.frame(xtabs(~ fraudulent + telecommuting, c_df))
tb.logo <- as.data.frame(xtabs(~ fraudulent + has_company_logo, c_df))
tb.qs <- as.data.frame(xtabs(~ fraudulent + has_questions, c_df))
tb.edu <- as.data.frame(xtabs(~ fraudulent + addr_low_edu, c_df))
tb.us <- as.data.frame(xtabs(~ fraudulent + us_based, c_df))

## Xtabs - Other Attribute -------------------------------------------------
tb.dlr_title <- as.data.frame(xtabs(~ fraudulent + dollar_title, c_df))
tb.dlr_desc <- as.data.frame(xtabs(~ fraudulent + dollar_desc, c_df))
tb.html_desc <- as.data.frame(xtabs(~ fraudulent + html_desc, c_df))
tb.html_req <- as.data.frame(xtabs(~ fraudulent + html_req, c_df))
tb.html_benf <- as.data.frame(xtabs(~ fraudulent + html_benf, c_df))
tb.email <- as.data.frame(xtabs(~ fraudulent + email_link, c_df))
tb.sh_prof <- as.data.frame(xtabs(~ fraudulent + short_prof, c_df))
tb.lg_prof <- as.data.frame(xtabs(~ fraudulent + not_long_prof, c_df))
tb.sh_desc <- as.data.frame(xtabs(~ fraudulent + short_desc, c_df))
tb.sh_req <- as.data.frame(xtabs(~ fraudulent + short_req, c_df))
tb.spm <- as.data.frame(xtabs(~ fraudulent + spam_wrd, c_df))
tb.punct <- as.data.frame(xtabs(~ fraudulent + cons_punct, c_df))


## Text Processing Fuction -----------------------------------------------
txtProcs <- function(data){
  
  # Gather Data
  df <- data %>% 
        mutate(job_id = job_id %>% as.numeric(),
               title = title %>% as.character(),
               location = location %>% as.character(),
               company_profile = company_profile %>% as.character(),
               description = description %>% as.character(),
               requirements = requirements %>% as.character(),
               benefits = benefits %>% as.character(),
               required_education = required_education %>% as.character(),
               telecommuting = telecommuting %>% as.factor(),
               has_company_logo = has_company_logo %>% as.factor(),
               has_questions = has_questions %>% as.factor()
               )
  
  # Text Cleaning from Unique Characters
  df$company_profile <- gsub("[^a-zA-Z0-9@#$%&*()_+/';:?.,!^-|{}]+"," ",df$company_profile)
  df$description <- gsub("[^a-zA-Z0-9@#$%&*()_+/';:?.,!^-|{}]+"," ",df$description)
  df$requirements <- gsub("[^a-zA-Z0-9@#$%&*()_+/';:?.,!^-|{}]+"," ",df$requirements)
  df$benefits <- gsub("[^a-zA-Z0-9@#$%&*()_+/';:?.,!^-|{}]+"," ",df$benefits)
  
  # Corpus Making
  corpus_prof <- as.vector(df$company_profile) %>%
    VectorSource() %>% 
    Corpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(stripWhitespace)
  
  corpus_desc <- as.vector(df$description) %>%
    VectorSource() %>% 
    Corpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(stripWhitespace)
  
  corpus_req <- as.vector(df$requirements) %>%
    VectorSource() %>% 
    Corpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(stripWhitespace)
  
  corpus_benf <- as.vector(df$benefits) %>%
    VectorSource() %>% 
    Corpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(stripWhitespace)
  
  # Reconstructing Dataframe from Corpus
  dfcorp_prof <- data.frame(text=sapply(corpus_prof,paste,collapse=" "), stringsAsFactors = F)
  dfcorp_desc <- data.frame(text=sapply(corpus_desc,paste,collapse=" "), stringsAsFactors = F)
  dfcorp_req <- data.frame(text=sapply(corpus_req,paste,collapse=" "), stringsAsFactors = F)
  dfcorp_benf <- data.frame(text=sapply(corpus_benf,paste,collapse=" "), stringsAsFactors = F)
  
  df_clean <- df %>% 
              mutate(company_profile = dfcorp_prof$text,
                     description = dfcorp_desc$text,
                     requirements = dfcorp_req$text,
                     benefits = dfcorp_benf$text)
  
  # Replacing empty row-col value into 'NA's
  df_clean <- df_clean %>% na_if("")
  
  ## Word Counting
  # In company_profile
  prof_count <- df_clean %>% 
    unnest_tokens(output = count_prof, input = company_profile) %>% 
    count(job_id, count_prof, sort=T) %>% 
    group_by(job_id) %>% 
    summarize(count_prof = sum(n))
  
  # In description
  desc_count <- df_clean %>% 
    unnest_tokens(output = count_desc, input = description) %>% 
    count(job_id, count_desc, sort=T) %>% 
    group_by(job_id) %>% 
    summarize(count_desc = sum(n))
  
  # In requirements
  req_count  <- df_clean %>% 
    unnest_tokens(output = count_req, input = requirements) %>% 
    count(job_id, count_req, sort=T) %>% 
    group_by(job_id) %>% 
    summarize(count_req = sum(n))
  
  # In benefits
  benf_count <- df_clean %>% 
    unnest_tokens(output = count_benf, input = benefits) %>% 
    count(job_id, count_benf, sort=T) %>% 
    group_by(job_id) %>% 
    summarize(count_benf = sum(n))
  
  # Left Join
  df_cc <- left_join(df_clean, prof_count, by = 'job_id')
  df_cc1 <- left_join(df_cc, desc_count, by = 'job_id')
  df_cc2 <- left_join(df_cc1, req_count, by = 'job_id')
  df_cc3 <- left_join(df_cc2, benf_count, by = 'job_id')
  
  # New Object
  df_cx <- df_cc3
  
  ## The making of new variables
  # Low Edu requirements
  df_cx <- df_cx %>% 
            mutate(addr_low_edu = ifelse(is.na(required_education),0,
                                         str_detect(required_education, fixed("high school", ignore_case = T))) 
                   %>% as.integer()
    )
  
  # Location in USA
  df_cx <- df_cx %>%
            mutate(us_based = ifelse(is.na(location),0,
                                     str_detect(location, fixed("us", ignore_case = T))) 
                   %>% as.integer()
    )
  
  # Money Symbol in TItle
  df_cx <- df_cx %>%
            mutate(dollar_title = ifelse(is.na(title),0,
                                         str_detect(title, fixed("$", ignore_case = T))) 
                   %>% as.integer()
    )
  
  # Money Symbol in Description
  df_cx <- df_cx %>%
            mutate(dollar_desc = ifelse(is.na(description),0,
                                        str_detect(description, fixed("$", ignore_case = T))) 
                   %>% as.integer()
    )
  
  # HTML Link in Description
  df_cx <- df_cx %>% 
            mutate(html_desc = ifelse(is.na(description),0,
                                      str_detect(description, "(http[s]?://)|(#url?_)+"))
                   %>% as.integer()
    )
  
  # HTML Link in Requirements
  df_cx <- df_cx %>% 
            mutate(html_req = ifelse(is.na(requirements),0,
                                     str_detect(requirements, "(http[s]?://)|(#url?_)+"))
                   %>% as.integer()
    )
  
  # HTML Link in Benefits
  df_cx <- df_cx %>% 
            mutate(html_benf = ifelse(is.na(benefits),0,
                                      str_detect(benefits, "(http[s]?://)|(#url?_)+"))
                   %>% as.integer()
    )
  
  # Word Counting
  df_cx <- df_cx %>% 
            mutate(short_prof = ifelse(count_prof<=10,1,0) %>% as.integer(),
                   not_long_prof = ifelse(count_prof>10 & count_prof<=100,1,0) %>% as.integer(),
                   short_desc = ifelse(count_desc<=10,1,0) %>% as.integer(),
                   short_req = ifelse(count_req<=10,1,0) %>% as.integer())
  
  # Email Link in Jobpost Naration
  df_cx <- df_cx %>% 
            mutate(email_link = 
                     (
                       (!is.na(title) & str_detect(title, fixed("#email", ignore_case = T))) |
                         (!is.na(company_profile) & str_detect(company_profile, fixed("#email", ignore_case = T))) |
                         (!is.na(description) & str_detect(description, fixed("#email", ignore_case = T))) |
                         (!is.na(requirements) & str_detect(requirements, fixed("#email", ignore_case = T))) |
                         (!is.na(benefits) & str_detect(benefits, fixed("#email", ignore_case = T)))
                     )
                   %>% as.integer()
    )
  
  # Spam Words in Jobposts
  df_cx <- df_cx %>% 
            mutate(spam_wrd = 
                     (
                       (!is.na(title) & str_detect(title, "(easy money)|(be your own boss)|(work from home)|(no experience)")) |
                         (!is.na(company_profile) & str_detect(company_profile, "(easy money)|(be your own boss)|(work from home)|(no experience)")) |
                         (!is.na(description) & str_detect(description, "(easy money)|(be your own boss)|(work from home)|(no experience)")) |
                         (!is.na(requirements) & str_detect(requirements, "(easy money)|(be your own boss)|(work from home)|(no experience)")) |
                         (!is.na(benefits) & str_detect(benefits, "(easy money)|(be your own boss)|(work from home)|(no experience)"))
                     )
                   %>% as.integer()
    )
  
  # Consecutive Punctuation
  df_cx <- df_cx %>% 
            mutate(cons_punct = 
                     (
                       (!is.na(title) & str_detect(title, fixed("!!", ignore_case = T))) |
                         (!is.na(company_profile) & str_detect(company_profile, fixed("!!", ignore_case = T))) |
                         (!is.na(description) & str_detect(description, fixed("!!", ignore_case = T))) |
                         (!is.na(requirements) & str_detect(requirements, fixed("!!", ignore_case = T))) |
                         (!is.na(benefits) & str_detect(benefits, fixed("!!", ignore_case = T)))
                     )
                   %>% as.integer()
    )
  
  # Rearanging the columns
  col_order <- c("job_id","title", "location", "company_profile", "description", "requirements", "benefits", 
                 "required_education", "count_prof", "count_desc", "count_req", "count_benf", "telecommuting",
                 "has_company_logo", "has_questions", "addr_low_edu", "us_based", "dollar_title", "dollar_desc",
                 "html_desc", "email_link", "html_req", "html_benf", "short_prof", "not_long_prof", 
                 "short_desc", "short_req", "spam_wrd", "cons_punct")
  
  df_fin <- df_cx[,col_order]
  
  # Changing Data Type
  raw <- df_fin %>% 
          mutate(job_id = job_id %>% as.numeric(),
                 title = title %>% as.character(),
                 location = location %>% as.character(),
                 company_profile = company_profile %>% as.character(),
                 description = description %>% as.character(),
                 requirements = requirements %>% as.character(),
                 benefits = benefits %>% as.character(),
                 count_prof = count_prof %>% as.numeric(), count_desc = count_desc %>% as.numeric(),
                 count_req = count_req %>% as.numeric(), count_benf = count_benf %>% as.numeric()
          ) %>% 
          mutate_if(is.integer, as.factor)
  
  return(raw)
  
}

