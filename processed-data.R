      if (!is.null(input$survey_id) && input$survey_id != ""){
        incProgress(0.1, detail = "Retrieving Alchemer quotas...")
        
        # Specify the survey ID
        survey_id <- as.numeric(input$survey_id)
        
        # Define the base URL
        base_url <- 'https://api.alchemer.com/v5/'
        
        # Function to handle API requests
        make_request <- function(endpoint, params) {
          url <- paste0(base_url, endpoint)
          response <- GET(url, query = params)
          
          if (status_code(response) == 200) {
            content_text <- content(response, as = "text", encoding = "UTF-8")
            result <- fromJSON(content_text)
            return(result)
          } else {
            stop(paste0('Error: ', status_code(response), ' - ', content(response, as = "text", encoding = "UTF-8")))
          }
        }
        
        # Set up the parameters
        params <- list(
          'api_token' = api_token,
          'api_token_secret' = api_secret
        )
        
        # Get survey details
        survey_details <- make_request(paste0('survey/', survey_id), params)
        
        # Get quotas for the survey
        quotas_data <- make_request(paste0('survey/', survey_id, '/quotas'), params)
        
        # Extract the 'quotas' element and convert to a data frame
        if (!is.null(quotas_data$quotas)) {
          quotas_df <- as.data.frame(quotas_data$quotas)
          print("Quotas found")
        } else {
          print("No quotas found.")
        }
        
        quotas_df <- dplyr::select(quotas_df, any_of(c("name", "responses", "limit")))
        quotas_df$responses <- as.numeric(quotas_df$responses)
        quotas_df$limit <- as.numeric(quotas_df$limit)
        quotas_df$quota_filled <- quotas_df$responses/quotas_df$limit
        quotas_df$quota_filled <- sprintf("%.1f%%", quotas_df$quota_filled * 100)
        colnames(quotas_df) <- c("Attribute", "Completes", "Max Cap", "Quota Filled")
        
      } else{
        quotas_df <- NULL
      }
      
      if (!is.null(input$weighted_file)){
        survey <- input$weighted_file$data_path
      } else{
        if (!is.null(input$alchemer_file)){
          survey <- haven::read_sav(input$alchemer_file$datapath)
        } else if (!is.null(input$alchemer_link) && input$alchemer_link != ""){
          
          incProgress(0.3, detail = "Retrieving realtime export from Alchemer server (normally takes 15-30 seconds)...")
          lucid_spss <- input$alchemer_link
          response <- httr::GET(lucid_spss)
          Sys.sleep(15)
          response <- httr::GET(lucid_spss)
          if (!dir.exists("Test-Data")) {
            dir.create("Test-Data", recursive = TRUE)
          }
          writeBin(httr::content(response, "raw"), "Test-Data/Test SPSS.zip")
          
          
          tryCatch({
            # Attempt to parse the file
            survey <- haven::read_sav("Test-Data/Test SPSS.zip")
          }, error = function(e) {
            # Handle the specific error
            if (grepl("Unable to read from", e$message)) {
              message("Alchemer realtime export unsuccessful. Attempting once more.")
              response <- httr::GET(lucid_spss)
              Sys.sleep(15)
              response <- httr::GET(lucid_spss)
              if (!dir.exists("Test-Data")) {
                dir.create("Test-Data", recursive = TRUE)
              }
              writeBin(httr::content(response, "raw"), "Test-Data/Test SPSS.zip")
              survey <- haven::read_sav("Test-Data/Test SPSS.zip")
              
            } else {
              # For other errors, re-throw the error
              stop(e)
            }
          })
          
        } else{
          survey <- haven::read_sav(input$kgs_file$datapath)
        }
        
        incProgress(0.1, detail = "Processing...")
        
        # ------Speeder Removals
        pagetimevars <- colnames(survey)
        pagetimevars <- pagetimevars[grep("pagetime|_Pagetime", pagetimevars)]
        finalpagetimevars <- vector()
        for (varname in pagetimevars){
          current <- attr(survey[[varname]], "label")
          # print(current)
          if (grepl("pagetime", current)){
            finalpagetimevars <- c(finalpagetimevars, varname)
          }
        }
        pagetimevars <- finalpagetimevars
        
        x <- dplyr::select(survey, any_of(pagetimevars))
        if (ncol(x) >=30){
          pagetime_medians <- data.frame(pagetimevar=NULL, pagetimemedian=NULL)
          for (varname in colnames(x)){
            times <- x[[varname]]
            times <- times[times!=""]
            currentmedian <- median(as.numeric(na.omit(times)))
            pagetime_medians <- rbind(pagetime_medians, data.frame(pagetimevar=varname, pagetimemedian=currentmedian))
          }
          
          for (varname in colnames(x)){
            newvar <- paste0(varname, "_is_speeder")
            current_speeder_threshold <- pagetime_medians[pagetime_medians[["pagetimevar"]]==varname,][["pagetimemedian"]]/3
            # print(current_speeder_threshold)
            x[[newvar]] <- ifelse(x[[varname]]=="", NA,
                                  ifelse(as.numeric(x[[varname]]) < current_speeder_threshold, TRUE, FALSE))
          }
          
          x <- dplyr::select(x, -any_of(c(pagetimevars)))
          numNAs <- rowSums(is.na(x))
          num_speeder_qs <- rowSums(x, na.rm = TRUE)
          survey$proportion_of_speeder_responses <- num_speeder_qs/(ncol(x)- numNAs)
          survey$is_50pct_speeder <- num_speeder_qs/(ncol(x)- numNAs) > .5
          survey$is_33pct_speeder <- num_speeder_qs/(ncol(x)- numNAs) > .33
          
          survey<- survey[survey[["is_33pct_speeder"]]==FALSE,]
          
          survey <- dplyr::select(survey, -any_of(c(pagetimevars)))
          
        }
        
        # ---------
        
        
        
        # survey <- survey[substr(survey[["lalvoterid"]], 4,5)=="MI",]
        
        # for (varname in c("QFirstName",
        #                   "QRegVote", "QAge", "QVtlk", "QEarlyVote", "QVotePlan", "QMotivation2024",
        #                   "QApproval", "QTrumpApproval", "QTrumpHarrisFull_Voted", "QTrumpHarrisFull",
        #                   "QTrumpHarrisFullLeaner", "QTrumpHarris", "QTrumpHarrisLeaner",
        #                   "QShapiroVP", "QShapiroVPLeaner", "QPASenateBallot_Voted", "QPASenateBallot",
        #                   "QPASenateBallotLeaner", "QWISenateBallot_Voted", "QWISenateBallot",
        #                   "QWISenateBallotLeaner", "QMISenateBallot_Voted", "QMISenateBallot",
        #                   "QMISenateBallotLeaner", "Q2020Ballot", "QPartyID", "QPartyGOP",
        #                   "QPartyDem", "QPartyLeanView", "QIdeology", "QGender", "QRace_1",
        #                   "QRace_2", "QRace_3", "QRace_4", "QRace_5", "QRace_6", "QRace_7",
        #                   "QRace_other", "QEducation", "QMarital")){
        #   attr(survey[[varname]], "label") <- gsub(varname, "", attr(survey[[varname]], "label"), ignore.case = T)
        #   attr(survey[[varname]], "label") <- gsub(toupper(varname), "", attr(survey[[varname]], "label"))
        #   attr(survey[[varname]], "label") <- stringr::str_trim(gsub("\\.", " ", attr(survey[[varname]], "label")))
        #
        # }
        #
        # for (varname in sprintf("QRace_%s", 1:7)){
        #   newlabel <- paste(attr(survey[[varname]], "label"), ":", "What is your race or ethnicity?", sep="")
        #   attr(survey[[varname]], "label") <- newlabel
        # }
        
        # weights <- read.csv(input$weights_csv$datapath)
        weights <- rv$current_weights_csv
        in_voter_file <- c("turnout_2024_bins", "general_2020")
        # weights <- read.csv("data/survey/weights-lv-MI-2024.csv")
        
        weight_categories <- unique(weights[["category"]])
        
        if (all(c("QCONSENTFNAME", "QCONSENTLNAME", "QCONSENTZIP") %in% colnames(survey)) | ("lalvoterid" %in% colnames(survey))){
          if (all(c("QCONSENTFNAME", "QCONSENTLNAME", "QCONSENTZIP") %in% colnames(survey))){
            clean_name <- function(x) {
              x <- gsub(" ", "", x)
              x <- gsub("-", "", x)
              tolower(x)
            }
            
            survey$QCONSENTZIP <- stringr::str_pad(survey$QCONSENTZIP, 5, "left", 0)
            survey$QCONSENTFNAME <- clean_name(survey$QCONSENTFNAME)
            survey$QCONSENTLNAME <- clean_name(survey$QCONSENTLNAME)
            
            survey[["ids"]] <- paste(survey[["QCONSENTFNAME"]], survey[["QCONSENTLNAME"]], survey[["QCONSENTZIP"]])
            survey[["ids"]] <- tolower(survey[["ids"]])
            
            ids <- paste(
              sprintf("'%s'", gsub("'", "", survey[["ids"]])),
              collapse = ", "
            )
            query <- paste(
              "SELECT lalvoterid, voters_firstname, voters_lastname, residence_addresses_zip, voters_age,",
              "(REPLACE(REPLACE(LOWER(voters_firstname), ' ', ''), '-', '') ||' ' || REPLACE(REPLACE(LOWER(voters_lastname), ' ', ''), '-', '') || ' ' || LPAD(LOWER(residence_addresses_zip), 5, 0)) as id",
              "FROM v3 as v WHERE id in (%s)"
            )
            
            query <- sprintf(query, ids)
            incProgress(0.2, detail = "Matching respondents to voter file (normally takes 10-15 seconds)...")
            res <- EchelonSurveyTools::presto_get(query, "PostgreSQL")
            
            # Remove duplicates.
            res <- merge(res, survey[, c("ids", "QAge")], by.x = "id", by.y = "ids", all.x = T)
            res$age_diff <- abs(res$voters_age - res$QAge)
            res <- res[order(res$age_diff), ]
            res <- res[!duplicated(res$id), ]
            
            survey <- survey[survey$ids %in% res$id, ]
            survey <- survey[order(abs(survey$QAge - setNames(res$QAge, res$id)[survey$ids])), ]
            print(sum(duplicated(survey$ids)))
            survey <- survey[!duplicated(survey$ids), ]
            survey$lalvoterid <- setNames(res$lalvoterid, res$id)[survey$ids]
            
          }
          
          file_path <- "Test-Data/test-voter-file.csv"
          
          if ((!file.exists(file_path)) || all(survey$lalvoterid %in% read.csv(file_path)[["lalvoterid"]])==FALSE){
            
            incProgress(0.3, detail = "Retrieving more voter file data from SQL tables (normally takes ~15 seconds)...")
            
            cols <- c(
              "v.lalvoterid as lalvoterid",
              "residence_addresses_state",
              "residence_addresses_zip",
              "us_congressional_district",
              "designated_market_area_dma",
              "precinct",
              "city_ward",
              "town_ward",
              "town_district",
              "county",
              "voters_fips",
              "parties_description",
              "general_2018",
              "primary_2018",
              "general_2016",
              "primary_2016",
              "presidential_primary_2016",
              "presidential_primary_2020",
              "general_2014",
              "primary_2014",
              "general_2012",
              "primary_2012",
              "primary_2020",
              "general_2020",
              "primary_2022",
              "general_2022",
              "pri_blt_2022",
              "pri_blt_2020",
              "pri_blt_2018",
              "pri_blt_2016",
              "pri_blt_2014",
              "presidential_primary_2012",
              "ballottype_general_2020",
              "voters_calculatedregdate",
              "voters_age",
              "voters_gender",
              "ethnicgroups_ethnicgroup1desc",
              "((CASE WHEN primary_2022 = 'Y' THEN 1 ELSE 0 END) +
    (CASE WHEN primary_2020 = 'Y' or presidential_primary_2020 = 'Y' THEN 1 ELSE 0 END) +
       (CASE WHEN primary_2018 = 'Y' THEN 1 ELSE 0 END) +
       (CASE WHEN primary_2016 = 'Y' or presidential_primary_2016 = 'Y' THEN 1 ELSE 0 END)) as primary_vote_count"
            )
            
            query <- paste(
              "SELECT %s",
              ", (CASE WHEN party_hard = 'Dem Primary' OR party_hard = 'Dem Non-Primary' THEN 'Democratic' WHEN party_hard = 'GOP Primary' OR party_hard = 'GOP Non-Primary' THEN 'Republican' WHEN party_hard = 'Unknown' THEN 'Unknown' END) as party_hard, party_hard as party_hard_original",
              "FROM v3 as v",
              "WHERE v.lalvoterid IN (%s)"
            )
            
            query <- sprintf(
              query,
              paste(cols, collapse = ", "),
              paste(sprintf("'%s'", survey$lalvoterid), collapse = ", ")
            )
            
            ids <- paste(sprintf("'%s'", survey[["lalvoterid"]]), collapse = ", ")
            res <- EchelonSurveyTools::presto_get(query, "PostgreSQL")
            
            
            ids <- paste(sprintf("'%s'", survey[["lalvoterid"]]), collapse = ", ")
            turnout_2024 <- EchelonSurveyTools::presto_get(
              sprintf("SELECT lalvoterid, v_model.score as score FROM v_model WHERE lalvoterid IN (%s) AND model = 'turnout_general_2024'", ids),
              "PostgreSQL"
            )
            
            res$turnout_2024_score <- setNames(turnout_2024$score, turnout_2024$lalvoterid)[res$lalvoterid]
            res$turnout_2024_score <- replace(res$turnout_2024_score, is.na(res$turnout_2024_score), median(res$turnout_2024_score, na.rm = TRUE))
            write.csv(res, file_path, row.names = FALSE)
            
            
            
          } #END retrieve vf appends
          res <- readr::read_csv(file_path)
          res[["lalvoterid"]] <- as.character(res[["lalvoterid"]])
          survey[["lalvoterid"]] <- as.character(survey[["lalvoterid"]])
          
          incProgress(0.1, detail = "Cleaning and processing data, aligning columns with weighting variable names...")
          
          if ("ballottype_general_2020" %in% weights[["category"]]){
            res$ballottype_general_2020 <- replace(res$ballottype_general_2020, is.na(res$ballottype_general_2020), "Did not vote")
            res$ballottype_general_2020 <- paste("Voted", res$ballottype_general_2020)
            res$ballottype_general_2020 <- replace(
              res$ballottype_general_2020,
              res$ballottype_general_2020 == "Voted Did not vote",
              "Did not vote"
            )
          }
          
          common_cols <- intersect(colnames(survey), colnames(res))
          common_cols <- common_cols[common_cols!="lalvoterid"]
          survey <- dplyr::select(survey, -any_of(c(common_cols)))
          
          survey <- dplyr::left_join(survey, res, by = "lalvoterid")
        }
        
        
        if ("QGender" %in% weights[["category"]]){
          weights[weights[["category"]]=="QGender",][["category"]] <- "Gender"
        }
        
        if ("Gender" %in% weights[["category"]]){
          current_attr <- weights[weights[["category"]]=="Gender",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Male", "Female")))){
            survey[["Gender"]] <- sjlabelled::as_character(survey[["QGender"]])
            survey[["Gender"]] <- dplyr::recode(survey[["Gender"]],
                                                "Man"="Male", "Woman"="Female", "In some other way"="Female",
                                                "Some other way"="Female")
          } else if (identical(sort(current_attr), sort(c("Man", "Woman")))){
            survey[["Gender"]] <- sjlabelled::as_character(survey[["QGender"]])
            survey[["Gender"]] <- dplyr::recode(survey[["Gender"]],
                                                "Male"="Man", "Female"="Woman", "In some other way"="Woman", "Some other way"="Woman")
          } else if (identical(sort(current_attr), sort(c("Men", "Women")))){
            survey[["Gender"]] <- sjlabelled::as_character(survey[["QGender"]])
            survey[["Gender"]] <- dplyr::recode(survey[["Gender"]],
                                                "Man"="Men", "Woman"="Women", "In some other way"="Women",
                                                "Male"="Men", "Female"="Women", "Some other way"="Women")
          } else{
            stop("Error: Set of attributes for Gender weight not recognized")
          }
        }
        
        
        if ("Age" %in% weights[["category"]]){
          current_attr <- weights[weights[["category"]]=="Age",][["attribute"]]
          current_attr <- sort(current_attr)
          lowerbound <- as.numeric(stringr::str_trim(sapply(stringr::str_split(current_attr, "-"), "[", 1))[1])
          upperbound <- as.numeric(stringr::str_trim(sapply(stringr::str_split(current_attr, "-"), "[", 2)))
          upperbound <- ifelse(is.na(upperbound), Inf, upperbound)
          endpoints <- c(lowerbound, upperbound)
          
          survey[["Age"]] <- EchelonSurveyTools::easy_bin(survey[["QAge"]], endpoints)
        }
        
        
        if ("Gender X Age" %in% weights[["category"]]){
          current_attr <- weights[weights[["category"]]=="Gender X Age",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Men Under 50", "Women Under 50", "Men 50+", "Women 50+")))){
            gender <- ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Man", "Male"), "Men", "Women")
            age <- ifelse(survey[["QAge"]] < 50, "Under 50", "50+")
            survey[["Gender X Age"]] <- paste(gender, age)
          }
          
          if (identical(sort(current_attr), sort(c("Men Under 40", "Women Under 40", "Men 40+", "Women 40+")))){
            gender <- ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Man", "Male"), "Men", "Women")
            age <- ifelse(survey[["QAge"]] < 40, "Under 40", "40+")
            survey[["Gender X Age"]] <- paste(gender, age)
          }
        }
        
        if ("Region" %in% weights[["category"]]){
          current_attr <- unique(weights[weights[["category"]]=="Region",][["attribute"]])
          if (identical(sort(current_attr), sort(c("Northeast", "Midwest", "South", "West")))){
            region <- read.csv("Test-Data/region_state_match.csv")
            region <- setNames(region[["Region"]], region[["State"]])
            region <- append(region, c("Washington DC" = "SOUTH"))
            region <- append(region, c("Washington, D C " = "SOUTH"))
            region <- append(region, c("Washington, D C" = "SOUTH"))
            survey[["Region"]] <- EchelonSurveyTools::capitalize(
              tolower(
                region[sjlabelled::as_character(survey[["QState"]])]
              )
            )
          } else if (identical(sort(current_attr), sort(c("WC", "Rural WWC", "Urban WWC", "Nonwhite")))){
            res <- read.csv("data/external/combined-political-geographies.csv", sep="\t")
            res$count <- 1
            
            geo <- read.csv("./data/external/political-regions.csv")
            geo <- geo[geo[["State"]] %in% unique(substr(survey$lalvoterid,4,5)), ]
            geo <- dplyr::select(geo, any_of(c("ID", "Category")))
            
            res <- res[order(res$count, decreasing=TRUE),]
            # res <- res[order(res$l2_county, decreasing=TRUE),]
            # res <- res[!duplicated(paste(res$l2_county, res$l2_precinct)),]
            
            res <- dplyr::left_join(res, geo, by=c("id"="ID"))
            res <- dplyr::select(res, any_of(c("l2_county", "l2_precinct", "Category")))
            colnames(res) <- c("county", "precinct", "Region")
            
            survey <- dplyr::select(survey, -any_of(c("Region")))
            survey <- dplyr::left_join(survey, res, by=c("county"="county", "precinct"="precinct"))
            
            res <- res[!duplicated(paste(res$county)),]
            testing <- dplyr::left_join(dplyr::select(survey, any_of(c("county"))), res, by=c("county"="county"))[["Region"]]
            
            survey$Region <- ifelse(is.na(survey$Region), testing, survey$Region)
          } else{
            stop("Error: Set of attributes for Region not recognized")
          }
          
          if (sum(is.na(survey$Region)) > 0){
            stop("Error: Null values for Region")
          }
        }
        
        
        
        if (("region_metro" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="region_metro",][["attribute"]]
          if (identical(sort(current_attr), sort(c("South Metro", "South Non-Metro", "Northeast Metro", "Northeast Non-Metro",
                                                   "West Metro", "West Non-Metro", "Midwest Metro", "Midwest Non-Metro")))){
            
            incProgress(0.1, detail = "Handling the 'region_metro' weight (est. 10 seconds)...")
            region <- read.csv("Test-Data/region_state_match.csv")
            region <- setNames(region[["Region"]], region[["State"]])
            region <- append(region, c("Washington DC" = "SOUTH"))
            region <- append(region, c("Washington, D C " = "SOUTH"))
            region <- append(region, c("Washington, D C" = "SOUTH"))
            survey[["Region"]] <- EchelonSurveyTools::capitalize(
              tolower(
                region[sjlabelled::as_character(survey[["QState"]])]
              )
            )
            
            shp <- sf::st_read(
              "Test-Data/tl_2019_us_cbsa/tl_2019_us_cbsa.shp"
            )
            
            x <- sf::st_as_sf(
              survey[survey$Vlong != "", c("Vrid", "Vlong", "Vlat")],
              coords = c("Vlong", "Vlat"),
              crs = sf::st_crs(shp),
              agr = "constant",
              stringsAsFactors = FALSE,
              remove = TRUE
            )
            
            x <- sf::st_join(x, shp[, c("geometry", "NAME", "MEMI")], join = sf::st_within)
            
            survey$MEMI <- setNames(x$MEMI, x$Vrid)[as.character(survey$Vrid)]
            survey[["MEMI"]] <- replace(
              survey[["MEMI"]],
              is.na(survey[["MEMI"]]),
              2
            )
            
            
            zipfips <- readxl::read_xlsx("Test-Data/ZIP_COUNTY_122021.xlsx")
            zipfips <- zipfips[!duplicated(zipfips$zip), ]
            
            # survey[["QZip"]] <- sprintf("%05d", survey[["QZip"]])
            
            survey$fips <- setNames(
              zipfips$county,
              zipfips$zip
            )[survey[["QZip"]]]
            
            msa <- readxl::read_xlsx(
              "Test-Data/qcew-county-msa-csa-crosswalk.xlsx",
              sheet = 3
            )
            
            msa[["fips"]] <- sprintf("%05d", msa[["County Code"]])
            msa[["metro"]] <- ifelse(
              grepl("MSA", msa[["MSA Title"]]),
              "Metro",
              "Non-Metro"
            )
            msa <- setNames(msa[["metro"]], msa[["fips"]])
            survey$metro <- msa[survey$fips]
            
            # Add extra.
            fips <- EchelonSurveyTools::presto_get(
              paste(
                "SELECT state, voters_fips, residence_addresses_zip, count(*) FROM v",
                "GROUP BY state, voters_fips, residence_addresses_zip"
              ),
              "PostgreSQL"
            )
            
            # NOTE: Redshift order isn't consistent so we need to sort this on something
            # to get the same answer every time.
            fips <- fips[order(fips$count, decreasing = TRUE), ]
            
            state_codes <- read.csv(
              "https://www2.census.gov/geo/docs/reference/state.txt?#",
              sep = "|"
            )
            
            state_codes <- setNames(
              sprintf("%02d", state_codes[["STATE"]]),
              state_codes[["STUSAB"]]
            )
            
            fips[["fips"]] <- paste0(
              state_codes[fips[["state"]]],
              fips[["voters_fips"]]
            )
            
            fips[["zip"]] <- sprintf("%05d", fips[["residence_addresses_zip"]])
            
            fips <- fips[fips$zip %in% survey$QZip[is.na(survey$metro)], ]
            fips <- fips[fips$fips %in% names(msa), ]
            
            survey[["metro"]] <- ifelse(
              !is.na(survey[["metro"]]),
              survey[["metro"]],
              setNames(msa[fips$fips], fips$zip)[survey$QZip]
            )
            
            survey[["metro"]] <- ifelse(
              !is.na(survey[["metro"]]),
              survey[["metro"]],
              ifelse(survey[["MEMI"]] == "1", "Metro", "Non-Metro")
            )
            
            survey[["region_metro"]] <- paste(survey[["Region"]], survey[["metro"]])
            
            for (varname in c("South Metro", "South Non-Metro", "Northeast Metro", "Northeast Non-Metro",
                              "West Metro", "West Non-Metro", "Midwest Metro", "Midwest Non-Metro")){
              if (varname %in% survey[["region_metro"]]==FALSE){
                weights <- weights[(weights[["category"]] != "region_metro") | (weights[["attribute"]] != varname),]
              }
            }
            
            
          } else{
            stop("Error: Set of attributes for region_metro not recognized")
          }
        }
        
        
        if ("DMA" %in% weights[["category"]]){
          current_attr <- weights[weights[["category"]]=="DMA",][["attribute"]]
          newlabels <- current_attr
          
          survey$DMA <- ifelse(survey$designated_market_area_dma %in% current_attr,
                               survey$designated_market_area_dma, "OTHER")
          survey$DMA <- sjlabelled::as_labelled(sjlabelled::as_numeric(factor(
            survey$DMA, levels=newlabels
          )))
        }
        
        
        if ("turnout_2024_bins" %in% weights[["category"]]){
          labs <- weights[weights[["category"]]=="turnout_2024_bins",][["attribute"]]
          bins <- as.numeric(
            unique(
              unlist(
                strsplit(labs, " - ")
              )
            )
          )
          
          survey[["turnout_2024_bins"]] <- cut(survey[["turnout_2024_score"]] * 100, bins, labs)
        }
        
        if ("turnout_2024_bins_noreg" %in% weights[["category"]]){
          labs <- weights[weights[["category"]]=="turnout_2024_bins_noreg" & weights[["attribute"]]!="Not Registered Before 2020",][["attribute"]]
          bins <- as.numeric(
            unique(
              unlist(
                strsplit(labs, " - ")
              )
            )
          )
          
          survey[["turnout_2024_bins"]] <- cut(survey[["turnout_2024_score"]] * 100, bins, labs)
          
          survey[["registered_before_2020"]] <- ifelse(
            survey$voters_calculatedregdate > "2020-11-03",
            "Not Registered Before 2020",
            "Registered Before 2020"
          )
          survey$turnout_2024_bins <- sjlabelled::as_character(survey$turnout_2024_bins)
          survey$turnout_2024_bins_noreg <- replace(
            survey$turnout_2024_bins,
            survey$registered_before_2020 == "Not Registered Before 2020",
            "Not Registered Before 2020"
          )
          
        }
        
        if ("party_hard" %in% weights[["category"]]){
          current_attr <- weights[weights[["category"]]=="party_hard",][["attribute"]]
          if (all(unique(survey[["party_hard_original"]]) %in% c("Dem Primary", "Dem Non-Primary", "GOP Primary", "GOP Non-Primary", "Unknown"))){
            weights[weights[["category"]]=="party_hard",][["category"]] <- "party_hard_original"
            
            newlabels <- current_attr
            survey$party_hard_original <- ifelse(survey$party_hard_original %in% current_attr,
                                                 survey$party_hard_original, "OTHER")
            survey$party_hard_original <- sjlabelled::as_labelled(sjlabelled::as_numeric(factor(
              survey$party_hard_original, levels=newlabels
            )))
          } else{
            stop("Error: Set of attributes for party_hard_original not recognized")
          }
        }
        
        
        if ("Race" %in% weights[["category"]]){
          qrace <- survey[, grepl("QRace_", colnames(survey))]
          qrace <- dplyr::select(qrace, -any_of(c("QRace_OE")))
          qrace <- dplyr::select(qrace, -any_of(c("QRace_other")))
          
          hispanic_colname <- names(qrace)[sapply(qrace, function(col) any(grepl("ispani", attr(col, "label"))))]
          hispanic <- qrace[[hispanic_colname]] == 1
          one_race_only <- rowSums(qrace) == 1
          
          labs <- stringr::str_trim(sapply(strsplit(sjlabelled::get_label(qrace), ":"), "[[", 1))
          qrace_single <- vector("list", length = length(labs))
          
          for (i in seq_len(length(labs))) {
            res <- qrace[[i]] == 1
            res <- res & one_race_only
            qrace_single[[i]] <- res
          }
          
          qrace_single <- setNames(qrace_single, labs)
          
          qrace_rolled <- rep("Mixed Race", nrow(survey))
          for (name in names(qrace_single)) {
            qrace_rolled <- ifelse(qrace_single[[name]], name, qrace_rolled)
          }
          
          qrace_rolled <- ifelse(survey[[hispanic_colname]] == 1, "Hispanic/Latino", qrace_rolled)
          
          # qrace_rolled <- ifelse((survey[["QRace_4"]]==1 & survey[["QRace_6"]]==1) & (survey[["QRace_1"]]==0 & survey[["QRace_2"]]==0 & survey[["QRace_3"]]==0 & survey[["QRace_5"]]==0 & survey[["QRace_7"]]==0),
          #                       "Asian", qrace_rolled)
          
          current_race_options <- sort(unique(qrace_rolled))
          current_white <- current_race_options[grepl("hite",current_race_options)]
          current_black <- current_race_options[grepl("lack",current_race_options)]
          current_hispanic <- current_race_options[grepl("ispanic",current_race_options)]
          current_asian <- current_race_options[grepl("Asian|awaiian|acific",current_race_options)]
          current_other <- current_race_options[grepl("Native A|Alaska|race or eth|Mixed|Something|Prefer",current_race_options)]
          
          weight_race <- weights[weights[["category"]]=="Race",][["attribute"]]
          weight_white <- weight_race[grepl("hite",weight_race)]
          weight_black <- weight_race[grepl("lack",weight_race)]
          weight_hispanic <- weight_race[grepl("ispanic",weight_race)]
          weight_asian <- weight_race[grepl("Asian|awaiian|acific",weight_race)]
          weight_other <- weight_race[grepl("Native A|Alaska|Other|Mixed|Something|Prefer",weight_race)]
          
          weighttest <- list(weight_white, weight_black, weight_hispanic, weight_asian, weight_other)
          
          detected_current_options <- c(current_white, current_black, current_hispanic, current_asian, current_other)
          if (identical(current_race_options, sort(detected_current_options))==FALSE){
            stop("Error: Race options detection")
          }
          
          if (all(sapply(weighttest, length)==1)==FALSE){
            stop("Error: Race weight attribute detection")
          }
          
          qrace_rolled <- ifelse(qrace_rolled %in% current_white, weight_white, qrace_rolled)
          qrace_rolled <- ifelse(qrace_rolled %in% current_black, weight_black, qrace_rolled)
          qrace_rolled <- ifelse(qrace_rolled %in% current_hispanic, weight_hispanic, qrace_rolled)
          qrace_rolled <- ifelse(qrace_rolled %in% current_asian, weight_asian, qrace_rolled)
          qrace_rolled <- ifelse(qrace_rolled %in% current_other, weight_other, qrace_rolled)
          
          qrace_rolled_levels <- c(
            weight_black,
            weight_white,
            weight_asian,
            weight_other,
            weight_hispanic
          )
          survey[["Race"]] <- sjlabelled::as_labelled(factor(
            x = qrace_rolled,
            levels = qrace_rolled_levels,
            labels = qrace_rolled_levels
          ))
          
        }
        
        if ("QEducation" %in% colnames(survey)){
          educ_labels <- sjlabelled::get_labels(survey$QEducation)
          bachIndex <- grep("bachelor", educ_labels, ignore.case = TRUE)
          preferNotToSay <- grep("Prefer not|Don't k", educ_labels, ignore.case = TRUE)
          
          if (length(bachIndex) != 1){
            stop("Error: Education option detection")
          }
          noncollege_options <- c(educ_labels[1:bachIndex-1], educ_labels[preferNotToSay])
          
          is_college <- ifelse(sjlabelled::as_character(survey$QEducation) %in% noncollege_options, "Non-College", "College")
        }
        
        if (("Race_Educ" %in% weights[["category"]]) & ("QEducation" %in% colnames(survey))){
          current_attr <- weights[weights[["category"]]=="Race_Educ",][["attribute"]]
          if (identical(sort(current_attr), sort(c("White College", "White Non-College", "Non-White College", "Non-White Non-College")))){
            is_white <- ifelse(survey$Race==2, "White", "Non-White")
            survey[["Race_Educ"]] <- paste(is_white, is_college)
          } else{
            stop("Error: Set of attributes for Race_Educ not recognized")
          }
        }
        
        if (("Gender_Educ" %in% weights[["category"]]) & ("QEducation" %in% colnames(survey))){
          current_attr <- weights[weights[["category"]]=="Gender_Educ",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Men College", "Men Non-College", "Women College", "Women Non-College")))){
            gender <- ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Man", "Men", "Male"),
                             "Men", ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Woman", "Women", "Female"), "Women", "Women"))
            survey[["Gender_Educ"]] <- paste(gender, is_college)
          } else{
            stop("Error: Set of attributes for Gender_Educ not recognized")
          }
        }
        
        if (("general_2020" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="general_2020",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Voted", "Did not vote")))){
            survey[["general_2020"]] <- ifelse((!is.na(survey[["general_2020"]])) & survey[["general_2020"]]=="Y",
                                               "Voted", "Did not vote")
          } else{
            stop("Error: Set of attributes for general_2020 not recognized")
          }
        }
        
        if (("Age_Educ" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Age_Educ",][["attribute"]]
          if (identical(sort(current_attr), sort(c("50+ College", "50+ Non-College", "Under 50 College", "Under 50 Non-College")))){
            overUnder50 <- ifelse(survey$QAge < 50, "Under 50", "50+")
            survey[["Age_Educ"]] <- paste(overUnder50, is_college)
          } else if (identical(sort(current_attr), sort(c("Non-College Under 45", "College Under 45", "Non-College 45+", "College 45+")))){
            overUnder45 <- ifelse(survey$QAge < 45, "Under 45", "45+")
            survey[["Age_Educ"]] <- paste(is_college, overUnder45)
          } else{
            stop("Error: Set of attributes for Age_Educ not recognized")
          }
        }
        
        if (("QOwnHomeConsolidated" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="QOwnHomeConsolidated",][["attribute"]]
          if ((identical(sort(current_attr), sort(c("Own", "Not Own")))) & ("Own" %in% sjlabelled::get_labels(survey[["QOwnHome"]]))){
            survey[["QOwnHomeConsolidated"]] <- ifelse(sjlabelled::as_character(survey[["QOwnHome"]])=="Own", "Own", "Not Own")
          } else{
            stop("Error: Set of attributes for QOwnHomeConsolidated not recognized")
          }
        }
        
        if (("Gender_PostFreq" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Gender_PostFreq",][["attribute"]]
          if ((identical(sort(current_attr), sort(c("Men A few times a month or more", "Women A few times a month or more", "Less often / DK")))) &
              (sjlabelled::get_labels(survey$QFreq_PostOnline)[3]=="A few times a month")){
            gender <- ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Man", "Men", "Male"),
                             "Men", ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Woman", "Women", "Female"), "Women", "Women"))
            survey[["Gender_PostFreq"]] <- paste(gender, ifelse(survey[["QFreq_PostOnline"]] <= 3,
                                                                "A few times a month or more",
                                                                "Less often / DK"
            ))
            survey$Gender_PostFreq <- ifelse(
              grepl("Less often / DK", survey[["Gender_PostFreq"]]),
              "Less often / DK",
              survey$Gender_PostFreq
            )
          } else{
            stop("Error: Set of attributes for Gender_PostFreq not recognized")
          }
        }
        
        
        
        if (("college_age" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="college_age",][["attribute"]]
          if (identical(sort(current_attr), sort(c("65+ College", "65+ Non-College", "Under 65 College", "Under 65 Non-College")))){
            overUnder65 <- ifelse(survey$QAge < 65, "Under 65", "65+")
            survey[["college_age"]] <- paste(overUnder65, is_college)
          } else{
            stop("Error: Set of attributes for college_age not recognized")
          }
        }
        
        if (("Q2020BallotConsolidated" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Q2020BallotConsolidated",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Donald Trump", "Joe Biden", "Others", "Didn't Vote", "Unsure")))){
            survey[["Q2020BallotConsolidated"]] <- sjlabelled::as_character(survey[["Q2020Ballot"]])
            survey[["Q2020BallotConsolidated"]] <- dplyr::recode(survey[["Q2020BallotConsolidated"]],
                                                                 "Jo Jorgensen"="Others",
                                                                 "Howie Hawkins"="Others",
                                                                 "Kanye West"="Others",
                                                                 "Someone else"="Others",
                                                                 "I did not vote"="Didn't Vote",
                                                                 "Unsure"="Unsure")
          } else if (identical(sort(current_attr), sort(c("Donald Trump", "Joe Biden", "Someone else", "Didnt vote/Unsure")))){
            survey[["Q2020BallotConsolidated"]] <- sjlabelled::as_character(survey[["Q2020Ballot"]])
            survey[["Q2020BallotConsolidated"]] <- dplyr::recode(survey[["Q2020BallotConsolidated"]],
                                                                 "Jo Jorgensen"="Someone else",
                                                                 "Howie Hawkins"="Someone else",
                                                                 "Kanye West"="Someone else",
                                                                 "Someone else"="Someone else",
                                                                 "I did not vote"="Didnt vote/Unsure",
                                                                 "Don’t know/Refused [DNR]"="Didnt vote/Unsure")
          } else{
            stop("Error: Set of attributes for Q2020BallotConsolidated not recognized")
          }
        }
        
        if (("Q2024BallotConsolidated" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Q2024BallotConsolidated",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Donald Trump", "Kamala Harris", "Other", "I did not vote/Not registered")))){
            survey[["Q2024BallotConsolidated"]] <- sjlabelled::as_character(survey[["Q2024Ballot"]])
            survey[["Q2024BallotConsolidated"]] <- ifelse(is.na(survey[["Q2024BallotConsolidated"]]), "I did not vote/Not registered", survey[["Q2024BallotConsolidated"]])
            survey[["Q2024BallotConsolidated"]] <- dplyr::recode(survey[["Q2024BallotConsolidated"]],
                                                                 "Jo Jorgensen"="Other",
                                                                 "Howie Hawkins"="Other",
                                                                 "Kanye West"="Other",
                                                                 "Someone else"="Other",
                                                                 "I did not vote"="I did not vote/Not registered",
                                                                 "Unsure"="I did not vote/Not registered",
                                                                 "I did not vote for this office"="I did not vote/Not registered")
          } else if (identical(sort(current_attr), sort(c("Donald Trump", "Kamala Harris", "Others", "Unsure", "Didn't Vote")))){
            if (identical(sort(sjlabelled::get_labels(survey[["Q2024Ballot"]])), sort(c("Donald Trump", "Kamala Harris", "Someone else")))){
              survey[["Q2024BallotConsolidated"]] <- sjlabelled::as_character(survey[["Q2024Ballot"]])
              survey[["Q2024BallotConsolidated"]] <- ifelse(is.na(survey[["Q2024BallotConsolidated"]]), "Unsure/Didn't Vote", survey[["Q2024BallotConsolidated"]])
              survey[["Q2024BallotConsolidated"]] <- dplyr::recode(survey[["Q2024BallotConsolidated"]],
                                                                   "Someone else"="Others",
                                                                   "Howie Hawkins"="Others",
                                                                   "Kanye West"="Others",
                                                                   "Someone else"="Others")
              unsure_novote <- weights[weights[["category"]]=="Q2024BallotConsolidated" & weights[["attribute"]] %in% c("Unsure", "Didn't Vote"),]
              weights <- weights[(weights[["category"]]!="Q2024BallotConsolidated") | (weights[["attribute"]] %in% c("Unsure", "Didn't Vote")==FALSE),]
              weights <- rbind(weights, data.frame(category="Q2024BallotConsolidated", attribute="Unsure/Didn't Vote", count=sum(unsure_novote[["count"]])))
            } else if (identical(sort(sjlabelled::get_labels(survey[["Q2024Ballot"]])), sort(c("Donald Trump", "Kamala Harris", "Someone else", "I did not vote for this office")))){
              survey[["Q2024BallotConsolidated"]] <- sjlabelled::as_character(survey[["Q2024Ballot"]])
              survey[["Q2024BallotConsolidated"]] <- ifelse(is.na(survey[["Q2024BallotConsolidated"]]), "Unsure/Didn't Vote", survey[["Q2024BallotConsolidated"]])
              survey[["Q2024BallotConsolidated"]] <- dplyr::recode(survey[["Q2024BallotConsolidated"]],
                                                                   "I did not vote for this office"="Unsure/Didn't Vote",
                                                                   "Someone else"="Others",
                                                                   "Howie Hawkins"="Others",
                                                                   "Kanye West"="Others",
                                                                   "Someone else"="Others")
              unsure_novote <- weights[weights[["category"]]=="Q2024BallotConsolidated" & weights[["attribute"]] %in% c("Unsure", "Didn't Vote"),]
              weights <- weights[(weights[["category"]]!="Q2024BallotConsolidated") | (weights[["attribute"]] %in% c("Unsure", "Didn't Vote")==FALSE),]
              weights <- rbind(weights, data.frame(category="Q2024BallotConsolidated", attribute="Unsure/Didn't Vote", count=sum(unsure_novote[["count"]])))
            } else{
              stop("Error: Set of labels for Q2024Ballot not recognized")
            }
          } else{
            stop("Error: Set of attributes for Q2024BallotConsolidated not recognized")
          }
        }
        
        if (("Q2020BallotConsolidated_general2020" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Q2020BallotConsolidated_general2020",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Donald Trump", "Joe Biden", "Others", "Didn't Vote")))){
            survey[["Q2020BallotConsolidated_general2020"]] <- sjlabelled::as_character(survey[["Q2020Ballot"]])
            survey[["Q2020BallotConsolidated_general2020"]] <- dplyr::recode(survey[["Q2020BallotConsolidated_general2020"]],
                                                                             "Jo Jorgensen"="Others",
                                                                             "Howie Hawkins"="Others",
                                                                             "Kanye West"="Others",
                                                                             "Someone else"="Others",
                                                                             "I did not vote"="Others",
                                                                             "Unsure"="Others")
            
            if ("Y" %in% survey$general_2020){
              survey[["general_2020"]] <- ifelse((!is.na(survey[["general_2020"]])) & survey[["general_2020"]]=="Y",
                                                 "Voted", "Did not vote")
            }
            survey[["Q2020BallotConsolidated_general2020"]] <- ifelse(survey[["general_2020"]]=="Voted",
                                                                      survey[["Q2020BallotConsolidated_general2020"]], "Didn't Vote")
          } else{
            stop("Error: Set of attributes for Q2020BallotConsolidated not recognized")
          }
        }
        
        
        
        if (("Q2020Ballot_Gender" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Q2020Ballot_Gender",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Donald Trump/Male", "Donald Trump/Female", "Joe Biden/Male",
                                                   "Joe Biden/Female", "Other/Didn't vote/Unsure")))){
            
            ballot2020 <- sjlabelled::as_character(survey[["Q2020Ballot"]])
            gender <- ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Man", "Men", "Male"),
                             "Male", ifelse(sjlabelled::as_character(survey[["QGender"]]) %in% c("Woman", "Women", "Female"), "Female", "Other"))
            
            survey[["Q2020Ballot_Gender"]] <- paste(ballot2020, gender, sep = "/")
            survey[["Q2020Ballot_Gender"]] <- ifelse(survey[["Q2020Ballot_Gender"]] %in% c(
              "Joe Biden/Female", "Joe Biden/Male",
              "Donald Trump/Female", "Donald Trump/Male"
            ),
            survey[["Q2020Ballot_Gender"]], "Other/Didn't vote/Unsure")
          } else{
            stop("Error: Set of attributes for Q2020Ballot_Gender not recognized")
          }
        }
        
        if (("QPartyConsolidated" %in% weights[["category"]])){
          if (all(c("QPartyID", "QPartyDem", "QPartyGOP", "QPartyLeanView") %in% colnames(survey))==FALSE){
            stop("Error: party columns for QPartyConsolidated weight not detected")
          }
          
          current_attr <- weights[weights[["category"]]=="QPartyConsolidated",][["attribute"]]
          if (identical(sort(current_attr), sort(c("Republican", "Independent", "Democrat", "A member of another party", "Unsure")))){
            current_party <- weights[weights[["category"]]=="QPartyConsolidated",]
            ind_sum <- sum(current_party[current_party[["attribute"]] %in% c("Independent", "A member of another party", "Unsure"),][["count"]])
            current_party <- current_party[current_party[["attribute"]] %in% c("Republican", "Independent", "Democrat"),]
            current_party[current_party[["attribute"]]=="Independent",][["count"]] <- ind_sum
            weights <- weights[weights[["category"]]!= "QPartyConsolidated",]
            weights <- rbind(weights, current_party)
            
            partylean_consolidate <- ifelse(
              survey[["QPartyLeanView"]] == 1, "Republican", ifelse(
                survey[["QPartyLeanView"]] == 2, "Democrat", ifelse(
                  survey[["QPartyLeanView"]] == 3, "Independent", NA
                )
              )
            )
            
            party_consolidate <- ifelse(
              survey[["QPartyID"]] == 1, "Republican", ifelse(
                survey[["QPartyID"]] == 2, "Democrat", ifelse(
                  survey[["QPartyID"]] == 3, "Independent",
                  sjlabelled::as_character(survey[["QPartyID"]])
                )
              )
            )
            
            survey[["QPartyConsolidated"]] <- ifelse(is.na(partylean_consolidate), party_consolidate, partylean_consolidate)
            
            survey[["QPartyConsolidated"]] <- sjlabelled::as_labelled(factor(
              x = survey[["QPartyConsolidated"]],
              levels = c("Republican", "Independent", "Democrat", "A member of another party", "Unsure")
            ))
            
            survey$QPartyConsolidated <- replace(
              survey$QPartyConsolidated,
              is.na(survey$QPartyConsolidated),
              5
            )
            
            survey$QPartyConsolidated <- replace(
              survey$QPartyConsolidated,
              survey$QPartyConsolidated > 3,
              2
            )
            
          } else if (identical(sort(current_attr), sort(c("Republican", "Independent", "Democrat")))){
            partylean_consolidate <- ifelse(
              survey[["QPartyLeanView"]] == 1, "Republican", ifelse(
                survey[["QPartyLeanView"]] == 2, "Democrat", ifelse(
                  survey[["QPartyLeanView"]] == 3, "Independent", NA
                )
              )
            )
            
            party_consolidate <- ifelse(
              survey[["QPartyID"]] == 1, "Republican", ifelse(
                survey[["QPartyID"]] == 2, "Democrat", ifelse(
                  survey[["QPartyID"]] == 3, "Independent",
                  sjlabelled::as_character(survey[["QPartyID"]])
                )
              )
            )
            
            survey[["QPartyConsolidated"]] <- ifelse(is.na(partylean_consolidate), party_consolidate, partylean_consolidate)
            
            survey[["QPartyConsolidated"]] <- sjlabelled::as_labelled(factor(
              x = survey[["QPartyConsolidated"]],
              levels = c("Republican", "Independent", "Democrat", "A member of another party", "Unsure")
            ))
            
            survey$QPartyConsolidated <- replace(
              survey$QPartyConsolidated,
              is.na(survey$QPartyConsolidated),
              5
            )
            
            survey$QPartyConsolidated <- replace(
              survey$QPartyConsolidated,
              survey$QPartyConsolidated > 3,
              2
            )
          } else if (identical(sort(current_attr), sort(c("Republican", "Independent", "Democratic")))){
            partylean_consolidate <- ifelse(
              survey[["QPartyLeanView"]] == 1, "Republican", ifelse(
                survey[["QPartyLeanView"]] == 2, "Democratic", ifelse(
                  survey[["QPartyLeanView"]] == 3, "Independent", NA
                )
              )
            )
            
            party_consolidate <- ifelse(
              survey[["QPartyID"]] == 1, "Republican", ifelse(
                survey[["QPartyID"]] == 2, "Democratic", ifelse(
                  survey[["QPartyID"]] == 3, "Independent",
                  sjlabelled::as_character(survey[["QPartyID"]])
                )
              )
            )
            
            survey[["QPartyConsolidated"]] <- ifelse(is.na(partylean_consolidate), party_consolidate, partylean_consolidate)
            
            survey[["QPartyConsolidated"]] <- sjlabelled::as_labelled(factor(
              x = survey[["QPartyConsolidated"]],
              levels = c("Republican", "Independent", "Democratic", "A member of another party", "Unsure")
            ))
            
            survey$QPartyConsolidated <- replace(
              survey$QPartyConsolidated,
              is.na(survey$QPartyConsolidated),
              5
            )
            
            survey$QPartyConsolidated <- replace(
              survey$QPartyConsolidated,
              survey$QPartyConsolidated > 3,
              2
            )
          } else{
            stop("Error: Set of attributes for QPartyConsolidated not recognized")
          }
        }
        
        if (("Education" %in% weights[["category"]])){
          current_attr <- weights[weights[["category"]]=="Education",][["attribute"]]
          educ_labels <- sjlabelled::get_labels(survey$QEducation)
          if (identical(sort(current_attr), sort(c("High school or less", "Some college", "Bachelor's degree",
                                                   "Graduate degree", "Prefer not to say")))){
            if (identical(sort(educ_labels), sort(c("High school or less", "Some college", "Associate degree",
                                                    "Bachelor’s degree", "Graduate degree", "Prefer not to say")))){
              educ <- survey[["QEducation"]]
              attr(educ, "labels") <- setNames(
                as.numeric(seq_len(6)),
                c(
                  "High school or less",
                  "Some college",
                  "Some college",
                  "Bachelor's degree",
                  "Graduate degree",
                  "Prefer not to say"
                )
              )
              
              survey[["Education"]] <- educ
              if (sum(sjlabelled::as_character(survey$Education)=="Prefer not to say")==0){
                weights <- weights[(weights[["category"]]!="Education") | (weights[["attribute"]]!="Prefer not to say"),]
              }
            } else if (identical(sort(educ_labels), sort(c("High school or less", "Some college", "Associate degree",
                                                           "Bachelor’s degree", "Graduate degree", "Don't know/Refused [DNR]")))){
              educ <- survey[["QEducation"]]
              attr(educ, "labels") <- setNames(
                as.numeric(seq_len(6)),
                c(
                  "High school or less",
                  "Some college",
                  "Some college",
                  "Bachelor's degree",
                  "Graduate degree",
                  "Prefer not to say"
                )
              )
              
              survey[["Education"]] <- educ
              if (sum(sjlabelled::as_character(survey$Education)=="Prefer not to say")==0){
                weights <- weights[(weights[["category"]]!="Education") | (weights[["attribute"]]!="Prefer not to say"),]
              }
            } else{
              stop("Error: Set of labels for QEducation not recognized")
            }
          } else if (identical(sort(current_attr), sort(c("HS or Less", "Some College", "Bachelors",
                                                          "Graduate", "Prefer not to say")))){
            if (identical(sort(educ_labels), sort(c("High school or less", "Some college", "Associate degree",
                                                    "Bachelor’s degree", "Graduate degree", "Prefer not to say")))){
              educ <- survey[["QEducation"]]
              attr(educ, "labels") <- setNames(
                as.numeric(seq_len(6)),
                c(
                  "HS or Less",
                  "Some College",
                  "Some College",
                  "Bachelors",
                  "Graduate",
                  "Prefer not to say"
                )
              )
              
              survey[["Education"]] <- educ
              if (sum(sjlabelled::as_character(survey$Education)=="Prefer not to say")==0){
                weights <- weights[(weights[["category"]]!="Education") | (weights[["attribute"]]!="Prefer not to say"),]
              }
            } else if (identical(sort(educ_labels), sort(c("High school or less", "Some college", "Associate degree",
                                                           "Bachelor’s degree", "Graduate degree", "Don't know/Refused [DNR]")))){
              educ <- survey[["QEducation"]]
              attr(educ, "labels") <- setNames(
                as.numeric(seq_len(6)),
                c(
                  "HS or Less",
                  "Some College",
                  "Some College",
                  "Bachelors",
                  "Graduate",
                  "Prefer not to say"
                )
              )
              
              survey[["Education"]] <- educ
              if (sum(sjlabelled::as_character(survey$Education)=="Prefer not to say")==0){
                weights <- weights[(weights[["category"]]!="Education") | (weights[["attribute"]]!="Prefer not to say"),]
              }
            } else{
              stop("Error: Set of labels for QEducation not recognized")
            }
          } else if (identical(sort(current_attr), sort(c("High school or less", "Some college", "Bachelor's degree",
                                                          "Graduate degree")))){
            if (identical(sort(educ_labels), sort(c("High school or less", "Some college", "Associate degree",
                                                    "Bachelor’s degree", "Graduate degree", "Prefer not to say")))){
              educ <- survey[["QEducation"]]
              attr(educ, "labels") <- setNames(
                as.numeric(seq_len(6)),
                c(
                  "High school or less",
                  "Some college",
                  "Some college",
                  "Bachelor's degree",
                  "Graduate degree",
                  "High school or less"
                )
              )
              
              survey[["Education"]] <- educ
            }
          } else{
            stop("Error: Set of attributes for Education not recognized")
          }
        }
        
        # -----FINAL CUSTOM CODE
        # varnames <- c("QTrumpHarrisFull")
        #
        # testing <- survey[survey[["QPartyID"]]==1 & survey[["QIdeology"]] %in% c(3,4,5),]
        # set.seed(10023)
        # testing <- testing[sample(nrow(testing), 20), ]
        # survey <- survey[survey[["Vrid"]] %in% testing$Vrid==FALSE,]
        #
        #
        # colnames(survey)[colnames(survey)=="var4830"] <- "QTrumpHarrisFull_Voted"
        #
        # for (varname in varnames){
        #   voted_var <- paste0(varname, "_Voted")
        #   leaner_var <- paste0(varname, "Leaner")
        #   combined_varname <- paste0(varname, "CombinedEarlyAndRegularBallot")
        #
        #   currentvars <- dplyr::select(survey, any_of(c(varname, voted_var, leaner_var)))
        #   currentvars <- as.data.frame(apply(currentvars, MARGIN=c(1,2), function(y) ifelse(is.na(y), 0, y)))
        #   combined_var <- sjlabelled::as_labelled(rep(0, nrow(currentvars)))
        #   for (i in 1:(length(attr(survey[[varname]], "labels")) - 1)){
        #     combined_var <- ifelse(currentvars[[voted_var]]==i | currentvars[[leaner_var]]==i | currentvars[[varname]]==i, i, combined_var)
        #   }
        #   combined_var <- ifelse(combined_var==0, length(attr(survey[[varname]], "labels")), combined_var)
        #   attr(combined_var, "labels") <- attr(survey[[varname]], "labels")
        #   combined_var <- sjlabelled::as_labelled(combined_var)
        #   survey[[combined_varname]] <- combined_var
        # }
        #
        #
        # ballotvars <- c("QTrumpHarris")
        # leanervars <- paste0(ballotvars, "Leaner")
        #
        # for (i in 1:length(ballotvars)){
        #   balname <- ballotvars[i]
        #   leanername <- leanervars[i]
        #   combinedname <- paste0(balname, "CombinedEarlyAndRegularBallot")
        #
        #   ballotlabels <- sjlabelled::get_labels(survey[[balname]])
        #   ballotlabel <- attr(survey[[balname]], "label")
        #   leanerlabels <- sjlabelled::get_labels(survey[[leanername]])
        #   leanerlabels <- c(paste("Initially unsure, then chose", leanerlabels[1:2]), leanerlabels[3])
        #   finallabels <- c(ballotlabels[1:2], leanerlabels[1:2], ballotlabels[3:4], leanerlabels[3])
        #   print(combinedname)
        #   print(finallabels)
        #
        #   combinedbal <- ifelse((!is.na(survey[[leanername]])) & survey[[leanername]] %in% c(1,2), paste("Initially unsure, then chose", sjlabelled::as_character(survey[[leanername]])),
        #                         sjlabelled::as_character(survey[[balname]]))
        #   survey[[combinedname]] <- sjlabelled::as_labelled(sjlabelled::as_numeric(factor(combinedbal,
        #                                                                                   levels=finallabels)))
        #
        #   attr(survey[[combinedname]], "label") <- paste0(balname, " + ", leanername, " Combined")
        # }
        #
        #
        # placeholder <- ifelse(is.na(survey$QTrumpHarrisFull_Voted), NA,
        #                       ifelse(survey$QTrumpHarrisFull_Voted==1, 1,
        #                              ifelse(survey$QTrumpHarrisFull_Voted==2, 6, NA)))
        #
        # survey$QTrumpHarrisCombinedEarlyAndRegularBallot <- replace(survey$QTrumpHarrisCombinedEarlyAndRegularBallot, TRUE,
        #                                                             ifelse(is.na(survey$QTrumpHarrisCombinedEarlyAndRegularBallot), placeholder, survey$QTrumpHarrisCombinedEarlyAndRegularBallot))
        #
        #
        
        # ---------------END CUSTOM CODE
        
        # haven::write_sav(setNames(survey, gsub("\\s", "_", colnames(survey))), "../../Partial/trouble-test.sav")
        weights <- weights[!duplicated(paste(weights$category, weights$attribute)),]
        
        if (!is.null(input$survey_id) && input$survey_id != "" && as.character(input$survey_id)=="8093690"){
          to_remove <- c("399", "1584", "957", "91", "4157", "578", "25", "1170", "785", 
                         "1071", "2827", "1423", "551", "36", "1832", "5354", "5407")
          print("Removing")
          print(nrow(survey))
          survey <- survey[survey[["Vrid"]] %in% to_remove==FALSE,]
          print(nrow(survey))
        }
        
        print("before weights")
        haven::write_sav(setNames(survey, gsub("\\s", "_", colnames(survey))), "Test-Data/Test-weighted.sav")
        
        survey <- EchelonSurveyTools::survey_weight(
          survey,
          weights,
          cap = 6,
          .id = "weights",
          check = TRUE
        )
        print("helloworld2")
      }
      
      if (!is.null(input$survey_id) && input$survey_id != "" && as.character(input$survey_id)=="8121740"){
                                survey[["general_2022"]] <- ifelse((!is.na(survey[["general_2022"]])) & survey[["general_2022"]]=="Y",
                                         "Voted", "Did not vote")
        for (varname in c("general_2020", "general_2022")){

          survey[[varname]] <- sjlabelled::as_labelled(sjlabelled::as_numeric(factor(survey[[varname]], levels=c("Did not vote", "Voted"))))
        }
        
        
        # Recodes
        race <- sjlabelled::as_character(survey$Race)
        race <- factor(
          race,
          levels = c(
            "White",
            "Black or African American",
            "Hispanic/Latino",
            "Asian",
            "Native and Other"
          ),
          labels = c(
            "White, non-Hispanic",
            "Black, non-Hispanic",
            "Hispanic",
            "AAPI, non-Hispanic",
            "Other/2+ races, non-Hispanic"
          )
        )
        survey[["RACERECODE"]] <- sjlabelled::as_labelled(race)
        
        race <- sjlabelled::as_character(survey$Race)
        race <- factor(
          race,
          levels = c(
            "White",
            "Black or African American",
            "Hispanic/Latino",
            "Asian",
            "Native and Other"
          ),
          labels = c(
            "White, non-Hispanic",
            "Black, non-Hispanic",
            "Hispanic",
            "Other/2+ races, non-Hispanic",
            "Other/2+ races, non-Hispanic"
          )
        )
        survey[["RACE4"]] <- sjlabelled::as_labelled(race)
        
        party <- sjlabelled::as_character(survey$QPartyID)
        party <- factor(
          party,
          levels = c(
            "Republican",
            "Democrat",
            "Independent",
            "Member of another party",
            "Unsure"
          ),
          labels = c(
            "Republican",
            "Democrat",
            "Ind/Other/Unsure",
            "Ind/Other/Unsure",
            "Ind/Other/Unsure"
          )
        )
        survey[["PARTYSIMPLE"]] <- sjlabelled::as_labelled(party)
        
        party <- sjlabelled::as_character(survey$QPartyID)
        lean <- sjlabelled::as_character(survey$QPartyLeanView)
        x <- ifelse(!is.na(lean), paste("Lean", lean), party)
        x <- factor(
          x,
          levels = c(
            "Republican",
            "Lean Republicans",
            "Lean Both equally often",
            "Lean Unsure",
            "Lean Democrats",
            "Democrat"
          ),
          labels = c(
            "Republican",
            "Leans Republican",
            "Ind/Other/Unsure - No lean",
            "Ind/Other/Unsure - No lean",
            "Leans Democrat",
            "Democrat"
          )
        )
        survey[["PID5"]] <- sjlabelled::as_labelled(x)
        
        survey$PID3 <- sjlabelled::set_labels(survey$QPartyConsolidated, labels=c("Republican/Leans R"=1,
                                                                                  "Ind/Other/Unsure - No lean"=2,
                                                                                  "Democrat/Leans D"=3))
        
        relig <- sjlabelled::as_character(survey[["QReligionNew"]])
        relig <- replace(
          relig,
          survey$QReligionNew %in% c(1, 5) & survey$QBornAgain == 1,
          "Evangelical Protestant"
        )
        
        relig <- replace(
          relig,
          survey$QReligionNew %in% c(1, 5) & survey$QBornAgain %in% 2:3,
          "Non-Evangelical Protestant"
        )
        
        relig <- replace(
          relig,
          survey$QReligionNew == 3,
          "Non-Evangelical Protestant"
        )
        
        relig <- replace(
          relig,
          survey$QReligionNew == 2,
          "Catholic"
        )
        
        
        relig <- replace(
          relig,
          survey[["QReligionNew"]] %in% c(10, 11, 13),
          "Religiously Unaffiliated"
        )
        
        relig <- replace(
          relig,
          survey[["QReligionNew"]] %in% c(4, 6, 7, 8, 9, 12),
          "Other Religious Faiths"
        )
        
        relig <- factor(
          relig,
          levels = c(
            "Evangelical Protestant",
            "Catholic",
            "Non-Evangelical Protestant",
            "Other Religious Faiths",
            "Religiously Unaffiliated"
          )
        )
        
        survey$RELIG <- sjlabelled::as_labelled(relig)
        
      }
      
      if (!is.null(input$survey_id) && input$survey_id != "" && as.character(input$survey_id) %in% c("8093690", "8118729")){
            varnames <- sprintf("QMAHA%s", 1:11)
for (varname in varnames){
  varA <- paste0(varname, "A")
  varB <- paste0(varname, "B")
  rotatedB <- as.numeric(dplyr::recode(as.character(sjlabelled::remove_all_labels(survey[[varB]])), "1"="4", "2"="3",
                                       "3"="2", "4"="1"))
  survey[[varname]] <- replace(survey[[varA]], TRUE, ifelse(is.na(survey[[varA]]), rotatedB, survey[[varA]]))
}

x <- dplyr::select(survey, any_of(c(sprintf("QMAHA%s", 1:11))))
for (varname in c("QMAHA3", "QMAHA7", "QMAHA8", "QMAHA10")){
  x[[varname]] <- as.numeric(dplyr::recode(as.character(sjlabelled::remove_all_labels(x[[varname]])), "1"="4", "2"="3",
                                           "3"="2", "4"="1"))
}
x <- rowSums(apply(x, MARGIN=c(1,2), function(y) ifelse(y %in% c(1,2), 1, 0)))
survey[["NumMAHAIdeasAgree"]] <- x

varnames <- c("QTrust_HHS", "QTrust_Pharma", "QTrust_NIH", 
"QTrust_Hospitals", "QTrust_PBM", "QTrust_Ag", "QTrust_FDA", 
"QTrust_Food", "QTrust_Farmers", "QTrust_Biotech")
value_labels <- c("0  No Trust"=0,"1"=1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9, "10  Most Trust"=10, "Unsure"=11)
for (varname in varnames){
  storelabel <- attr(survey[[varname]], "label")
  x <- sjlabelled::as_character(survey[[varname]])
  survey[[varname]] <- sjlabelled::as_labelled(sjlabelled::set_labels(as.numeric(value_labels[x]), labels=value_labels))
  attr(survey[[varname]], "label") <- storelabel
}
            
            
            
            
        race <- sjlabelled::as_character(survey$Race)
        race <- factor(
          race,
          levels = c(
            "White",
            "Black or African American",
            "Hispanic/Latino",
            "Asian",
            "Native and Other"
          ),
          labels = c(
            "White, non-Hispanic",
            "Black, non-Hispanic",
            "Hispanic",
            "AAPI, non-Hispanic",
            "Other/2+ races, non-Hispanic"
          )
        )
        survey[["RACERECODE"]] <- sjlabelled::as_labelled(race)
        
        race <- sjlabelled::as_character(survey$Race)
        race <- factor(
          race,
          levels = c(
            "White",
            "Black or African American",
            "Hispanic/Latino",
            "Asian",
            "Native and Other"
          ),
          labels = c(
            "White, non-Hispanic",
            "Black, non-Hispanic",
            "Hispanic",
            "Other/2+ races, non-Hispanic",
            "Other/2+ races, non-Hispanic"
          )
        )
        survey[["RACE4"]] <- sjlabelled::as_labelled(race)
        
        party <- sjlabelled::as_character(survey$QPartyID)
        party <- factor(
          party,
          levels = c(
            "Republican",
            "Democrat",
            "Independent",
            "Member of another party",
            "Unsure"
          ),
          labels = c(
            "Republican",
            "Democrat",
            "Ind/Other/Unsure",
            "Ind/Other/Unsure",
            "Ind/Other/Unsure"
          )
        )
        survey[["PARTYSIMPLE"]] <- sjlabelled::as_labelled(party)
        
        party <- sjlabelled::as_character(survey$QPartyID)
        lean <- sjlabelled::as_character(survey$QPartyLeanView)
        x <- ifelse(!is.na(lean), paste("Lean", lean), party)
        x <- factor(
          x,
          levels = c(
            "Republican",
            "Lean Republicans",
            "Lean Both equally often",
            "Lean Unsure",
            "Lean Democrats",
            "Democrat"
          ),
          labels = c(
            "Republican",
            "Leans Republican",
            "Ind/Other/Unsure - No lean",
            "Ind/Other/Unsure - No lean",
            "Leans Democrat",
            "Democrat"
          )
        )
        survey[["PID5"]] <- sjlabelled::as_labelled(x)
        
        survey$PID3 <- sjlabelled::set_labels(survey$QPartyConsolidated, labels=c("Republican/Leans R"=1,
                                                                                  "Ind/Other/Unsure - No lean"=2,
                                                                                  "Democrat/Leans D"=3))
        # survey$PID3 <- factor(
        #   survey$PID3,
        #   levels = c("Republican", "Independent", "Democratic"),
        #   labels = c(
        #     "Republican/Leans R",
        #     "Ind/Other/Unsure - No lean",
        #     "Democrat/Leans D"
        #   )
        # )
        
        relig <- sjlabelled::as_character(survey[["QReligion"]])
        relig <- replace(
          relig,
          survey$QReligion %in% c(1, 5) & survey$QBornAgain == 1,
          "Evangelical Protestant"
        )
        
        relig <- replace(
          relig,
          survey$QReligion %in% c(1, 5) & survey$QBornAgain %in% 2:3,
          "Non-Evangelical Protestant"
        )
        
        relig <- replace(
          relig,
          survey$QReligion == 3,
          "Non-Evangelical Protestant"
        )
        
        relig <- replace(
          relig,
          survey$QReligion == 2,
          "Catholic"
        )
        
        
        relig <- replace(
          relig,
          survey[["QReligion"]] %in% c(10, 11, 13),
          "Religiously Unaffiliated"
        )
        
        relig <- replace(
          relig,
          survey[["QReligion"]] %in% c(4, 6, 7, 8, 9, 12),
          "Other Religious Faiths"
        )
        
        relig <- factor(
          relig,
          levels = c(
            "Evangelical Protestant",
            "Catholic",
            "Non-Evangelical Protestant",
            "Other Religious Faiths",
            "Religiously Unaffiliated"
          )
        )
        
        survey$RELIG <- sjlabelled::as_labelled(relig)
      }
      
      
      weightvars <- unique(weights$category)
      updated_nums <- data.frame(category=NULL, attribute=NULL, current_percentage=NULL)
      for (varname in (weightvars)){
        currentprops <- prop.table(table(sjlabelled::as_character(survey[[varname]])))
        
        current <- data.frame(category=varname, attribute=names(currentprops), current_unweighted_percentage=unname(as.numeric(currentprops)))
        updated_nums <- rbind(updated_nums, current)
      }
      
      benchmark_monitoring <- dplyr::left_join(weights, updated_nums, by=c("category"="category", "attribute"="attribute"))
      
      colnames(benchmark_monitoring) <- c("Category", "Attribute", "Weighting Benchmark", "Current Unweighted %")
      benchmark_monitoring[["Weighting Benchmark"]] <- round(benchmark_monitoring[["Weighting Benchmark"]], 4)
      benchmark_monitoring[["Current Unweighted %"]] <- round(benchmark_monitoring[["Current Unweighted %"]], 4)
      benchmark_monitoring[["Ratio"]] <- round(benchmark_monitoring[["Current Unweighted %"]]/benchmark_monitoring[["Weighting Benchmark"]],4)
      benchmark_monitoring[["Difference"]] <- round(benchmark_monitoring[["Current Unweighted %"]] - benchmark_monitoring[["Weighting Benchmark"]],4)
      
      # Process the data using the survey ID and weights
      # Replace with your actual data processing code
      df1 <- quotas_df
      df2 <- benchmark_monitoring
      df3 <- benchmark_monitoring
      
      haven::write_sav(setNames(survey, gsub("\\s", "_", colnames(survey))), "Test-Data/Test-weighted.sav")
      print('Finished processing')
      print(rv$display_tables)
      
      list(df1 = df1, df2 = df2, df3 = df3, df4=survey)
