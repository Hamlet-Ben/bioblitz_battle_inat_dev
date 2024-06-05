library(shiny)
library(shinyMobile)
library(shinyalert)
library(RMariaDB)
library(rinat)
library(shinybusy)


#load details of games currently set up
RPPDb <- dbConnect(RMariaDB::MariaDB(), user=localuser, password=localuserpassword, dbname=localdb, host='35.177.196.110')
game_dets <- dbReadTable(RPPDb, "bioblitz_battle_inat_games") #team data
dbDisconnect(RPPDb)

#load latest species rarity scores

load("sp_score_res")


shinyApp(
  ui = f7Page(
    options = list(dark = FALSE),
    add_busy_spinner(spin = "fading-circle"),
    
f7SingleLayout(

  navbar = f7Navbar(title = "Rock Pool Bioblitz Battle Game - Latest Scores"),
  div(tags$image(src = "Banner.jpg", width = "100%") , style="text-align: center;"),
  h2(textOutput("Game_name")),
  f7Select(
    inputId = "game_sel",
    label = "",
    choices = c("Select a game", game_dets$Name),
    selected = NULL
  ),
  f7Button("refresh", "REFRESH", color = "teal"),
  uiOutput("res_table"),
  br(),
  h2(textOutput("rec_tab_title")),
  f7Select("sort_by", label = "Sort record list by:", choices = c("Observation time", "Species name","Species score", "Recorder name")),
  f7Radio("team_sel", label = ""),
  uiOutput("rec_list")
)

),

server = function(input, output, session) {
  
  GAME_DAT <- reactiveVal()
  
  observe({
    print(input$refresh)
  if(!input$game_sel == "Select a game"){
   
    output$Game_name <- renderText(input$game_sel)
    
    this_game_dets <- subset(game_dets, Name == input$game_sel)
    
    this_game_dets$start <- strptime(paste(this_game_dets$Date, this_game_dets$Time_start), format = "%Y-%m-%d %H:%M:%S")
    this_game_dets$end <- strptime(paste(this_game_dets$Date, this_game_dets$Time_finish), format = "%Y-%m-%d %H:%M:%S")
    
    
  #load in data from iNat
  team_names <- strsplit(this_game_dets$Teams, "; ")[[1]]
  team_inat_projs <- strsplit(this_game_dets$Inat_projs, "; ")[[1]]
  
  game_dat <- list()
  
  for (team in team_names) {
    #get iNat project associated with the team
    team_inat_proj <- team_inat_projs[team_names == team]
    
    #download data
    rinat_try <- try(team_game_dat <- get_inat_obs_project_ben(team_inat_proj, type = "obs"))
    
    if(!class(rinat_try) == "try-error"){
    
    #add team name variable
    team_game_dat$team_name <- team
    
    game_dat[[team]] <- team_game_dat
    }
  }
  
  game_dat <- do.call("rbind", game_dat)
  
  
  if (!is.null(game_dat)) {

    #convert observation time variable to date-time format
  game_dat$time_observed_at <- strptime(game_dat$time_observed_at, format = "%Y-%m-%dT%H:%M:%S")
  
  #convert submission time variable to date-time format
  game_dat$created_at <- strptime(game_dat$created_at, format = "%Y-%m-%dT%H:%M:%S")
  
    #subset data to only the time period for this game & in the correct location

  game_dat <- subset(game_dat, time_observed_at > this_game_dets$start & time_observed_at < this_game_dets$end)
  
  #subset data to only records submitted before the deadline (an hour after the game ends)
  game_dat <- subset(game_dat, created_at <  this_game_dets$end + (60*60))
  
  #if no data left then break
  
  if(nrow(game_dat) == 0){
    game_dat <- NULL
    #give message saying no data collected yet
    output$rec_tab_title <- renderText("No data collected so far")
  }else{
    
    #check if any recorded species are not on the UK species list - add with a score of 20 if so
    missing_species <- game_dat$taxon.name[!game_dat$taxon.name %in% names(sp_score_res)]
    
    for (sp in missing_species) {
      sp_score_res[[sp]] <- 0
    }
    
    game_dat$score[!is.na(game_dat$taxon.name)] <- round(unlist(sp_score_res[game_dat$taxon.name]), 2)
    game_dat$score[!game_dat$taxon.rank == "species"] <- NA
    
    #save to csv for output
    game_dat_output <- game_dat[,c(1,4,5,8:10,15,27, 32, 61, 62, 73,75,76)]
    
    game_dat_output$Image <- unlist(lapply(game_dat$photos, function(x){x$large_url[1]}))
    
    write.csv(game_dat_output, "game_dat_output.csv", row.names = F)
    
    
    #remove data for taxa not at species level
    game_dat <- subset(game_dat, taxon.rank == "species")
    
    
    GAME_DAT(game_dat)
    
    
    #calculate team score
    
    team_scores <- vector()
    team_scores_verif <- vector()
    team_sp_count <- vector()
    
    for (team in team_names) {
      team_sp_dat <- subset(game_dat, team_name == team)
      team_sp_dat <- subset(team_sp_dat, !duplicated(taxon.name))
      
      team_sp_dat_verif <- subset(game_dat, quality_grade == "research")
      team_sp_dat_verif <- subset(team_sp_dat_verif, team_name == team)
      team_sp_dat_verif <- subset(team_sp_dat_verif, !duplicated(taxon.name))
      
      team_scores <- c(team_scores, sum(team_sp_dat$score))
      team_scores_verif <- c(team_scores_verif, sum(team_sp_dat_verif$score))
      
      team_sp_count <- c(team_sp_count, nrow(team_sp_dat))
      
    }
    
    #whole game scores
    all_sp_dat <- subset(game_dat, !duplicated(taxon.name))
    all_sp_dat_verif <- subset(game_dat, quality_grade == "research")
    all_sp_dat_verif <- subset(all_sp_dat_verif, !duplicated(taxon.name))
    
    all_score <- sum(all_sp_dat$score)
    all_scores_verif <- sum(all_sp_dat_verif$score)
    all_sp_count <- nrow(all_sp_dat)
    
    res_tab <- data.frame("Team" = team_names, "Species" = team_sp_count, "Score" = team_scores, "Verified" = team_scores_verif)
    
    #sort in order
    
    res_tab <- res_tab[order(res_tab$Verified, decreasing = T),]
    
    #add all team scores
    
    res_tab <- rbind(res_tab, c("All teams", all_sp_count, all_score, all_scores_verif))
    
    #display scores
    output$rec_tab_title <- renderText("Species records:")

    output$res_table <- renderUI({
      f7Table(res_tab, colnames = gsub("\\."," ",names(res_tab)) ,card = TRUE)
    }) 
    
    # update team selector
    updateF7Radio(
      "team_sel",
      label = "Display",
      choices = c("All teams", unique(game_dat$team_name)),
    )
    
    
  }
  

  }else{
    #give message saying no data collected yet
    output$rec_tab_title <- renderText("No data collected so far")
    
  }
  }else{
    output$latest_scores <- renderText("")
    output$Game_name <- renderText("No game selected")
    output$rec_tab_title <- renderText("")
    GAME_DAT(NULL)
  }
  }
  )
  
  #show records table
  
  observe({
    
    game_dat <- GAME_DAT()
    
    if(!is.null(GAME_DAT())){
      
     
      #make names vector - common name is available, latin if not
      
      game_sp_names <- game_dat$taxon.common_name.name
      game_sp_names[is.na(game_sp_names)] <- game_dat$taxon.name[is.na(game_sp_names)]
      
      rec_thumbs <- rep("", nrow(game_dat))
      
      recs_with_images <- which(game_dat$observation_photos_count > 0)
      
      for (rec in recs_with_images) {
        rec_thumbs[rec] <- game_dat$photos[[rec]]$square_url
      }
      
      record_tab <- data.frame("Species" = game_sp_names,"Score" = round(game_dat$score,2), "Recorder" = game_dat$user_login, "Team" = game_dat$team_name,"Time" = substr(game_dat$time_observed_at,12,16), "Image" = rec_thumbs, "url" = game_dat$uri, "Status" = game_dat$quality_grade)
      
      record_tab$Status[record_tab$Status == "needs_id"] <- "unverified"
      record_tab$Status[record_tab$Status == "research"] <- "VERIFIED"
      
      #subset to specific team if selected

      if(!is.null(input$team_sel)){
        if(!input$team_sel == "All teams"){
          record_tab <- subset(record_tab, Team == input$team_sel)
        }  
      }
      
      
      #sort by selected variable
      
      sort_cols <- names(record_tab)[c(5,1,2,3)]
      names(sort_cols) <- c("Observation time", "Species name","Species score", "Recorder name")
      
      
      record_tab <- record_tab[order(record_tab[[sort_cols[input$sort_by]]], decreasing = input$sort_by %in% c("Observation time","Species score")),]
      
      #split by teams
      
      record_team_tabs <- lapply(unique(record_tab$Team), function(x){
        subset(record_tab, Team == x)
      })
      
      names(record_team_tabs) <- unique(record_tab$Team)
      
      #show list
      output$rec_list <- renderUI({
        f7List(
          mode = "media",
          lapply(unique(record_tab$Team), function(i){
            f7ListGroup(
              title = i,
              lapply(1:nrow(record_team_tabs[[i]]), function(j) {
                f7ListItem(
                  title = record_team_tabs[[i]]$Species[j],
                  subtitle = paste(record_team_tabs[[i]]$Recorder[j], "-", record_team_tabs[[i]]$Time[j]),
                  record_team_tabs[[i]]$Status[j],
                  media = tags$img(
                    src = record_team_tabs[[i]]$Image[j]
                  ),
                  right = record_team_tabs[[i]]$Score[j],
                  href = record_team_tabs[[i]]$url[j]
                )
              })
              
            )
            
          })
          
          
          
          
        )
      })
      
    }
    
    
  })
  
  
}
)
  