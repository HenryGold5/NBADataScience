#Shot Charts Data
library(dplyr)

read.csv("~/RProjects/NBADataScience/playoff_shots.csv") -> shotsData

courtImg <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"

court <- grid::rasterGrob(jpeg::readJPEG(RCurl::getURLContent(courtImg)), 
                          width = grid::unit(1,"npc"),
                          height = grid::unit(1,"npc"))

scatterPlotShots <- function(player = "",team = NA) {
  if (!is.na(team)) {
    player <- team
  }
  if (player == "") {
    return("Error: No player or team entered. Please try again")
  }
  shotsData %>% 
    dplyr::filter(PLAYER_NAME == player) %>% 
    dplyr::select(LOC_X, LOC_Y, EVENT_TYPE) %>% 
    ggplot2::ggplot(ggplot2::aes(x=LOC_X, y=LOC_Y)) +
    ggplot2::annotation_custom(court, -250,250,-50,420) +
    ggplot2::geom_point(ggplot2::aes(color = EVENT_TYPE), alpha = 0.7) +
    ggplot2::xlim(-250,250) +
    ggplot2::ylim(-50,420) +
    ggplot2::scale_color_manual(values = c("#21763E", "#BC2C3A"))
}

percentagePlotShots <- function(player = "") {
  lebronData <- shotsData %>% 
    dplyr::filter(PLAYER_NAME == player) %>% 
    dplyr::select(LOC_X, LOC_Y, EVENT_TYPE)
  
  test <- hexbin::hexbin(x = lebronData$LOC_X, 
                         y = lebronData$LOC_Y, 
                         xbins = 5, 
                         IDs = TRUE)
  
  test <- test@cID
  
  lebronData <- lebronData %>% 
    cbind(test)
  
  lebronDataFinal <- lebronData %>% 
    dplyr::mutate(makeShot = ifelse(EVENT_TYPE == "Made Shot", 1, 0)) %>% 
    dplyr::group_by(test) %>% 
    dplyr::summarise(LOC_X = mean(LOC_X),
                     LOC_Y = mean(LOC_Y),
                     percent = mean(makeShot),
                     numShots = n())
  
  lebronDataFinal %>% 
    ggplot2::ggplot(ggplot2::aes(x=LOC_X, y=LOC_Y)) +
    ggplot2::annotation_custom(court, -250,250,-50,420) +
    ggplot2::geom_point(ggplot2::aes(color = percent, size = numShots)) +
    ggplot2::xlim(-250,250) +
    ggplot2::ylim(-50,420) +
    ggplot2::scale_colour_distiller(palette = "YlGn", direction = 1) +
    ggplot2::geom_point(shape = 1, ggplot2::aes(size = numShots), colour="black")
}

scatterPlotShots("LeBron James")

percentagePlotShots("Stephen Curry")
