#' Unique elements function
unique.elements <- function(df) {
  #df<-data.frame(cbind(c("a","a","b","b","c","c","d","d","d","d","a","b","c"),c("b","c","a","c","a","b","a","b","c","e","e","e","e")), stringsAsFactors = FALSE)
  ur <- 1
  bucket <- union(df[1,1],df[1,2])
  for (ii in 1:nrow(df)-1){
    nbucket <- union(bucket,union(df[ii+1,1],df[ii+1,2]))
    if (length(nbucket) >= length(bucket) + 2) {
      ur <- c(ur,ii+1)
      bucket <- nbucket
    }
  }
  return(ur)
}

#' Get Calendar Data Function
#'
#' This fuction allows you to fetch Google calendar data
#' @param Google Access Token
#' @param MaxResults Per Page set to 2500
#' @keywords Google Calendar
#' @export
#' @examples
#' get_google_calendar_df(google_token)

get_google_calendar_df <- function(google_token = google_token,
                                   maxResults = 2500){

  url <- paste0("https://www.googleapis.com/calendar/v3/users/me/calendarList")
  reqs <- GET(url, config(token = google_token))
  MyItems <- content(reqs)$items
  cal <- sapply(1:length(MyItems), function(x) MyItems[[x]]$accessRole) == 'owner'
  CalendarID <- MyItems[[which(cal)[2]]]$id
  url <- paste0("https://www.googleapis.com/calendar/v3/users/me/calendarList")
  #MyItems <- content(req)$items
  Empty <- T
  NoToken <- T
  Continue <- T
  Items <- NULL
  counter <- 0
  data_df <- NULL
  while (Continue && counter < 200){
    if (Empty & NoToken){
      url <- paste0("https://www.googleapis.com/calendar/v3/calendars/",CalendarID,"/events?maxResults=",maxResults,"&singleEvents=true")
      req <- GET(url, config(token = google_token))
      MyItems <- content(req)$items
      Items <- c(Items,MyItems)
      PageToken <- content(req)$nextPageToken
      NoToken <- is.null(PageToken)
      Empty <- length(Items)==0


    } else if (!Empty & !NoToken){
      url <- paste0("https://www.googleapis.com/calendar/v3/calendars/",CalendarID,"/events?maxResults=",maxResults,"&singleEvents=true&pageToken=",PageToken)
      req <- GET(url, config(token = google_token))
      MyItems <- content(req)$items
      Items <- c(Items,MyItems)
      PageToken <- content(req)$nextPageToken
      NoToken <- is.null(PageToken)
      Empty <- length(Items)==0

    } else { Continue <- FALSE }
    counter <- counter + 1
    cat(paste0('Downloaded ', counter * maxResults, ' events...\n\r'))
    flush.console()
  }

  ocaldata <- NULL
  opeople <- NULL
  cat('Deduping people \r')
  for (ii in 1:length(Items)){
    st <- strptime( Items[[ii]]$start$dateTime, "%Y-%m-%dT%H:%M:%S")
    en <- strptime( Items[[ii]]$end$dateTime, "%Y-%m-%dT%H:%M:%S")
    if (length(st) > 0){
      duration <- as.numeric(en - st, units = 'mins')
    } else {
      duration <- 0
      st <- 0
      en <- 0
    }
    title <- Items[[ii]]$summary[[1]]
    title <- ifelse(is.null(title), "", title)
    numatt <- length(Items[[ii]]$attendees)
    real_meet <- numatt > 0
    if (real_meet){

      cpeeps <- NULL
      if (length(Items[[ii]]$attendees)>0 && !is.na(Items[[ii]]$attendees)){
        cemails <- sapply(1:length(Items[[ii]]$attendees), function(x) Items[[ii]]$attendees[[x]]$email)
        cnames <- sapply(1:length(Items[[ii]]$attendees), function(x) Items[[ii]]$attendees[[x]]$displayName)
        cnames[sapply(cnames, is.null)] <- cemails[sapply(cnames, is.null)]
        cnames <- unlist(cnames)
        ctime <- rep(st,length(cemails))
        id <- rep(Items[[ii]]$id, length(cemails))
        cduration <- rep(duration,length(cemails))
        if (!all(is.null(cemails[[1]]))){
          cpeeps <- data.frame(id, cemails,cnames,ctime,cduration,stringsAsFactors = F)
        }


      }
      cpeeps <- unique(cpeeps)
      if (!is.null(cpeeps) && any(cpeeps$cduration > 0)){
        cpeeps <- cpeeps %>% filter(cemails != CalendarID)
        opeople <- rbind(opeople,cpeeps)
      }

    }
    num_attendees <-  numatt
    if (duration > 0){
      status = Items[[ii]]$status
      link = Items[[ii]]$htmlLink
      sequence = Items[[ii]]$sequence
      organizer_email = Items[[ii]]$organizer$email
      organizer_name = Items[[ii]]$organizer$displayName
      organizer_name <- ifelse(is.null(organizer_name), organizer_email, organizer_name)
      location <- ifelse(is.null(Items[[ii]]$location),"", Items[[ii]]$location)
      recurring <- ifelse(is.null(Items[[ii]]$recurringEventId), FALSE, TRUE)
      curmeet <- data.frame(id = Items[[ii]]$id,
                            icaluid = Items[[ii]]$iCalUID,
                            status,
                            link,
                            sequence,
                            location,
                            recurring,
                            organizer_email,
                            organizer_name,
                            duration,
                            real_meet,
                            num_attendees,
                            title, st, en,
                            stringsAsFactors = F)
      ocaldata <- rbind(ocaldata,curmeet)
    }
  }

  # Data Processing ---------------------------------------------
  opeople <- opeople %>% filter(cemails != CalendarID)

  data_df$people <- opeople
  data_df$caldata <- ocaldata

  return(data_df)
}
