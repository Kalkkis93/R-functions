## My R-codes
## Julius Kallio


library(ggplot2)


#################################################################################
#################################################################################
#################################################################################
## READING THE DATA

# Operative data.
d <- read.table("~/the/location/of/the/operative_data.csv", sep=";", header=TRUE)

# Municipality code data.
d2 <- read.table("~/the/location/of/the/mc_data.csv", sep=";", header=TRUE)

# Usage data.
d3 <- read.table("~/the/location/of/the/usage_data.csv", sep=";", header=TRUE)


#################################################################################
#################################################################################
#################################################################################
## UPDATING THE DATA

# Set the correct format to date variables.
d$createdDate <- as.Date(d$createdDate, format = "%Y-%m-%d")
d$submittedDate <- as.Date(d$submittedDate, format = "%Y-%m-%d")
d$verdictGivenDate <- as.Date(d$verdictGivenDate, format = "%Y-%m-%d")
d3$date <- as.Date(d3$datetime, format = "%Y-%m-%d")

# Create variables leadTime and submitTime.
d$leadTime <- as.numeric(d$verdictGivenDate - d$submittedDate)
d$submitTime <- as.numeric(d$submittedDate - d$createdDate)

# Create variables kunta.numerot and kunta.nimet.
kunta.numerot <- as.character(d2$Kuntakoodi)
kunta.nimet <- character(nrow(d2))
for (i in 1:nrow(d2)) {
  numero <- kunta.numerot[i]
  while (nchar(numero) < 3)
    numero <- paste("0", numero, sep="")
  kunta.numerot[i] <- numero
  kunta0 <- as.character(d2$Kunta[i])
  kunta <- ""
  while (kunta0 != "") {
    c <- substring(kunta0, 1, 1)
    if (c == "\212")
      kunta <- paste(kunta, "ä", sep="")
    else if (c == "\213")
      kunta <- paste(kunta, "ö", sep="")
    else
      kunta <- paste(kunta, c, sep="")
    kunta0 <- substring(kunta0, 2, nchar(kunta0))
  }
  kunta.nimet[i] <- kunta
}

# Create variables kuntanumerot and vuodet.
kuntanumerot <- vuodet <- character(nrow(d))
for (i in 1:nrow(d)) {
  appId <- d$applicationId[i]
  kuntanumerot[i] <- substring(appId, 4, 6)
  vuodet[i] <- substring(appId, 8, 11)
}


#################################################################################
#################################################################################
#################################################################################
## FUNCTIONS

# A function that returns the municipality number of a city.
haeNumero <- function(knimi) {
  mch <- match(knimi, kunta.nimet)
  return(as.numeric(kunta.numerot[mch]))
}

# A function that returns the name of a city by municipality number.
haeNimi <- function(kuntanro) {
  mch <- match(kuntanro, kunta.numerot)
  return(kunta.nimet[mch])
}

# A function to calculate a number of the days in which the author
# has processed the application after submission.
authorDays <- function(appId) {
  # Find a right row from the operative data.
  d.row <- d[d$applicationId == appId,]

  # Check if the lead time is a correct number.
  if (!is.na(d.row$leadTime)) {

    # If lead time = 0, then the number of the days must be 1.
    if (d.row$leadTime == 0)
      return(1)

    # If lead time > 0, then we need the usage data to compute
    # the number of the days.
    else if (d.row$leadTime > 0) {

      # Take the submittedDate and verdictGivenDate from the operative data.
      sd <- as.Date(d[d$applicationId == appId,]$submittedDate)
      vd <- as.Date(d[d$applicationId == appId,]$verdictGivenDate)

      # Find the dates from the usage data.
      dates <- d3[d3$applicationId == appId,]$date

      # Drop the days before submittedDate and after verdictGivenDate.
      dates <- dates[dates > sd & dates < vd]

      # Compute the number of the days.
      dates <- factor(dates)
      dates <- levels(dates)
      numberOfDates <- length(dates)
      return(numberOfDates)
    }
  }
  return(NA)
}

# A function to make a plot of the days.
plotDays <- function(appId) {
  # Find the dates from the usage data.
  x <- d3[d3$applicationId == appId,]$date

  # Make a variable y for plot.
  y <- rep(1, length(x))

  # Make a color variable.
  Action <- rep("Other", length(x))

  # Find the submitted date and the verdict given date.
  sd <- d[d$applicationId == appId,]$submittedDate
  vd <- d[d$applicationId == appId,]$verdictGivenDate

  # Update the color variable.
  Action[x == sd] <- "Submitted"
  Action[x == vd] <- "Verdict Given"

  # Make the plot.
  qplot(x, y, col=Action, main=appId, xlab="Date", ylab="", ylim="")
}

# A function to find applications that the authors have processed
# outside of some time interval.
authorTime <- function(start, end) {
  autData <- d3[as.character(d3$role) == "authority",]
  times <- substring(autData$datetime, 12, 13)
  times <- as.numeric(times)
  retData <- autData[times < start | times > end,]
  return(retData)
}

# A function to find out when an action x has been done first time
# with an application and when it has been done ith time.
ith <- function(appId, x, i) {
  appData <- d3[d3$applicationId == appId,]
  appData <- appData[appData$action == x,]
  if (nrow(appData) >= 1) {
    cat(paste("First time: ", appData[1,]$datetime, "\n", sep=""))
  }
  if (nrow(appData) >= i) {
    cat(paste(i, "th time: ", appData[i,]$datetime, "\n", sep=""))
  }
}

# Compute the author days in some city.
effMed <- function(city) {
  # Find the municipality id.
  cn <- haeNumero(city)

  # Find the applications from this city.
  apps <- d[d$municipalityId == cn,]
  apps <- apps[!is.na(apps$leadTime),]
  apps <- apps[apps$isCanceled == "false",]

  cat(paste("Processing ", city, "...", "\n", sep=""))
  cat(paste("Number of applications:", nrow(apps), "\n"))

  # Compute the author days.
  aps <- ads <- lts <- c(1)
  if (nrow(apps) > 0) {
    for (i in 1:nrow(apps)) {
      ad <- authorDays(as.character(apps[i,]$applicationId))
      if (!is.na(ad)) {
        aps <- c(aps, as.character(apps[i,]$applicationId))
        ads <- c(ads, ad + 1)
        lts <- c(lts, apps[i,]$leadTime)
        cat(paste(100 * i/nrow(apps), "processed\n", sep="% "))
      }
    }
  }
  cat("READY!\n")
  if (length(aps) > 2) {
    res <- cbind(city, aps, ads, lts)
    res <- data.frame(res[2:nrow(res),])
    names(res) <- c("City", "ApplicationId", "AuthorDays", "LeadTime")
    return(res)
  }
  else if (length(aps) == 2) {
    res <- cbind(city, aps[2], ads[2], lts[2])
    res <- data.frame(res)
    names(res) <- c("City", "ApplicationId", "AuthorDays", "LeadTime")
    return(res)
  }
  return(NA)
}
