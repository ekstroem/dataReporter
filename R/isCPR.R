#' @title Check if a variable consists of Danish CPR numbers
#'
#' @description A \code{\link{checkFunction}} that checks if \code{v} consists exclusively
#' of valid Danish civil registration (CPR) numbers, ignoring missing values. This
#' function is intended for use as a precheck in \code{\link{makeDataReport}}, ensuring
#' that CPR numbers are not included in a \code{dataReporter} output document.
#'
#' @param v A variable (vector) to check. This variable is allowed to have any class.
#'
#' @param ... Not in use.
#'
#' @return A \code{\link{checkResult}} with three entires:
#' \code{$problem} (a logical indicating whether the variable consists
#' of CPR numbers), \code{$message} (if a problem was found,
#' the following message: "Warning: The variable seems to consist of
#' Danish civil registration (CPR) numbers.",
#' otherwise "") and \code{$problemValues} (always \code{NULL}).
#'
#' @examples
#'
#' CPRs <- c("010188-3639", "020187-1476", "040506-8664", "010290-3684", "010291-1180",
#'           "010293-1599", "010294-1268", "010295-1360", "010296-3970", "010297-2007",
#'           "010270-2905", "010271-0134", "010272-1403", "010273-3088", "010274-1633")
#' nonCPRs <- c(1:10)
#' mixedCPRs <- c(CPRs, nonCPRs)
#'
#' #identify problem
#' isCPR(CPRs)
#'
#' #no problem as there are no CPRs
#' isCPR(nonCPRs)
#'
#' #no problem because not ALL values are CPRs
#' isCPR(mixedCPRs)
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}},
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#'
#' @importFrom stats na.omit
#' @importFrom haven as_factor
#' @export
isCPR <- function(v, ...) { #Note: Implementation works until the year 2036...
  out <- list(problem=FALSE, message="", problemValues = NULL)
  m <- "Warning: The variable seems to consist of Danish civil regristration (CPR) numbers."
  
  if (any(c("labelled", "haven_labelled") %in% class(v))) v <- dataReporter_as_factor(v)
  
  v <- as.character(na.omit(v))
  if (length(v) == 0) return(checkResult(out)) #if v consists only of NAs
  posCPR <- FALSE
  chars <- nchar(v)

  if (!all(chars %in% c(10,11))) return(checkResult(out))

  if (all(chars == 10)) {
    posCPR <- grepl("[0-9]{10}", v)
  }
  if (all(chars== 11)) {
    posCPR <- grepl("[0-9]{6}-[0-9]{4}", v)
  }

  if (!all(posCPR)) return(checkResult(out))

  if (!all(isDanishDate(substring(v, 1, 6)))) return(checkResult(out))

  v <- gsub("-", "", v)

  year <- as.numeric(substring(v, 5, 6))
  digit7 <- substring(v, 7, 7)

  noCheckPl <- year<36 & year>=7 & digit7 >= 4 #is this right?

  if (!all(noCheckPl)) {
    check <- function(x) {
      x <- as.numeric(strsplit(x, "")[[1]])
      a <- c(4, 3, 2, 7, 6, 5, 4, 3, 2, 1)
      (x %*% a) %% 11 == 0 #note: x %*% a = a %*% x for 1 x n vectors in R
    }
    res <- sapply(v[!noCheckPl], check)
    if (!all(res)) return(checkResult(out))
  } else if (!all(digit7[noCheckPl]>3)) return(checkResult(out))

  out$problem <- TRUE
  out$message <- m
  checkResult(out)
}


#Make it a checkFunction
#' @include checkFunction.R allClasses.R
isCPR <- checkFunction(isCPR, "Identify Danish CPR numbers",
                       classes = allClasses())


##########################################Not exported below#########################################

#Checks whether strs contains only entries on the form DDMMYY
isDanishDate <- function(strs) {
  if (!(all(nchar(strs) == 6) & all(grepl("[0-9]{6}", strs)))) return(FALSE)

  ds <- as.numeric(substring(strs, 1, 2))
  ms <- as.numeric(substring(strs, 3, 4))

  if (any(ms > 13)) return(FALSE)

  mds <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  maxDs <- mds[ms]

  if (any(ds > maxDs)) return(FALSE)

  TRUE
}

