#'@importFrom haven as_factor
dataReporter_as_factor <- function(v) {
  as_factor(v)
}



## Adding a verbatim copy of the unexported function dataReporter_haven_replace_with
## so a note does not pop up when checking the package.
dataReporter_haven_replace_with_old <- function(x, from, to) 
{
    stopifnot(length(from) == length(to))
    #out <- x
    out <- rep(NA, length(x))
    matches <- match(x, from, incomparables = NA)

    if (anyNA(matches)) {
      out[!is.na(matches)] <- to[matches[!is.na(matches)]]
    } else {
      out <- to[matches]
    }

    tagged <- haven::is_tagged_na(x)
    if (!any(tagged)) {
        return(out)
    }
    matches <- match(haven::na_tag(x), haven::na_tag(from), incomparables = NA)
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
    out
}
