library(googleway)

mydf <- data.frame(region = 1:3,
                   from_lat = 41.8674336,
                   from_long = -87.6266382,
                   to_lat = c(41.887544, 41.9168862, 41.8190937),
                   to_long = c(-87.626487, -87.64847, -87.6230967))

mykey <- "AIzaSyBrrXJNWhLWdlC043ERxpiHx0lRYL7Sq-c"

pls <- lapply(1:nrow(mydf), function(x){

    foo <- google_directions(origin = unlist(mydf[x, 2:3]),
                             destination = unlist(mydf[x, 4:5]),
                             key = mykey,
                             mode = "driving",
                             simplify = TRUE)

    ## Decode the polyline into lat/lon coordinates
    pl <- decode_pl(foo$routes$overview_polyline$points)

    return(pl)
})
