
vdelta <- 20
vmax <- 50
a <- 8

vphi <- 20
d <- 1
r <- 0.5

update_delta <- function(fps) {

    print(vdelta)
    print(vdelta + (1-vdelta/vmax)*a)
    
    spf <- 1/fps
    v <- numeric(fps)
    v[1] <- vdelta
    for (k in 2:fps) {
        v[k] <- v[k-1] + (1 - v[k-1]/vmax) * a * spf
    }
    print(v)
}

update_phi <- function(fps) {
    
    print(vphi)
    print(vphi * (1 - d*r))
    
    spf <- 1/fps
    v <- numeric(fps)
    v[1] <- vphi
    for (k in 2:fps) {
        v[k] <- v[k-1] * (1 - d*r)^spf
    }
    print(v)
}