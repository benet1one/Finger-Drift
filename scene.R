
Scene <- R6Class("Scene", list(
    width = 0,
    height = 0,
    bg = "",
    xlim = c(0, 0),
    ylim = c(0, 0),
    
    initialize = function(width = 300, height = 150, bg = "tan") {
        self$width <- width
        self$height <- height
        self$xlim <- width  * c(-0.5, +0.5)
        self$ylim <- height * c(-0.5, +0.5)
        self$bg <- bg
        self
    },
    open = function() {
        graphics.off()
        windows(width = self$width, height = self$height, bg = self$bg)
    },
    update = function() {
        par(mar = rep(0, 4),
            cex = 3,
            lwd = 3)
        plot.new()
        plot.window(xlim = self$xlim, ylim = self$ylim)
        rect(xleft  = self$xlim[1L], ybottom = self$ylim[1L],
             xright = self$xlim[2L], ytop    = self$ylim[2L],
             col = "#00000000", border = "black", lwd = 1)
    },
    boom = function(pos) {
        points(pos[1L], pos[2L], pch = 16, cex = 1, col = "orangered")
        points(pos[1L], pos[2L], pch = 8,  cex = 2, col = "orangered")
    },
    random_point = function(margin = 0.8) {
        lim <- c(self$xlim[2L], self$ylim[2L]) * margin
        runif(2L, -lim, +lim)
    },
    random_spawnpoint = function(margin = 1.1) {
        wall <- runif(1L, max = self$width + self$height)
        if (wall < self$width) {
            x <- runif(1L, min = self$xlim[1L], max = self$xlim[2L])
            y <- sample(self$ylim, 1L)
            c(x, y*margin)
        } else {
            x <- sample(self$xlim, 1L)
            y <- runif(1L, min = self$ylim[1L], max = self$ylim[2L])
            c(x*margin, y)
        }
    }
))