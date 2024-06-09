
Game <- R6Class("Game", list(
    scene = Scene$new(),
    player = Car$new(),
    bots = list(),
    dead = logical(),
    time = 0,
    frame = 0L,
    spawn_rate = 0.6,
    next_spawn = 1,
    over = FALSE,
    
    initialize = function(scene = Scene$new(), 
                          player = Car$new(color = "black"),
                          starting_spawn_rate = 0.6) {
        self$scene <- scene
        self$player <- player
        self$spawn_rate <- starting_spawn_rate
        self
    },
    update = function() {
        self$time <- self$time + spf
        self$frame <- self$frame + 1L
        self$kill_bots()
        self$kill_player()
        self$drive_bots()
        self$spawn_bots()
    },
    spawn_bots = function() {
        if (sum(!self$dead) >= max_bots)
            return(self)
        self$next_spawn <- self$next_spawn + self$spawn_rate*spf
        n_spawns <- floor(self$next_spawn)
        for (k in seq_len(n_spawns))
            self$spawn()
        self$next_spawn <- self$next_spawn - n_spawns
        self$spawn_rate <- self$spawn_rate + spawn_rate_delta*spf
        self
    },
    spawn = function() {
        spawnpoint <- self$scene$random_spawnpoint()
        where <- match(TRUE, self$dead, nomatch = length(self$dead) + 1L)
        self$bots[[where]] <- Car$new(pos = spawnpoint)
        self$dead[where] <- FALSE
        self
    },
    drive_bots = function() {
        for (k in which(!self$dead)) {
            target <- self$bots [[k]] $ target(self$player$pos)
            self$bots [[k]] $ drive(target)
        }
        self
    },
    kill_player = function() {
        pos <- self$player$pos
        xlim <- self$scene$xlim
        ylim <- self$scene$ylim
        if (pos[1L] < xlim[1L] || pos[1L] > xlim[2L] ||
            pos[2L] < ylim[1L] || pos[2L] > ylim[2L]) {
            self$over <- TRUE
            self$scene$boom(pos)
            return(self)
        }
            
        if (sum(!self$dead) == 0L)
            return(self)
        pos_mat <- sapply(self$bots[!self$dead], \(bot) bot$pos)
        dim(pos_mat)[1L] <- 2L
        collision <- find_collision(pos, pos_mat)
        if (collision["sq_dist"] < collision_radius) {
            self$over <- TRUE
            self$scene$boom(pos)
        }

        self
    },
    kill_bots = function(pos_mat = NULL, midpoint = NULL) {
        if (is.null(pos_mat)) {
            if (sum(!self$dead) < 2L) {
                return(self)
            }
            pos_mat <- sapply(self$bots[!self$dead], \(bot) bot$pos)
            midpoint <- apply(pos_mat, 1L, median)
        } 
        if (ncol(pos_mat) < 2L) {
            return(self)
        }
        bot <- find_collision(midpoint, pos_mat) ["j"]
        collision <- find_collision(pos_mat[,  bot, drop = FALSE], 
                                    pos_mat[, -bot, drop = FALSE])
        
        if (collision["sq_dist"] < collision_radius) {
            other_bot <- (1:ncol(pos_mat)) [-bot] [collision["j"]]
            remove <- c(bot, other_bot)
            self$scene$boom(rowMeans(pos_mat[, remove]))
            pos_mat <- pos_mat[, -remove, drop = FALSE]
            self$dead[!self$dead][remove] <- TRUE
        } else {
            pos_mat <- pos_mat[, -bot, drop = FALSE]
        }
        self$kill_bots(pos_mat = pos_mat, midpoint = midpoint)
    },
    plot = function() {
        self$scene$update()
        self$player$plot()
        for (bot in self$bots[!self$dead])
            bot$plot()
        self
    }
))

find_collision <- function(pos, pos_mat) {
    min_dist <- squared_distance(pos, pos_mat[, 1L])
    min_j <- 1L
    for (j in 1:ncol(pos_mat)) {
        dist <- squared_distance(pos, pos_mat[, j])
        if (dist < collision_radius)
            return(c(j = j, sq_dist = dist))
        if (dist < min_dist) {
            min_dist <- dist
            min_j <- j
        }
    }
    c(j = min_j, sq_dist = dist)
}

squared_distance <- function(p, q) {
    sum((p - q)^2)
}
