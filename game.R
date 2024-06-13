
windowsFonts(sans = "Advent Pro Bold",
             mono = "Ubuntu Mono")

Game <- R6Class("Game", list(
    scene = Scene$new(),
    player = Car$new(),
    bots = list(),
    dead = logical(),
    time = 0,
    frame = 0L,
    spawn_rate = 0.6,
    next_spawn = 1,
    bomb_rate = 1/8,
    next_bomb = 0.2,
    bomb = c(NA_real_, NA_real_),
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
        self$explode_bomb()
        self$drive_bots()
        self$spawn_bots()
        self$spawn_bomb()
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
    spawn_bomb = function() {
        if (!is.na(self$bomb[1L]))
            return(self)
        if (self$next_bomb < 1) {
            self$next_bomb <- self$next_bomb + self$bomb_rate*spf
            return(self)
        }
        self$next_bomb <- self$next_bomb - 1
        self$bomb <- self$scene$random_point(margin = 0.8)
        self
    },
    explode_bomb = function() {
        if (is.na(self$bomb[1L]))
            return(self)
        dist <- squared_distance(game$player$pos, self$bomb)
        if (dist < collision_radius && !all(self$dead)) {
            bot_pos <- self$bot_positions()
            self$dead[] <- TRUE
            self$bomb[] <- NA_real_
            for (j in 1:ncol(bot_pos)) 
                self$scene$boom(bot_pos[, j])
        }
        self
    },
    bot_positions = function() {
        if (all(game$dead))
            return(array(dim = c(2, 0), dimnames = list(c("x", "y"))))
        pos_mat <- sapply(game$bots[!game$dead], \(bot) bot$pos)
        dim(pos_mat)[1L] <- 2L
        rownames(pos_mat) <- c("x", "y")
        pos_mat
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
            pos_mat <- game$bot_positions()
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
        text(x = self$scene$xlim[1L] + 10, 
             y = self$scene$ylim[2L] - 10, 
             labels = round(100*self$time),
             family = "mono",
             cex = 0.8)
        
        if (!is.na(self$bomb[1L]))
            points(self$bomb[1L], self$bomb[2L], col = "springgreen4", pch = 16)
        
        self$player$plot()
        for (bot in self$bots[!self$dead]) bot$plot()
        
        if (self$over)  {
            text(x = 0, y = 0, labels = "GAME OVER", cex = 3, family = "mono")
            self$scene$boom(self$player$pos)
        }
        self    
    }
))

find_collision <- function(pos, pos_mat) {
    min_dist <- squared_distance(pos, pos_mat[, 1L])
    min_j <- 1L
    if (ncol(pos_mat) >= 2L) for (j in 2:ncol(pos_mat)) {
        dist <- squared_distance(pos, pos_mat[, j])
        if (dist < collision_radius)
            return(c(j = j, sq_dist = dist))
        if (dist < min_dist) {
            min_dist <- dist
            min_j <- j
        }
    }
    c(j = min_j, sq_dist = min_dist)
}

closest_wall <- function(game, pos = game$player$pos) {
    xlim <- game$scene$xlim
    ylim <- game$scene$ylim
    xdist <- xlim[2L] - abs(pos[1L])
    ydist <- ylim[2L] - abs(pos[2L])
    
    if (xdist < ydist) {
        x <- if (pos[1L] < 0) xlim[1L]  else xlim[2L]
        y <- pos[2L]
        list(dist = xdist, pos = c(x=unname(x), y=unname(y)))
    } else {
        x <- pos[1L]
        y <- if (pos[2L] < 0) ylim[1L]  else ylim[2L]
        list(dist = ydist, pos = c(x=unname(x), y=unname(y)))
    }
}
closest_car <- function(game, pos = game$player$pos) {
    
    if (all(game$dead))
        return(closest_wall(game, pos))
    
    pos_mat <- game$bot_positions()
    collision <- find_collision(pos, pos_mat)
    list(dist = sqrt(collision["sq_dist"]) |> unname(),
         pos = pos_mat[, collision["j"]])
}
closest_car_or_wall <- function(game, pos = game$player$pos) {

    wall <- closest_wall(game, pos)
    
    if (all(game$dead))
        return(wall)
    
    car <- closest_car(game)
    
    if (wall$dist < car$dist)
        return(wall)
    else
        return(car)
}

closest_deaths <- function(game, n = 4L, lambda = 0.05) {
    
    pos <- game$player$pos + game$player$velocity * lambda
    bot_pos <- game$bot_positions()
    wall_pos <- cbind(
        c(x = pos[1L], y = game$scene$ylim[1L]),
        c(x = pos[1L], y = game$scene$ylim[2L]),
        c(x = game$scene$xlim[1L], y = pos[2L]),
        c(x = game$scene$xlim[2L], y = pos[2L])
    )
    
    all_pos <- cbind(bot_pos, wall_pos)
    sq_dists <- apply(all_pos, 2L, squared_distance, pos)
    top <- order(sq_dists)[1:n]
    all_pos <- all_pos[, top]
    angles <- apply(all_pos, 2L, \(p) angle_of(p - pos))
    rbind(all_pos, dist = sqrt(sq_dists[top]), angle = angles)
}

squared_distance <- function(p, q) {
    sum((p - q)^2)
}
