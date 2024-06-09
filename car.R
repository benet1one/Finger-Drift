
Car <- R6Class("Car", list(
    pos = c(x = 0, y = 0),
    velocity = c(x = 0, y = 0),
    direction = 0,
    acceleration = 300,
    max_speed = 500,
    rotate_speed = 8,
    friction = 0.98,
    color = "gray30",
    
    initialize = function(pos = c(0, 0), color = "gray30") {
        self$pos <- pos
        self$color <- color
        self$direction <- (angle_of(pos) + pi) %% twopi
        self
    },
    drive = function(direction = 0) {
        self$pos <- self$pos + self$velocity * spf
        self$rotate(direction)
        self$accelerate_and_drift()
        self
    },
    rotate = function(dir = 0) {
        if (is.na(dir))
            dir <- 0
        else if (is.character(dir))
            dir <- switch(
                dir,
                a = -1,
                d = +1,
                left  = -1,
                right = +1,
                0
            )
        self$direction <- self$direction + dir * self$rotate_speed * spf
        self$direction <- self$direction %% twopi
        self
    },
    accelerate_and_drift = function() {
        phi <- angle_of(self$velocity)
        delta <- self$direction
        speed <- norm(self$velocity, "2") * cos(phi - delta)
        accel <- (1 - max(speed, 0)/self$max_speed) * self$acceleration * spf
        
        angle <- min_angle_difference(phi, delta)
        decay <- 1 - angle/pi
            
        vphi <- norm(self$velocity, "2")
        vphi <- vphi * (1 - decay * self$friction)^spf

        self$velocity <- 
            accel * cos_sin(delta) +
            vphi  * cos_sin(phi)
    },
    target = function(target = c(0, 0), deviation = 0.1) {
        relative_pos <- self$pos - target
        target_angle <- angle_of(relative_pos)
        angle_left <- (target_angle - self$direction) %% twopi
        angle_diff <- angle_left - pi
        if (abs(angle_diff) < deviation)
            0
        else if (angle_diff > 0)
            +1
        else
            -1
    },
    plot = function() {
        # dir_pos <- self$pos + 8 * cos_sin(self$direction)
        # lines(x = c(self$pos[1L], dir_pos[1L]), 
        #       y = c(self$pos[2L], dir_pos[2L]),
        #       col = self$color)
        # points(x = self$pos[1L], y = self$pos[2L], pch = 16, col = self$color)
        
        # x <- car_shape$x
        # y <- car_shape$y
        # rotation <- cos_sin(self$direction)
        # x <- x*rotation[1L] - y*rotation[2L]
        # y <- y*rotation[1L] + x*rotation[2L]
        # x <- x + self$pos[1L]
        # y <- y + self$pos[2L]
        
        theta <- (car_shape$theta + self$direction) %% twopi
        shape <- sapply(theta, \(t) self$pos + car_size * cos_sin(t))
        polygon(shape[1L, ], shape[2L, ], col = self$color, border = "#00000000")
    }
))

angle_of <- function(vector) {
    atan2(vector[2L], vector[1L])
}

cos_sin <- function(angle) {
    c(x = cos(angle), y = sin(angle))
}

# not used
min_angle_difference <- function(angle1, angle2) {
    d <- c(angle1 - angle2) * c(-1, +1)
    min(d %% twopi)
}

car_x <- 3
car_y <- 2
car_size <- 4
car_shape <- data.frame(
    x = c(-1, -1,  1,  1) * car_x,
    y = c(-1,  1,  1, -1) * car_y
)
car_shape$theta <- atan2(car_shape$y, car_shape$x)
