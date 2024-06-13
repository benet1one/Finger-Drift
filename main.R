
library(R6)

source("parameters.R")
source("car.R")
source("scene.R")
source("game.R")


repeat {
    game <- Game$new()
    game$scene$open()
    
    repeat {
        # if (game$frame %% 40L == 0L)
        #     target <- if (sum(!game$dead) > 0) {
        #         game$scene$random_point()
        #     } else {
        #         game$scene$random_point(margin = 0.4)
        #     }
        # direction <- game$player$target(target)
        # points(target[1L], target[2L], cex = 0.5, pch = 3)
        
        deaths <- closest_deaths(game)
        w <- 5
        x <- weighted.mean(deaths["x", ], deaths["dist", ]^-w)
        y <- weighted.mean(deaths["y", ], deaths["dist", ]^-w)
        angle <- angle_of(game$player$pos - c(x, y))
        direction <- game$player$target_angle(angle)
        
        game$player$drive(direction)
        game$update()
        game$plot()
        # points(deaths["x", ], deaths["y", ], cex = 0.5, col = "white")
        points(x, y, pch = 3, cex = 0.5, col = "red3")
        if (game$over)
            break
        Sys.sleep(spf)
    }
    
    Sys.sleep(2)
}
    

