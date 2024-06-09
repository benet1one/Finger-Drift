
library(R6)

source("parameters.R")
source("car.R")
source("scene.R")
source("game.R")


repeat {
    game <- Game$new()
    game$scene$open()
    
    # targets <- replicate(50, game$scene$random_point()) |> t()
    # targets <- targets[rep(1:nrow(targets), each = 100), ]
    
    repeat {
        # target <- targets[k, ]
        if (game$frame %% 40L == 0L)
            target <- if (sum(!game$dead) > 0) {
                game$scene$random_point()
            } else {
                game$scene$random_point(margin = 0.4)
            }
        
        direction <- game$player$target(target)
        game$player$drive(direction)
        game$update()
        game$plot()
        if (game$over)
            break
        points(target[1L], target[2L], cex = 0.5, pch = 3)
        Sys.sleep(spf)
    }
}
    

