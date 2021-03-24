###NOTE THIS CODE WAS SLIGHTLY ADJUSTED BASED ON THE FOLLOWING GIST. 
####https://gist.github.com/robinvanemden/30969b48a44c2742a18ae14861793741
###Credit for the work goes to Robin Van Emden

library(contextual)
library(data.table)
library(animation)

## 1. Bandit Simulation ---------------------------------------------------------------------------

horizon            <- 200
weights            <- c(0.3,0.35,0.25, 0.38, 0.2)

policy             <- ThompsonSamplingPolicy$new()
bandit             <- BasicBernoulliBandit$new(weights = weights)
agent              <- Agent$new(policy,bandit, "TS")
simulator          <- Simulator$new(agents      = agent,
                                    horizon     = horizon,
                                    set_seed    = 22,
                                    save_theta  = TRUE,
                                    simulations = 1)

hist               <- simulator$run()

td                 <- data.frame(hist$get_theta("TS",to_numeric_matrix = TRUE))
td                 <- data.table(rbind(rep(1,ncol(td)),td))

## 2. Bandit Animation ----------------------------------------------------------------------------

plot_curves        <- function(td,i) {
  plot(NULL, xlim = c(0,1), ylim = c(0,8), ylab = "Density", xlab  = "Theta",
       main      = paste0("ThompsonSampling\nt = ",
                          sprintf("%03d", trunc(i/2)+1),
                          " | choice = ",
                          hist$data$choice[[i]],
                          " | reward = ",
                          hist$data$reward[[i]], "\n"))
  curve(dbeta(x,as.numeric(td[i,1]),as.numeric(td[i,6])), from=0, to=1, col="green", add = TRUE)
  curve(dbeta(x,as.numeric(td[i,2]),as.numeric(td[i,7]))-0.05, from=0, to=1, col="red", add = TRUE)
  curve(dbeta(x,as.numeric(td[i,3]),as.numeric(td[i,8]))-0.1, from=0, to=1, col="blue", add = TRUE)
  curve(dbeta(x,as.numeric(td[i,3]),as.numeric(td[i,9]))-0.15, from=0, to=1, col="orange", add = TRUE)
  curve(dbeta(x,as.numeric(td[i,3]),as.numeric(td[i,10]))-0.20, from=0, to=1, col="black", add = TRUE)
  legend("bottomright",bty ="n",xpd = TRUE, inset=c(-0.42, 0), title = "Arms",
         legend = c("Arm 1", "Arm 2", "Arm 3","Arm 4","Arm 5"), fill = c("green", "red", "blue","orange","black"))
}

plot_lines        <- function(td,i) {
  abline(v=td[i,11],col="green")
  abline(v=td[i,12],col="red")
  abline(v=td[i,13],col="blue")
  abline(v=td[i,14],col="orange")
  abline(v=td[i,15],col="black")
}

message("Starting compilation of animation")
library(animation)

animation::ani.options(interval = 0.06, ani.width = 450, ani.height = 400, verbose = FALSE)
saveHTML({
  for (i in seq(from = 1, to = horizon)) {
    par(mar = c(5,4,4,9))
    plot_curves(td,i)
    ani.pause()
    plot_curves(td,i)
    plot_lines(td,i+1)
    ani.pause()
  }
}, htmlfile = "index.html", img.name = "ts", navigator = FALSE, imgdir = "ts",
autoplay = FALSE, single.opts = "'theme': 'light', 'utf8': false,'controls':
                              ['first', 'previous', 'play', 'next', 'last', 'loop','speed']")
message("Completed animation")
invisible(tryCatch(dev.off(), error=function(e){}))
