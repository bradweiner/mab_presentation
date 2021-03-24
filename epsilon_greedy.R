###NOTE THIS CODE WAS SLIGHTLY ADJUSTED BASED ON THE FOLLOWING GIST. 
####https://gist.github.com/robinvanemden/30969b48a44c2742a18ae14861793741
###Credit for the work goes to Robin Van Emden

test

library(contextual)
library(data.table)
library(animation)

## 1. Bandit Simulation ---------------------------------------------------------------------------

# Run a simulation that saves the policy's theta values

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
bandit             <- BasicBernoulliBandit$new(weights = c(0.3,0.35,0.25, 0.38, 0.2))
agent              <- Agent$new(policy,bandit, "EG")
simulator          <- Simulator$new(agents      = agent,
                                    horizon     = 200,
                                    set_seed    = 8,
                                    save_theta  = TRUE,
                                    simulations = 1)
hist <- simulator$run()

# retrieve saved parameter values

td <- hist$get_theta("EG",to_numeric_matrix = TRUE)

## 2. Bandit Animation ----------------------------------------------------------------------------

# Create color matrix based on theta values

color_matrix <- matrix(c("gray","gray","gray","gray","gray"),nrow(td),5)
color_matrix[cbind(which(td[,"exploit"]==1),td[as.logical(td[,"exploit"]),"choice"])] <- "green"
color_matrix[cbind(which(td[,"exploit"]==0),td[!as.logical(td[,"exploit"]),"choice"])] <- "red"
colnames(color_matrix) <- c("C1","C2","C3","C4","C5")
cm <- as.data.frame(color_matrix, stringsAsFactors = FALSE)

message("Starting compilation of animation")
library(animation)
animation::ani.options(interval = 0.3, ani.width = 450, ani.height = 400, verbose = FALSE)
saveHTML({
  animation::ani.options(interval = 0.05)
  for (i in 1:200) {
    
    
    par(mar = c(5,4,4,9))
    barplot(c(td[i,"mean1"]+0.015, td[i,"mean2"]+0.015, td[i,"mean3"]+0.015, td[i,"mean4"]+0.015, td[i,"mean5"]+0.015),
            ylim      = c(0,1.015),
            ylab      = "Average Reward",
            xlab      = "Arm",
            main      = paste0("EpsilonGreedy\nt = ",
                               sprintf("%03d", i),
                               " | choice = ",
                               hist$data$choice[[i]],
                               " | reward = ",
                               hist$data$reward[[i]], "\n"),
            names.arg = c("1",    "2",    "3",    "4",    "5"),
            col       = c(cm[i,"C1"], cm[i,"C2"], cm[i,"C3"], cm[i,"C4"], cm[i,"C5"])
    )
    box()
    axis(side = 1, at = c(0.7,1.9,3.1), labels = FALSE)
    legend("bottomright",bty ="n",xpd = TRUE, inset=c(-0.42, 0),
           title = "Choice",
           legend = c("Exploiting", "Exploring"), fill = c("green", "red"))
    ani.pause()
  }
}, htmlfile = "index.html", img.name = "eg", navigator = FALSE, imgdir = "eg",
autoplay = FALSE, single.opts = "'theme': 'light', 'utf8': false,'controls':
                              ['first', 'previous', 'play', 'next', 'last', 'loop','speed']")
message("Completed animation")
invisible(tryCatch(dev.off(), error=function(e){}))
