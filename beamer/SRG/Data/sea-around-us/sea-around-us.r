sau.dat <- read.csv("./sea-around-us/sea-around-us-comparison.csv")
## sau.dat <- read.csv("sea-around-us-comparison.csv")

make.total.catch.comparison.plot <- function(data, lwd = 2, lty = 1){
  plot(data$TOTAL / 1000~data$Year,
       xlab = "",
       ylab = "Total Catch ('000s mt)",
       ylim = c(0,500),
       type = "l",
       lty = lty,
       lwd = lwd,
       col = "blue")
  lines(data$TOTAL_SAUP/1000~data$Year,
        lwd = lwd,
        lty = lty,
        col = "red")
  legend("topleft", c("2016 hake assessment","Sea Around Us Project"),
         col = c("blue", "red"), lty = c(1, 1), bty = 'n')
}

make.total.catch.can.comparison.plot <- function(data, lwd = 2, lty = 1){
  plot(data$CANtotal[-1] / 1000 ~ data$Year[-1],
       xlab = "",
       ylab = "Total Catch (CAN '000s mt)",
       ylim = c(0,500),
       type = "l",
       lty = lty,
       lwd = lwd,
       col = "blue")
  lines(data$CAN_SAUP_Total[-1] / 1000 ~ data$Year[-1],
        lwd = lwd,
        lty = lty,
        col = "red")
  legend("topleft", c("2016 hake assessment","Sea Around Us Project"),
         col = c("blue", "red"), lty = c(1, 1), bty = 'n')
}

make.total.catch.us.comparison.plot <- function(data, lwd = 2, lty = 1){
  plot(data$Ustotal / 1000 ~ data$Year,
       xlab = "",
       ylab = "Total Catch (CAN '000s mt)",
       ylim = c(0,500),
       type = "l",
       lty = lty,
       lwd = lwd,
       col = "blue")
  lines(data$US_SAUP_Total / 1000 ~ data$Year,
        lwd = lwd,
        lty = lty,
        col = "red")
  legend("topleft", c("2016 hake assessment","Sea Around Us Project"),
         col = c("blue", "red"), lty = c(1, 1), bty = 'n')
}

##make.total.catch.us.comparison.plot(sau.dat)
