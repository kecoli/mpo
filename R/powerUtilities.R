powerUtilities = function()
{
  x = seq(.01,3,.01)
  lwd = 2
  y = log(x)
  ylim = c(min(y),max(y))
  plot(x,y,type = "l",ylim =c(-8,2),lwd = lwd,xlim = c(0,3.01),
       xlab = "WEALTH V",ylab = "U(V)")
  gamma = -1
  y = (x^gamma - 1)/gamma
  lines(x,y,lty = 8,lwd = lwd)
  gamma = .5
  y = (x^gamma - 1)/gamma
  lines(x,y,lty = 3,lwd = lwd)
  abline(v = 0)
  legend(1.2,-5.5,c("Gamma  =  .5", "Log Utility","Gamma  =  -1"),
       lty = c(3,1,8),lwd = 2)
}
