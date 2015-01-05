library(lattice)
bandplot <- function(formula, data, ...) {
    my.panel.bands <- function(x, y, upper, lower, col, fill, subscripts, ...) {
        order <- order(x)
        upper <- upper[subscripts[order]]
        lower <- lower[subscripts[order]]
        x <- x[order]
        panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col=fill, border = FALSE, ...)
        panel.lines(x, upper, col = 'red');
        panel.lines(rev(x), rev(lower), col = 'red');
    }

    my.prepanel <- function(x,y,upper,lower,...) { 
        p<-prepanel.default.xyplot(x,y,...);
        p$ylim <- c(min(lower), max(upper));
        p
    }

    my.panel <- function(x, y,...) {
           panel.superpose(x, y, panel.groups = my.panel.bands, type='b', col='gray',...);
           panel.xyplot(x, y, type='p', cex=0.6, lty=1,...);           
           panel.loess(x, y, ...);
    }

    xyplot(formula, data, prepanel = my.prepanel, panel = my.panel, ...)
}

my.count <- 20
my.x <- rnorm(my.count)
my.data <- data.frame(x = my.x, y = 5 + 2 * my.x)
bandplot(y ~ x, data = my.data, upper = my.data$y + 5, lower = my.data$y - 1, groups = (x<0))

my.data <-  data.frame(x=my.x, y=my.x + runif(my.count, -1, 1))
bandplot(y ~ x, data = my.data, upper=my.data$y+2, lower=my.data$y-2, groups=(x<100))
