x <- c(0, 10, 0, 0)
y <- c(0, 0, 10, 0)
z <- c(0, 0, 0, 10)
labels <- c("", "X", "Y", "Z")
i <- c(1, 2, 1, 3, 1, 4)

# *3d interface

open3d()
text3d(x, y, z, labels)
points3d(0, 3, 0, col = 'red', size = 10)
points3d(2, 0, 0, col = 'red', size = 10)
points3d(0, 1, 3, col = 'red', size = 10)
points3d(0, 1, 2, col = 'green', size = 10)
points3d(-1, 0, 1, col = 'green', size = 10)
points3d(1, 1, 1, col = 'green', size = 10)
points3d(0, 0, 0, col = 'black', size = 10)
text3d(.005, .005, -.2, 'Test point')
segments3d(x[i], y[i], z[i])