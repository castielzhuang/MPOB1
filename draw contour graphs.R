result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino1_2_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Contour abNino1_2v2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.4,0.4), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.4,0.4,by=0.4), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="FFB Yield", xlab="Nino 1+2", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino34_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Contour abNino34v2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.5,0.5), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.5,0.5,by=0.5), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="FFB Yield", xlab="Nino 3.4", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/BEST_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Contour BESTv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.5,0.5), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.5,0.5,by=0.5), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="FFB Yield", xlab="BEST", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/MEI_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Contour MEIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.8,0.8), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.8,0.8,by=0.8), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="FFB Yield", xlab="MEI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/ONI_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Contour ONIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.9,0.9), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.9,0.9,by=0.9), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="FFB Yield", xlab="ONI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/SOI_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Contour SOIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.5,0.5), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.5,0.5,by=0.5), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="FFB Yield", xlab="Negaitve SOI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/abNino1_2_lagest.csv")
max(result[2:12])
min(result[2:12])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/Contour abNino1_2v2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:10, z = as.matrix(result[2:12]), zlim = range(-0.2,0.2), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.2,0.2,by=0.2), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Average OER (%)", xlab="Nino 1+2", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/abNino34_lagest.csv")
max(result[2:12])
min(result[2:12])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/Contour abNino34v2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:10, z = as.matrix(result[2:12]), zlim = range(-0.6,0.6), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.6,0.6,by=0.6), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Average OER (%)", xlab="Nino 3.4", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/BEST_lagest.csv")
max(result[2:12])
min(result[2:12])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/Contour BESTv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:10, z = as.matrix(result[2:12]), zlim = range(-0.2,0.2), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.2,0.2,by=0.2), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Average OER (%)", xlab="BEST", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/MEI_lagest.csv")
max(result[2:12])
min(result[2:12])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/Contour MEIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:10, z = as.matrix(result[2:12]), zlim = range(-0.3,0.3), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.3,0.3,by=0.3), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Average OER (%)", xlab="MEI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/ONI_lagest.csv")
max(result[2:12])
min(result[2:12])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/Contour ONIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:10, z = as.matrix(result[2:12]), zlim = range(-0.9,0.9), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.9,0.9,by=0.9), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Average OER (%)", xlab="ONI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/SOI_lagest.csv")
max(result[2:12])
min(result[2:12])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/AverageOER/main/Contour SOIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:10, z = as.matrix(result[2:12]), zlim = range(-0.4,0.4), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.4,0.4,by=0.4), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Average OER (%)", xlab="Negaitve SOI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/abNino1_2_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/Contour abNino1_2v2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.05,0.05), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.1,0.1,by=0.1), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Oil Yield", xlab="Nino 1+2", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/abNino34_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/Contour abNino34v2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.1,0.1), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.1,0.1,by=0.1), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Oil Yield", xlab="Nino 3.4", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/BEST_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/Contour BESTv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.1,0.1), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.1,0.1,by=0.1), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Oil Yield", xlab="BEST", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/MEI_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/Contour MEIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.2,0.2), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.2,0.2,by=0.2), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Oil Yield", xlab="MEI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/ONI_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/Contour ONIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.1,0.1), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.1,0.1,by=0.1), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Oil Yield", xlab="ONI", cex.main=3, cex.lab=3)
})
dev.off()

result<-read.csv("~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/SOI_lagest.csv")
max(result[2:25])
min(result[2:25])
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/main/Contour SOIv2.tiff", width = 3000, height = 2500, res = 300)
par(mai=c(1,1,1,1),lwd=2)
filled.contour(x = result$X, y = 0:23, z = as.matrix(result[2:25]), zlim = range(-0.1,0.1), nlevels = 100, color.palette = colorRampPalette(c("DeepSkyBlue4","White","Red3")), key.axes =axis(4, seq(-0.1,0.1,by=0.1), cex.axis=2.5), plot.axes={
  axis(1,cex.axis=3)
  axis(2,cex.axis=3)
},plot.title={
  title(main="Oil Yield", xlab="Negaitve SOI", cex.main=3, cex.lab=3)
})
dev.off()