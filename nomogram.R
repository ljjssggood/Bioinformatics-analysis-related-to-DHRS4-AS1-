library(rms)
dnoma <- read.delim(opt$file, check.names=F)
rownames(dnoma) <- dnoma[,1]
if(file_test('-f', opt$variable)){
	msv <- readLines(opt$variable)
}else{
	msv <- strsplit(opt$variable, ',')[[1]]
}
times <- as.numeric(strsplit(opt$time, ',')[[1]])
da <- dnoma[, c('time', 'status', msv)]
dd <- datadist(da)
options(datadist='dd')
fm <- paste('Surv(time, status) ~ `', paste(msv,  collapse='` + `'), '`', sep='')
model <- cph(formula=as.formula(fm), x=T, y=T, data=da, surv=T, time.inc=median(times))
surv <- Survival(model)
fun.list <- list()
for(i in times){
	fun <- eval(parse(text=paste('function(x)surv('i, ',lp=x)', sep='')))
	fun.list <- c(fun.list, fun)
}
nom <- nomogram(model, fun=fun.list, lp=F, funlabel=paste(times/12, '-Year Survival', sep=''), maxscale=100, fun.at=c('1', '0.95', '0.9', '0.8', '0.6','0.4', '0.2', '0.1', '0'))

png(paste(odir, '/nomogram.png',sep=''), width=10, height=7, res=600, units='in')
plot(nom, xfrac=0.25)
dev.off()
pdf(paste(odir, '/nomogram.pdf',sep=''), width=10, height=7)
plot(nom, xfrac=0.25)
dev.off()