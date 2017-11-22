setwd("Y:\\dbs-package")
code_files<-paste0("v0.4/R/",c(#"additionalmath.c.template",
                            "calccb.R",
                            "nan.plot.R",
                            "parconf.R",
                            "parsetgen.R",
                            "plot.mod.frame.R",
                            "read.list.R",
                            "read.slv.R"
                            #"slv25tab.csv"
                            ))
#package.skeleton(name="dbs",force =T, code_files=code_files)

### build, install,  check
system("R CMD build \"Y:\\dbs-package\"")
#system("R CMD INSTALL --build --compile-both \"Y:\\dbs-package\\dist\\dbs_0.9.1.tar.gz\"")
system("R CMD check  \"C:\\Users\\evgen\\YandexDisk\\dbs_0.99.tar.gz\"")

install.packages("Z:\\_temp\\dbs_0.99.tar.gz", repos = NULL, type = "source")
library("dbs")
#detach("package:dbs", unload=TRUE)
#remove.packages("dbs")

### testing
load(file="data/data_cb_isopt.RData")
test<-subset(data_cb_isopt, subset=(var_id=="X_T_D" & main_factor=="0-0.5-0"), select=c("t", "quant_0.5"))
nan.plot(as.numeric(test$t),test$quant_0.5, type="l", log="xy")
nan.plot(as.numeric(test$t),test$quant_0.5, type="l", ylim=c(1,100), log="y")
nan.plot(as.numeric(test$t),test$quant_0.5, type="l", xlim=c(1,40), ylim=c(100,1500))
nan.points(100,10000, pch=19)

### testing calccb
example4_cb<-calccb(input=example4_output,
                    x.col="t",
                    x.seq=seq(0,96,by=0.5),
                    y.col=c("C0","C1"),
                    #factor.col = c("Dose"),
                    par_calc = F
)
xyplot(quant_0.025+quant_0.5+quant_0.975~t|var_id+group,
       data=example4_cb,
       type="l",
       lty=c(2,1,2),
       xlab="Time, h",
       ylab="Concentration of drug, ng/ml",
       main="All CB simulations")

### testing calcop
example4_op<-calcop(input=example4_output_op,
                    x.col="t",
                    x.seq=seq(0,96,by=0.5),
                    y.col=c("C0","C1")
                    #factor.col = c("T")
)

### testing plot.mod.frame
pdf(width=2*5, height=6*5)
plot(example4_cb,log="", lty=c(2,1,2), col=c("blue", "black", "blue"))
dev.off()

download.file(url="https://sourceforge.net/projects/dbsolve/files/dbs-package/example4.dat", destfile="example4.dat")