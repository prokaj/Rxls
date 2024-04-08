patch<-
"--- old/RODBC/src/RODBC.c	2015-06-29 12:15:44.000000000 +0200
+++ new/RODBC/src/RODBC.c	2015-08-21 14:19:34.834382399 +0200
@@ -29,13 +29,15 @@
 #ifdef WIN32
 # include <windows.h>
 # undef ERROR
+// comment out the next few lines in the original source and include the R_interactive line below.
 /* enough of the internals of graphapp objects to allow us to find the
    handle of the RGui main window */
-typedef struct objinfo {
-	int	kind, refcount;
-	HANDLE	handle;
-} *window;
-__declspec(dllimport) window RConsole;
+//typedef struct objinfo {
+//	int	kind, refcount;
+//	HANDLE	handle;
+//} *window;
+//__declspec(dllimport) window RConsole;
+__declspec(dllimport) extern int R_Interactive;
 #else
 # include <unistd.h>
 #endif
@@ -232,10 +234,15 @@
 	if(asLogical(ReadOnly))
 	    SQLSetConnectAttr(thisHandle->hDbc, SQL_ATTR_ACCESS_MODE,
 			      (SQLPOINTER) SQL_MODE_READ_ONLY, 0);
+#ifdef WIN32
+	HWND desktopHandle = GetDesktopWindow();
+#endif
 	retval =
 	    SQLDriverConnect(thisHandle->hDbc,
 #ifdef WIN32
-			     RConsole ? RConsole->handle : NULL,
+			     // insert the next line and comment the original
+			     R_Interactive ? desktopHandle : NULL,
+			     //RConsole ? RConsole->handle : NULL,
 #else
 			     NULL,
 #endif
@@ -248,7 +255,8 @@
 			     (SQLSMALLINT) buf1_len,
 			     &tmp1,
 #ifdef WIN32
-			     RConsole ? SQL_DRIVER_COMPLETE : SQL_DRIVER_NOPROMPT
+			     // insert the next line and comment the original
+			     (R_Interactive && (desktopHandle!=NULL)) ? SQL_DRIVER_COMPLETE : SQL_DRIVER_NOPROMPT
 #else
 			     SQL_DRIVER_NOPROMPT
 #endif
"
get.position<-function(x,prefix=c("+","-"))
{
  prefix <- match.arg(prefix)
  position <- as.numeric(strsplit(sub(sprintf(".*[%s]([0-9]+),([0-9]+).*",prefix),
                                      "\\1:\\2",x[1]),":")[[1]])
  names(position) <- c("start","length")
  position
}

decode_hunk<-function(x){
  old.position<-get.position(x[1],"-")
  new.position<-get.position(x[1],"+")
  x<-x[-1]
  new<-grep("^[+]",x)
  old<-grep("^-",x)
  old.text<-sub("^.","",x[-new])
  new.text<-sub("^.","",x[-old])
  list(old=c(as.list(old.position),list(text=old.text)),
       new=c(as.list(new.position),list(text=new.text)))
}

read.patch<-function(file,text="",
                     .lines=if(!missing(file))readLines(file)else unlist(strsplit(text,split = "\n")))
  {
  ind<-grep("^$|^[+][+][+]|^---",.lines)
  header<-.lines[ind]
  .lines<-.lines[-ind]
  hunks<- split.default(.lines,findInterval(seq_along(.lines),grep("^@@",.lines)))
  list(header=header,hunks=lapply(hunks,decode_hunk))
}

fname<-function(x,n=1,prefix="^-{3}",basedir=getwd()){
    x<-grep(prefix,x,value=TRUE)
    x<-sub(paste(prefix,"*"),"",x)
    x<-sub("[-[:space:][:digit:]:\\.+]*$","",x)
    normalizePath(
      file.path(basedir,paste(strsplit(x,"/")[[1]][-seq_len(n)],collapse="/")),
      mustWork = FALSE
    )
}

trimline<-function(x){
  gsub("^ | $","",gsub("[[:space:]]+"," ",x))
}

testlines<-function(x,y){
  all(trimline(x)==trimline(y))
}
apply.patch<-function(patch,n=1, basedir=getwd()){
  with(patch,{
    old.file<-fname(x=header,basedir=basedir,n=n,prefix="^-{3}")
    new.file<-fname(x=header,basedir=basedir,n=n,prefix="^[+]{3}")
    .lines=readLines(old.file)
    offset<-1
    .out=character(0)
    for(hunk in hunks){
      ind<-seq_len(hunk$old$start-offset)
      .out<-c(.out,.lines[ind])
      .lines<-.lines[-ind]
      offset<-offset+length(ind)
      ind2<-seq_along(hunk$old$text)
      if(testlines(.lines[ind2],hunk$old$text)){
        .lines<-.lines[-ind2]
        .out<-c(.out,hunk$new$text)
        offset<-offset+length(ind2)
      }else{
        warning(sprintf("hunk:\n%s\ndoes not match and skipped!!",
                        paste(paste("|",trimline(.lines[ind2]),"|",trimline(hunk$old$text),"|",sep=""),
                              collapse="\n")))
      }
    }
    .out<-c(.out,.lines)
    writeLines(.out,con=new.file)
  })
}

patch.RODBC<-function(file,patch=readLines(file)){
  wd.old<-getwd();on.exit(setwd(wd.old))
  wd<-tempfile()
  dir.create(wd)
  x<-download.packages("RODBC",destdir=wd,type = "source")
  setwd(wd)
  untar(tarfile = x[1,2],exdir = wd)
  if(inherits(try(system("patch1 -Np1 ",input = patch,intern=T,ignore.stderr = T),
                  silent = T),what = "try-error")){
    cat("applying poor man's patch")
    apply.patch(patch = read.patch(text=patch))
  }
  dcf<-read.dcf("RODBC/DESCRIPTION")
  dcf[1,"Version"]<-paste(dcf[1,"Version"],1,sep=".")
  dcf[1,"Date"]<-Sys.Date()
  dcf[1,"Description"]<-paste(dcf[1,"Description"],"modified for Rstudio")
  write.dcf(dcf,file="RODBC/DESCRIPTION")
  system("$R_HOME/bin/R CMD build RODBC")
  tools::write_PACKAGES(".")
  update.packages(contriburl = paste("file://",wd,sep=""))
  unlink(wd,T,T)
  ##  tar(tarfile = x[1,2],compression = "gzip",
  ##      files = dir("RODBC",full.names = TRUE, recursive = T))
  ##file.show(file.path(wd,"RODBC","src","RODBC.c"))
}

patch.RODBC(patch = patch)
