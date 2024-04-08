require(knitr)
require(tools)
setwd("F:/Desktop/2013/2013_AEGON/R/")
attach(name="knitall",what=local({
  collect.rnws<-function(base.file,root.dir,
                         Rsubdir="R",
                         pat=sprintf("%s-([0-9]+).Rnw$",base.file),
                         first.n){
    if(missing(base.file)) 
      stop("'base.file' is missing!")
    if(missing(root.dir)){
      root.dir<-file.path(dirname(base.file),Rsubdir)
    }
    old.dir<-getwd()
    setwd(root.dir)
    on.exit(setwd(old.dir))
    
    base.file<-file_path_sans_ext(basename(base.file))
    
    flist<-dir(pattern=pat)
    flist<-flist[order(as.numeric(sub(pat,"\\1",flist)))] #,as.numeric(sub(pat,"\\2",flist)))]
    if(!missing(first.n) && first.n>0) 
      flist<-flist[seq_len(first.n)]
    txt<-lapply(flist,readLines,encoding="UTF-8")
    id<-sapply(txt,"[",1)
    txt<-unlist(txt)
    attr(txt,"files")<-flist
    attr(txt,"id")<-id
    invisible(txt)
  }
  
  knit.txt<-function(txt=collect.rnws("../Rxls.tex"),attrs=attributes(txt)){
    opts_knit$restore()
    opts_chunk$restore()
    pat_rnw();
    render_pv(); 
    opts_knit$set('out.format'='latex')
    res<-knit(text=txt,output=NULL,envir=new.env())
    attributes(res)<-attrs
    invisible(res)
  }
  
  split.at<-function(x,at)
    split(x,findInterval(seq_along(x),at-.5))
  
  save.knit<-function(txt=knit.txt(),
                      files=attr(txt,"files"),
                      id=attr(txt,"id")){
    res<-unlist(strsplit(txt,split="\n"))
    ind<-match(id,res)
    res<-split.at(res,ind)
    files<-sub("Rnw$","tex",files)
    names(res)<-files
    for(nn in files){
      con<-file(description=nn,open="w",encoding="UTF-8")
      writeLines(res[[nn]],sep="\n",con=con)
      close(con)
    }
    invisible(res)
  }
  
  tangle.txt<-function(txt){
    i<-0
    chknum<-function(x,options){
      cat(sprintf("### chunk num:%d\n",i));flush.console()
      i<<-i+1
      sprintf("### chunk num:%d\n %s",i,paste(x,collapse=""))
    }
    oknit<-opts_knit$set(tangle=TRUE,documentation=2)
    on.exit(opts_knit$set(oknit),add=TRUE)
    ochunk<-opts_chunk$set(comment=NULL,eval=FALSE,echo=TRUE)
    knit_hooks$set(chunk=chknum)
    on.exit(opts_chunk$set(ochunk),add=TRUE)
    txt<-knit(text=txt,output=NULL,tangle=TRUE)
    txt<-strsplit(txt,"\n")[[1]]
    ind<-grep("## ----[a-z='\",]*-*",txt,ignore.case = TRUE)
    labels<-sprintf("## chunk %d.",seq_along(ind))
    txtind<-seq_along(txt)+findInterval(seq_along(txt),ind)
    txt[txtind]<-txt
    txt[ind+seq_along(ind)-1]<-labels
    txt
  }
  
  all.knit<-function(base.file="../Rxls.tex",...){
    txt<-collect.rnws(base.file,...)
    con<-file(open="w",description="../Rfiles/R_chunks_in_Rxls_user_guide.R",
              encoding="utf-8")
    on.exit(close(con))
    writeLines(tangle.txt(txt),con=con)
    save.knit(knit.txt(txt))
  }
  
  short.lines<-function(x,width=opts_chunk$get("tidy.opts")$width.cutoff+20){
    ind<-sapply(x,nchar)>width
    if(any(ind)){
      x<-as.list(x)
      x[ind]<-lapply(x[ind],break.line,width=width)
      x<-unlist(x)
    }
    x
  }
  
  break.line<-function(line,width){
    if(nchar(line)<=width) return(line)
    sp<-gregexpr("\\s+",line)[[1]]
    ind<-max(which(sp<width))
    if(length(ind)==0){
      b<-width;e<-b+1
    }else{
      b<-sp[ind]-1
      e<-b+attr(sp,"match.length")[ind]+1
    }
    c(substr(line,1,b),break.line(substr(line,e,nchar(line)),width=width))
  }
  
  showx<-function(name,x){
    x<-short.lines(unlist(strsplit(x,"\n")))
    cat(name,":\n|",paste(x,collapse="\n"),"|\n",sep="");flush.console();
    paste("\\noindent{\\color{",name,"color}",
          knitr:::escape_latex(x),"}\n",sep="")
  }
  
  render_pv<-function(){
    knit_hooks$restore()
    knit_hooks$set(
      source=function(x,options){
        x<-knitr:::hilight_source(x,"latex",options)
        showx("src",x)
      },
      output=function(x,options){
        showx("out",x)
      },
      warning=function(x,options){
        showx("warning",x)
      },
      message=function(x,options){
        showx("message",x)
      },
      error=function(x,options){
        showx("error",x)
      },
      chunk=function(x,options){
        sprintf("\\begin{kframe}\n%s\\end{kframe}\n",paste(x,collapse=""))
      },
      document=function(x){
        ##browser()
        sub("\n+$","\n",paste(x,collapse=""))
      }
    )
  }
  environment()
}))
