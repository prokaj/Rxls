calc.cn<-function(lx,
                  nu=1/(1+i),
                  i=if (!missing (nu)) (1/nu)-1){
  x <- seq_along(lx)-1
  ## 0-val indul az indexelés
  lx1<- lx[-length(lx)]
  qx <- -0.95*diff(lx)/(lx1+(lx1==0))
  ## módosított halandóság
  lx <- lx[1]*cumprod(c(1,1-qx))
  ## módosított l
  df <- data.frame(x=x,lx=lx,qx=c(qx,1),Dx=lx*nu^x)
  df$dx <- with(df,lx*qx)
  df$Cx <- with(df,dx*nu^(x+.5))
  df$Nx <- rev(cumsum(rev(df$Dx)))
  df$Mx <- rev(cumsum(rev(df$Cx)))
  df$Rx <- rev(cumsum(rev(df$Mx)))
  attr(df,"nu")<-nu
  attr(df,"i")<-i
  df
}
withObj(THISXL$workbooks(.wbname)$worksheets(),{
          if(is.null(.$item(wsname)))
            with(.$add(),{
                   .[["name"]]<-wsname
                   lx<-XLreaddf.cols('lxdata')
                   cn<-calc.cn(lx[[nem]],i=i)
                   XLwritedf (cn,XLrange=.$cells(1,1))
               })
      })
