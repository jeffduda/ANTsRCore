#' joint label and intensity fusion
#'
#' A multiple atlas voting scheme to customize labels for a new subject. This
#' function will also perform intensity fusion. It almost directly calls the
#' \code{C++} in the ANTs executable so is much faster than other variants in ANTsR.
#' One may want to normalize image intensities for each input image before
#' passing to this function.  If no labels are passed, we do intensity fusion.
#' Note on computation time: the underlying \code{C++}
#' is multithreaded.  You can control the number of threads by setting the
#' environment variable \code{ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS} e.g. to use all or
#' some of your CPUs.  This will improve performance substantially. For instance,
#' on a macbook pro from 2015, 8 cores improves speed by about 4x.
#'
#' @param targetI antsImage to be approximated
#' @param targetIMask mask with value 1
#' @param atlasList list containing antsImages with intensity images
#' @param beta weight sharpness, default to 2
#' @param rad neighborhood radius, default to 2
#' @param labelList optional list containing antsImages with segmentation labels
#' @param rho ridge penalty increases robustness to outliers but also
#'   makes image converge to average
#' @param usecor employ correlation as local similarity
#' @param rSearch radius of search, default is 3
#' @param nonnegative constrain weights to be non-negative
#' @param verbose boolean
#' @return approximated image, segmentation and probabilities
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich, Nicholas J. Tustison
#' @keywords fusion, template
#' @importFrom grDevices colorRampPalette dev.off heat.colors hsv png rainbow rgb
#' @importFrom graphics hist par plot points
#' @importFrom magrittr %>%
#' @importFrom methods is new
#' @importFrom stats ar as.formula coefficients convolve
#' @importFrom stats cor cor.test cov dist formula glm lm
#' @importFrom stats lm.fit loess median model.matrix na.omit
#' @importFrom stats optimize p.adjust pchisq pf pnorm ppois
#' @importFrom stats predict pt qchisq qf qnorm qt quantile
#' @importFrom stats residuals rnorm sd spec.pgram spline stl
#' @importFrom stats t.test toeplitz ts var
#' @importFrom utils data glob2rx read.csv setTxtProgressBar tail txtProgressBar write.csv
#' @examples
#'
#' set.seed(123)
#' ref <- ri( 1 )
#' ref<-resampleImage(ref,c(50,50),1,0)
#' ref<-iMath(ref,"Normalize")
#' mi<- ri( 2 )
#' mi2<-ri( 3 )
#' mi3<-ri( 4 )
#' mi4<-ri( 5 )
#' mi5<-ri( 6 )
#' refmask<-getMask(ref)
#' refmask<-iMath(refmask,"ME",2) # just to speed things up
#' ilist<-list(mi,mi2,mi3,mi4,mi5)
#' seglist<-list()
#' for ( i in 1:length(ilist) )
#'  {
#'  ilist[[i]]<-iMath(ilist[[i]],"Normalize")
#'  mytx<-antsRegistration(fixed=ref , moving=ilist[[i]] ,
#'    typeofTransform = c("Affine"), verbose = TRUE )
#'  mywarpedimage<-antsApplyTransforms(fixed=ref,
#'  moving=ilist[[i]],
#'    transformlist=mytx$fwdtransforms)
#'  ilist[[i]]=mywarpedimage
#'  seg<-thresholdImage( ilist[[i]],"Otsu", 3)
#'  seglist[[i]]<-seg
#'  }
#' r<-2
#' pp<-jointLabelFusion(ref,refmask,ilist, rSearch=2,
#'   labelList=seglist, rad=rep(r, length(dim(ref)) ) )
#' pp2<-jointLabelFusion(ref,refmask,ilist, rSearch=2,
#'   labelList=seglist, rad=rep(r, length(dim(ref)) ) )
#' testthat::expect_equal(pp2$segmentation, pp$segmentation)
#' pp<-jointLabelFusion(ref,refmask,ilist, rSearch=2,
#'   rad=rep(r, length(dim(ref)) ) )
#'
#' \dontrun{
#' ref = antsImageRead( getANTsRData("ch2") )
#' n = 50
#' ref = resampleImage(ref,c(n,n,n),1,0)
#' ref = iMath(ref,"Normalize")
#' refmask = getMask(ref)
#' ilist = list()
#' seglist = list()
#' for ( k in 1:5 ) {
#' mi = antsImageClone( ref ) + rnorm( n^3, 0, 0.1 )
#' mykseg = kmeansSegmentation( mi, 3, refmask )$segmentation
#' ilist[[ k ]] = mi
#' seglist[[ k ]] = mykseg
#' }
#' pp = jointLabelFusion( ref, refmask,ilist, rSearch=2,
#'  labelList=seglist, rad=rep(2, length(dim(ref)) ), verbose=TRUE )
#' plot( ref, pp$segmentation )
#' plot( pp$intensity  )
#' }
#'
#' @export jointLabelFusion
jointLabelFusion <- function(
  targetI,
  targetIMask,
  atlasList,
  beta=4,
  rad=2,
  labelList = NULL,
  rho=0.01,
  usecor=FALSE,
  rSearch=3,
  nonnegative = FALSE,
  verbose = FALSE )
{
  targetI = check_ants(targetI)
  targetIMask = check_ants(targetIMask)
  segpixtype = 'unsigned int'
  if ( is.null( labelList ) ) doJif = TRUE else doJif = FALSE
  if ( ! doJif ) {
    if ( length(labelList) != length(atlasList) )
      stop("length(labelList) != length(atlasList)")
    inlabs = sort( unique(  labelList[[ 1 ]][ targetIMask == 1 ]  ) )
    labsum = labelList[[1]]
    for ( n in 2:length( labelList ) ) {
      inlabs = sort( unique( c( inlabs, labelList[[ n ]][ targetIMask == 1 ]  ) ) )
      labsum = labsum + labelList[[ n ]]
      }
    mymask = antsImageClone( targetIMask )
    mymask[ labsum == 0 ] = 0
    } else mymask = ( targetIMask )
  tdir <- tempdir()
  segdir <- tempdir()
  osegfn <- tempfile(pattern = "antsr", tmpdir = segdir, fileext = "myseg.nii.gz")
  if ( file.exists( osegfn ) ) file.remove( osegfn )
  probs <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = "prob%02d.nii.gz")
  probsbase <- basename( probs )
  searchpattern <- sub("%02d", "*", probsbase)
  mydim <- as.numeric( targetIMask@dimension )
  if ( ! doJif ) {
    outimg <- new("antsImage", segpixtype, mydim)
    outimgi <- new("antsImage", 'float', mydim)
    outs <- paste("[",
      antsrGetPointerName(outimg),",",
      antsrGetPointerName(outimgi), ",", probs, "]", sep = "")
    } else {
      outimgi <- new("antsImage", 'float', mydim)
      outs <- antsrGetPointerName(outimgi)
    }
  # this is a temporary FIXME for some type issue i cant figure out right now
#  outs <- paste("[",
#    osegfn,",",
#    antsrGetPointerName(outimgi), ",", probs, "]", sep = "")
  mymask <- antsImageClone( mymask, segpixtype)
  if ( length( rad ) == 1 ) myrad = rep( rad, mydim ) else myrad = rad
  if ( length( myrad ) != mydim )
    stop("patch radius dimensionality must equal image dimensionality")
  myrad = paste( myrad, collapse='x')
  if ( verbose == TRUE ) vnum = 1 else vnum = 0
  if ( nonnegative == TRUE ) nnum = 1 else nnum = 0
  myargs <- list(
    d = mydim,
    t = targetI,
    a = rho, # or alpha in the paper
    b = beta,
    c = nnum,  # constrain non-negative
    p = myrad, # patch radius
    m = "PC",
    s = rSearch,
#    e =     # -e, --exclusion-image label[exclusionImage] # FIXME
    x = mymask,
    o = outs,
    v = vnum )
  # now add the intensity and label images
  kct = length( myargs )
  for ( k in 1:length( atlasList ) )
    {
    kct = kct + 1
    myargs[[ kct ]] = atlasList[[ k ]]
    names( myargs  )[[ kct ]]  = "g"
    if ( ! doJif ) {
      kct = kct + 1
      castseg = antsImageClone( labelList[[ k ]], segpixtype )
      myargs[[ kct ]] = castseg
      names( myargs )[[ kct ]]  = "l"
      }
    }
  .Call("antsJointFusion", .int_antsProcessArguments(c(myargs)), PACKAGE = "ANTsRCore" )
  if ( doJif ) return( outimgi )
  probsout <- list.files(path = tdir,
    pattern = glob2rx(searchpattern), full.names = TRUE,
    recursive = FALSE)
  pimg <- antsImageRead( probsout[1] )
  probimgs <- c( pimg )
  for (x in c( 2:length( probsout ) ) ) {
    probimgs <- c( probimgs, antsImageRead( probsout[x] ) )
  }
  segmat = imageListToMatrix( probimgs, mymask )
  finalsegvec = apply( segmat, FUN=which.max , MARGIN=2 )
  finalsegvec2 = finalsegvec * 0
  # map finalsegvec to original labels
  inlabsNz = inlabs[ inlabs > 0 ]
  for ( i in sort( unique( finalsegvec ) ) ) {
    finalsegvec2[ finalsegvec == i ] = inlabsNz[ i ]
    }
  outimg = makeImage( mymask, finalsegvec2 ) * mymask
  return( list(
    segmentation = outimg,
    intensity = outimgi,
    probabilityimages = probimgs,
    jlfmask = mymask )
    )
}
