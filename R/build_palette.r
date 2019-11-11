#'Build a palette
#'
#'This function prepares one of the four included colour palettes
#'or builds a new colour palette.
#'
#'Note that \code{colrange} only needs to be specified if \code{name = "usr."}
#'When choosing colours, it is best to avoid light colours or tints as these
#'will lead to a colour palette lacking noticeable differences across the 3 x 3
#'colour grid.
#'
#'@param name Name of colour palette or \code{usr} for option to design a new
#'  palette. Colour palette names include
#'  \code{BlueYellow}, \code{CyanMagenta}, \code{BlueRed} and
#'  \code{GreenBlue}.
#'@param colrange List with a character vector of length two and a numeric
#'  vector of length two.
#'@param colour A character vector of two colour names from the colours() range or valid hexadecimal colors.
#'@param difC A numeric vector of two integers 1, 2, 3 or 4. Values
#'  control how much a colour changes in value across the grid. One corresponds
#'  with a small change in colour value, and four corresponds with a large
#'  change in colour value.
#'@param flipVertical Whether the palette should be flipped vertically (ie. replace top portion with bottom portion)
#'@param flipHorizontal Whether the palette should be flipped horizontally (ie. replace left portion with right portion)
#'@examples
#'#use one of four prepared colour palettes
#'p <- build_palette(name = "CyanMagenta")
#'view(p)
#'
#'#design a new palette
#'p <- build_palette(name = "usr", colrange =
#'  list(colour = c("darkblue", "chartreuse4"), difC = c(3, 4)))
#'view(p)
#'@export
#'@importFrom "grDevices" "colorRamp" "rgb" "colorRampPalette" "colours" "colors"

build_palette <- function(name, colrange = list(colour = NULL, difC = NULL), flipVertical = FALSE, flipHorizontal = FALSE){
  
  if(name == "usr"){
    if(missing(colrange))
      stop("Need to specify colours and degree of colour change when 'name = 'usr''\n")
    
    if(length(colrange$colour) != 2)
      stop("Need to specify 2 colours in colour. \n")
    
    
    isValidColour <- tryCatch ({ col2rgb(colrange$colour) }, error=function(e){ return(NULL) } )
    if (is.null(isValidColour)) {
      stop("one of the colours specified by (", paste(colrange$colour[], collapse = " , "), ") is not a valid colour. Please
           use one of the colours from colors() or use a valid hexadecimal colour.\n")
    }
    
    isWhite <- FALSE
    #Check RGB values of passed values to make sure the colour is not close to white
    lapply(colrange$colour, function(x) {
      rgb <- col2rgb(x)
      if(length(rgb[rgb >= 200]) == 3) 
       isWhite <- TRUE
    })
    
    if(isWhite)
      stop ("colours cannot be white or too close to white. Please select another colour from the colors() range or a hexadecimal value that is not white.")
    
    if(colrange$colour[1] == colrange$colour[2]) 
      stop("Colours must not be the same value. Please
           use a different colour from colors() or use a different hexadecimal colour. \n")
    
    
    if (!all(colrange$difC %in% 1:4))
      stop("difC must be a vector of two elements with values spanning 1 through to 4")
    
    grad1 <- colorRampPalette(c("white", colrange$colour[1]))
    dif1 <- grad1(10)
    dif1 <- rev(dif1[1:4])
    startC1 <- dif1[colrange$difC[1]]
    
    grad2 <- colorRampPalette(c("white", colrange$colour[2]))
    dif2 <- grad2(10)
    dif2 <- rev(dif2[1:4])
    startC2 <- dif2[colrange$difC[2]]
    
    
    ramp1 <- colorRamp(c(startC1, colrange$colour[1]))
    ramp2 <- colorRamp(c(startC2, colrange$colour[2]))
  
  }
  else if(name == "BlueYellow"){
    ramp1 <- colorRamp(c("#CCCCFF", "#0000FF"))
    ramp2 <- colorRamp(c("#FFFFCC", "#FFFF00"))
    
  }
  else if(name == "CyanMagenta"){
    ramp1 <- colorRamp(c("#f9e5f4", "#cc0099"))
    ramp2 <- colorRamp(c("#eff9ff", "#66ccff"))
    
  }
  else if(name == "BlueRed"){
    ramp1 <- colorRamp(c("#CCCCFF", "#0000FF"))
    ramp2 <- colorRamp(c("#FFCCCC", "#FF0000"))
    
  }
  else if(name == "GreenBlue"){
    ramp1 <- colorRamp(c("#b3e6cc", "#339966"))
    ramp2 <- colorRamp(c("#e6e6ff", "#6666ff"))
    
  }
  else
    stop("Palette name not recognised. Must be one of BlueYellow, CyanMagenta, BlueRed, GreenBlue or usr.\n")
  
  
  #build a data frame to get light, middle, and dark colours
  #colour ramp only accepts values 0-1 (0=lightest, 1=darkest)
  lmd1 <- c(0, .5, 1, 0, .5, 1, 0, .5, 1)
  lmd2 <- c(0, 0, 0, .5, .5, .5, 1, 1, 1)
  
  lmd_df <- as.data.frame(cbind(lmd1, lmd2))
  
  #apply colour ramps
  match1 <- with(lmd_df, ramp1(lmd1))
  match2 <- with(lmd_df, ramp2(lmd2))
  
  match_df <- as.data.frame(cbind(match1, match2))
  
  colnames(match_df) <- c("red1", "green1", "blue1", "red2", "green2", "blue2")
  
  #average two single hue colour palettes
  match_df$red.ave <- round((match_df$red1 + match_df$red2) / 2)
  match_df$green.ave <- round((match_df$green1 + match_df$green2) / 2)
  match_df$blue.ave <- round((match_df$blue1 + match_df$blue2) / 2)
  
  match_df$colour.ave <- paste(match_df$red.ave, match_df$green.ave, match_df$blue.ave)
  
  colours <- match_df$colour.ave
  colours <- sapply(strsplit(colours, " "), function(colours) rgb(colours[1], colours[2], colours[3], maxColorValue = 255))
  
  #If we flip vertically
  if(flipVertical) {
    colours <- replace(colours, c(1,9), colours[c(9, 1)]) #Switch [9] and [1]
    colours <- replace(colours, c(8,4), colours[c(4, 8)]) #switch [8] and [4]
    colours <- replace(colours, c(6,2), colours[c(2, 6)]) #Switch [6] and [2]
  }
  
  #If we flip hoizontally
  if(flipHorizontal) {
    colours <- replace(colours, c(7,3), colours[c(3, 7)]) #Switch [7] and [3]
    colours <- replace(colours, c(4,2), colours[c(2, 4)]) #switch [2] and [4]
    colours <- replace(colours, c(8,6), colours[c(6, 8)]) #Switch [6] and [8]
  }
  
  oldClass(colours) <- c("palette", class(colours))
  
  colours
  
}
