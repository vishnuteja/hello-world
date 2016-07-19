#Library of R used for fetching the webpages from URL into the R-program code. 
library(RCurl) 

# Vector containing all the URL's for parsing
Urls <- c("https://www.csuohio.edu/engineering/eecs/faculty-staff",
          "http://engineering.case.edu/eecs/",
          "http://my.clevelandclinic.org/research",
          "https://en.wikipedia.org/wiki/Data_mining",
          "https://en.wikipedia.org/wiki/Data_mining#Data_mining")

# Vector of lists to store the words parsed from the page
Words_Vector <- vector("list", length(Urls)) 

for(i in 1:length(Urls))
{
  # html variable will store the source code that is fetched from URL using getURL() function
  html <- getURL(Urls[i], followlocation = TRUE)
  
  # gsub("<head>.*?</head>", " ", html) will replace the content inside <head/> tag with empty space
  # gsub("<.*?>", " ", ...) will replace the content inside all HTML tags with empty space
  # \\W is the regular expression for non-words
  # \\d is for the digits/numbers
  # \\s.*? will find 1 or more empty white spaces 
    
  plain_text_string <- gsub("\\W|\\d|\\s.*?", " ", gsub("<.*?>", " ", gsub("<head>.*?</head>", " ", html)))
  
  # Convert all the letters into lower case  
  plain_text_string <- tolower(plain_text_string)
  
  # Split the plain text with \\s (space) as delimiter  
  Words_Vector[i] <- strsplit(plain_text_string, "\\s")
  
  #Words_Vector[i]
}

# table function will find the frequency of all unique values inside the vector and stores the word and frequency values 
d1 <- table(Words_Vector[1])
d2 <- table(Words_Vector[2])
d3 <- table(Words_Vector[3])
d4 <- table(Words_Vector[4])
d5 <- table(Words_Vector[5])

# Create a new matrix with size 5 rows and 5 columns

m <- matrix(nrow = 5, ncol = 5)

# Find the word counts inside the webpages with the words engineering, professor, research, data & mining. Stored them in the m(5X5) 

m[1,1] <- d1[names(d1)=="engineering"]
m[1,2] <- d1[names(d1)=="professor"]
m[1,3] <- d1[names(d1)=="research"]
m[1,4] <- d1[names(d1)=="data"]
m[1,5] <- d1[names(d1)=="mining"]

m[2,1] <- d2[names(d2)=="engineering"]
m[2,2] <- d2[names(d2)=="professor"]
m[2,3] <- d2[names(d2)=="research"]
m[2,4] <- d2[names(d2)=="data"]
m[2,5] <- d2[names(d2)=="mining"]

m[3,1] <- d3[names(d3)=="engineering"]
m[3,2] <- d3[names(d3)=="professor"]
m[3,3] <- d3[names(d3)=="research"]
m[3,4] <- d3[names(d3)=="data"]
m[3,5] <- d3[names(d3)=="mining"]

m[4,1] <- d4[names(d4)=="engineering"]
m[4,2] <- d4[names(d4)=="professor"]
m[4,3] <- d4[names(d4)=="research"]
m[4,4] <- d4[names(d4)=="data"]
m[4,5] <- d4[names(d4)=="mining"]

m[5,1] <- d5[names(d5)=="engineering"]
m[5,2] <- d5[names(d5)=="professor"]
m[5,3] <- d5[names(d5)=="research"]
m[5,4] <- d5[names(d5)=="data"]
m[5,5] <- d5[names(d5)=="mining"]

# Replace all the NA values with 0. 

m[is.na(m)] <- 0

print(m)

# Formula for calculating Cosine Similarity
# cos(d1, d2) = (d1 • d2) /||d1|| ||d2||

# Create a new matrix to store the cosine simliarity values of matrix m. 

cos_mat <- array(0,c(ncol(m),ncol(m)))

# Start from row 2 to calculate the cosine simliarity values of the half(diagonal) of the matrix. 
#Because the diagonal values are 1. and the rest half diagonal part is the mirror image of the diagonal. 

for (i in 2:ncol(m)) 
{
  for (j in 1:(i-1)) 
  {
    cos_mat[i,j] <- crossprod(m[,i] , m[,j]) / sqrt( crossprod(m[,i]) * crossprod(m[,j]) )
  }
}

#Create the tranpose of matrix

transpose_mat <- t(cos_mat)

#Club the values of matrix and it's tranpose. 
cos_mat <- cos_mat + transpose_mat

#Replace all the diagonal values with 1. 
diag(cos_mat) = 1

print(cos_mat)
