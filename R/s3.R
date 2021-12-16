#' Download a folders and all its contents recursively from an AWS S3 bucket
#'
#' @param folders A vector of names of folderss inside the bucket to download.
#' Can be single folders names or paths to particular folderss. See examples
#' @param key_pair_file The name of the R file which sets the variables `key` and `secret` to
#' the AWS IAM key and secret for the connection
#' @region The AWS region name to use. It MUST match what the bucket region is. You need to ask
#' the creator of the S3 bucket what this is (or try all regions one by one)
#' @param multipart_limit The number of bytes to trigger a multipart download. If any file is
#' larger than this, it will use the S3 multipart download which is significantly faster
#' @param bucket The name of the S3 bucket
#' @return Nothing (folders structure downloaded in the directory given by `getwd()`)
#' @importFrom aws.s3 get_bucket bucketlist save_object object_size
#' @importFrom purrr map map_chr map_lgl map
#' @importFrom here here
#' @importFrom tictoc tic toc
#' @importFrom stats setNames
#' @export
#' @examples
#' \dontrun{
#' s3_download("models-2021-ss-input-files")
#' s3_download("models-2021-ss-input-files/2021.00.04_base_v1")
#' s3_download(c("models-2021-ss-input-files/2021.00.04_base_v1",
#'                models-2021-ss-input-files/2021.00.02_add_wt_at_age"))
#' }
s3_download <- function(folders = NULL,
                        key_pair_file = here("aws/key_pair.R"),
                        region = "ca-central-1",
                        multipart_limit = 1e6,
                        bucket = "hakestore"){

  if(is.null(folders)){
    stop("folders cannot be NULL, it must be a vector or folders or paths to folders found in the S3 bucket",
         call. = FALSE)
  }
  if(is.null(key_pair_file)){
    stop("key_pair_file is required", call. = FALSE)
  }
  source(key_pair_file)
  if(!exists("key")){
    stop("The variable 'key' does not exist. Check your key_pair_file ",
         "location and contents and try again",
         call. = FALSE)
  }
  if(!exists("secret")){
    stop("The variable 'secret' does not exist. Check your key_pair_file ",
         "location and contents and try again",
         call. = FALSE)
  }
  Sys.setenv("AWS_DEFAULT_REGION" = region,
             "AWS_ACCESS_KEY_ID" = key,
             "AWS_SECRET_ACCESS_KEY" = secret)
  bl <- bucketlist(key = key, secret = secret)

  # Bucket contents, a list of all the object names and sizes and a couple other things
  bc <- get_bucket(bucket)

  # Create a list of vectors of length 2: file name and size
  folders_pattern <- paste(folders, collapse = "|")
  files_sizes <- map(bc, ~{
    if(length(grep(folders_pattern, .x[[1]]))){
      c(name = .x[[1]], size = object_size(.x))
    }else{
      NULL
    }
  }) %>% setNames(NULL)
  # Remove NULLs from the list (non-matching folders names)
  files_sizes[map_lgl(files_sizes, is.null)] <- NULL

  tic()
  # Download all files listed in files
  map(files_sizes, ~{
    save_object(.x[1],
                .x[1],
                bucket = bucket,
                multipart = ifelse(.x[2] > multipart_limit, TRUE, FALSE))
  })
  toc()
  message("Files downloaded to:")
  print(file.path(getwd(), folders))
}


