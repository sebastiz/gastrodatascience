#For S3 account- get this from
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIXFU25BL2KIHIHXA",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "eu-west-2")

library("aws.s3")

#Get all the buckets
get_bucket(
  bucket = 'gastrodatascience.com',
  key = "AKIAIXFU25BL2KIHIHXA",
  secret = "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"
)

s3save(mtcars, bucket = "gastrodatascience.com", object = "mtcars.Rdata")
#To get the session token

aws.signature::use_credentials()
get_session_token()
