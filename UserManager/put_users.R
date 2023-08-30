library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

user = c("USER123",'nick')
password = c("PASSWORD123","123")

userpass.df = data.frame(user = user,
                         password = password)

saveRDS(userpass.df, "UserManager/userpass.df.rds")

put_object(
  file = file.path("UserManager", "userpass.df.rds"), 
  object = "userpass.df.rds", 
  bucket = "cryptomlbucket/mlprophet_users"
)
