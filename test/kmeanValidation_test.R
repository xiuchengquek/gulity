data(mtcars)

mt_kmeans <- kmeans(mtcars, 4)
kp <- kmeanProfiler$new(mtcars)

kv <- kmeanValidation$new(kp)


test_kv_initialize =  function(){
  checkEquals(kv$kp, kp)
  checkEquals(kv$df, kp$df)
  checkEquals(kv$itermax, kp$itermax)

}


test_kv_methods = function(){
  kv_melt <- kv$gen_rand(4)
  checkEquals(800,nrow(kv_melt))
  #checkTrue(class(kv_melt) == 'data.frame', "check if kv$assest return dataframe")
}
