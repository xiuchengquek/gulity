data(mtcars)

mt_kmeans <- kmeans(mtcars, 4)
kp <- kmeanProfiler$new(mtcars)

test_initialization = function(){
  ## check that initilization works with no itermax set.
  checkEquals(data.matrix(mtcars),kp_100$df);
  checkEquals(10 ,kp$itermax);
};

kp_100<- kmeanProfiler$new(mtcars,100)

test_initilization_itermax =  function(){
  checkEquals(data.matrix(mtcars),kp_100$df);
  checkEquals(100 ,kp_100$itermax);
}

test_kmeanProfiler_methods = function(){
  checkEquals(length(mt_kmeans$size), length(kp$k_extract(4)$size));
  checkEquals(4, length(kp$k_children(4)))
}
