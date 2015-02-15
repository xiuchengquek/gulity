library(RUnit)


source('./src/kmerProfiler.R')


testsuite.kp <- defineTestSuite('kp', dirs='./test', testFileRegexp='.+_test.R',
                                testFuncRegexp='test_.+')

testResults <- runTestSuite(testsuite.kp)
printTextProtocol(testResults)
