
test.segmentCN <- function() {
	txt1 <- segmentCN("hello world")
	checkEquals(txt1, c("hello", "world"))
}
