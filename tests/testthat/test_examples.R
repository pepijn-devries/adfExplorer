tempsink <- tempfile("tempsink")
sink(tempsink)
test_examples()
sink()