require(profr)

x <- profr(main(percentageOfData = 0.05))

top_n(x, n = 10, time) %>%
  arrange(start)