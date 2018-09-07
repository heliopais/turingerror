setup({
  library(data.table)
  d = data.table(
    trials1 = c(10,10,10),
    estimates1 = c(0.1,0.1,0.1),
    trials2 = c(10,10,10),
    estimates2 = c(0.2,0.2,0.2)
  )

  d2 = data.table(
    trials1 = c(10,10,10),
    estimates1 = c(0.2,0.2,0.2),
    trials2 = c(10,10,10),
    estimates2 = c(0.1,0.1,0.1)
  )

  d3 = data.table(
    trials1 = c(10,10,10),
    estimates1 = c(0.2,0.2,0.2),
    trials2 = c(10,10,10),
    estimates2 = c(0.1,0.1,1.1)
  )

  d4 = data.table(
    trials1 = c(10,10,10),
    estimates1 = c(0.2,0.2,-0.1),
    trials2 = c(10,10,10),
    estimates2 = c(0.1,0.1,1.1)
  )

  d5 = data.table(
    trials1 = c(10,10,0),
    estimates1 = c(0.2,0.2,0.2),
    trials2 = c(10,10,10),
    estimates2 = c(0.1,0.1,0.1)
  )

  save(d, d2, d3, d4, d5,
       file = 'fixtures-evaluators.RData')
})
