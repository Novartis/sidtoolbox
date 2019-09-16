onclick(id = "saveSession", expr = {
  saveRDS(object = values, file = "values.RDS")
})

onclick(id = "loadSession", expr = {
  load("values.RDS")
})