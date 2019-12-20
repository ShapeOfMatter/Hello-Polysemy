do
  i <- readTTY
  j <- readPlus
  let k = i <> j
  writeTTY k
  writePlus k 

