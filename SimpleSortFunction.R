# SIMPLE SORT FUNCTION

findSmallest <- function(arr){
  smallest = arr[1]
  smallest_index = 1
  larr <- length(arr)
  for (i in seq(2,larr,1)){
    if (arr[i] < smallest) {
      smallest = arr[i]
      smallest_index = i
    }
  }
return(smallest_index)}


simpleSort <- function(arr){
  larr <- length(arr)
  newArr <- array()
  for (i in seq(2, larr, 1)){
    smallest = findSmallest(arr)
    newArr = c(newArr, arr[smallest])
    arr = arr[-smallest]
  }
  newArr = c(newArr, max(arr))
  return(newArr[-1])}
