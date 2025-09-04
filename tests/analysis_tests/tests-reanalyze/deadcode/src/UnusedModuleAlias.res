module Actual = {
  let value = 1
}

module Alias = Actual

let useActual = Actual.value
