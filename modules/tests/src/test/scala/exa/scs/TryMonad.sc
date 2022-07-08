import exa.CatsTry.TreeMonad._

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
branch(leaf(100), leaf(200)).
  flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
