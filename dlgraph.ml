open Graph

module Sdg = Imperative.Digraph.Abstract(String)
module Vs  = Set.Make(Sdg.V)
module Vss = Set.Make(Vs)
module Sm  = Map.Make(String)