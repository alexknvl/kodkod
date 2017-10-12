package kodkod.syntax

import java.util

import cats.Eq

//trait ListSyntax {
//
//}
//
//final case class ListSyntax[V](base: List[V]) {
//    def freeze: FrozenList[V] = FrozenList(base)
//}
//
//final case class MapSyntax[K, V]() {
//
//}
//
//trait JavaSyntax {
//    def emptyHashMap[K, V](implicit eq: Eq[K]): util.HashMap[K, V] =
//        new util.HashMap[K, V]()
//    def emptyHashSet[K](implicit eq: Eq[K]): util.HashSet[K] =
//        new util.HashSet[K]()
//}