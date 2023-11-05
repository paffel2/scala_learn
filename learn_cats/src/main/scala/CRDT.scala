import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import cats.instances.all._

object CRDT extends App {

  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int) = {
      GCounter(
        counters + (machine -> (counters.getOrElse(machine, 0) + amount))
      )
    }

    def merge(that: GCounter): GCounter =
      GCounter(that.counters ++ this.counters.map { case (k, v) =>
        k -> v.max(that.counters.getOrElse(k, 0))
      })

    def total: Int = this.counters.values.sum
  }
  object wrapper {
    trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
      def combine(a1: A, a2: A): A
      def empty: A
    }

    object BoundedSemiLattice {

      implicit val intInstance: BoundedSemiLattice[Int] = {
        new BoundedSemiLattice[Int] {
          def combine(a1: Int, a2: Int): Int = a1.max(a2)
          def empty: Int = 0
        }
      }

      /*implicit val setInstance[S]: BoundedSemiLattice[Set[S]] =
      new BoundedSemiLattice[Set[S]] {
        def combine(a1: Set[S], a2: Set[S]): Set[S] = a1.union(a2)
        def empty: Set[S] = Set[S].empty
      }*/
    }
  }

  import wrapper._

  final case class GenericCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit
        m: CommutativeMonoid[A]
    ): GenericCounter[A] =
      GenericCounter(
        counters + (machine -> (counters
          .getOrElse(machine, m.empty) |+| amount))
      )
    def total(implicit m: CommutativeMonoid[A]): A =
      this.counters.values.toList.combineAll

    def merge(that: GenericCounter[A])(implicit
        m: BoundedSemiLattice[A]
    ): GenericCounter[A] =
      GenericCounter(that.counters |+| this.counters)

  }

  trait GenericCounterTC[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit
        m: CommutativeMonoid[V]
    ): F[K, V]
    def merge(f1: F[K, V], f2: F[K, V])(implicit
        b: BoundedSemiLattice[V]
    ): F[K, V]
    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
  }
  object GenericCounterTC {
    def apply[F[_, _], K, V](implicit counter: GenericCounterTC[F, K, V]) =
      counter

    implicit def mapGenericCounterTC[K, V]: GenericCounterTC[Map, K, V] =
      new GenericCounterTC[Map, K, V] {

        def merge(f1: Map[K, V], f2: Map[K, V])(implicit
            b: BoundedSemiLattice[V]
        ): Map[K, V] =
          f1 |+| f2

        def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
          f.values.toList.combineAll

        def increment(f: Map[K, V])(k: K, v: V)(implicit
            m: CommutativeMonoid[V]
        ): Map[K, V] =
          f + (k -> (v |+| f.getOrElse(k, m.empty)))
      }
  }

  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
    def get[K, V](f: F[K, V])(k: K): Option[V]
    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)
    def values[K, V](f: F[K, V]): List[V]
  }

  implicit val mapKeyValueStore: KeyValueStore[Map] = new KeyValueStore[Map] {

    def values[K, V](f: Map[K, V]): List[V] = f.values.toList

    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
    override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
      f.getOrElse(k, default)
  }

}
