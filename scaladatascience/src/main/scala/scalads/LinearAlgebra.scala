package scalads

object LinearAlgebra extends App {
    def add(v: Seq[Double], w: Seq[Double]): Seq[Double] = {
        assert(v.length == w.length)
        v.indices.map(i => v(i) + w(i))
    }
    def sub(v: Seq[Double], w: Seq[Double]): Seq[Double] = {
        assert(v.length == w.length)
        (0 until v.length).map(i => v(i) - w(i))
    }
    def vector_sum(arr: Seq[Seq[Double]]): Seq[Double] = {
        for (i <- arr(0).indices)
            yield arr.map(_(i)).sum
    }
    def vector_mean(arr: Seq[Seq[Double]]) = {
       vector_sum(arr).map(_/arr.length)
    }
    def scalar_multiply(v: Seq[Double], s: Double) = {
        v.map(_*s)
    }
    def dot(v: Seq[Double], w: Seq[Double]) = {
        assert(v.length == w.length)
        v.indices.map(i => v(i) * w(i)).sum
    }
    def sum_of_squares(v: Seq[Double]) = {
        v.map(n => n*n).sum
    }
    def magnitude(v: Seq[Double]) = {
        math.sqrt(sum_of_squares(v))
    }
    def squared_distance(v: Seq[Double], w: Seq[Double]) = {
        sum_of_squares(sub(v,w))
    }
    def distance(v: Seq[Double], w: Seq[Double]) = {
        math.sqrt(squared_distance(v,w))
    }
    def identity_matrix(n: Int) = {
        Seq.tabulate(n,n)(
            (a,b) => if (a == b) 1.0 else 0.0
        )
    }
}
