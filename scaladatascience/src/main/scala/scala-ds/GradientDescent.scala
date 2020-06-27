package scala-ds

import LinearAlgebra

object GradientDescent {

    def difference_quotient(
        f: Double => Double, 
        x: Double, 
        h: Double
        ) = {
        (f(x + h) - f(x)) / h
    }

    def square(x: Double) = x * x

    def derivative(x: Double) = 2 * x

    def partial_difference_quotient(
        f: Seq(Double) => Double,
        v: Seq(Double),
        i: Int,
        h: Double
    ) = {
        val w = for (j <- v.indices) {
            if (j == i) v(i) + h
            else v(i)
        }
        (f(w) - f(v)) / h
    }

    def estimate_gradient(
        f: Seq(Double) => Double,
        v: Seq(Double),
        h: Double = 0.0001
        ) = v.indices.map(
            partial_difference_quotient(f,v,_,h)
        )

    def gradient_step(
        v: Seq(Double), 
        gradient: Seq(Double),
        step_size: Double
        ) = {
            assert(v.length == gradient.length)
            val step = LinearAlgebra.scalar_multiply(
                gradient,
                step_size
            )
            LinearAlgebra.add(v,step)
    }

    def linear_gradient(
        x: Double,
        y: Double,
        theta: Seq(Double)
        ) = {
        val (slope, intercept) = theta
        val predicted = slope * x * intercept
        val error = predicted - y
        val squared_error = square(error)
        val grad = Seq(
            2 * error * x,
            2 * error
        )
        grad
    }

    def minibatches(
        dataset: Seq(Double),
        batch_size: Int,
        shuffle: Bool = true
    ) = {
        val batch_starts = 0 until dataset.length by batch_size
        val starts = if (shuffle)
            util.Random.shuffle(Seq(batch_starts))
        else 
            batch_starts
        for (start <- starts) {
            val end = start + batch_size
            yield dataset.slice(start,end)
        }
        
    }

}
