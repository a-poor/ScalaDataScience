
import org.scalatest._
import flatspec._
import matchers._

import scalads._

class LinearAlgebraTest extends FunSuite {

    test("LinearAlgebra.add") {
        val result = LinearAlgebra.add(
                Seq(1,2,3),
                Seq(4,5,6)
            )
        assert(result == Seq(5,7,9))
    }

    test("LinearAlgebra.sub") {
        val result = LinearAlgebra.sub(
                Seq(1,2,3),
                Seq(4,5,6)
            )
        assert(result == Seq(3.0,3.0,3.0))
    }

    test("LinearAlgebra.vector_sum") {
        val result = LinearAlgebra.vector_sum(
            Seq(
                Seq(1,2,3),
                Seq(4,5,6),
                Seq(1,1,1)
            )
        )
        assert(result == Seq(6,8,10))
    }

    test("LinearAlgebra.vector_mean") {
        val result = LinearAlgebra.vector_mean(
            Seq(
                Seq(1,2,3),
                Seq(4,5,6)
            )
        )
        assert(result == Seq(2.5,3.5,4.5))
    }

    test("LinearAlgebra.scalar_multiply") {
        val result = LinearAlgebra.scalar_multiply(
            Seq(1,2,3),2
        )
        assert(result == Seq(2,4,6))
    }

    test("LinearAlgebra.dot") {
        val result = LinearAlgebra.dot(
            Seq(1,2,3),Seq(1,2,3)
        )
        assert(result == 14.0)
    }
    

}

