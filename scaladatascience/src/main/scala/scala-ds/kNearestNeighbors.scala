package scala-ds

object kNearestNeighbors {
    def raw_majority_vote(labels: Seq[Int]) = {
        labels.groupby(n => n)
    }
}