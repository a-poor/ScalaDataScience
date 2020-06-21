package scalads

object kNearestNeighbors extends App {
    def raw_majority_vote(labels: Seq[Int]) = {
        labels.groupby(n => n)
    }
}