package scala-ds

object MachineLearning {

    def accuracy(tp: Int, fp: int, fn: Int, tn: Int) = {
        val correct = tp + tn
        val total = tp + fp + fn + tn
        correct.toDouble / total.toDouble
    }

    def precision(tp: Int, fp: int, fn: Int, tn: Int) = {
        tp.toDouble / (tp + fp).toDouble
    }

    def recall(tp: Int, fp: int, fn: Int, tn: Int) = {
        tp.toDouble / (tp + fn).toDouble
    }

    def f1_score(tp: Int, fp: int, fn: Int, tn: Int) = {
        val p = precision(tp,fp,fn,tn)
        val r = recall(tp,fp,fn,tn)
        2 * p * r / (p + r)
    }

}