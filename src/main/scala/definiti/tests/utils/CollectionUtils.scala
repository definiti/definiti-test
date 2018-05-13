package definiti.tests.utils

import definiti.common.utils.{CollectionUtils => CommonCollectionUtils}

private[tests] object CollectionUtils {
  def scalaSeq[A](list: java.util.List[A]): Seq[A] = CommonCollectionUtils.scalaSeq(list)

  def scalaSeq[A](set: java.util.Set[A]): Seq[A] = CommonCollectionUtils.scalaSeq(set)

  def scalaSeq[A](stream: java.util.stream.Stream[A]): Seq[A] = CommonCollectionUtils.scalaSeq(stream)

  def javaList[A](seq: Seq[A]): java.util.List[A] = CommonCollectionUtils.javaList(seq)

  def squash[A, B](maps: Seq[Map[A, B]]): Map[A, B] = CommonCollectionUtils.squash(maps)

  def fill[A](seq: Seq[A], defaultElement: A, size: Int): Seq[A] = {
    if (seq.length >= size) {
      seq
    } else {
      seq ++ Seq.fill(size - seq.length)(defaultElement)
    }
  }
}
