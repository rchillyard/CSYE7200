package edu.neu.coe.csye7200.fp.ga

/**
  * @author scalaprof
  */
trait Gene[+A] extends Seq[Allele[A]] with Identifier

trait Allele[+A] extends (() => A)

trait Locus[+A] extends Seq[Gene[A]]

trait Chromosome[+A] extends Seq[Locus[A]] {
  def isSex: Boolean

  def loci: Int
}

trait Genotype[A, B] extends Seq[Chromosome[A]] {
  def genomic: Genomic[A]

  // TODO fix the types
  def express(expresser: Expresser[A, B]): Phenotype[B]
}

trait Genomic[+A] {
  def chromosomes: Int

  def ploidy: Int
}

trait Expresser[A, B] {
  // TODO fix the types
  def expressLocus(locus: Locus[A]): Trait[B]
}

trait Phenotype[+B] extends Seq[Trait[B]]

trait Trait[+B] extends Identifier {
  def fitness[C](environment: Environment[C]): Unit
}

trait Environment[+C] extends Seq[EcoFactor[C]] with Identifier

trait EcoFactor[+C] extends (() => C) with Identifier

trait Identifier {
  def identify: String
}
