package edu.neu.coe.csye7200

object StructuralTypes {

    type Renderable = {
        def render: String
    }

    def toString(r: Renderable): String = r.render

    case class Large(x: BigInt) {
        def render: String = Prime.primeFactorMultiplicity(x).toString()
    }
}