module ComplexNumber =
    [<Sealed>]
    type ComplexNumber =
        static member (-) : ComplexNumber -> ComplexNumber -> ComplexNumber
        static member (+) : ComplexNumber -> ComplexNumber -> ComplexNumber
        static member (*) : ComplexNumber -> ComplexNumber -> ComplexNumber
        static member (/) : ComplexNumber -> ComplexNumber -> ComplexNumber

    val makeComplex : float * float -> ComplexNumber
    val getValue : ComplexNumber -> float * float