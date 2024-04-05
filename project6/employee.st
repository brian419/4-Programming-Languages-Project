Object subclass: Employee [
    | firstName lastName |

    Employee class >> newWithFirstName: first lastName: last [
        ^self new initializeWithFirstName: first lastName: last
    ]

    initializeWithFirstName: first lastName: last [
        firstName := first.
        lastName := last.
    ]

    setFirstName: first [
        firstName := first.
    ]

    getFirstName [
        ^firstName.
    ]

    setLastName: last [
        lastName := last.
    ]

    getLastName [
        ^lastName.
    ]

    getInfo [
        ^firstName, ' ', lastName
    ]

    getEarning [
        self subclassResponsibility.
    ]
]





