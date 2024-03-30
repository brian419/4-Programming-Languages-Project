Object subclass: Employee [
    | firstName lastName |
    
    Employee class >> new: first last: last [
        ^ super new initialize: first last: last
    ]

    initialize: first last: last [
        firstName := first.
        lastName := last.
    ]

    getFirstName [
        ^ firstName
    ]

    setFirstName: first [
        firstName := first
    ]

    getLastName [
        ^ lastName
    ]

    setLastName: last [
        lastName := last
    ]

    getEarning [
        self subclassResponsibility
    ]

    getInfo [
        ^ firstName, ' ', lastName
    ]
]
