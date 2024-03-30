Object subclass: Employee [
    | name earning |

    Employee class >> readFrom: aFileName [
        | file employees |
        file := FileStream readOnlyFileNamed: aFileName.
        employees := OrderedCollection new.
        [file atEnd] whileFalse: [
            | line employeeData |
            line := file nextLine.
            employeeData := line findTokens: ','.
            (employeeData size >= 2) ifTrue: [
                | empName empEarning |
                empName := employeeData first.
                empEarning := employeeData second asNumber.
                employees add: (self new name: empName earning: empEarning)
            ]
        ].
        file close.
        ^ employees
    ]

    Employee >> name [
        ^ name
    ]

    Employee >> earning [
        ^ earning
    ]

    Employee >> name: aName earning: anEarning [
        name := aName.
        earning := anEarning
    ]

    Employee >> getInfo [
        ^ 'Name: ', name, ', Earning: $', earning asString
    ]
]

"SomeClass subclass: SomeClass [
    "Define your method for reading from file here if SomeClass is a custom class"
]"
