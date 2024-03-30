Employee subclass: SalariedEmployee [
    | weeklySalary |

    SalariedEmployee class >> new: first last: last salary: salary [
        ^ super new: first last: last initializeSalary: salary
    ]

    initializeSalary: salary [
        weeklySalary := salary.
    ]

    getEarning [
        ^ weeklySalary
    ]

    getInfo [
        ^ 'Salaried Employee: ', (super getInfo), ' Weekly Salary: ', weeklySalary printString
    ]
]
