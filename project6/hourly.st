Employee subclass: HourlyEmployee [
    | hoursWorked hourlyRate |

    HourlyEmployee class >> new: first last: last hours: hours rate: rate [
        ^ super new: first last: last initializeHours: hours rate: rate
    ]

    initializeHours: hours rate: rate [
        hoursWorked := hours.
        hourlyRate := rate.
    ]

    getEarning [
        | earning |
        earning := hoursWorked <= 40
            ifTrue: [hoursWorked * hourlyRate]
            ifFalse: [hoursWorked <= 50
                        ifTrue: [40 * hourlyRate + (hoursWorked - 40) * hourlyRate * 1.5]
                        ifFalse: [40 * hourlyRate + 10 * hourlyRate * 1.5 + (hoursWorked - 50) * hourlyRate * 2]].
        ^ earning
    ]

    getInfo [
        ^ 'Hourly Employee: ', (super getInfo), ' Hours Worked: ', hoursWorked printString, ', Hourly Rate: ', hourlyRate printString
    ]
]
