Employee subclass: HourlyEmployee [
    | hoursWorked hourlyRate |

    HourlyEmployee class >> new: first last: last hours: hours rate: rate [
        ^ super new: first last: last initializeHours: hours rate: rate
    ]

    initializeHours: hours [
        hoursWorked := hours asNumber.
    ]

    initializeRate: rate [
        hourlyRate := rate asNumber.
    ]

    "getInfo [
        Transcript show: 'Hourly Employee: ', (super getInfo); cr.
        Transcript show: 'Hours Worked: ', hoursWorked printString, ', ', 'Hourly Rate: ', hourlyRate printString; cr.
        
    ]"

    getInfo [
        ^ super getInfo.
    ]

    getHoursWorked [
        ^ hoursWorked
    ]

    getHourlyRate [
        ^ hourlyRate
    ]


    getEarning [
        | earning earningpart1 earningpart2 earningpart3 earningpart4 earningpart5|
        hoursWorked := hoursWorked asNumber. 
        hourlyRate := hourlyRate asNumber.  

        hoursWorked <= 40 ifTrue: [
            earning := hoursWorked * hourlyRate.
        ] ifFalse: [
            hoursWorked <= 50 ifTrue: [
                earningpart1 := 40 * hourlyRate.
                earningpart2 := (hoursWorked - 40) * hourlyRate * 1.5.
                earning := earningpart1 + earningpart2.
                "earning := 40 * hourlyRate + (hoursWorked - 40) * hourlyRate * 1.5."
            ] ifFalse: [
                
                earningpart3 := (hoursWorked - 50) * hourlyRate * 2.
                earningpart4 := 10 * hourlyRate * 1.5.
                earningpart5 := 40 * hourlyRate.
                earning := earningpart3 + earningpart4 + earningpart5.
                "earning := 40 * hourlyRate + 10 * hourlyRate * 1.5 + (hoursWorked - 50) * hourlyRate * 2."
            ]
        ].
        
        ^ earning.
    ]

]


        
