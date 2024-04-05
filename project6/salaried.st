Employee subclass: SalariedEmployee [
    | weeklySalary |

    SalariedEmployee class >> new: first last: last salary: salary [
        ^ super new: first last: last initializeSalary: salary
    ]

    initializeSalary: salary [
        weeklySalary := salary asNumber.
    ]



    "getInfo [
        Transcript show: 'Salaried Employee: ', (super getInfo); cr.
        Transcript show: 'Weekly Salary: ', weeklySalary printString; cr.
    ]"

    getInfo [
        ^ super getInfo.
    ]

    "getEarning [
        
        Transcript show: 'earned: $', weeklySalary printString; cr.
        Transcript show: ' '; cr.
        ^ weeklySalary
    ]"

    ""getEarning [
        ^ weeklySalary
    ]""
]

