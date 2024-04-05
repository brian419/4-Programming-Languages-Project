Employee subclass: CommissionEmployee [
    | salesAmount commissionRate minimalWeeklyPay |

    CommissionEmployee class >> new: first last: last basePay: basePay sales: sales rate: rate [
        ^ super new: first last: last initializeBasePay: basePay sales: sales rate: rate
    ]


    initializeWeeklyPay: basePay [
        minimalWeeklyPay := basePay asNumber.
    ]

    initializeSalesAmount: sales [
        salesAmount := sales asNumber.
    ]

    initializeCommissionRate: rate [
        commissionRate := rate.
    ]

    

    getEarning [

        | earning |
        salesAmount := salesAmount asNumber.
        commissionRate := commissionRate asNumber.
        minimalWeeklyPay := minimalWeeklyPay asNumber.
        earning := (salesAmount * commissionRate) > minimalWeeklyPay
            ifTrue: [salesAmount * commissionRate]
            ifFalse: [minimalWeeklyPay].
        "Transcript show: 'earned: $', earning printString; cr.
        Transcript show: ' '; cr."
        ^ earning.

    ]



    "getInfo [
        Transcript show: 'Commission Employee: ', (super getInfo); cr.
        Transcript show: 'Minimum salary: ', minimalWeeklyPay printString, ', ', 'Sales amount: ', salesAmount printString, ', ', 'Comission rate: ', commissionRate; cr.
        
    ]"

    getInfo [
        ^ (super getInfo).
    ]

    getMinimumSalary [
        ^ minimalWeeklyPay.
    ]

    getSalesAmount [
        ^ salesAmount.
    ]

    getCommissionRate [
        ^ commissionRate.
    ]

]



