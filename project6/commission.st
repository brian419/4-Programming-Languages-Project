Employee subclass: CommissionEmployee [
    | salesAmount commissionRate minimalWeeklyPay |

    CommissionEmployee class >> new: first last: last basePay: basePay sales: sales rate: rate [
        ^ super new: first last: last initializeBasePay: basePay sales: sales rate: rate
    ]

    initializeBasePay: basePay sales: sales rate: rate [
        minimalWeeklyPay := basePay.
        salesAmount := sales.
        commissionRate := rate.
    ]

    getEarning [
        ^ (salesAmount * commissionRate) > minimalWeeklyPay
            ifTrue: [salesAmount * commissionRate]
            ifFalse: [minimalWeeklyPay]
    ]

    getInfo [
        ^ 'Commission Employee: ', (super getInfo), ' minimum salary: ', minimalWeeklyPay printString, ', sales amount: ', salesAmount printString, ', commission rate: ', (commissionRate * 100) printString, '%'
    ]
]
