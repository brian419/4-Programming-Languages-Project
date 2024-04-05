FileStream fileIn: 'employee.st'.
FileStream fileIn: 'salaried.st'.
FileStream fileIn: 'hourly.st'.
FileStream fileIn: 'commission.st'.

|f countEmployees a tokens employees salaryEmployee hourlyEmployee commissionEmployee args|
f := FileStream open: Smalltalk arguments first mode: FileStream read.
args := Smalltalk arguments.
args isEmpty ifTrue: [
    Transcript show: 'Usage: gst employee.st salaried.st hourly.st commission.st -f main.st datafile action'; nl.
    Transcript show: 'or'; nl.
    Transcript show: 'Usage: gst employee.st salaried.st hourly.st commission.st -f main.st action datafile operator threshold'; nl.
    Transcript show: 'Valid actions: count print min max total avg'; nl.
    Transcript show: 'Valid operators: eq ne gt ge lt le'; nl.
    
    "Transcript show: ''; cr."
].
f isNil ifTrue: [
    Transcript show: 'Unable to open xxxx.dat for reading'; nl.
    ObjectMemory quit.


].

countEmployees := 0.
a := f nextLine. 
employees := Array new.


[f atEnd ] whileFalse: [

    
    tokens:= a subStrings.
    a := f nextLine. 

    (tokens at: 1) = 'salaried' ifTrue: [
        employees := employees, {tokens}.
        countEmployees := countEmployees + 1.

    ].
    (tokens at: 1) = 'hourly' ifTrue: [
        employees := employees, {tokens}.
        countEmployees := countEmployees + 1.

    ].
    (tokens at: 1) = 'commission' ifTrue: [
        employees := employees, {tokens}.
        countEmployees := countEmployees + 1.

    ].
].
f close.




args := Smalltalk arguments.
args isEmpty ifTrue: [
    Transcript show: 'Usage: gst employee.st salaried.st hourly.st commission.st -f main.st datafile action'; nl.
    Transcript show: 'or'; nl.
    Transcript show: 'Usage: gst employee.st salaried.st hourly.st commission.st -f main.st action datafile operator threshold'; nl.
    Transcript show: 'Valid actions: count print min max total avg'; nl.
    Transcript show: 'Valid operators: eq ne gt ge lt le'; nl.
    
    "Transcript show: ''; cr."
] ifFalse: [
    |  minEmployee minEarning maxEmployee maxEarning totalEarnings avgEarnings operator threshold employee |
    (Smalltalk arguments at: 1) = (Smalltalk arguments at: 1) ifFalse: [
        
        Transcript show: 'Unable to open ', (Smalltalk arguments at: 1), ' for reading'; nl.
        (Smalltalk arguments at: 2) = 'count' ifTrue: [
            
        ].
        (Smalltalk arguments at: 2) = 'print' ifTrue: [
            
        ].
        (Smalltalk arguments at: 2) = 'min' ifTrue: [
            
        ].
        (Smalltalk arguments at: 2) = 'max' ifTrue: [
            
        ].
        (Smalltalk arguments at: 2) = 'total' ifTrue: [
            
        ].
        (Smalltalk arguments at: 2) = 'avg' ifTrue: [
            
        ].
    ] ifTrue: [
        (Smalltalk arguments at: 2) = 'count' ifTrue: [
            (Smalltalk arguments size < 3) ifTrue: [
                Transcript show: 'The number of employees is: ', countEmployees printString; nl.
            ].
        ].
        (Smalltalk arguments at: 2) = 'print' ifTrue: [
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ge']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                employees do: [:each | 
                    
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).

                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).

                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).    
                    ].
                    (each at: 1) = 'salaried' ifTrue: [
                        (salaryEmployee getEarning) >= threshold ifTrue: [
                            
                            Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                            Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                            Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        (hourlyEmployee getEarning) >= threshold ifTrue: [
                            Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                            Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                            Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.    
                        ].
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        (commissionEmployee getEarning) >= threshold ifTrue: [
                            Transcript show: 'Commission Employee: ', (each at: 2), ' ', (each at: 3); nl.
                            Transcript show: 'minimum salary: $', (each at: 4), ', sales amount: $', (each at: 5), ', commission rate: ', (each at: 6); nl.
                            Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'eq']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                employees do: [:each | 
                    employee := nil.
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
        
                    ].

                    (each at: 1) = 'salaried' ifTrue: [
                        (salaryEmployee getEarning) = threshold ifTrue: [
                            Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                            Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                            Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        (hourlyEmployee getEarning) = threshold ifTrue: [
                            Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                            Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                            Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.

                        ].
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        (commissionEmployee getEarning) = threshold ifTrue: [
                            Transcript show: 'Commission Employee: ', (each at: 2), ' ', (each at: 3); nl.
                            Transcript show: 'minimum salary: $', (each at: 4), ', sales amount: $', (each at: 5), ', commission rate: ', (each at: 6); nl.
                            Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                ].
            ].
            
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ne']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                employees do: [:each | 
                    employee := nil.
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).

                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).

                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).        
                    ].

                    (each at: 1) = 'salaried' ifTrue: [
                        (salaryEmployee getEarning) ~= threshold ifTrue: [
                            Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                            Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                            Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        (hourlyEmployee getEarning) ~= threshold ifTrue: [ 
                            Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                            Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                            Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        (commissionEmployee getEarning) ~= threshold ifTrue: [
                            Transcript show: 'Commission Employee: ', (each at: 2), ' ', (each at: 3); nl.
                            Transcript show: 'minimum salary: $', (each at: 4), ', sales amount: $', (each at: 5), ', commission rate: ', (each at: 6); nl.
                            Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                ].
            ].

            
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'le']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                employees do: [:each | 
                    employee := nil.
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                    ].
                    (each at: 1) = 'salaried' ifTrue: [
                        (salaryEmployee getEarning) <= threshold ifTrue: [
                            Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                            Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                            Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        (hourlyEmployee getEarning) <= threshold ifTrue: [
                            Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                            Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                            Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        (commissionEmployee getEarning) <= threshold ifTrue: [
                            Transcript show: 'Commission Employee: ', (each at: 2), ' ', (each at: 3); nl.
                            Transcript show: 'minimum salary: $', (each at: 4), ', sales amount: $', (each at: 5), ', commission rate: ', (each at: 6); nl.
                            Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                ].
            ].
            
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'gt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                employees do: [:each | 
                    employee := nil.
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                    ].

                    (each at: 1) = 'salaried' ifTrue: [
                        (salaryEmployee getEarning) > threshold ifTrue: [
                            Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                            Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                            Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        (hourlyEmployee getEarning) > threshold ifTrue: [
                            Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                            Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                            Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        (commissionEmployee getEarning) > threshold ifTrue: [
                            Transcript show: 'Commission Employee: ', (each at: 2), ' ', (each at: 3); nl.
                            Transcript show: 'minimum salary: $', (each at: 4), ', sales amount: $', (each at: 5), ', commission rate: ', (each at: 6); nl.
                            Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                ].
            ].
            
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'lt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                employees do: [:each | 
                    
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).
            
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).        
                    ].

                    (each at: 1) = 'salaried' ifTrue: [
                        (salaryEmployee getEarning) < threshold ifTrue: [
                            Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                            Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                            Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        (hourlyEmployee getEarning) < threshold ifTrue: [
                            Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                            Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                            Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        (commissionEmployee getEarning) < threshold ifTrue: [
                            Transcript show: 'Commission Employee: ', (each at: 2), ' ', (each at: 3); nl.
                            Transcript show: 'minimum salary: $', (each at: 4), ', sales amount: $', (each at: 5), ', commission rate: ', (each at: 6); nl.
                            Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                            Transcript show: ' '; cr.
                        ].
                    ].
                ].
            ].
            (Smalltalk arguments size < 3) ifTrue: [
                employees do: [:each | 
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee setFirstName: (each at: 2).
                        salaryEmployee setLastName: (each at: 3).
                        salaryEmployee initializeSalary: (each at: 4).
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee setFirstName: (each at: 2).
                        hourlyEmployee setLastName: (each at: 3).
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee setFirstName: (each at: 2).
                        commissionEmployee setLastName: (each at: 3).
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                    ].
                    (each at: 1) = 'salaried' ifTrue: [
                        Transcript show: 'Salaried employee: ',(salaryEmployee getInfo) ; nl.
                        Transcript show: 'weekly salary: ', (salaryEmployee getEarning) printString; nl.
                        Transcript show: 'earned $', (salaryEmployee getEarning) printString; nl.
                        Transcript show: ' '; cr.

                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        Transcript show: 'Hourly employee: ', (hourlyEmployee getInfo); nl.
                        Transcript show: 'hours worked: ', (hourlyEmployee getHoursWorked) printString, ' ', 'Hourly Rate: ', (hourlyEmployee getHourlyRate) printString; nl.
                        Transcript show: 'earned $', (hourlyEmployee getEarning) printString; nl.
                        Transcript show: ' '; cr.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        Transcript show: 'Commission Employee: ', (commissionEmployee getInfo); nl.
                        Transcript show: 'minimum salary: ', (commissionEmployee getMinimumSalary) printString, ', ', 'sales amount: ', (commissionEmployee getSalesAmount) printString, ', ', 'commission rate: ', (commissionEmployee getCommissionRate); nl.
                        Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                        Transcript show: ' '; cr.
                    ].
                ].            
            ].
            
        ].
        (Smalltalk arguments at: 2) = 'min' ifTrue: [
            (Smalltalk arguments size < 3) ifTrue: [
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    
                    ].

                    minEarning isNil ifTrue: [
                        minEarning := earning.
                        minEmployee := each.
                    ] ifFalse: [
                        (earning < minEarning) ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ].
                    ].
                ].
                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].   
            ].
        
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ge']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning >= threshold) ifTrue: [
                        minEarning isNil ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ] ifFalse: [
                            (earning < minEarning) ifTrue: [
                                minEarning := earning.
                                minEmployee := each.
                            ].
                        ].
                    ].
                ].

                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].
            
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'eq']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning = threshold) ifTrue: [
                        minEarning isNil ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ] ifFalse: [
                            (earning < minEarning) ifTrue: [
                                minEarning := earning.
                                minEmployee := each.
                            ].
                        ].
                    ].
                ].

                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [   
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ne']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning ~= threshold) ifTrue: [
                        minEarning isNil ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ] ifFalse: [
                            (earning < minEarning) ifTrue: [
                                minEarning := earning.
                                minEmployee := each.
                            ].
                        ].
                    ].
                ].

                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'gt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning > threshold) ifTrue: [
                        minEarning isNil ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ] ifFalse: [
                            (earning < minEarning) ifTrue: [
                                minEarning := earning.
                                minEmployee := each.
                            ].
                        ].
                    ].
                ].

                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'lt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning < threshold) ifTrue: [
                        minEarning isNil ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ] ifFalse: [
                            (earning < minEarning) ifTrue: [
                                minEarning := earning.
                                minEmployee := each.
                            ].
                        ].
                    ].
                ].

                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].
            
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'le']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                minEarning := nil.
                minEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  

                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning <= threshold) ifTrue: [
                        minEarning isNil ifTrue: [
                            minEarning := earning.
                            minEmployee := each.
                        ] ifFalse: [
                            (earning < minEarning) ifTrue: [
                                minEarning := earning.
                                minEmployee := each.
                            ].
                        ].
                    ].
                ].
                (minEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ',(minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (minEmployee at: 4); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (minEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (minEmployee at: 2), ' ', (minEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (minEmployee at: 4), ', ', 'sales amount: ', (minEmployee at: 5), ', ', 'commission rate: ', (minEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].
        ].
        (Smalltalk arguments at: 2) = 'max' ifTrue: [
            (Smalltalk arguments size < 3) ifTrue: [
                | maxEmployee maxEarning |
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    maxEarning isNil ifTrue: [
                        maxEarning := earning.
                        maxEmployee := each.
                    ] ifFalse: [
                        (earning > maxEarning) ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ].
                    ].
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [

                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (minEmployee at: 2), ' ', (minEarning at: 3); nl.
                    Transcript show: 'hours worked: ', (minEmployee at: 4), ', ', 'hourly rate: ', (minEmployee at: 5); nl.
                    Transcript show: 'earned $', (minEarning printString); nl.
                    Transcript show: ' '; cr.

                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.
                ].
            ].

            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ge']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning >= threshold) ifTrue: [
                        maxEarning isNil ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ] ifFalse: [
                            (earning > maxEarning) ifTrue: [
                                maxEarning := earning.
                                maxEmployee := each.
                            ].
                        ].
                    ].
    
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.    
                    
                ].

            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'eq']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning = threshold) ifTrue: [
                        maxEarning isNil ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ] ifFalse: [
                            (earning > maxEarning) ifTrue: [
                                maxEarning := earning.
                                maxEmployee := each.
                            ].
                        ].
                    ].
    
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.    
                    
                ].

            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ne']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning ~= threshold) ifTrue: [
                        maxEarning isNil ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ] ifFalse: [
                            (earning > maxEarning) ifTrue: [
                                maxEarning := earning.
                                maxEmployee := each.
                            ].
                        ].
                    ].
    
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.    
                    
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'gt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning > threshold) ifTrue: [
                        maxEarning isNil ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ] ifFalse: [
                            (earning > maxEarning) ifTrue: [
                                maxEarning := earning.
                                maxEmployee := each.
                            ].
                        ].
                    ].
    
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.    
                    
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'lt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning < threshold) ifTrue: [
                        maxEarning isNil ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ] ifFalse: [
                            (earning > maxEarning) ifTrue: [
                                maxEarning := earning.
                                maxEmployee := each.
                            ].
                        ].
                    ].
    
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.    
                    
                ].
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'le']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                
                maxEarning := nil.
                maxEmployee := nil.

                employees do: [:each |
                    | earning |
                    earning := 0.0.  
    
                    (each at: 1) = 'salaried' ifTrue: [
                        salaryEmployee := SalariedEmployee new.
                        salaryEmployee initializeSalary: (each at: 4).
                        earning := salaryEmployee getEarning.
                    ].
                    (each at: 1) = 'hourly' ifTrue: [
                        hourlyEmployee := HourlyEmployee new.
                        hourlyEmployee initializeHours: (each at: 4).
                        hourlyEmployee initializeRate: (each at: 5).
                        earning := hourlyEmployee getEarning.
                    ].
                    (each at: 1) = 'commission' ifTrue: [
                        commissionEmployee := CommissionEmployee new.
                        commissionEmployee initializeWeeklyPay: (each at: 4).
                        commissionEmployee initializeSalesAmount: (each at: 5).
                        commissionEmployee initializeCommissionRate: (each at: 6).
                        earning := commissionEmployee getEarning.
                    ].
                    (earning <= threshold) ifTrue: [
                        maxEarning isNil ifTrue: [
                            maxEarning := earning.
                            maxEmployee := each.
                        ] ifFalse: [
                            (earning > maxEarning) ifTrue: [
                                maxEarning := earning.
                                maxEmployee := each.
                            ].
                        ].
                    ].
    
                ].
                (maxEmployee at: 1) = 'salaried' ifTrue: [
                    Transcript show: 'Salaried employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'weekly salary: ', (maxEmployee at: 4); nl.
                    Transcript show: 'earned: $ ', maxEarning printString; nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'hourly' ifTrue: [
                    Transcript show: 'Hourly employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'hours worked: ', (maxEmployee at: 4), ' ', 'hourly rate: ', (maxEmployee at: 5); nl.
                    Transcript show: 'earned $', (maxEarning printString); nl.
                    Transcript show: ' '; cr.
                ].
                (maxEmployee at: 1) = 'commission' ifTrue: [
                    Transcript show: 'Commission employee: ', (maxEmployee at: 2), ' ', (maxEmployee at: 3); nl.
                    Transcript show: 'minimum salary: ', (maxEmployee at: 4), ', ', 'sales amount: ', (maxEmployee at: 5), ', ', 'commission rate: ', (maxEmployee at: 6); nl.
                    Transcript show: 'earned $', (commissionEmployee getEarning) printString; nl.
                    Transcript show: ' '; cr.    
                    
                ].
            ].

        ].
        (Smalltalk arguments at: 2) = 'total' ifTrue: [
            (Smalltalk arguments size < 3) ifTrue: [
                
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    totalEarnings := totalEarnings + employee getEarning.
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'eq']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    (employee getEarning) = threshold ifTrue: [
                        totalEarnings := totalEarnings + employee getEarning.
                    ].
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ne']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    (employee getEarning) ~= threshold ifTrue: [
                        totalEarnings := totalEarnings + employee getEarning.
                    ].
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'gt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    (employee getEarning) > threshold ifTrue: [
                        totalEarnings := totalEarnings + employee getEarning.
                    ].
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'ge']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    (employee getEarning) >= threshold ifTrue: [
                        totalEarnings := totalEarnings + employee getEarning.
                    ].
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'lt']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    (employee getEarning) < threshold ifTrue: [
                        totalEarnings := totalEarnings + employee getEarning.
                    ].
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            (Smalltalk arguments size >= 3 and: [(Smalltalk arguments at: 3) = 'le']) ifTrue: [
                threshold := (Smalltalk arguments at: 4) asNumber.
                totalEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    (employee getEarning) <= threshold ifTrue: [
                        totalEarnings := totalEarnings + employee getEarning.
                    ].
                ].

                Transcript show: 'Total earnings of all employees: $', totalEarnings printString; nl.
            ].
            
        ].
        (Smalltalk arguments at: 2) = 'avg' ifTrue: [
            (Smalltalk arguments size < 3) ifTrue: [
                | totalEarnings avgEarnings countEmployees |
                countEmployees := employees size.
                totalEarnings := 0.0.
                avgEarnings := 0.0.

                employees do: [:each | 
                    | employee |
                    employee := nil.

                    (each at: 1) = 'salaried' ifTrue: [
                        employee := SalariedEmployee new.
                        employee initializeSalary: (each at: 4).
                    ].

                    (each at: 1) = 'hourly' ifTrue: [
                        employee := HourlyEmployee new.
                        employee initializeHours: (each at: 4).
                        employee initializeRate: (each at: 5).
                    ].

                    (each at: 1) = 'commission' ifTrue: [
                        employee := CommissionEmployee new.
                        employee initializeWeeklyPay: (each at: 4).
                        employee initializeSalesAmount: (each at: 5).
                        employee initializeCommissionRate: (each at: 6).
                    ].

                    totalEarnings := totalEarnings + (employee getEarning).
                ].
                avgEarnings := totalEarnings / countEmployees.
                Transcript show: 'Average earnings of all employees: $', avgEarnings printString; nl.
            ].
        ].
    ].

].
