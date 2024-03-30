#ifndef HOURLYEMPLOYEE_H
#define HOURLYEMPLOYEE_H

#include "Employee.h"

using namespace std;

class HourlyEmployee : public Employee {
public:
    HourlyEmployee(const string&, const string&, double, double);
    virtual ~HourlyEmployee() = default;
    
    virtual double getEarning() const override;
    virtual string getInfo() const override;

private:
    double hoursWorked;
    double hourlyRate;
};

#endif 
