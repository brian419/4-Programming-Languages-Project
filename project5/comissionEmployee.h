#ifndef COMISSIONEMPLOYEE_H
#define COMISSIONEMPLOYEE_H

#include "Employee.h"

using namespace std;

class ComissionEmployee : public Employee {
public:
    ComissionEmployee(const string&, const string&, double, double, double);
    virtual ~ComissionEmployee() = default;

    virtual double getEarning() const override;
    virtual string getInfo() const override;

private:
    double salesAmount;
    double comissionRate;
    double minimalWeeklyPay;
};

#endif 
