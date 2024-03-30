#ifndef SALARIEDEMPLOYEE_H
#define SALARIEDEMPLOYEE_H

#include "Employee.h"

using namespace std;

class SalariedEmployee : public Employee {
public:
    SalariedEmployee(const string&, const string&, double);
    virtual ~SalariedEmployee() = default;

    virtual double getEarning() const override;
    virtual string getInfo() const override;

private:
    double weeklySalary;
};

#endif 



// g++ -std=c++11 main.cpp reading.cpp salariedEmployee.cpp hourlyEmployee.cpp comissionEmployee.cpp Employee.cpp