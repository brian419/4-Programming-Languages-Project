#include <sstream>
#include <iomanip>
#include "salariedEmployee.h"

using namespace std;

SalariedEmployee::SalariedEmployee(const string& first, const string& last, double salary)
    : Employee(first, last), weeklySalary(salary) {}

double SalariedEmployee::getEarning() const {
    return weeklySalary;
}

string SalariedEmployee::getInfo() const {
    ostringstream oss;

    oss << fixed << setprecision(2);
    oss << "Salaried Employee: " + Employee::getInfo() + "\nWeekly Salary: " << weeklySalary;

    return oss.str();
}