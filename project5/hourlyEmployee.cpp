#include <sstream>
#include <iomanip>
#include "hourlyEmployee.h"

using namespace std;

HourlyEmployee::HourlyEmployee(const string& first, const string& last, double hours, double rate)
    : Employee(first, last), hoursWorked(hours), hourlyRate(rate) {}

double HourlyEmployee::getEarning() const {
    double earning = 0;
    if (hoursWorked <= 40) {
        earning = hoursWorked * hourlyRate;
    } else if (hoursWorked <= 50) {
        earning = 40 * hourlyRate + (hoursWorked - 40) * hourlyRate * 1.5;
    } else {
        earning = 40 * hourlyRate + 10 * hourlyRate * 1.5 + (hoursWorked - 50) * hourlyRate * 2;
    }
    return earning;
}


string HourlyEmployee::getInfo() const {
    ostringstream oss;

    oss << fixed << setprecision(2);
    oss << "Hourly Employee: " + Employee::getInfo() + "\nHours Worked: " << hoursWorked <<
        ", Hourly Rate: " << hourlyRate;

    return oss.str();
}




