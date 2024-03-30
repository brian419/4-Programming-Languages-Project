#include <sstream>
#include <iomanip>
#include "comissionEmployee.h"

using namespace std;

ComissionEmployee::ComissionEmployee(const string& first, const string& last, double basePay, double sales, double rate)
    : Employee(first, last), minimalWeeklyPay(basePay), salesAmount(sales), comissionRate(rate) {}

double ComissionEmployee::getEarning() const {
    return (salesAmount * comissionRate) > minimalWeeklyPay ? (salesAmount * comissionRate) : minimalWeeklyPay;
}


string ComissionEmployee::getInfo() const {
    ostringstream oss;

    oss << fixed << setprecision(2);
    oss << "Comission Employee: " + Employee::getInfo() +
           "\nminimum salary: " << minimalWeeklyPay <<
           ", sales amount: " << salesAmount <<
           ", comission rate: " << comissionRate * 100 << "%";

    return oss.str();

}

